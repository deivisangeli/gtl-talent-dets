"""Additive qualification scope; never repair headings or change legacy awards."""
import collections
import copy
import re
import types
from pathlib import Path

import education_regex as legacy
import enrich_event_study_sources as enrichment

VERSION = "postsecondary_v1"
FAMILIES = ("ba_bs", "ma", "ms", "phd", "bachelors_other", "masters_other",
            "doctorate_other", "postsecondary_credential")
EXTRA_LABELS = {"other_doctorate": ["DE", "DEng"],
                "other_credential": ["Kand", "Diploma", "Certificate", "Cert"]}
EXTRA_KEYS = {legacy.canon(s) for labels in EXTRA_LABELS.values() for s in labels}
EXTRA_PATTERN = re.compile(r"(?<![A-Za-z])(?:" + "|".join(
    legacy.spelling(s) for s in sorted(EXTRA_KEYS, key=lambda x: -len(x))) + r")(?![A-Za-z])", re.I)
LOWER_SCHOOL = re.compile(r"\b(?:high\s+sch\w*|secondary|elementary|grammar|preparatory)\b", re.I)
POSTSECONDARY = re.compile(r"\b(?:univ\w*|college|col\.?|polytech\w*|technische|medical\s+sch\w*|law\s+sch\w*|ETH|MIT)\b", re.I)
GENERIC_CREDENTIALS = {"dipl", "diploma", "cert", "certificate", "engr", "drs"}


def extended_parser():
    # Execute the frozen source in a separate namespace. Its functions resolve
    # the extended token table here; the imported legacy module is untouched.
    module = types.ModuleType("amws_postsecondary_token_extension")
    exec(compile(Path(legacy.__file__).read_text(encoding="utf-8"), legacy.__file__, "exec"), module.__dict__)
    module.LABELS = copy.deepcopy(module.LABELS)
    for group, labels in EXTRA_LABELS.items():
        module.LABELS[group].extend(labels)
    module.LABEL_GROUP = {module.canon(label): group for group, labels in module.LABELS.items() for label in labels}
    module.DEGREE = re.compile(r"(?<![A-Za-z])(?:" + "|".join(
        module.spelling(s) + (r"(?=[fItl][a-z])" if module.LABEL_GROUP[s] == "phd" else r"(?!)") + "|" +
        module.spelling(s) + r"(?![A-Za-z])" for s in sorted(module.LABEL_GROUP, key=lambda x: (-len(x), x))) + r")", re.I)
    module.RULE_VERSION = VERSION
    return module


EXTENDED = extended_parser()


def extra_awards(text, birth_year=None, edition=1986):
    if not EXTRA_PATTERN.search(text):
        return []
    # Same heading and career-boundary rules as multi_v03.
    result = []
    for d in EXTENDED.extract(text, birth_year, edition)["degrees"]:
        key = legacy.canon(d["label_as_written"])
        if key not in EXTRA_KEYS:
            continue
        # D.E. is a degree, but French/Spanish 'de' and an unqualified state
        # abbreviation embedded in a school name are not degree evidence.
        if key == "de":
            label = d["label_as_written"]
            if label.islower() and "." not in label:
                continue
            before = text[max(0, d["label_start"] - 15):d["label_start"]]
            if re.search(r"\b(?:Univ|Col|State)\s*$", before, re.I) and not d["year_raw"]:
                continue
        result.append(d)
    return result


def qualification(award):
    """Return family and eligibility; uncertain levels cannot prove local training."""
    if str(award.get("honorary", False)).lower() == "true":
        return None, "honorary"
    existing = award.get("requested_family")
    if existing in FAMILIES[:4]:
        return existing, "eligible"
    group = award["group"]
    key = legacy.canon(award["label_as_written"])
    if key == "dr":
        return None, "title"
    family = {"bachelors": "ba_bs", "other_bachelor": "bachelors_other",
              "masters_academic": "masters_other", "masters_professional": "masters_other",
              "phd": "phd", "other_doctorate": "doctorate_other",
              "other_credential": "postsecondary_credential"}.get(group)
    if family is None:
        return None, "not_a_qualification"
    if group == "masters_academic":
        family = enrichment.family(award, legacy) or "masters_other"
    institution = award.get("institution_as_written") or ""
    if LOWER_SCHOOL.search(institution):
        return None, "below_postsecondary"
    if family == "postsecondary_credential" and key in GENERIC_CREDENTIALS:
        if key == "engr" and not institution:
            return None, "title"
        if not POSTSECONDARY.search(institution):
            return family, "unresolved_level"
    return family, "eligible"


def fingerprint(award):
    return (award["source_key"], int(award["label_start"]), int(award["label_end"]), award["label_as_written"])


def select_for_research(award):
    return award.get("requested_family") in FAMILIES and award.get("training_eligibility") == "eligible" and bool(award.get("institution_as_written"))
