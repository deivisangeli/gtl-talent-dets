"""All-earned-postsecondary award scope under the multi_v04 parser.

The same qualification rules as postsecondary_qualifications (postsecondary_v1),
applied to multi_v04 awards, plus one exclusion: specialty-board and society
diplomas/certificates (flag `board_certification`) are licences, not training.
The frozen modules are imported, never modified.
"""
import copy
import re
import types
from pathlib import Path

import education_regex_v04 as parser
import enrich_event_study_sources as enrichment
import postsecondary_qualifications as v1

VERSION = "postsecondary_v04"
FAMILIES = v1.FAMILIES
fingerprint = v1.fingerprint


def with_extra_labels(module):
    """Re-execute a parser source with the postsecondary label extension, as
    postsecondary_qualifications.extended_parser does for the frozen file."""
    ext = types.ModuleType(module.__name__ + "_postsecondary")
    exec(compile(Path(module.__file__).read_text(encoding="utf-8"), module.__file__, "exec"), ext.__dict__)
    ext.LABELS = copy.deepcopy(ext.LABELS)
    for group, labels in v1.EXTRA_LABELS.items():
        ext.LABELS[group].extend(labels)
    ext.LABEL_GROUP = {ext.canon(s): g for g, labels in ext.LABELS.items() for s in labels}
    if hasattr(ext, "build_degree"):
        ext.DEGREE = ext.build_degree(ext.LABEL_GROUP)
        return ext
    ext.DEGREE = re.compile(r"(?<![A-Za-z])(?:" + "|".join(
        ext.spelling(s) + (r"(?=[fItl][a-z])" if ext.LABEL_GROUP[s] == "phd" else r"(?!)") + "|" +
        ext.spelling(s) + r"(?![A-Za-z])" for s in sorted(ext.LABEL_GROUP, key=lambda x: (-len(x), x))) + r")", re.I)
    return ext


EXTENDED = with_extra_labels(parser)


def extra_awards(ext, text, birth_year=None, edition=1986):
    """postsecondary_qualifications.extra_awards against a given extended parser."""
    if not v1.EXTRA_PATTERN.search(text):
        return []
    out = []
    for d in ext.extract(text, birth_year, edition)["degrees"]:
        key = parser.canon(d["label_as_written"])
        if key not in v1.EXTRA_KEYS:
            continue
        # D.E. is a degree; French/Spanish 'de' and a state abbreviation in a
        # school name are not.
        if key == "de":
            label = d["label_as_written"]
            if label.islower() and "." not in label:
                continue
            before = text[max(0, d["label_start"] - 15):d["label_start"]]
            if re.search(r"\b(?:Univ|Col|State)\s*$", before, re.I) and not d["year_raw"]:
                continue
        out.append(d)
    return out


def qualification(award):
    """postsecondary_v1 family/eligibility, with board certifications excluded."""
    # Flags are a list on fresh parses and a JSON string on ledger rows.
    if "board_certification" in (award.get("flags") or []):
        return None, "board_certification"
    return v1.qualification(award)


def parse(text, birth_year=None, edition=1986):
    """multi_v04 record flags, the primary prediction, and every award in
    biography order with requested_family, family and training_eligibility."""
    primary = parser.extract(text, birth_year, edition)
    spans = {(d["label_start"], d["label_end"]) for d in primary["degrees"]}
    ds = [dict(d) for d in primary["degrees"]]
    ds += [dict(d) for d in extra_awards(EXTENDED, text, birth_year, edition) if (d["label_start"], d["label_end"]) not in spans]
    for d in ds:
        d["requested_family"] = enrichment.family(d, parser) or ""
        d["family"], d["training_eligibility"] = qualification(d)
    return primary, sorted(ds, key=lambda d: (d["label_start"], d["label_end"]))
