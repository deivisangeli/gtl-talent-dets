"""Label-free regex runner. Frozen configuration contains the audited vocabulary."""
import re

WORD = r"[A-Z][A-Z'’()-]*"
GIVEN = rf"{WORD}(?:[ \t]+{WORD}){{0,5}}"
SUFFIX = r"(?:JR|SR|II|III|IV|V|VI|3RD|\(JR\)|\(SR\))"


def flags(name):
    out = []
    if not name.strip():
        return ["missing_name"]
    if name.count("(") != name.count(")"):
        out.append("unbalanced_parentheses")
    if re.search(r"[^A-ZÀ-ÖØ-Þ ,()'’\-]", name):
        out.append("unexpected_name_glyph")
    parts = name.split(",")
    if len(parts) == 1:
        out.append("missing_surname_separator")
    if len(parts) > 3 or (len(parts) == 3 and not re.fullmatch(SUFFIX, parts[2].strip())):
        out.append("ambiguous_internal_separator")
    if len(parts) == 2 and re.fullmatch(SUFFIX, parts[1].strip()):
        out.append("incomplete_suffix_only_name")
    if re.search(r"\b(?:UNIV|COL|HOSP|LAB|DEPT)\b|[0-9]", name):
        out.append("possible_prose_or_ocr")
    return out


def structural_ok(name):
    return not set(flags(name)) & {"unbalanced_parentheses", "unexpected_name_glyph",
                                    "ambiguous_internal_separator", "incomplete_suffix_only_name",
                                    "possible_prose_or_ocr"}


def surname_alternation(surnames):
    return "(?:" + "|".join(re.escape(s).replace(r"\ ", r"[ \t]+")
                             for s in sorted(set(surnames), key=lambda s: (-len(s), s))) + ")"


def apply(name, configuration, disabled=()):
    traces = []
    text = name
    surnames = surname_alternation(configuration["audited_surnames"])
    for rule in configuration["rules"]:
        rule_id = rule["rule_id"]
        if rule_id in disabled:
            continue
        before = text
        matches = []
        if rule["kind"] == "spacing":
            pattern = rule["pattern"]
            matches = [dict(start=m.start(), end=m.end(), text=m.group()) for m in re.finditer(pattern, text)]
            text = re.sub(pattern, rule["replacement"], text)
        elif rule["kind"] == "word_joins" and structural_ok(text) and "," in text:
            surname, rest = text.split(",", 1)
            for pair in rule["surname_joins"]:
                pattern = "^" + pair["pattern"] + "$"
                if re.fullmatch(pattern, surname):
                    matches.append(dict(text=surname, replacement=pair["replacement"], component="surname"))
                    surname = pair["replacement"]
            for pair in rule["given_joins"]:
                pattern = pair["pattern"]
                for m in re.finditer(pattern, rest):
                    matches.append(dict(text=m.group(), replacement=pair["replacement"], component="given_names"))
                rest = re.sub(pattern, pair["replacement"], rest)
            text = surname + "," + rest
        elif rule["kind"] in {"surname_separator", "broad_separator"} and "," not in text and structural_ok(text):
            head = surnames if rule["kind"] == "surname_separator" else r"[A-Z][A-Z'’-]{1,35}"
            m = re.fullmatch(rf"(?P<surname>{head})[ \t]+(?P<given>{GIVEN})", text)
            if m and not re.fullmatch(SUFFIX, m["given"]):
                matches.append(dict(text=m.group(), surname=m["surname"], given_names=m["given"]))
                text = m["surname"] + ", " + m["given"]
        elif rule["kind"] == "prefix":
            m = re.fullmatch(rule["pattern"] + rf"(?P<name>{surnames}, {GIVEN}(?:, {SUFFIX})?)", text)
            if m and structural_ok(m["name"]):
                matches.append(dict(text=m.group(), replacement=m["name"]))
                text = m["name"]
        if text != before:
            traces.append(dict(rule_id=rule_id, before=before, after=text, matches=matches))
    return dict(name_regex_cleaned=text, rule_ids=[t["rule_id"] for t in traces],
                trace=traces, review_flags=flags(text))


def candidate_configuration(surnames):
    return dict(version="name_regex_v1", audited_surnames=sorted(set(surnames)), rules=[
        dict(rule_id="S01_comma_spacing", kind="spacing", pattern=r",[ \t]*(?=\S)", replacement=", ",
             description="Exactly one space after existing name commas; letters and boundaries unchanged"),
        dict(rule_id="S02_reviewed_word_joins", kind="word_joins",
             given_joins=[dict(pattern=rf"(?<![A-Z]){p}(?![A-Z])", replacement=r) for p, r in
                          [(r"EDW[ \t]+ARD", "EDWARD"), (r"LAW[ \t]+RENCE", "LAWRENCE"),
                           (r"LOW[ \t]+ELL", "LOWELL"), (r"W[ \t]+ARREN", "WARREN")]],
             surname_joins=[dict(pattern=p, replacement=r) for p, r in
                            [(r"SCHW[ \t]+ARTZ", "SCHWARTZ"), (r"W[ \t]+ARNER", "WARNER"),
                             (r"W[ \t]+ARREN", "WARREN")]],
             description="Only explicitly reviewed word splits, within the relevant name component; skip damaged layouts"),
        dict(rule_id="S03_audited_surname_separator", kind="surname_separator",
             description="Insert a separator only after a complete audited surname, longest multiword surnames first"),
        dict(rule_id="S04_prior_entry_prefix", kind="prefix",
             pattern=r"^(?:UNIV|COL|HOSP|LAB|INC|DEPT),?\s+\d{2,4}[-, ]+",
             description="Drop bounded institution/year spillover before an intact audited surname-first name"),
    ])
