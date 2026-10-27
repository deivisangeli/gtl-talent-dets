"""Post-evaluation scope correction; vocabulary is unchanged from frozen v1.

This candidate has no fresh held-out evaluation. The original frozen classifier
and its genuine held-out results are preserved in the pilot output directory.
"""
import re
from classifier import classify_entry as frozen_v1_classify, DATE

VERSION = 'amws_ed16_science_v1_1_boundary_guard'
GUARD = re.compile(
    r'\bP(?:ROF|RODF|ROL|RCI|RO|[\ufffd\u00b0]F)\s*E(?:XP|IP|VN)\b'
    r'|\b(?:UNIV|UNIVERSITY|COLLEGE|COL|INST|PHD|MD|DVM|BSC|MSC|BS|BA|MS|MA|BEE|AB|SB)\b',
    re.I)


def classify_entry(field_raw, raw_text_adjusted, name_raw=''):
    start = len(name_raw) if name_raw and raw_text_adjusted.startswith(name_raw) else 0
    birth = re.search(r'(?:^|[,.;\s])b\s+', raw_text_adjusted[start:], re.I)
    if birth:
        start += birth.end()
    dob = DATE.search(raw_text_adjusted, start, min(start + 120, len(raw_text_adjusted)))
    if dob:
        start = dob.end()
    match = GUARD.search(raw_text_adjusted, start)
    end = match.start() if match else len(raw_text_adjusted)
    result = frozen_v1_classify(field_raw, raw_text_adjusted[:end], name_raw)
    result.update(rules_version=VERSION, scope_guard_offset=end,
                  scope_guard_trigger=match.group() if match else '',
                  evaluation_status='posthoc_no_fresh_holdout')
    return result
