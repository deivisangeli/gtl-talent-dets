"""Post-hoc diagnostic adapter for verified earlier-edition discipline columns.

No keyword/rule/normalization changes. Supply an isolated, authoritative source
field to the frozen API instead of imposing the edition-16 biography layout.
This adapter was introduced after the original transport results were viewed.
"""


def classify_verified_field(mod,field_raw):
    if not mod.base.field_ok(field_raw):
        return dict(science_group='other',other_reason='missing' if not field_raw.strip() else 'damaged',
            review_required=True,physical_match=False,biological_match=False,
            input_source='verified_source_field_unusable',selected_heading='',matched_rule_ids=[],
            evidence=[],rules_version=mod.VERSION,scope_guard_offset=0,scope_guard_trigger='',ocr_assisted=False)
    # The added section marker only terminates the isolated field. It contributes
    # no classification evidence and is not presented as original biography text.
    p=mod.classify_entry(field_raw,field_raw+' Educ:','')
    for e in p['evidence']:
        assert field_raw[e['start']:e['end']]==e['evidence']
        e['evidence_source']='field_raw'
    p['input_source']='verified_original_source_field'
    p['scope_guard_offset']=len(field_raw)
    p['scope_guard_trigger']='isolated source field'
    return p
