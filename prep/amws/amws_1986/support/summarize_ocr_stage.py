import csv,collections,re,sys
from pathlib import Path
stage=Path(sys.argv[1]);name=sys.argv[2] if len(sys.argv)>2 else 'entries.csv'
rows=list(csv.DictReader((stage/name).open(encoding='utf-8-sig')))
for col in ('regex_parse_flag','review_reasons','segmentation_rule','birth_country','disposition'):
    if col in rows[0]:print(col,collections.Counter(r[col] for r in rows).most_common(25))
bad=[r for r in rows if r.get('birth_place') and r.get('birth_date') and not re.match("^[A-Z][A-Z'’ .-]+, [A-Z]",r.get('name_raw',''))]
print('Name exceptions with birth information:',len(bad))
print([(r['source_key'],r.get('name_raw'),r.get('birth_place'),r.get('birth_date')) for r in bad[:20]])
if name=='review_candidates.csv':
    for reason in ('malformed_birth_date','fuzzy_geocode','multiple_biography_headers'):
        print(reason,[(r['source_key'],r.get('name_raw'),r.get('birth_date'),r.get('birth_city'),r.get('geo_matched_name'),r.get('raw_text_adjusted','')[:140]) for r in rows if reason in r['review_reasons']][:20])
