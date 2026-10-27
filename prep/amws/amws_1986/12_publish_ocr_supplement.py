"""Stage, verify, and publish appended AMWS files without changing existing rows."""
from __future__ import annotations
import argparse
from collections import Counter, defaultdict
import csv
import json
import os
from pathlib import Path
import re
import shutil
from difflib import SequenceMatcher
import sys
from datetime import datetime, timezone
from ocr_supplement import read_csv, write_csv, write_json, sha, dispositions, eligible, normalized, DEFAULT_ROOT,review_covers_flags

FILES=['Data/intermediary/amws/amws_ed86_full.csv','Data/processed/amws/amws_ed86.csv',
       'Data/processed/amws/amws_ed86_filtered.csv','Data/processed/amws/amws_ed86_final.csv']
COMBINED='output/amws/amws_all_editions_persons.csv'


def baseline(root,stage):
    marker=stage/'baseline_manifest.json'
    if marker.exists(): return json.loads(marker.read_text(encoding='utf-8'))
    paths=FILES+[p.replace('.csv','.xlsx') for p in FILES]+['output/amws/amws_combined_us_geocoded.csv']
    protected=['output/us_panel_county_amws_combined_year.csv','output/us_panel_county_amws_1986_year.csv',
               'output/amws/gender_inference/amws_gender_records.csv']
    records=[]
    for rel in paths+protected:
        path=root/rel
        if not path.exists(): continue
        records.append(dict(path=rel,sha256=sha(path),protected=rel not in paths))
        if rel in paths:
            target=stage/'baseline'/rel; target.parent.mkdir(parents=True,exist_ok=True)
            shutil.copy2(path,target);assert sha(target)==records[-1]['sha256']
    write_json(marker,records);return records


def row_key(r): return (r['doc_id'],r['lineid'],r['entry_instance'])


def duplicate_candidates(existing,entries,stage):
    by_name=defaultdict(list)
    by_text=defaultdict(list)
    clean_name=lambda s:re.sub(r'[^A-Z]','',s.upper())
    for r in existing:
        name=clean_name(r['name_raw'])
        if name and r['birth_year']:by_name[(name,r['birth_year'])].append(r)
        text=normalized(r['raw_text_adjusted'])
        if len(text)>50:by_text[text].append(r)
    candidates=[]
    for r in entries:
        text=normalized(r['raw_text_adjusted'])
        possible=by_name.get((clean_name(r['name_raw']),r['birth_year']),[])+by_text.get(text,[])
        seen=set()
        for old in possible:
            identity=(old['doc_id'],old['lineid'])
            if identity in seen:continue
            seen.add(identity)
            candidates.append(dict(source_key=r['source_key'],new_name=r['name_raw'],birth_year=r['birth_year'],
                existing_doc=old['doc_id'],existing_lineid=old['lineid'],existing_name=old['name_raw'],
                existing_birth_place=old['birth_place'],new_birth_place=r['birth_place'],
                similarity=round(SequenceMatcher(None,normalized(old['raw_text_adjusted']),normalized(r['raw_text_adjusted']),autojunk=False).ratio(),4)))
        # Also flag overlaps inside the supplement; a name match only opens review.
        name=clean_name(r['name_raw'])
        if name and r['birth_year']:
            by_name[(name,r['birth_year'])].append(r)
        if len(text)>50:by_text[text].append(r)
    write_csv(stage/'duplicate_candidates.csv',candidates, ['source_key','new_name','birth_year','existing_doc','existing_lineid','existing_name','existing_birth_place','new_birth_place','similarity'])
    return candidates


def build(root,stage):
    manifest=baseline(root,stage)
    entries=dispositions(stage)
    cols,old_processed=read_csv(stage/'baseline'/FILES[1])
    keys={row_key(r) for r in old_processed}
    candidates=duplicate_candidates(old_processed,entries,stage)
    decisions=json.loads((stage/'duplicate_decisions.json').read_text(encoding='utf-8')) if (stage/'duplicate_decisions.json').exists() else {}
    assert all(d.get('action') in ('duplicate','distinct') and d.get('evidence') for d in decisions.values()),'Duplicate decisions need a supported action and biography/source evidence'
    reviewed=json.loads((stage/'manual_overrides.json').read_text(encoding='utf-8')) if (stage/'manual_overrides.json').exists() else []
    reviewed={r['source_key']:r for r in reviewed}
    duplicates={key for key,d in decisions.items() if d['action']=='duplicate'}
    assert duplicates.issubset({r['source_key'] for r in entries}),'Stale duplicate decision'
    pending=set(r['source_key'] for r in candidates if r['source_key'] not in decisions)
    accepted=[];all_new=[];registry=[]
    for r in entries:
        if row_key(r) in keys: raise ValueError('New OCR key collides with existing source key')
        if r['source_key'] in duplicates:
            r['disposition']='duplicate';r['disposition_reason']=decisions[r['source_key']]['evidence']
        elif r['source_key'] in pending:
            r['disposition']='unresolved';r['disposition_reason']='duplicate_candidate_requires_review'
        elif r['review_reasons'] and not review_covers_flags(r,reviewed.get(r['source_key'],{})) and r['disposition']=='included':
            r['disposition']='unresolved';r['disposition_reason']='flagged_fields_require_scan_review'
        r['source_file']=str(root/'Data/amws ed 16'/r['source_file'])
        r['accounting_category']=r['disposition'] if r['disposition'] in ('included','duplicate','unresolved') else 'excluded'
        if r['disposition']!='duplicate':
            projected=dict(r)
            if r['disposition'] in ('notice','orphan'):
                for col in ('birth_place','birth_date','birth_year','birth_city','birth_state','birth_country','geo_lat','geo_lon','geo_geoid','geo_county_name','geo_matched_name'):projected[col]=''
                projected['is_us_birth']='FALSE';projected['is_us_geocoded']='FALSE';projected['geocoding_status']='no_birth_information'
            if r['disposition']=='unresolved':
                # Ownership/validity is not established. Keep candidate values in the
                # disposition ledger, but withhold the year from analysis-facing copies.
                projected['birth_year']=''
                if any(f in r['review_reasons'] for f in ('name_uncertain','name_outside_source_range','name_recognition_suspect','name_suffix_missing')):projected['name_raw']=''
                if any(f in r['review_reasons'] for f in ('birth_city_punct','birth_city_digits','birth_city_symbols','birth_city_section_text')):
                    projected['birth_city']=''
                if 'malformed_birth_date' in r['review_reasons']:projected['birth_date']=''
                if any(f in r['review_reasons'] for f in ('fuzzy_geocode','new_york_county_conflict')):
                    for col in ('geo_lat','geo_lon','geo_geoid','geo_county_name','geo_matched_name'):projected[col]=''
                    projected['is_us_geocoded']='FALSE';projected['geocoding_status']='unresolved_scan_review'
            all_new.append(projected)
        if r['disposition']=='included':accepted.append(r)
    out=stage/'candidates'
    for rel in FILES[:3]:
        schema,old=read_csv(stage/'baseline'/rel)
        additions=all_new if 'filtered' not in rel else [r for r in all_new if r['birth_city'] and r['birth_year']]
        # Unreviewed uncertain fields remain in full/processed; they do not enter final counts.
        rows=old+[{c:r.get(c,'') for c in schema} for r in additions]
        write_csv(out/rel,rows,schema)
        assert rows[:len(old)]==old
        print(rel,len(old),'->',len(rows),flush=True)
    final_cols,old_final=read_csv(stage/'baseline'/FILES[3])
    largest=max(int(r['entry_id']) for r in old_final)
    additions=[]
    for index,r in enumerate(accepted,largest+1):
        new={c:r.get(c,'') for c in final_cols};new['entry_id']=str(index);additions.append(new)
        registry.append(dict(source_key=r['source_key'],entry_id=str(index),doc_id=r['doc_id'],lineid=r['lineid'],entry_instance=r['entry_instance']))
    final=old_final+additions
    assert len({r['entry_id'] for r in final})==len(final) and all(eligible(r) for r in final)
    write_csv(out/FILES[3],final,final_cols)
    write_csv(stage/'source_registry.csv',registry,['source_key','entry_id','doc_id','lineid','entry_instance'])
    common=['edition','source_record_id']+[c for c in final_cols if c!='entry_id']
    _,early=read_csv(stage/'baseline/output/amws/amws_combined_us_geocoded.csv')
    combined=[]
    for r in early:
        if r['kept'].upper() not in ('TRUE','1'):continue
        row=dict(edition=r['edition'],source_record_id=f'{r["edition"]}:{r["lineid"]}',
          name_raw=f'{r["last"]}, {r["first"]}',raw_text_adjusted='',birth_place=r['birthplace_orig'],birth_date='',
          birth_year=r['birth_year'],birth_city=r['city'],birth_state=r['state'],birth_country='USA',is_us_birth='TRUE',
          is_us_geocoded='TRUE' if r['geoid'] else 'FALSE',geo_lat=r['lat'],geo_lon=r['lon'],geo_geoid=r['geoid'].zfill(5) if r['geoid'] else '',
          geo_county_name=r['county_name'],geo_matched_name='',geocoding_status='geocoded' if r['geoid'] else 'no_geocoder_match')
        combined.append(row)
    for r in final:combined.append(dict(edition='1986',source_record_id=f'1986:{r["entry_id"]}',**{c:r[c] for c in final_cols if c!='entry_id'}))
    # Geographic candidate rows in early files retain their existing membership.
    write_csv(out/COMBINED,combined,common)
    write_csv(stage/'entry_dispositions.csv',entries,list(entries[0]))
    _,pages=read_csv(stage/'pages.csv');_,assignments=read_csv(stage/'block_assignments.csv')
    disposition_by_key={r['source_key']:r['disposition'] for r in entries}
    by_page=defaultdict(set)
    for a in assignments:
        page_key=a['block_key'].rsplit(':',1)[0]
        by_page[page_key].add(a['source_key'])
    accounting=[]
    for page in pages:
        keys=by_page[page['page_key']]
        counts=Counter(disposition_by_key[k] for k in keys)
        accounting.append(dict(page, candidate_count=len(keys),
            accounting_status='accounted' if keys else 'blank_or_layout_only',
            reason='verified_blank_page_in_reviewed_delivery' if page['page_key']=='16_G_800_900.pdf:106' else ('all_body_blocks_assigned' if keys else 'no_body_blocks'),
            disposition_counts=json.dumps(dict(counts),sort_keys=True)))
    write_csv(stage/'page_accounting.csv',accounting)
    counts=Counter(r['disposition'] for r in entries)
    summary=dict(baseline_final=len(old_final),extracted_candidates=len(entries),added_final=len(additions),
        final_rows=len(final),combined_rows=len(combined),early_kept_rows=len(combined)-len(final),
        dispositions=dict(counts),pending_duplicates=len(pending),
        accounting_categories=dict(Counter(r['accounting_category'] for r in entries)),
        downstream_outputs='Analysis panels, gender inference, education enrichment and geolocation need a separate refresh.',
        unique_people_caveat='1986 is not deduplicated against earlier editions; totals are records under the existing convention.')
    write_json(stage/'summary.json',summary);print(json.dumps(summary,indent=2))


def verify_xlsx(csv_path,xlsx_path):
    import openpyxl
    columns,rows=read_csv(csv_path)
    wb=openpyxl.load_workbook(xlsx_path,read_only=True,data_only=False)
    assert len(wb.worksheets)==1
    iterator=wb.active.iter_rows(values_only=True)
    values=lambda row:[str(x) if x is not None else '' for x in row]
    assert values(next(iterator))==columns
    count=0
    for actual,expected in zip(iterator,rows,strict=True):
        assert values(actual)==[expected[c] for c in columns],f'CSV/XLSX mismatch at row {count+2}'
        count+=1
    wb.close();return count


def validate(root,stage):
    report=[]
    for rel in FILES+[COMBINED]:
        count=verify_xlsx(stage/'candidates'/rel,(stage/'candidates'/rel).with_suffix('.xlsx'))
        report.append(dict(path=rel,rows=count,csv_sha256=sha(stage/'candidates'/rel),xlsx_sha256=sha((stage/'candidates'/rel).with_suffix('.xlsx'))))
        print('Verified every cell:',rel,count,flush=True)
    _,pages=read_csv(stage/'pages.csv');assert len(pages)==1792
    _,blocks=read_csv(stage/'effective_blocks.csv');_,assigned=read_csv(stage/'block_assignments.csv')
    assert {r['block_key'] for r in blocks if r['block_status']=='body'}=={r['block_key'] for r in assigned}
    _,entries=read_csv(stage/'entry_dispositions.csv')
    assert len({r['source_key'] for r in entries})==len(entries)
    _,sample=read_csv(stage/'audit_sample200.csv')
    reviews=json.loads((stage/'audit_reviews.json').read_text(encoding='utf-8'))
    assert len(reviews)==200 and {r['source_key'] for r in sample}=={r['source_key'] for r in reviews}
    assert all(r['scan_reviewed'] and r['birth_fields_checked'] and r['segmentation_checked'] and r['geocode_checked'] for r in reviews)
    accepted_keys={r['source_key'] for r in entries if r['disposition']=='included'}
    assert all(r['source_key'] in accepted_keys for r in sample),'Audit must contain accepted biographies'
    assert len({r['source_file'] for r in sample})==14
    assert len(json.loads((stage/'known_issue_reviews.json').read_text(encoding='utf-8')))==33
    current={r['source_key']:r for r in entries}
    for r in reviews:
        assert sha(r['source_scan'])==r['scan_sha256']
        assert sha(r['evidence_image'])==r['evidence_sha256']
        assert all(current[r['source_key']][c]==v for c,v in r['fields'].items()), 'Audited field changed after scan review'
    for file,count in [('segmentation_guard_reviews.json',219),('interleaved_header_reviews.json',13)]:
        ledger=json.loads((stage/file).read_text(encoding='utf-8'))
        assert len(ledger)==count and all(r['scan_reviewed'] for r in ledger)
        for r in ledger:assert sha(r['source_scan'])==r['scan_sha256']
    for r in json.loads((stage/'known_issue_reviews.json').read_text(encoding='utf-8')):
        assert sha(r['scan_path'])==r['scan_sha256']
    summary=json.loads((stage/'summary.json').read_text(encoding='utf-8'))
    assert summary['pending_duplicates']==0
    assert sum(summary['accounting_categories'].values())==len(entries)
    assert summary['final_rows']==summary['baseline_final']+summary['added_final']
    assert summary['combined_rows']==summary['early_kept_rows']+summary['final_rows']
    _,registry=read_csv(stage/'source_registry.csv')
    assert len(registry)==summary['added_final'] and len({r['entry_id'] for r in registry})==len(registry)
    # Validate all new geocodes as strings and coordinates, without altering old records.
    for r in entries:
        if r['disposition']=='included' and r['birth_country']=='USA':
            assert re.fullmatch(r'\d{5}',r['geo_geoid'])
            assert -90<=float(r['geo_lat'])<=90 and -180<=float(r['geo_lon'])<=180
        if r['birth_year']: assert 1800<=int(r['birth_year'])<=1986
    published={}
    if (stage/'publication.json').exists():
        published={r['path']:r['sha256'] for r in json.loads((stage/'publication.json').read_text(encoding='utf-8'))['installed']}
        for rel,h in published.items():
            assert sha(root/rel)==h,'Published file changed after installation'
            if rel.endswith('.csv'):assert sha(stage/'candidates'/rel)==h,'Rerun changed published records'
    for m in json.loads((stage/'baseline_manifest.json').read_text(encoding='utf-8')):
        assert sha(root/m['path'])==published.get(m['path'],m['sha256']),'Official/protected input changed before publication'
    source=json.loads((stage/'input_manifest.json').read_text(encoding='utf-8'))
    for record in source['files']:
        assert sha(Path(source['source'])/record['path'])==record['sha256'],'Reviewed OCR source changed'
    write_json(stage/'validation.json',dict(status='passed',files=report,pages=1792,audit_records=200,known_issues=33))


def publish(root,stage):
    report=json.loads((stage/'validation.json').read_text(encoding='utf-8'))
    assert report['status']=='passed'
    files=FILES+[COMBINED]+[p.replace('.csv','.xlsx') for p in FILES+[COMBINED]]
    for checked in report['files']:
        assert sha(stage/'candidates'/checked['path'])==checked['csv_sha256'],'Staged CSV changed after validation'
        assert sha((stage/'candidates'/checked['path']).with_suffix('.xlsx'))==checked['xlsx_sha256'],'Staged XLSX changed after validation'
    manifest=json.loads((stage/'baseline_manifest.json').read_text(encoding='utf-8'))
    expected={m['path']:m['sha256'] for m in manifest}
    if (stage/'publication.json').exists():
        prior=json.loads((stage/'publication.json').read_text(encoding='utf-8'))
        assert all(sha(root/r['path'])==r['sha256'] for r in prior['installed'])
        print('Already published; verified unchanged outputs. No records appended twice.');return
    for rel,h in expected.items(): assert sha(root/rel)==h,'Baseline changed; refusing publication'
    backup=stage/'publication_backup';installed=[];new_files=[]
    for rel in files:
        target=root/rel
        if target.exists():
            saved=backup/rel;saved.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(target,saved)
            assert sha(saved)==sha(target)
        else:new_files.append(rel)
    try:
        for rel in files:
            target=root/rel;temporary=target.with_name(target.name+'.ocr-installing')
            shutil.copy2(stage/'candidates'/rel,temporary)
            assert sha(temporary)==sha(stage/'candidates'/rel)
            os.replace(temporary,target);installed.append(dict(path=rel,sha256=sha(target)))
        for rel in files:assert sha(root/rel)==sha(stage/'candidates'/rel)
        for rel,h in expected.items():
            if rel not in files:assert sha(root/rel)==h
    except BaseException:
        for rel in files:
            saved=backup/rel
            if saved.exists():shutil.copy2(saved,root/rel)
            elif rel in new_files and (root/rel).exists():(root/rel).unlink()
            temporary=(root/rel).with_name(Path(rel).name+'.ocr-installing')
            if temporary.exists():temporary.unlink()
        raise
    write_json(stage/'publication.json',dict(status='published',installed=installed,backup=str(backup),
      timestamp=datetime.now(timezone.utc).isoformat()))
    print('Published all 10 verified files with preserved baseline backups.')


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('command',choices=('baseline','build','validate','publish'))
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--stage',type=Path)
    a=p.parse_args();stage=a.stage or a.data_root/'output/amws/ocr_supplement_20261011'
    {'baseline':baseline,'build':build,'validate':validate,'publish':publish}[a.command](a.data_root,stage)


if __name__=='__main__':main()
