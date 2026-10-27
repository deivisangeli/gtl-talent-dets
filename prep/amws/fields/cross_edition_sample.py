"""Source-preserving 125-per-edition manual field sample; no classifier fitting."""
import argparse
import collections
import csv
import json
import os
from pathlib import Path
import random
import shutil
import subprocess
import sys
from pilot import DEFAULT_ROOT, now, read, rows, sha, write

NAME='field_classification_sample_20261023'
RELEASE='output/amws/amws_education_postsecondary_v06_20261021/amws_open_alex.csv'
BIOS='output/amws/education_enriched_20261011_updated_final/source_biographies.csv'
OLD='output/amws/physical_science_dummy_20261020'
SIZES={'1906':3555,'1938':18963,'1955':32720,'1986':79073}
csv.field_size_limit(16*1024*1024)


def paths(root):
    rels=[RELEASE,BIOS,OLD+'/amws_ed16_physical_science_audit.csv',OLD+'/amws_ed16_physical_science_dummy.csv',
          OLD+'/manifest.json','input/amws/amws_1906.xlsx','input/amws/amws_1938.dta','input/amws/amws_1955.dta']
    rels += ['output/amws/field_regex_pilot_20261020/sample_manifest.json',
             'output/amws/physical_science_accuracy_audit_20261020/blind_sample.json',
             'output/amws/field_of_work_sample_20261020/sampling_metadata.json',
             'output/amws/field_of_work_sample_20261020/manual_reviews.json']
    rels += ['output/amws/physical_science_accuracy_audit_20261020/manual_labels.json',
             'output/amws/physical_science_accuracy_audit_20261020/manifest.json']
    return [root/p for p in rels]


def checks(root): return {str(p):sha(p) for p in paths(root)}


def sample_keys(root):
    pools=collections.defaultdict(list)
    for r in rows(root/RELEASE): pools[r['source_key'].split(':')[0]].append(r['source_key'])
    assert {ed:len(v) for ed,v in pools.items()}==SIZES
    assert all(len(v)==len(set(v)) for v in pools.values())
    rng=random.Random(20261023)
    return [k for ed in SIZES for k in rng.sample(sorted(pools[ed]),125)]


def prepare(root,out,rscript):
    if (out/'sample_manifest.json').exists():
        assert checks(root)==read(out/'sample_manifest.json')['input_checksums_sha256']
        if (out/'sample_records_frozen.json').exists():
            assert sha(out/'sample_records.json')==read(out/'sample_records_frozen.json')['sha256']
            return
    before=checks(root); keys=sample_keys(root); wanted=set(keys)
    assert len(wanted)==500
    # Freeze keys and inputs before loading fields or any manual review.
    if not (out/'sample_manifest.json').exists():
        write(out/'sample_manifest.json',dict(created_at_utc=now(),seed=20261023,
        population_sizes=SIZES,sample_size=500,sample_per_edition=125,
        method='One random.Random(20261023), chronological editions, sample(sorted(keys),125) per edition',
        sampled_keys_in_order=keys,input_checksums_sha256=before,
        inclusion='All retained v06 entries; no exclusions for prior sampling',
            unit='edition-specific source_key'),True)
    subprocess.run([rscript,str(Path(__file__).with_name('cross_edition_sources.R')),str(root),str(out)],check=True)
    early=read(out/'early_source_records.json'); early_map={r['source_key']:r for r in early}
    assert len(early_map)==375
    biographies={r['source_key']:r for r in rows(root/BIOS) if r['source_key'] in wanted}
    assert set(biographies)==wanted
    old_audit={r['source_key']:r for r in rows(root/OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in wanted}
    old_dummy={r['source_key']:r for r in rows(root/OLD/'amws_ed16_physical_science_dummy.csv') if r['source_key'] in wanted}
    assert set(old_audit)==set(old_dummy)=={k for k in keys if k.startswith('1986:')}
    release={r['source_key']:r for r in rows(root/RELEASE) if r['source_key'] in wanted}
    pilot=set(read(root/'output/amws/field_regex_pilot_20261020/sample_manifest.json')['sample_keys_in_draw_order'])
    audit={r['source_key'] for r in read(root/'output/amws/physical_science_accuracy_audit_20261020/blind_sample.json')}
    prior100=read(root/'output/amws/field_of_work_sample_20261020/manual_reviews.json')
    if isinstance(prior100,dict):
        prior100keys=set(prior100)
    else: prior100keys={r['source_key'] for r in prior100}
    records=[]
    for i,k in enumerate(keys,1):
        ed=k.split(':')[0]; b=biographies[k]; r=release[k]
        overlaps=[name for name,known in [('original_500_pilot',pilot),('independent_500_audit',audit),('original_100_sample',prior100keys)] if k in known]
        if ed!='1986':
            src=early_map[k]; field=src['field_raw']; name=src['name_raw']; provenance=dict(source_file=src['source_file'],source_column=src['source_column'],lineid=src['lineid'])
            existing=None
        else:
            a=old_audit[k]; d=old_dummy[k]
            assert r['physical_science']==a['physical_science']==d['physical_science']
            assert r['physical_science_mixed']==a['mixed'] and r['physical_science_unresolved']==a['unresolved']
            field=a['field_raw']; name=a['name_raw']; provenance=dict(source_file=str(root/OLD/'amws_ed16_physical_science_audit.csv'),source_column='field_raw',source_references_json=a['source_references_json'])
            existing={c:int(a[c]) for c in ['physical_science','mixed','unresolved']}
        records.append(dict(sample_id=i,source_key=k,edition=ed,name_raw=name,field_raw=field,
            raw_text_adjusted=b['raw_text_adjusted'],biography_source_file=b['source_file'],
            biography_source_column=b['source_column'],source_provenance=provenance,
            existing_1986_flags=existing,prior_sample_overlap=overlaps,
            population_weight=SIZES[ed]/134311,expansion_weight=SIZES[ed]/125))
    write(out/'sample_records.json',records,True)
    write(out/'sample_records_frozen.json',dict(created_at_utc=now(),sha256=sha(out/'sample_records.json')),True)
    assert checks(root)==before
    print('Frozen 500 keys and source records: 125 per edition.')


def review(out,start,end,full):
    for r in read(out/'sample_records.json'):
        if start<=r['sample_id']<=end:
            print(f"[{r['sample_id']}] {r['source_key']} {r['name_raw']} | FIELD: {r['field_raw']}")
            if full or not r['field_raw'].strip(): print('BIO: '+r['raw_text_adjusted'])


def add(out,file,revise=False):
    assert not (out/'manual_frozen.json').exists()
    sample={r['sample_id']:r for r in read(out/'sample_records.json')}
    gold=read(out/'manual_decisions.json') if (out/'manual_decisions.json').exists() else []
    seen={g['sample_id'] for g in gold}
    for decision in read(file):
        # Explicit reviewer input: id, normalized field, physical, mixed, evidence source,
        # literal excerpt, reasoning, optional confidence, optional source-field usability.
        if len(decision)==4:
            i,field,physical,mixed=decision
            source='field_raw'; evidence=sample[i]['field_raw']; extra=[]
            reason='Stated source discipline: '+field+'. '+('Includes Physical and Biological work.' if mixed else 'Physical work is established.' if physical==1 else 'No separate Physical discipline is stated.')
        else:
            i,field,physical,mixed,source,evidence,reason,*extra=decision
        if revise:
            assert i in seen
            prior=next(g for g in gold if g['sample_id']==i)
            log=read(out/'manual_revision_log.json') if (out/'manual_revision_log.json').exists() else []
            log.append(dict(revised_at_utc=now(),previous=prior,explicit_manual_replacement=decision))
            write(out/'manual_revision_log.json',log)
            gold=[g for g in gold if g['sample_id']!=i]; seen.remove(i)
        assert i not in seen and physical in [0,1,'unclear'] and mixed in [0,1]
        assert not mixed or physical==1
        r=sample[i]; assert source in ['field_raw','raw_text_adjusted']
        start=r[source].find(evidence); assert evidence and start>=0,(i,evidence)
        gold.append(dict(sample_id=i,source_key=r['source_key'],normalized_field=field,
            review_physical_science=physical,review_mixed=mixed,review_unresolved=int(physical=='unclear'),
            evidence_source=source,evidence=evidence,evidence_start=start,evidence_end=start+len(evidence),
            reasoning=reason,confidence=extra[0] if extra else 'high',
            source_field_usable=extra[1] if len(extra)>1 else bool(r['field_raw'].strip())))
        seen.add(i)
    write(out/'manual_decisions.json',sorted(gold,key=lambda g:g['sample_id']))
    print(f'{len(gold)}/500 manual records saved.')


def freeze(out):
    sample=read(out/'sample_records.json'); gold=read(out/'manual_decisions.json')
    assert len(gold)==500 and [g['source_key'] for g in gold]==[r['source_key'] for r in sample]
    write(out/'manual_frozen.json',dict(created_at_utc=now(),manual_sha256=sha(out/'manual_decisions.json'),
        method='Each entry manually reviewed using source field and biography; existing 1986 flags retained separately.'),True)
    print('500 manual decisions frozen.')


def publish(root,out):
    assert checks(root)==read(out/'sample_manifest.json')['input_checksums_sha256']
    assert sha(out/'manual_decisions.json')==read(out/'manual_frozen.json')['manual_sha256']
    sample=read(out/'sample_records.json'); gold=read(out/'manual_decisions.json'); final=[]
    for r,g in zip(sample,gold,strict=True):
        assert r['source_key']==g['source_key']
        assert r[g['evidence_source']][g['evidence_start']:g['evidence_end']]==g['evidence']
        existing=r['existing_1986_flags']
        flags=existing or dict(physical_science=int(g['review_physical_science']==1),mixed=g['review_mixed'],unresolved=g['review_unresolved'])
        disagreement=int(existing is not None and g['review_physical_science']!='unclear' and existing['physical_science']!=g['review_physical_science'])
        method='existing_1986_regex' if existing else 'manual_source_field' if g['source_field_usable'] else 'manual_biography_recovery'
        field_method='source_field_normalized' if g['source_field_usable'] else 'unresolved' if g['review_unresolved'] else 'manual_biography_recovery'
        final.append(dict(sample_id=r['sample_id'],source_key=r['source_key'],edition=r['edition'],name_raw=r['name_raw'],
            field_raw=r['field_raw'],normalized_field=g['normalized_field'],**flags,
            classification_method=method,field_recovery_method=field_method,
            source_field_nonblank=int(bool(r['field_raw'].strip())),
            source_field_usable=int(g['source_field_usable']),review_physical_science=g['review_physical_science'],
            review_mixed=g['review_mixed'],review_unresolved=g['review_unresolved'],review_disagreement=disagreement,
            field_identified=int(not g['review_unresolved']),confidence=g['confidence'],
            evidence_source=g['evidence_source'],evidence=g['evidence'],
            evidence_start=g['evidence_start'],evidence_end=g['evidence_end'],reasoning=g['reasoning'],
            source_provenance_json=json.dumps(r['source_provenance'],ensure_ascii=False),
            biography_source_file=r['biography_source_file'],biography_source_column=r['biography_source_column'],
            prior_sample_overlap=';'.join(r['prior_sample_overlap']),population_weight=r['population_weight'],
            expansion_weight=r['expansion_weight'],raw_text_adjusted=r['raw_text_adjusted']))
    write(out/'classified_records.json',final)
    columns=list(final[0]); write(out/'audit_payload.json',dict(columns=columns,rows=[[r[c] for c in columns] for r in final]))
    def stats(rs):
        return dict(n=len(rs),physical_science=sum(r['physical_science'] for r in rs),mixed=sum(r['mixed'] for r in rs),
            unresolved=sum(r['unresolved'] for r in rs),source_field_nonblank=sum(r['source_field_nonblank'] for r in rs),
            source_field_usable=sum(r['source_field_usable'] for r in rs),
            field_identified=sum(r['field_identified'] for r in rs),
            field_recovery_methods=dict(collections.Counter(r['field_recovery_method'] for r in rs)),
            review_physical_science=sum(r['review_physical_science']==1 for r in rs),
            review_mixed=sum(r['review_mixed'] for r in rs),
            review_unresolved=sum(r['review_unresolved'] for r in rs),review_disagreements=sum(r['review_disagreement'] for r in rs),
            methods=dict(collections.Counter(r['classification_method'] for r in rs)))
    by_ed={ed:stats([r for r in final if r['edition']==ed]) for ed in SIZES}
    weighted={flag:sum(SIZES[ed]/134311*by_ed[ed][flag]/125 for ed in SIZES)
              for flag in ['physical_science','mixed','unresolved','source_field_nonblank','source_field_usable','field_identified','review_physical_science','review_mixed','review_unresolved']}
    summary=dict(created_at_utc=now(),population_sizes=SIZES,sample_per_edition=125,by_edition=by_ed,
        raw_sample=stats(final),population_weighted_shares=weighted,
        source_inventory=read(out/'source_inventory.json'),
        prior_overlap_counts=dict(collections.Counter(o for r in sample for o in r['prior_sample_overlap'])),
        interpretation='Descriptive stratified sample. Earlier flags are manual; 1986 flags retain existing regex. Not classifier accuracy validation.')
    write(out/'summary.json',summary)
    lines=['AMWS CROSS-EDITION FIELD SAMPLE: OCTOBER 23, 2026','',
        '500 entries: 125 randomly sampled from each of 1906, 1938, 1955 and 1986.',
        'Source: v06 retained person release, 134,311 edition-specific entries.',
        'Seed 20261023; one Python random.Random generator in chronological edition order.',
        'Within each edition: sample(sorted(source_keys),125), without replacement.',
        'No exclusions for previous samples and no replacement of difficult records.',
        'All manual decisions are Codex interpretations, not an independent human benchmark.','',
        'Source file inspection (retained entries / nonblank original source fields):',
        '1906: 3,555 / 3,555; 1938: 18,963 / 18,827; 1955: 32,720 / 32,720.',
        '1906 field is workbook column 4 (no header); 1938 field_legacy and 1955',
        'specialization are read from original Stata files through haven. Every',
        'retained source key joins uniquely by edition and original lineid.',
        '1986 reuses the existing keyed field audit/dummy; its raw files are not reread.',
        'Biographies are joined by source_key to source_biographies.csv.','',
        f"Specific fields identified: {summary['raw_sample']['field_identified']}/500.",
        'All 375 earlier-edition sample entries had usable existing source fields.',
        'For 1986, 118 stored fields were usable; six others were recovered manually',
        'from biographies and one truncated biography remains unresolved. Original',
        'field_raw values, including blanks or debris, are preserved.',
        'Review of a broad valid field may use biography evidence to establish its',
        'Physical component without treating the original field as missing.','',
        'All 500 records received individual manual review, specific-field normalization,',
        'literal evidence, confidence and reasoning. Blank or unusable fields use',
        'substantive biography research/professional evidence. Degrees, affiliations',
        'and society memberships alone do not establish a Physical classification.',
        'For earlier editions the flags are manual decisions. Existing 1986 flags',
        'are preserved; review_* columns record manual interpretation separately.',
        'review_disagreement identifies a binary discrepancy without replacing old flags.',
        'This is not a new accuracy estimate for a cross-edition automated classifier.','',
        'Physical includes chemistry, physics, astronomy, Earth sciences, engineering,',
        'mathematics, statistics and computing. Physical + Biological receives 1.',
        'Biochemistry and biophysics alone remain Biological. Engineering includes',
        'biomedical and agricultural engineering. A zero means no established/detected',
        'Physical match; use unresolved flags to identify unknowns.',
        'mixed and unresolved are separate. The review_* flags can differ from the',
        'preserved 1986 flags, including its historical compound-term limitations.','',
        'Edition       n  Physical  Mixed  Unresolved  Field nonblank  Field usable']
    for ed,s in by_ed.items(): lines.append(f"{ed}        {s['n']:3d} {s['physical_science']:9d} {s['mixed']:6d} {s['unresolved']:11d} {s['source_field_nonblank']:15d} {s['source_field_usable']:13d}")
    lines+=['', 'Raw equally allocated sample counts:',json.dumps(summary['raw_sample'],indent=2),
        '', 'Population-weighted descriptive shares (sum_h N_h/N * count_h/125):']
    lines += [f'{k}: {v:.4%}' for k,v in weighted.items()]
    lines+=['Weights: population_weight=N_h/134311; expansion_weight=N_h/125.',
        'Each edition has equal sample size, not equal population size. Unweighted',
        'sample shares do not estimate pooled population shares. Weighted estimates',
        'combine manual earlier-edition flags with retained 1986 predictions.',
        'They are descriptive estimates, not a newly classified full population.','',
        'Previous-sample overlap counts: '+json.dumps(summary['prior_overlap_counts']),
        'Previous decisions remain available through the frozen source files; no',
        'prior result is silently overwritten.','', '1986 binary review disagreements:']
    for r in final:
        if r['review_disagreement']: lines += [f"{r['source_key']}: existing={r['physical_science']}, review={r['review_physical_science']}; {r['normalized_field']}. {r['reasoning']}"]
    lines+=['', 'Files: audit CSV, source inventory, frozen sample and manual decisions,',
        'classified_records.json, summary.json, validation.json, manifest.json and code/.',
        'Evidence offsets: zero-based Unicode code points; exclusive end, in the',
        'named evidence_source column. Preserve field_raw and raw_text_adjusted.',
        'Reproduce with cross_edition_sample.py prepare/review/add/freeze/publish,',
        'export_cross_edition_sample.mjs, then cross_edition_sample.py validate.',
        'Existing frozen manual decisions are inputs; no automatic relabeling.', '']
    (out/'report.txt').write_text('\n'.join(lines),encoding='utf-8')
    snap=out/'code'; snap.mkdir(exist_ok=True)
    for name in ['cross_edition_sample.py','cross_edition_sources.R','export_cross_edition_sample.mjs','pilot.py','CROSS_EDITION_SAMPLE.md']:
        shutil.copyfile(Path(__file__).with_name(name),snap/name)
    print(json.dumps(summary,indent=2))


def validate(root,out):
    m=read(out/'sample_manifest.json'); assert checks(root)==m['input_checksums_sha256']
    assert sample_keys(root)==m['sampled_keys_in_order']
    assert sha(out/'sample_records.json')==read(out/'sample_records_frozen.json')['sha256']
    assert sha(out/'manual_decisions.json')==read(out/'manual_frozen.json')['manual_sha256']
    data=read(out/'classified_records.json'); sample=read(out/'sample_records.json'); gold=read(out/'manual_decisions.json')
    subprocess.run([shutil.which('Rscript') or 'Rscript',str(Path(__file__).with_name('cross_edition_sources.R')),
                    str(root),str(out),'verify'],check=True)
    keys={r['source_key'] for r in sample}
    bios={r['source_key']:r for r in rows(root/BIOS) if r['source_key'] in keys}
    original={r['source_key']:r for r in rows(root/OLD/'amws_ed16_physical_science_audit.csv') if r['source_key'] in keys}
    early={r['source_key']:r for r in read(out/'early_source_records.json')}
    exported=list(rows(out/'amws_cross_edition_field_sample_500.csv'))
    assert len(data)==len(sample)==len(gold)==len(exported)==500
    assert [r['source_key'] for r in data]==sample_keys(root) and len({r['source_key'] for r in data})==500
    assert collections.Counter(r['edition'] for r in data)=={ed:125 for ed in SIZES}
    for r,s,g,c in zip(data,sample,gold,exported,strict=True):
        assert r['source_key']==s['source_key']==g['source_key']==c['source_key']
        assert all(c[k]==str(v) for k,v in r.items())
        assert s[g['evidence_source']][g['evidence_start']:g['evidence_end']]==g['evidence']
        assert s['raw_text_adjusted']==bios[s['source_key']]['raw_text_adjusted']
        if s['edition']!='1986':
            assert s['field_raw']==early[s['source_key']]['field_raw']
            assert s['name_raw']==early[s['source_key']]['name_raw']
        else:
            assert s['field_raw']==original[s['source_key']]['field_raw']
            assert all(r[flag]==int(original[s['source_key']][flag]) for flag in ['physical_science','mixed','unresolved'])
        assert r['physical_science'] in [0,1] and not(r['mixed'] and r['unresolved'])
        assert not r['mixed'] or r['physical_science']==1
        assert not r['unresolved'] or r['physical_science']==0
        if s['edition']=='1986': assert all(r[k]==v for k,v in s['existing_1986_flags'].items())
        else:
            assert r['physical_science']==int(g['review_physical_science']==1)
            assert r['mixed']==g['review_mixed'] and r['unresolved']==g['review_unresolved']
    summary=read(out/'summary.json')
    for ed in SIZES:
        subset=[r for r in data if r['edition']==ed]; stats=summary['by_edition'][ed]
        for flag in ['physical_science','mixed','unresolved','source_field_nonblank','source_field_usable','field_identified','review_mixed','review_unresolved']:
            assert stats[flag]==sum(r[flag] for r in subset)
        assert stats['field_recovery_methods']==dict(collections.Counter(r['field_recovery_method'] for r in subset))
    assert summary['raw_sample']['field_identified']==sum(r['field_identified'] for r in data)
    for flag,p in summary['population_weighted_shares'].items():
        assert abs(p-sum((int(r[flag]==1) if flag=='review_physical_science' else r[flag])*r['expansion_weight'] for r in data)/134311)<1e-12
    assert read(out/'export_validation.json')['artifact_tool_cell_preservation_passed']
    for name in ['cross_edition_sample.py','cross_edition_sources.R','export_cross_edition_sample.mjs','pilot.py','CROSS_EDITION_SAMPLE.md']:
        assert sha(out/'code'/name)==sha(Path(__file__).with_name(name))
    write(out/'validation.json',dict(status='PASS',validated_at_utc=now(),unique_keys=500,per_edition=125,
        source_joins_complete=True,seeded_draw_verified=True,original_source_checksums_unchanged=True,
        evidence_offsets_verified=500,preserved_1986_flags=125,csv_cells_preserved=True,counts_and_weights_reconciled=True))
    products={str(p.relative_to(out)):sha(p) for p in out.rglob('*') if p.is_file() and p.name!='manifest.json'}
    write(out/'manifest.json',dict(status='complete',created_at_utc=now(),input_checksums_sha256=m['input_checksums_sha256'],
        output_checksums_sha256=products,code_checksums_sha256={p.name:sha(p) for p in (out/'code').iterdir()},
        seed=20261023,manual_decisions_are_frozen_inputs=True,
        reproduction_command='python -X utf8 prep/amws/fields/cross_edition_sample.py prepare --data-root "<root>"; use frozen manual decisions; publish; node export_cross_edition_sample.mjs; validate'))
    print('PASS: 500 distinct keys, 125 per edition, exact source/evidence checks and preserved 1986 flags.')


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__); p.add_argument('command',choices=['prepare','review','add','revise','freeze','publish','validate'])
    p.add_argument('--data-root',type=Path,default=Path(os.environ.get('TALENT_DETS_DATA_DIR',DEFAULT_ROOT)))
    p.add_argument('--rscript',default=shutil.which('Rscript') or 'Rscript')
    p.add_argument('--start',type=int,default=1); p.add_argument('--end',type=int,default=500)
    p.add_argument('--full',action='store_true'); p.add_argument('--labels',type=Path)
    a=p.parse_args(); out=a.data_root/'output/amws'/NAME
    if a.command=='prepare': prepare(a.data_root,out,a.rscript)
    elif a.command=='review': review(out,a.start,a.end,a.full)
    elif a.command=='add': add(out,a.labels)
    elif a.command=='revise': add(out,a.labels,True)
    elif a.command=='freeze': freeze(out)
    elif a.command=='publish': publish(a.data_root,out)
    elif a.command=='validate': validate(a.data_root,out)
