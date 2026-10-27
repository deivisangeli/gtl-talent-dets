"""Sealed, manually referenced four-edition affiliation calibration (500+500)."""
import argparse
import collections
import csv
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import random
import re
import unicodedata

import amws_name_cleaning as base
import amws_workplace_feasibility as workplace

RELEASE="amws_university_affiliation_regex_20261023"
EDITIONS=workplace.EDITIONS
SAMPLE_FIELDS=["sample_id","partition","biography_fingerprint","person_fingerprint"]+[f for f in workplace.SAMPLE_FIELDS if f!="sample_id"]


def destination(root=None):
    return workplace.root_path(root)/"output/land_grants"/RELEASE


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def fingerprint(text):
    text=" ".join(unicodedata.normalize("NFKC",text).casefold().split())
    return hashlib.sha256(text.encode("utf-8")).hexdigest() if text else ""


def source_frame(root=None):
    thin=[]
    with base.locations(root)[0].open(encoding="utf-8-sig",newline="") as f:
        for index,row in enumerate(csv.DictReader(f),1):
            ed=row["source_key"].split(":")[0]
            name=row["name_raw"] if ed=="1986" else ", ".join(x for x in (row["last"],row["first"]) if x)
            thin.append(dict(source_key=row["source_key"],edition=ed,v06_source_row=str(index),v06_name=name,
                             v06_birth_year=row["birth_year"],v06_record_sha256=base.record_digest(row)))
    if len(thin)!=134311 or len({r["source_key"] for r in thin})!=len(thin):
        raise ValueError("v06 population changed")
    frame=workplace.original_entries(thin,root)
    for row in frame:
        row["biography_fingerprint"]=fingerprint(row["biography_text"])
        row["person_fingerprint"]=fingerprint(row["v06_name"]+"|"+row["v06_birth_year"]) if row["v06_name"].strip() and row["v06_birth_year"].strip() else ""
    return frame


def draw(frame,previous):
    previous=set(previous)
    development=[];rng=random.Random(49)
    for ed in EDITIONS:
        pool=sorted((r for r in frame if r["edition"]==ed and r["source_key"] not in previous),key=lambda r:r["source_key"])
        development.extend(rng.sample(pool,125))
    dev_ids={r["source_key"] for r in development}
    protected=[r for r in frame if r["source_key"] in previous|dev_ids]
    bios={r["biography_fingerprint"] for r in protected if r["biography_fingerprint"]}
    people={r["person_fingerprint"] for r in protected if r["person_fingerprint"]}
    test=[];rng=random.Random(50);pools={}
    for ed in EDITIONS:
        pool=sorted((r for r in frame if r["edition"]==ed and r["source_key"] not in previous|dev_ids
                     and (not r["biography_fingerprint"] or r["biography_fingerprint"] not in bios)
                     and (not r["person_fingerprint"] or r["person_fingerprint"] not in people)),key=lambda r:r["source_key"])
        pools[ed]=len(pool)
        test.extend(rng.sample(pool,125))
    return [dict(sample_id=str(i),partition="development" if i<=500 else "test",**row)
            for i,row in enumerate(development+test,1)],pools


def prepare(root=None):
    dest=destination(root)
    if dest.exists():raise ValueError("Release exists; do not resample")
    checks=workplace.protected_checksums(root)
    prior=workplace.destination(root)
    complete=read_json(prior/"completion_manifest.json")
    checks[str(prior/"completion_manifest.json")]=base.digest(prior/"completion_manifest.json")
    checks.update({str(prior/name):value for name,value in complete["output_checksums"].items()})
    workplace.verify_checksums(checks)
    frame=source_frame(root)
    old={r["source_key"] for r in base.read_csv(prior/"sample_80.csv")}
    sample,pools=draw(frame,old)
    dest.mkdir(parents=True)
    for part in ("development","test"):
        base.write_csv(dest/f"{part}_sample_500.csv",[r for r in sample if r["partition"]==part],SAMPLE_FIELDS)
    base.write_csv(dest/"sample_1000.csv",sample,SAMPLE_FIELDS)
    base.write_json(dest/"sample_manifest.json",dict(development_seed=49,test_seed=50,records_per_edition_partition=125,
                    algorithm="One RNG per partition; ascending editions; source_key-sorted pools; test excludes original80/development IDs and nonempty biography/person fingerprints",
                    fingerprint="NFKC + casefold + whitespace collapse; biography only; exact full v06 name + | + birth year when both present; no fuzzy linkage",
                    test_pool_rows=pools,sample_ids=[r["source_key"] for r in sample],input_checksums=checks,
                    frozen_samples={p.name:base.digest(p) for p in dest.glob("*_sample_500.csv")}|{"sample_1000.csv":base.digest(dest/"sample_1000.csv")},
                    prepared_at_utc=datetime.now(timezone.utc).isoformat()))
    for part in ("development","test"):(dest/f"{part}_reviews.jsonl").touch()
    return dest


def sample(partition,root=None):
    if partition not in {"development","test"}:raise ValueError("Unknown partition")
    if partition=="test" and not (destination(root)/"frozen_rules.json").exists():
        raise ValueError("Test biographies are sealed until rule freeze")
    return base.read_csv(destination(root)/f"{partition}_sample_500.csv")


def show(partition,start,end,root=None):
    for row in sample(partition,root):
        if start<=int(row["sample_id"])<=end:
            print(f"\n[{row['sample_id']}] {row['source_key']} NAME {row['raw_name']}")
            print("ADDRESS:",row["address_text"])
            print("FIELD:",row["field_text"])
            print("BIRTHPLACE:",row["birthplace_text"])
            print("BIO:",row["biography_text"])


def evidence(row,field,text,occurrence=1):
    if field not in {"address_text","biography_text"} or not text:
        raise ValueError("Evidence must be original employment-bearing text")
    start=-1
    for _ in range(occurrence):start=row[field].find(text,start+1)
    if start<0:raise ValueError(f"Literal text absent at {row['source_key']}: {text!r}")
    return dict(field=field,start=start,end=start+len(text),text=text)


def reviews(partition,root=None):
    return [json.loads(s) for s in (destination(root)/f"{partition}_reviews.jsonl").read_text(encoding="utf-8").splitlines() if s]


def check_review(review,row):
    if any(review[k]!=row[k] for k in ("sample_id","source_key","edition","partition")) or review["full_entry_reviewed"] is not True:
        raise ValueError("Review identity/coverage mismatch")
    if review["decision"] not in {"positive","negative","unresolved","positive_with_unresolved"} or not review["explanation"]:
        raise ValueError("Explicit review decision/explanation required")
    if bool(review["affiliations"]) != (review["decision"] in {"positive","positive_with_unresolved"}):
        raise ValueError("Positive reference mismatch")
    if ("unresolved" in review["decision"]) != bool(review["unresolved_evidence"]):
        raise ValueError("Unresolved references require evidence")
    for ev in [*review["negative_evidence"],*review["unresolved_evidence"],*[e for a in review["affiliations"] for e in [a["institution"],*a["alternatives"],*a["contexts"],*a["roles"],*a["dates"]]]]:
        text=row[ev["field"]]
        if ev["field"] not in {"address_text","biography_text"} or not ev["text"] or text[ev["start"]:ev["end"]]!=ev["text"]:
            raise ValueError("Reference evidence is not literal")
    for a in review["affiliations"]:
        if a["temporal_status"] not in {"current","past","mixed","uncertain"} or not a["contexts"] or not a["appointment_types"]:
            raise ValueError("Incomplete affiliation reference")


class ManualBatch:
    """Only explicitly supplied human institutions/contexts become references."""
    def __init__(self,partition="development",root=None):
        self.root=root;self.partition=partition
        self.rows={int(r["sample_id"]):r for r in sample(partition,root)}

    def ev(self,i,text,field="biography_text",occurrence=1):return evidence(self.rows[i],field,text,occurrence)

    def block(self,i,institution,start,end,roles=(),dates=(),temporal="uncertain",types=("regular",),field="biography_text",alternatives=()):
        """Human-selected bounds; no school or job is inferred from unselected text."""
        text=self.rows[i][field]
        lo=text.index(start);hi=text.index(end,lo)+len(end)
        return self.affiliation(i,institution,text[lo:hi],roles,dates,temporal,types,field,alternatives=alternatives)

    def affiliation(self,i,institution,context,roles=(),dates=(),temporal="uncertain",types=("regular",),field="biography_text",occurrence=1,alternatives=()):
        ev=self.ev(i,context,field,occurrence)
        within=context.find(institution)
        inst=dict(field=field,start=ev["start"]+within,end=ev["start"]+within+len(institution),text=institution)
        if within<0:raise ValueError("Institution must occur within the human-selected employment context")
        def inside(value):
            at=context.find(value)
            if at<0:raise ValueError(f"Role/date absent from context: {value}")
            return dict(field=field,start=ev["start"]+at,end=ev["start"]+at+len(value),text=value)
        return dict(institution=inst,contexts=[ev],roles=[inside(x) for x in roles],dates=[inside(x) for x in dates],
                    temporal_status=temporal,appointment_types=list(types),alternatives=[self.ev(i,*x) for x in alternatives])

    def entry(self,i,affiliations=(),negative=(),unresolved=(),explanation="Full original entry read; retain only explicitly supported university/college employment, excluding degree-only institutions and memberships."):
        row=self.rows[i]
        return dict(**{k:row[k] for k in ("sample_id","source_key","edition","partition")},full_entry_reviewed=True,
                    decision="positive_with_unresolved" if affiliations and unresolved else "positive" if affiliations else "unresolved" if unresolved else "negative",
                    affiliations=list(affiliations),negative_evidence=[self.ev(i,*x) for x in negative],
                    unresolved_evidence=[self.ev(i,*x) for x in unresolved],explanation=explanation)

    def save(self,items):
        dest=destination(self.root)
        if (dest/"completion_manifest.json").exists():raise ValueError("Completed release immutable")
        frozen=(dest/"frozen_rules.json").exists()
        if self.partition=="development" and frozen:raise ValueError("Development references frozen")
        if self.partition=="test" and not frozen:raise ValueError("Test references sealed")
        old=reviews(self.partition,self.root);seen={r["sample_id"] for r in old}
        for item in items:
            if item["sample_id"] in seen:raise ValueError("Duplicate review")
            check_review(item,self.rows[int(item["sample_id"])])
            seen.add(item["sample_id"])
        with (dest/f"{self.partition}_reviews.jsonl").open("a",encoding="utf-8",newline="\n") as f:
            for item in items:f.write(json.dumps(item,ensure_ascii=False)+"\n")
        return len(seen)


def checked_references(partition,root=None):
    rows=sample(partition,root);refs=reviews(partition,root)
    by={r['sample_id']:r for r in rows}
    if len(refs)!=500 or len({r['sample_id'] for r in refs})!=500 or set(by)!={r['sample_id'] for r in refs}:
        raise ValueError(f'Exactly 500 completed {partition} reviews required')
    for ref in refs:check_review(ref,by[ref['sample_id']])
    return rows,refs


def reference_groups(ref):
    """Appointment blocks with a common documented literal form score as one institution.

    No inferred identity/campus mapping. Full and abbreviated literal alternatives
    must have been explicitly included by the reviewer, before predictions.
    """
    from amws_university_affiliation_engine import norm
    groups=[]
    for a in ref['affiliations']:
        forms={norm(e['text']) for e in [a['institution'],*a['alternatives']]}
        hits=[g for g in groups if forms&g['forms']]
        group=dict(forms=forms,blocks=[a])
        for g in hits:group['forms']|=g['forms'];group['blocks']+=g['blocks'];groups.remove(g)
        groups.append(group)
    return groups


def expected_temporal(group):
    states={b['temporal_status'] for b in group['blocks']}
    if 'mixed' in states or {'current','past'}<=states:return 'mixed'
    if 'current' in states:return 'current'
    if 'uncertain' in states:return 'uncertain'
    return 'past'


def evaluate(rows,refs,version):
    from amws_university_affiliation_engine import extract_affiliations,norm
    references={r['sample_id']:r for r in refs};predictions=[];errors=[];counts={}
    def counter(ed,family='all'):
        return counts.setdefault((ed,family),collections.Counter())
    for row in rows:
        ref=references[row['sample_id']];groups=reference_groups(ref)
        result=extract_affiliations(row,row['edition'],version)
        predictions.append(dict(**{k:row[k] for k in ('sample_id','source_key','edition','partition')},version=version,**result))
        ed=row['edition'];total=counter(ed);total['entries']+=1;total['reference_institutions']+=len(groups)
        total['unresolved_entries']+=bool(ref['unresolved_evidence']);total['flagged_entries']+=bool(result['review_flags'])
        total['negative_entries']+=ref['decision']=='negative'
        unmatched=set(range(len(groups)));seen=set()
        for pred in result['affiliations']:
            fam=counter(ed,pred['rule_family']);key=norm(pred['institution']['text'])
            duplicate=key in seen;seen.add(key)
            for ct in (total,fam):ct['predicted_institutions']+=1;ct['duplicates']+=duplicate
            candidates=[i for i in sorted(unmatched) if key in groups[i]['forms']]
            if candidates and not duplicate:
                j=candidates[0];unmatched.remove(j);group=groups[j]
                for ct in (total,fam):
                    ct['true_positive']+=1;ct['temporal_denominator']+=1
                    ct['temporal_correct']+=pred['temporal_status']==expected_temporal(group)
                for field in ('roles','dates'):
                    gold={norm(ev['text']) for a in group['blocks'] for ev in a[field]}
                    proposed={norm(ev['text']) for ev in pred[field]}
                    for ct in (total,fam):
                        ct[field+'_reference']+=len(gold);ct[field+'_predicted']+=len(proposed);ct[field+'_matched']+=len(gold&proposed)
                if pred['temporal_status']!=expected_temporal(group):
                    errors.append(dict(sample_id=row['sample_id'],source_key=row['source_key'],edition=ed,partition=row['partition'],kind='temporal_mismatch',rule_family=pred['rule_family'],prediction=pred,reference=group['blocks']))
            else:
                for ct in (total,fam):ct['false_positive']+=1;ct['predictions_on_unresolved']+=bool(ref['unresolved_evidence'])
                errors.append(dict(sample_id=row['sample_id'],source_key=row['source_key'],edition=ed,partition=row['partition'],kind='duplicate_prediction' if duplicate else 'unsupported_or_literal_mismatch',rule_family=pred['rule_family'],prediction=pred,reference=ref))
        for j in sorted(unmatched):
            total['false_negative']+=1
            errors.append(dict(sample_id=row['sample_id'],source_key=row['source_key'],edition=ed,partition=row['partition'],kind='missed_reference_institution',rule_family='',prediction=None,reference=groups[j]['blocks']))
    metrics=[]
    for (ed,family),ct in sorted(counts.items()):
        tp=ct['true_positive'];fp=ct['false_positive'];fn=ct['false_negative']
        precision=tp/(tp+fp) if tp+fp else None
        recall=tp/(tp+fn) if family=='all' and tp+fn else None
        metrics.append(dict(edition=ed,rule_family=family,**dict(ct),precision=precision,recall=recall,
                            f1=2*precision*recall/(precision+recall) if precision is not None and recall is not None and precision+recall else 0 if family=='all' else None,
                            temporal_accuracy=ct['temporal_correct']/ct['temporal_denominator'] if ct['temporal_denominator'] else None,
                            precision_target_met=precision>=.95 if precision is not None else False))
    return dict(version=version,metrics=metrics,predictions=predictions,errors=errors)


def write_jsonl(path,rows):
    with Path(path).open('x',encoding='utf-8',newline='\n') as f:
        for row in rows:f.write(json.dumps(row,ensure_ascii=False)+'\n')


def save_evaluation(folder,result):
    folder.mkdir(parents=True,exist_ok=False)
    base.write_json(folder/'metrics.json',result['metrics'])
    fields=sorted({k for r in result['metrics'] for k in r})
    base.write_csv(folder/'metrics.csv',result['metrics'],fields)
    write_jsonl(folder/'predictions.jsonl',result['predictions'])
    write_jsonl(folder/'error_queue.jsonl',result['errors'])


def develop(root=None):
    import amws_university_affiliation_engine as engine
    dest=destination(root)
    if (dest/'frozen_rules.json').exists():raise ValueError('Rules already frozen')
    rows,refs=checked_references('development',root)
    for version in engine.VERSIONS:
        folder=dest/'development_versions'/version
        if folder.exists():
            spec=read_json(folder/'rule_specification.json')
            if not (folder/'engine.py').exists() or base.digest(folder/'engine.py')!=spec['engine_sha256']:raise ValueError('Missing original version engine snapshot')
            continue
        save_evaluation(folder,evaluate(rows,refs,version))
        (folder/'engine.py').write_bytes(Path(engine.__file__).read_bytes())
        base.write_json(folder/'rule_specification.json',dict(version=version,patterns=engine.PATTERNS,
            processing_order=['institutional_header','career_institution_marker','dated_academic_abbreviation','successive_institution_carry_forward'],
            disabled_families=['career_institution_marker','dated_academic_abbreviation','successive_institution_carry_forward'] if version=='v00_headers' else ['successive_institution_carry_forward'] if version=='v01_career' else [],
            preprocessing='Original Unicode text; no spelling repairs; evidence end-exclusive codepoint offsets. NFKC/casefold/alphanumeric normalization is used only for duplicate scoring.',
            engine_sha256=base.digest(Path(engine.__file__)),references_sha256=base.digest(dest/'development_reviews.jsonl')))
    return dest/'development_versions'


def freeze(root=None,version='v03_structural_guards'):
    import amws_university_affiliation_engine as engine
    dest=destination(root);checked_references('development',root)
    if (dest/'frozen_rules.json').exists():raise ValueError('Immutable freeze already exists')
    if version not in engine.VERSIONS:raise ValueError('Unknown version')
    folder=dest/'development_versions'/version
    metrics=read_json(folder/'metrics.json')
    if any(r['partition']=='test' for r in reviews('test',root)):raise ValueError('Test already opened')
    snapshots=dest/'frozen_code';snapshots.mkdir()
    for path in [Path(engine.__file__),Path(__file__)]:
        (snapshots/path.name).write_bytes(path.read_bytes())
    files={str(p):base.digest(p) for p in snapshots.iterdir()}
    files[str(dest/'development_reviews.jsonl')]=base.digest(dest/'development_reviews.jsonl')
    files[str(dest/'sample_manifest.json')]=base.digest(dest/'sample_manifest.json')
    for p in (dest/'development_versions').rglob('*'):
        if p.is_file():files[str(p)]=base.digest(p)
    base.write_json(dest/'frozen_rules.json',dict(version=version,frozen_at_utc=datetime.now(timezone.utc).isoformat(),
                    engine_sha256=base.digest(Path(engine.__file__)),frozen_checksums=files,
                    all_development_precision_targets_met=all(m['precision_target_met'] for m in metrics if m['rule_family']=='all'),
                    selection='Final pre-test structural experiment; targets are reported, including failures. No production recommendation is implied.',
                    test_biographies_displayed=False,test_predictions_evaluated=False))
    return dest/'frozen_rules.json'


def evaluate_test(root=None):
    import amws_university_affiliation_engine as engine
    dest=destination(root);frozen=read_json(dest/'frozen_rules.json')
    workplace.verify_checksums(frozen['frozen_checksums'])
    if base.digest(Path(engine.__file__))!=frozen['engine_sha256']:raise ValueError('Extractor changed since freeze')
    folder=dest/'test_evaluation'
    if folder.exists():raise ValueError('Single cached test evaluation already exists; do not rerun')
    rows,refs=checked_references('test',root)
    result=evaluate(rows,refs,frozen['version']);save_evaluation(folder,result)
    base.write_json(folder/'evaluation_manifest.json',dict(evaluations=1,evaluated_at_utc=datetime.now(timezone.utc).isoformat(),
        freeze_sha256=base.digest(dest/'frozen_rules.json'),references_sha256=base.digest(dest/'test_reviews.jsonl'),
        sample_sha256=base.digest(dest/'test_sample_500.csv'),engine_sha256=frozen['engine_sha256'],
        output_checksums={p.name:base.digest(p) for p in folder.iterdir() if p.is_file()}))
    return folder


def validate(root=None):
    import amws_university_affiliation_engine as engine
    dest=destination(root);manifest=read_json(dest/'sample_manifest.json');frozen=read_json(dest/'frozen_rules.json')
    workplace.verify_checksums(manifest['input_checksums']);workplace.verify_checksums(frozen['frozen_checksums'])
    for name,value in manifest['frozen_samples'].items():
        if base.digest(dest/name)!=value:raise ValueError('Frozen sample changed')
    frame=source_frame(root);previous={r['source_key'] for r in base.read_csv(workplace.destination(root)/'sample_80.csv')}
    reconstructed,pools=draw(frame,previous);stored=base.read_csv(dest/'sample_1000.csv')
    if stored!=[{k:r.get(k,'') for k in SAMPLE_FIELDS} for r in reconstructed]:raise ValueError('Source joins, original text or sampling failed reconstruction')
    for part in ('development','test'):
        rows,refs=checked_references(part,root)
        if collections.Counter(r['edition'] for r in rows)!={e:125 for e in EDITIONS}:raise ValueError('Edition allocation changed')
        # Determinism check applies to development only; do not re-evaluate test.
        if part=='development':
            for row in rows:
                if engine.extract_affiliations(row,version=frozen['version'])!=engine.extract_affiliations(row,version=frozen['version']):raise ValueError('Non-deterministic extraction')
    testmanifest=read_json(dest/'test_evaluation/evaluation_manifest.json')
    if testmanifest['evaluations']!=1:raise ValueError('Test evaluation count')
    for name,value in testmanifest['output_checksums'].items():
        if base.digest(dest/'test_evaluation'/name)!=value:raise ValueError('Cached evaluation changed')
    for part,path in [('development',dest/'development_versions'/frozen['version']),('test',dest/'test_evaluation')]:
        by={r['source_key']:r for r in sample(part,root)}
        for line in (path/'predictions.jsonl').read_text(encoding='utf-8').splitlines():
            result=json.loads(line);row=by[result['source_key']];seen=set()
            for a in result['affiliations']:
                key=engine.norm(a['institution']['text'])
                if key in seen:raise ValueError('Duplicate extraction');
                seen.add(key)
                for ev in [a['institution'],*a['contexts'],*a['roles'],*a['dates']]:
                    if row[ev['field']][ev['start']:ev['end']]!=ev['text']:raise ValueError('Prediction offsets changed source')
    result=dict(source_rows=len(frame),sample_rows=len(stored),completed_reviews=1000,unique_ids=len({r['source_key'] for r in stored}),
                records_per_edition_partition=125,reproducible_selection=True,original_source_joins_and_text_equal=True,
                protected_checksums_unchanged=True,conservative_duplicate_leakage_detected=0,
                test_evaluations=1,engine_unchanged_since_freeze=base.digest(Path(engine.__file__))==frozen['engine_sha256'])
    base.write_json(dest/'validation.json',result)
    return result


def finish(root=None):
    dest=destination(root)
    if (dest/'completion_manifest.json').exists():raise ValueError('Completed release immutable')
    result=validate(root);frozen=read_json(dest/'frozen_rules.json')
    dev=read_json(dest/'development_versions'/frozen['version']/'metrics.json');test=read_json(dest/'test_evaluation/metrics.json')
    lines=['# AMWS university-affiliation regex calibration','',
           '1,000 entries: 125 development and 125 independent test entries from each of 1906, 1938, 1955 and 1986. Full original fields and literal manual evidence are retained. All entries remain in the denominator.',
           '', 'The 500 development biographies were manually referenced before prediction evaluation. The engine, preprocessing, patterns, processing order, development references and version results were frozen before opening test biographies. All 500 test references were completed before the single cached test evaluation. Test errors were not used to change rules.',
           '', '## Institution extraction results','',
           '| Partition | Edition | Correct / predicted | Reference institutions | Precision | Recall | F1 | Temporal correct / matched | ≥95% precision |',
           '|---|---:|---:|---:|---:|---:|---:|---:|---|']
    for part,metrics in [('Development',dev),('Test',test)]:
        for m in metrics:
            if m['rule_family']!='all':continue
            pct=lambda k:f"{100*m[k]:.1f}%" if m[k] is not None else 'undefined'
            lines.append(f"| {part} | {m['edition']} | {m.get('true_positive',0)} / {m.get('predicted_institutions',0)} | {m.get('reference_institutions',0)} | {pct('precision')} | {pct('recall')} | {pct('f1')} | {m.get('temporal_correct',0)} / {m.get('temporal_denominator',0)} | {'met' if m['precision_target_met'] else 'not met'} |")
    lines+=['','## Interpretation and limitations','',
        'This is a calibration experiment, not production affiliation data. Unmet precision targets and missed recoverable affiliations remain failures. No population-wide accuracy or campus resolution is claimed. Strict literal-form mismatches are included as errors rather than silently accepted as institution identities.',
        '', 'Scoring is one-to-one within each record. Manually documented alternative literal forms define equivalence; repeated appointment blocks sharing such a form are consolidated for institution scoring. No inferred university-name dictionary, canonical identifiers, person-specific overrides or sample-specific spelling repairs are used. Role and printed-date counts in metrics.json are exact normalized literal matches conditional on a matched institution; they are not complete timeline accuracy. Temporal classes combine current and past spells into mixed. Empty prediction denominators yield undefined precision.',
        '', 'Biographies are checked for duplicate leakage using NFKC, case folding and whitespace collapse. Exact normalized v06 full-name/birth-year signatures also exclude test candidates when both fields exist. Empty fingerprints are ignored. These checks are conservative and do not establish complete person linkage.',
        '', '## Structural rules and failure cases','',
        'Institutional headers provide explicit publication-time affiliation in the first three editions. Dated academic roles can identify abbreviated institutions; bounded successive clauses can carry an academic role to another institution. The 1986 extractor bounds professional experience and concurrent positions and excludes education and membership sections. Full trace records retain the rule identifier, order, affected original span and extracted literal text.',
        '', 'Useful formats include “Instr. chem, [institution], 40-42” and “Prof Exp: ... PROF ..., [university]”. Difficult cases include missing career headings, OCR-damaged institution markers, institutions named only after several roles, multiple universities in a single comma clause, closed appointments followed by address-only affiliation, hospital/institute relationships and neighboring-entry spillover. Exact error examples and original evidence are in each error_queue.jsonl.',
        '', 'Address-only affiliation does not prove paid employment. Education, honorary degrees, memberships, trusteeships and residential addresses are excluded from manual employment references. Explicit negatives and unresolved cases are separate. Predictions on records with unresolved references are separately counted, and unresolved references never earn successful-extraction credit.',
        '', 'The generic extractor leaves unresolved dates unexpanded: resolved_years is empty. It preserves printed dates and roles with original offsets. Current/past classification is intentionally conservative; a closed appointment does not establish continuing employment. The code does not resolve historical institution identities, campuses or geographic locations.',
        '', 'Next work should use the error queues to design new general rules on development data, improve boundaries and institution carry-forward, and then draw a new independent test sample. The present test must not be reused for tuning.',
        '', '## Reproduction','',
        'Workflow: prepare → full development review → develop → freeze → independent full test review → evaluate-test (once) → finish. Existing releases and original text remain unchanged. validation.json records source reconstruction and protected checksums. frozen_code contains the exact engine and workflow snapshots; sample_manifest.json and completion_manifest.json record input/output checksums.',
        '', 'Manual references: development_reviews.jsonl and test_reviews.jsonl. Samples: development_sample_500.csv, test_sample_500.csv and sample_1000.csv. Baseline and all rule versions, predictions, metrics and traces: development_versions/. Frozen test outputs: test_evaluation/.', '']
    (dest/'REPORT.md').write_text('\n'.join(lines),encoding='utf-8')
    base.write_json(dest/'completion_manifest.json',dict(completed_at_utc=datetime.now(timezone.utc).isoformat(),validation=result,
        freeze_sha256=base.digest(dest/'frozen_rules.json'),output_checksums={str(p.relative_to(dest)):base.digest(p) for p in dest.rglob('*') if p.is_file() and p.name!='completion_manifest.json'}))
    return dest/'REPORT.md'


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument("command",choices=["prepare","show","develop","freeze","evaluate-test","finish"]);p.add_argument("--root",type=Path)
    p.add_argument("--partition",choices=["development","test"],default="development")
    p.add_argument("--start",type=int,default=1);p.add_argument("--end",type=int,default=20)
    args=p.parse_args()
    if args.command=="prepare":print(prepare(args.root))
    elif args.command=='show':show(args.partition,args.start,args.end,args.root)
    elif args.command=='develop':print(develop(args.root))
    elif args.command=='freeze':print(freeze(args.root))
    elif args.command=='evaluate-test':print(evaluate_test(args.root))
    elif args.command=='finish':print(finish(args.root))


if __name__=="__main__":main()
