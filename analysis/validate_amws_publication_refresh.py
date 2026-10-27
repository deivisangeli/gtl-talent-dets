"""Compare refreshed estimates and validate protected publication sections."""
import csv
import json
import re
from pathlib import Path
from amws_publication_refresh import ROOT, REPO, RUN, TAG, digest, run_step


def read(path):
    with path.open(encoding='utf-8-sig',newline='') as stream:
        return list(csv.DictReader(stream))


def compare():
    backup=json.loads((RUN/'backup_complete.json').read_text())
    started=backup['timestamp']
    changes=[]; files=[]; samples=[]; membership=[]
    keys=('model_id','model','spec','specification','outcome','outcome_name','outcome_group','timing','timing_mode','geography',
          'treatment_spec','control_group','balance','post_max','profile','window','support',
          'resolution','weighting','seasonal','event_time','e','g','year','term','cohort','control','control_specification','region','distance_bin_km')
    measures=('estimate','att','se','std_error','p_value','ci_low','ci_high','ci_lower','ci_upper','conf_low','conf_high',
              'Estimate','Std. Error','Pr(>|t|)','mean_y','beta_per_1000_seats',
              'n','n_events','n_control_units','n_treated_units','n_units','n_rows','counties',
              'observations','cohort_counties','assigned_scientists','estimated_births','cohort_clusters')
    for record in backup['files']:
        source=Path(record['path'])
        if source.suffix!='.csv' or 'results' not in source.parts:
            continue
        estimate_file = any(x in source.name for x in ('estimate','coefficients','dynamic_att','simple_att','post_att_summary','model_status'))
        membership_file = source.name in ('sample_inclusion_audit.csv', 'calendar_sample_audit.csv',
            'county_sample_audit.csv', 'event_sample_audit.csv', 'treated_counties.csv', 'treatment_assignment.csv')
        if not (estimate_file or membership_file):
            continue
        refreshed=Path(str(source).replace('countyfix_amws1986_20260719_decennial_1800_1960',TAG)
                       .replace('decennial_20261002',TAG).replace('calendar_appendix_20261005',TAG))
        if not refreshed.exists() or refreshed.stat().st_mtime<started:
            continue
        old=read(Path(record['backup']));new=read(refreshed)
        if not old or not new: continue
        if membership_file:
            identity = [k for k in ('GEOID','id','event_id','stack_id','profile','window','g','timing','timing_mode','region')
                        if k in old[0] and k in new[0]]
            flags = [k for k in old[0] if k in new[0] and
                     (k.startswith(('retained','included','eligible','keep')) or k in ('role','sample_role','reason'))]
            selected = identity + flags
            if selected:
                old_rows = {tuple(r[k] for k in selected) for r in old}
                new_rows = {tuple(r[k] for k in selected) for r in new}
                membership.append({'source':str(refreshed),'columns':selected,'old_rows':len(old),'new_rows':len(new),
                    'removed': [dict(zip(selected,r)) for r in sorted(old_rows-new_rows)],
                    'added': [dict(zip(selected,r)) for r in sorted(new_rows-old_rows)]})
            if not estimate_file: continue
        common=[k for k in keys if k in old[0] and k in new[0]]
        def index(rows):
            result={tuple(r[k] for k in common):r for r in rows}
            return result if len(result)==len(rows) else None
        oi,ni=index(old),index(new)
        if oi is None or ni is None:
            files.append({'source':str(refreshed),'old_rows':len(old),'new_rows':len(new),'comparison':'nonunique_key'})
            continue
        files.append({'source':str(refreshed),'old_rows':len(old),'new_rows':len(new),'comparison':'matched'})
        for key in sorted(set(oi)|set(ni)):
            a,b=oi.get(key),ni.get(key)
            if a is None or b is None:
                changes.append({'source':str(refreshed),'key':json.dumps(dict(zip(common,key))),
                                'measure':'row','before':'present' if a else 'absent','after':'present' if b else 'absent'})
                continue
            for measure in measures:
                if measure not in a or measure not in b: continue
                if measure.startswith('n') or measure in ('counties','observations','cohort_counties','assigned_scientists','estimated_births','cohort_clusters'):
                    samples.append({'source':str(refreshed),'key':json.dumps(dict(zip(common,key))),
                                    'measure':measure,'before':a[measure],'after':b[measure]})
                if a[measure]==b[measure]: continue
                try:
                    before,after=float(a[measure]),float(b[measure])
                    if abs(before-after)<=1e-12*max(1,abs(before),abs(after)):continue
                except ValueError: pass
                changes.append({'source':str(refreshed),'key':json.dumps(dict(zip(common,key))),
                                'measure':measure,'before':a[measure],'after':b[measure]})
    with (RUN/'before_after_estimates.csv').open('w',newline='',encoding='utf-8') as stream:
        writer=csv.DictWriter(stream,fieldnames=('source','key','measure','before','after'))
        writer.writeheader();writer.writerows(changes)
    (RUN/'estimate_comparison_manifest.json').write_text(json.dumps(files,indent=2))
    with (RUN/'sample_counts_before_after.csv').open('w',newline='',encoding='utf-8') as stream:
        writer=csv.DictWriter(stream,fieldnames=('source','key','measure','before','after'))
        writer.writeheader();writer.writerows(samples)
    (RUN/'sample_membership_before_after.json').write_text(json.dumps(membership,indent=2))
    print('Compared',len(files),'estimate files;',len(changes),'changed values')


def preserved_sections():
    frozen=RUN/'backup/repo/project'
    project=REPO/'project'
    old=(frozen/'main.tex').read_text(encoding='utf-8-sig')
    new=(project/'main.tex').read_text(encoding='utf-8-sig')
    begin=old.index(r'\subsubsection{UK') if r'\subsubsection{UK' in old else -1
    assert begin>=0
    end=old.index(r'\section{Elite Schools}',begin)
    assert old[begin:end]==new[new.index(r'\subsubsection{UK'):new.index(r'\section{Elite Schools}')]
    begin=old.index(r"\section{World's-fair pooled synthetic-control design}")
    end=old.index(r'\section{Data cleaning}',begin)
    assert old[begin:end]==new[new.index(r"\section{World's-fair pooled synthetic-control design}"):new.index(r'\section{Data cleaning}')]
    old=(frozen/'mandatory_schooling.tex').read_text(encoding='utf-8-sig')
    new=(project/'mandatory_schooling.tex').read_text(encoding='utf-8-sig')
    assert old==new, 'Schooling figure volume changed beyond its external refreshed assets'
    old=(frozen/'report_companion.tex').read_text(encoding='utf-8-sig')
    new=(project/'report_companion.tex').read_text(encoding='utf-8-sig')
    for heading,next_heading in ((r'\clearpage\section{Compulsory Schooling Laws}',r'\clearpage\section{College Allocation}'),):
        assert old[old.index(heading):old.index(next_heading)]==new[new.index(heading):new.index(next_heading)]
    caption='UK fairs: pooled synthetic-DiD event studies with rematched placebos'
    def figure(text):
        location=text.index(caption)
        start=text.rfind(r'\begin{figure}',0,location)
        end=text.index(r'\end{figure}',location)+len(r'\end{figure}')
        return text[start:end]
    assert figure(old)==figure(new), 'UK companion synthetic DiD figure changed'
    print('Excluded manuscript and companion sections preserved')


if __name__=='__main__':
    run_step('verify')
    compare()
    preserved_sections()
