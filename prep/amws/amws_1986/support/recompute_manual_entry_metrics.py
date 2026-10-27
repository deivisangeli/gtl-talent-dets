"""Reproduce delivered counts without OCR, extraction, repairs, or source access."""
import argparse,json
from pathlib import Path
import build_manual_entry_audit as b

def recompute(root):
    pages=json.loads((root/'reviewed_entries.json').read_text(encoding='utf8'))
    rows=[x for p in pages for x in p['entries']]
    files=sorted({p['page_key'].split(':')[0] for p in pages})
    actual=dict(pooled=b.metrics(rows),
        per_pdf={f:b.metrics([x for x in rows if x['page_key'].split(':')[0]==f]) for f in files},
        per_page={p['page_key']:b.metrics(p['entries']) for p in pages})
    expected=json.loads((root/'results.json').read_text(encoding='utf8'))
    assert actual==expected,'Delivered metrics differ from frozen judgments'
    return dict(passed=True,pages=len(pages),entries=len(rows),groups=1+len(files)+len(pages))

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('--audit-root',type=Path,required=True)
    args=parser.parse_args();print(json.dumps(recompute(args.audit_root)))
