"""Locate verbatim source passages for human/agent review; no quality scoring."""
import argparse,difflib,json,re
from pathlib import Path
import manual_entry_audit as a
def norm(s):return re.sub('[^a-z0-9]','',s.lower())
def candidates(file,page):
    inv=next(p for p in a.read(a.ROOT/'inventory.json') if p['page_key']==f'{file}:{page}')
    paras=a.read(a.ROOT/'word_evidence'/f'{Path(file).stem}.json')['paragraphs']
    result=[]
    for name in inv['entries']:
        n=norm(name);scores=[];ng={n[i:i+2] for i in range(len(n)-1)}
        ranked=sorted(paras,key=lambda p:len(ng & {norm(p['text'][:len(name)+35])[i:i+2] for i in range(len(norm(p['text'][:len(name)+35]))-1)}),reverse=True)[:50]
        for p in ranked:
            t=norm(p['text'][:len(name)+35])
            score=max((difflib.SequenceMatcher(None,n,t[k:k+len(n)]).ratio() for k in (0,1,2,3)),default=0)
            scores.append((score,p['index'],p['text'][:140]))
        result.append(dict(name=name,candidates=sorted(scores,reverse=True)[:3]))
    return result
def word(file,lo,hi):
    ps=a.read(a.ROOT/'word_evidence'/f'{Path(file).stem}.json')['paragraphs']
    return '\n\n'.join(f"[{p['index']}] {p['text']}" for p in ps if lo<=p['index']<=hi)
def paddle(file,page):
    return (a.OLD/'ocr'/Path(file).stem/f'page_{page:04d}.txt').read_text(encoding='utf-8')
def main():
    p=argparse.ArgumentParser();p.add_argument('mode',choices=['candidates','word','paddle']);p.add_argument('file');p.add_argument('page',type=int);p.add_argument('--lo',type=int);p.add_argument('--hi',type=int);x=p.parse_args()
    if x.mode=='candidates':print(json.dumps(candidates(x.file,x.page),ensure_ascii=False,indent=2))
    elif x.mode=='word':print(word(x.file,x.lo,x.hi))
    else:print(paddle(x.file,x.page))
if __name__=='__main__':main()
