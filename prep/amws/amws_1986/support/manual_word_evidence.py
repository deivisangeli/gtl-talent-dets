"""Read literal Word text and OOXML list labels, without altering the document.

The earlier XML snapshot omitted generated list labels (often date fragments).
Keep the old snapshot verbatim and provide this separately for manual comparison.
"""
import re,zipfile,xml.etree.ElementTree as E
from collections import Counter
from pathlib import Path
import manual_entry_audit as a
W='{http://schemas.openxmlformats.org/wordprocessingml/2006/main}'
def val(node,tag,default=None):
    n=node.find(W+tag) if node is not None else None
    return n.get(W+'val',default) if n is not None else default

def numbering_value(number,fmt):
    """OOXML numbering display, without inserting diagnostic text into evidence."""
    if fmt in ('decimal','decimalZero'):return str(number).zfill(2) if fmt=='decimalZero' else str(number)
    if fmt in ('upperRoman','lowerRoman'):
        text='';remaining=number
        for value,symbol in ((1000,'M'),(900,'CM'),(500,'D'),(400,'CD'),(100,'C'),(90,'XC'),(50,'L'),(40,'XL'),(10,'X'),(9,'IX'),(5,'V'),(4,'IV'),(1,'I')):
            count,remaining=divmod(remaining,value);text+=symbol*count
        return text.lower() if fmt=='lowerRoman' else text
    if fmt in ('upperLetter','lowerLetter'):
        text=''
        while number>0:number,index=divmod(number-1,26);text=chr(65+index)+text
        return text.lower() if fmt=='lowerLetter' else text
    raise ValueError('Unsupported numbering format: '+str(fmt))
def read_word(path):
    with zipfile.ZipFile(path) as z:
        tree=E.fromstring(z.read('word/document.xml'));num=E.fromstring(z.read('word/numbering.xml'))
    abstracts={n.get(W+'abstractNumId'):n for n in num.findall(W+'abstractNum')}
    nums={n.get(W+'numId'):n for n in num.findall(W+'num')};counts={};labels={};unsupported=[]
    for p in tree.iter(W+'p'):
        np=p.find(W+'pPr/'+W+'numPr')
        if np is None:continue
        nid=val(np,'numId');ilvl=val(np,'ilvl','0');n=nums.get(nid)
        if n is None:unsupported.append(dict(num_id=nid,reason='missing numbering definition'));continue
        abstract=abstracts[val(n,'abstractNumId')]
        lvl=next((x for x in abstract.findall(W+'lvl') if x.get(W+'ilvl')==ilvl),None)
        override=next((x for x in n.findall(W+'lvlOverride') if x.get(W+'ilvl')==ilvl),None)
        if override is not None and override.find(W+'lvl') is not None:lvl=override.find(W+'lvl')
        start=int(val(override,'startOverride',val(lvl,'start','1')))
        key=(nid,ilvl);count=counts.get(key,start-1)+1;counts[key]=count
        fmt=val(lvl,'numFmt');template=val(lvl,'lvlText','')
        if fmt=='bullet':label=template
        elif fmt in ('decimal','decimalZero','upperRoman','lowerRoman','upperLetter','lowerLetter'):
            label=template
            for level in range(1,10):
                numvalue=counts.get((nid,str(level-1)),count if str(level-1)==ilvl else 1)
                label=label.replace('%'+str(level),numbering_value(numvalue,fmt))
        elif fmt=='none':label=''
        else:
            unsupported.append(dict(num_id=nid,level=ilvl,format=fmt,template=template));label='[UNRESOLVED LIST LABEL]'
        labels[id(p)]=dict(label=label,num_id=nid,level=ilvl,format=fmt)
    out=[];pieces=[];current=None;old_index=0;used=set()
    def flush():
        nonlocal old_index
        if pieces:
            literal=''.join(pieces);pieces.clear();meta=labels.get(id(current),{}) if id(current) not in used else {};used.add(id(current))
            out.append(dict(index=old_index,literal_text=literal,list_label=meta.get('label',''),numbering=meta,text=(meta.get('label','')+' '+literal).strip() if meta.get('label') else literal));old_index+=1
    def walk(node):
        nonlocal current
        if node.tag==W+'t':pieces.append(node.text or '');return
        if node.tag in (W+'br',W+'cr'):pieces.append('\n');return
        if node.tag==W+'tab':pieces.append('\t');return
        previous=current
        if node.tag==W+'p':flush();current=node
        for child in node:walk(child)
        if node.tag==W+'p':flush();current=previous
    walk(tree);flush()
    unused=[dict(xml_paragraph=i,**labels[id(p)]) for i,p in enumerate(tree.iter(W+'p')) if id(p) in labels and id(p) not in used]
    return out,unsupported,unused
def main():
    m=a.read(a.ROOT/'manifest.json');summary=[]
    for d in m['documents']:
        file=d['source_file'];ps,unsupported,unused=read_word(d['baseline']['path'])
        old=a.read(a.OLD/'baseline'/f'{Path(file).stem}.json')['paragraphs']
        assert len(ps)==len(old) and all(p['literal_text']==q['text'] for p,q in zip(ps,old))
        dest=a.ROOT/'word_evidence'/f'{Path(file).stem}.json'
        a.write(dest,dict(source=d['baseline'],source_sha256=d['docx_sha256'],extraction='Each original text run once, retaining original paragraph indices, with OOXML-generated list labels shown separately. No transcription correction or paragraph repair.',paragraphs=ps,unsupported_labels=unsupported,label_only_paragraphs=unused))
        summary.append(dict(source_file=file,paragraphs=len(ps),list_labels=sum(bool(p['list_label']) for p in ps),unsupported=unsupported,label_only_paragraphs=unused))
    a.write(a.ROOT/'word_evidence/index.json',summary)
    print('\n'.join(str({k:v for k,v in s.items() if k not in ('unsupported','label_only_paragraphs')})+' unsupported='+str(len(s['unsupported']))+' label-only='+str(len(s['label_only_paragraphs'])) for s in summary))
if __name__=='__main__':main()
