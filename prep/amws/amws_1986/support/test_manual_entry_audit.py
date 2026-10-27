"""Validation of manual-audit accounting and faithful evidence reading."""
import copy,json,tempfile,unittest,zipfile
from pathlib import Path
import manual_entry_audit as a
import manual_word_evidence as w
import build_manual_entry_audit as b

def entry(word='correct',paddle='correct',errors=(),structure=(),completeness='complete'):
    def method(v):return dict(key_information={f:v for f in b.FIELDS},errors=list(errors),structure=list(structure),completeness=completeness)
    return dict(word=method(word),paddle=method(paddle),comparison='tie')

class AuditTests(unittest.TestCase):
    def test_frozen_sampling_and_accounting(self):
        m,inv,review=b.validate_frozen(a.ROOT)
        self.assertEqual(sum(len(p['entries']) for p in review),339)
        self.assertEqual(len(m['previous_audit_pages']),36)
        self.assertEqual(len({p['page_key'].split(':')[0] for p in review}),9)
        self.assertTrue(all(p['eligible'] and p['source_only_reviewed'] for p in inv))

    def test_metrics_denominators_and_error_overlap(self):
        rows=[entry(errors=[{'severity':'minor'},{'severity':'substantive'}],structure=['merged','fragmented']),entry('absent','absent'),entry('source-unreadable','source-unreadable'),entry(completeness='partially omitted')]
        r=b.metrics(rows)
        self.assertEqual(r['word']['key_information']['name']['denominator'],2)
        self.assertEqual(r['word']['key_information']['name']['correct'],2)
        self.assertEqual(r['word']['counts']['content_error_identified'],1)
        self.assertEqual(r['word']['counts']['structural_defect'],1)
        self.assertEqual(r['word']['counts']['minor_only'],0)
        self.assertEqual(r['word']['counts']['completeness_partially omitted'],1)

    def test_missing_fields_are_failures_not_exclusions(self):
        r=b.metrics([entry('missing','incorrect'),entry()])
        self.assertEqual(r['word']['key_information']['name']['denominator'],2)
        self.assertEqual(r['word']['key_information']['name']['percent'],50)

    def test_incomplete_delivery_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p=Path(tmp)
            for f in ('manifest.json','inventory.json','inventory_freeze.json','inventory_errata.json','reviewed_entries.json','review_freeze.json'):
                (p/f).write_bytes((a.ROOT/f).read_bytes())
            rev=a.read(p/'reviewed_entries.json');rev[0]['status']='first_pass_needs_recheck';a.write(p/'reviewed_entries.json',rev)
            fr=a.read(p/'review_freeze.json');fr['reviewed_entries_sha256']=a.sha(p/'reviewed_entries.json');a.write(p/'review_freeze.json',fr)
            with self.assertRaises(AssertionError):b.validate_frozen(p)

    def test_word_nested_runs_once_and_generated_date_label(self):
        ns=w.W[1:-1]
        doc=f'<w:document xmlns:w="{ns}"><w:body><w:p><w:r><w:t>First</w:t></w:r><w:r><w:drawing><w:txbxContent><w:p><w:r><w:t>Inside</w:t></w:r></w:p></w:txbxContent></w:drawing></w:r></w:p><w:p><w:pPr><w:numPr><w:ilvl w:val="0"/><w:numId w:val="1"/></w:numPr></w:pPr><w:r><w:t>80; career</w:t></w:r></w:p></w:body></w:document>'
        numbering=f'<w:numbering xmlns:w="{ns}"><w:abstractNum w:abstractNumId="0"><w:lvl w:ilvl="0"><w:start w:val="77"/><w:numFmt w:val="decimal"/><w:lvlText w:val="%1-"/></w:lvl></w:abstractNum><w:num w:numId="1"><w:abstractNumId w:val="0"/></w:num></w:numbering>'
        with tempfile.TemporaryDirectory() as tmp:
            p=Path(tmp)/'fixture.docx'
            with zipfile.ZipFile(p,'w') as z:z.writestr('word/document.xml',doc);z.writestr('word/numbering.xml',numbering)
            ps,bad,unused=w.read_word(p)
        self.assertEqual([x['literal_text'] for x in ps],['First','Inside','80; career'])
        self.assertEqual(ps[2]['list_label'],'77-')
        self.assertEqual(ps[2]['text'],'77- 80; career')
        self.assertEqual(bad,[]);self.assertEqual(unused,[])

    def test_audit_word_evidence_has_no_unresolved_labels(self):
        _,_,pages=b.validate_frozen(a.ROOT);cache={}
        for p in pages:
            stem=Path(p['page_key'].split(':')[0]).stem
            if stem not in cache:cache[stem]=a.read(a.ROOT/'word_evidence'/f'{stem}.json')['paragraphs']
            for x in p['entries']:
                for i in x['word']['locations']:self.assertNotIn('UNRESOLVED LIST LABEL',cache[stem][i]['text'])

    def test_no_whole_biography_loss_in_reviewed_evidence(self):
        _,_,pages=b.validate_frozen(a.ROOT)
        for p in pages:
            for x in p['entries']:
                self.assertTrue(x['word']['locations']);self.assertTrue(x['paddle']['locations'])
                self.assertNotEqual(x['word']['completeness'],'wholly omitted')
                self.assertNotEqual(x['paddle']['completeness'],'wholly omitted')

if __name__=='__main__':unittest.main()
