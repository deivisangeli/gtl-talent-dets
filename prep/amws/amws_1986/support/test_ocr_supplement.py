import json
from pathlib import Path
import sys
import tempfile
import unittest
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
import ocr_supplement as pipeline


class SupplementTests(unittest.TestCase):
    def test_eligibility(self):
        row=dict(birth_city='Boston',birth_year='1920',birth_country='USA',is_us_birth='TRUE',is_us_geocoded='TRUE')
        self.assertTrue(pipeline.eligible(row))
        self.assertFalse(pipeline.eligible(dict(row,is_us_geocoded='FALSE')))
        self.assertTrue(pipeline.eligible(dict(row,birth_city='London',birth_country='England',is_us_birth='FALSE',is_us_geocoded='FALSE')))
        self.assertFalse(pipeline.eligible(dict(row,birth_city='')))

    def test_segmentation_provenance_and_boundaries(self):
        source=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011/segmentation_patterns.json'
        with tempfile.TemporaryDirectory() as td:
            stage=Path(td);(stage/'segmentation_patterns.json').write_bytes(source.read_bytes())
            def block(doc,page,index,text,printed,column=1,status='body'):
                return dict(block_key=f'{doc}:{page}:{index}',source_file=doc,pdf_page=page,printed_page=printed,
                    block_index=index,column=column,block_order=index,label='text',block_status=status,
                    bbox='[1,1,100,100]',text=text,source_lines='[]')
            rows=[block('16_C_400_600.pdf',200,1,'SMITH, JOHN, b Boston, Mass, Jan 1, 20. PHYSICS. Educ: A, BS, 40.',599),
                  block('16_C_600_800.pdf',1,1,'Prof Exp: Professor physics. Mailing Add: Boston MA 02101',600),
                  block('16_C_600_800.pdf',1,2,'SMITH, JANE, b London, Eng, Feb 2, 30. CHEMISTRY. Educ: B, BS, 50.',600,2),
                  block('16_D_600_800.pdf',1,1,'Prof Exp: orphan from preceding unavailable source.',600),
                  block('16_D_600_800.pdf',1,2,'SMITH, ROBERT, physics, see previous edition',600),
                  block('16_D_600_800.pdf',1,3,'Blank page marker',600,status='excluded_layout'),
                  block('16_E_601_700.pdf',1,1,'Prof Exp: unrelated continuation in a different volume.',601)]
            pipeline.write_csv(stage/'blocks.csv',rows);pipeline.segment(stage)
            _,entries=pipeline.read_csv(stage/'segmented_entries.csv')
            self.assertEqual(len(entries),5)
            self.assertIn('Professor physics',entries[0]['raw_text_adjusted'])
            self.assertEqual(entries[2]['segmentation_rule'],'orphan_continuation')
            self.assertEqual(entries[3]['segmentation_rule'],'see_previous_start')
            self.assertEqual(entries[4]['segmentation_rule'],'orphan_continuation')
            first=(stage/'segmented_entries.csv').read_bytes();pipeline.segment(stage)
            self.assertEqual(first,(stage/'segmented_entries.csv').read_bytes())
            _,assignments=pipeline.read_csv(stage/'block_assignments.csv')
            self.assertEqual(len(assignments),6)

    def test_multi_entry_block_and_garbled_start(self):
        source=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011/segmentation_patterns.json'
        with tempfile.TemporaryDirectory() as td:
            stage=Path(td);(stage/'segmentation_patterns.json').write_bytes(source.read_bytes())
            row=dict(block_key='16_C_400_600.pdf:1:0',source_file='16_C_400_600.pdf',pdf_page=1,
                printed_page=400,block_index=0,column=1,block_order=0,label='text',block_status='body',
                bbox='[1,1,100,100]',source_lines='[]',text='SMITH, JOHN, b Boston, Mass, Jan 1, 20. PHYSICS. Mailing Add: 02101 JONES, JANE, b London, Eng, Feb 2, 30. CHEMISTRY.')
            pipeline.write_csv(stage/'blocks.csv',[row]);pipeline.segment(stage)
            _,entries=pipeline.read_csv(stage/'segmented_entries.csv')
            self.assertEqual(len(entries),2)
            self.assertEqual([r['piece_no'] for r in entries],[1,2] if isinstance(entries[0]['piece_no'],int) else ['1','2'])
            self.assertTrue(entries[1]['raw_text_adjusted'].startswith('JONES, JANE'))

    def test_given_name_suffix_does_not_split_and_duplicate_page_break(self):
        source=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011/segmentation_patterns.json'
        with tempfile.TemporaryDirectory() as td:
            stage=Path(td);(stage/'segmentation_patterns.json').write_bytes(source.read_bytes())
            rows=[]
            for page,text in [(1,'HAHN, W(ALTER) C(HARLES), JR, b Boston, Mass, Jan 1, 20. PHYSICS.'),
                              (2,'PHARM, RUTGERS UNIV. Mailing Add: duplicated preceding continuation.'),
                              (2,'JONES, JANE, b London, Eng, Feb 2, 30. CHEMISTRY.')]:
                index=len(rows);rows.append(dict(block_key=f'16_C_400_600.pdf:{page}:{index}',source_file='16_C_400_600.pdf',
                    pdf_page=page,printed_page=399+page,block_index=index,column=1,block_order=index,label='text',
                    block_status='body',bbox='[1,1,100,100]',source_lines='[]',text=text))
            pipeline.write_csv(stage/'blocks.csv',rows)
            pipeline.write_json(stage/'segmentation_breaks.json',[dict(page_key='16_C_400_600.pdf:2',evidence='Repeated page scan verified')])
            pipeline.segment(stage);_,entries=pipeline.read_csv(stage/'segmented_entries.csv')
            self.assertEqual(len(entries),3)
            self.assertTrue(entries[0]['raw_text_adjusted'].startswith('HAHN, W(ALTER) C(HARLES), JR'))
            self.assertNotIn('RUTGERS',entries[0]['raw_text_adjusted'])
            self.assertEqual(entries[1]['segmentation_rule'],'orphan_continuation')

    def test_incomplete_uppercase_heading_and_explicit_scan_boundary(self):
        source=Path.home()/'Globtalent Dropbox/gtl_talent_dets/output/amws/ocr_supplement_20261011/segmentation_patterns.json'
        with tempfile.TemporaryDirectory() as td:
            stage=Path(td);(stage/'segmentation_patterns.json').write_bytes(source.read_bytes())
            texts=['HAWK, VIRGIL BROWN, b Boston, Mass, Jan 1, 20. AGRONOMY.',
                   'HAWK, WILLIAM ANDREW. THYROID. BOWEL DISEASES. EduC: Univ Pa. MD.',
                   'GARBLED HEADER belongs to a new person: verified scan.']
            rows=[dict(block_key=f'16_C_400_600.pdf:1:{i}',source_file='16_C_400_600.pdf',pdf_page=1,printed_page=400,block_index=i,column=1,block_order=i,label='text',block_status='body',bbox='[1,1,100,100]',source_lines='[]',text=t) for i,t in enumerate(texts)]
            pipeline.write_csv(stage/'blocks.csv',rows)
            pipeline.write_json(stage/'forced_entry_starts.json',[dict(piece_key='16_C_400_600.pdf:1:2:1',expected_text=texts[2],evidence='Explicit separate heading confirmed on scan.')])
            pipeline.segment(stage);_,entries=pipeline.read_csv(stage/'segmented_entries.csv')
            self.assertEqual(len(entries),3)
            self.assertNotIn('WILLIAM ANDREW',entries[0]['raw_text_adjusted'])
            self.assertEqual(entries[1]['segmentation_rule'],'no_birth_field_punctuation_variant')
            self.assertEqual(entries[2]['segmentation_rule'],'manual_scan_entry_start')


if __name__=='__main__':unittest.main()
