"""Large-file fallback after artifact-tool export fails its string-size limit.

Uses bundled openpyxl in write-only mode. Data remain literal strings and every
cell is verified against the CSV before publication. No input workbook is edited.
"""
import argparse
import csv
from pathlib import Path
import zipfile
import xml.etree.ElementTree as ET
from openpyxl import Workbook
from openpyxl.cell import WriteOnlyCell
from openpyxl.styles import Font,PatternFill,Alignment,Border,Side
from openpyxl.utils import get_column_letter

csv.field_size_limit(10_000_000)


def export(csv_path,xlsx_path,sheet_name,reference=None):
    csv_path=Path(csv_path);xlsx_path=Path(xlsx_path)
    with csv_path.open(encoding='utf-8-sig',newline='') as f:
        reader=csv.reader(f);headers=next(reader)
        wb=Workbook(write_only=True);sheet=wb.create_sheet(sheet_name)
        styled=sheet_name.endswith('final') or sheet_name=='amws_all_editions_persons'
        sheet.freeze_panes='B2' if styled else 'A2'
        sheet.sheet_view.showGridLines=not styled
        if reference:
            with zipfile.ZipFile(reference) as z:
                xml=ET.fromstring(z.read('xl/worksheets/sheet1.xml'))
            ns={'s':'http://schemas.openxmlformats.org/spreadsheetml/2006/main'}
            for col in xml.findall('s:cols/s:col',ns):
                for c in range(int(col.attrib['min']),int(col.attrib['max'])+1):
                    sheet.column_dimensions[get_column_letter(c)].width=float(col.attrib['width'])
        elif styled:
            widths={'edition':12,'source_record_id':23,'name_raw':28,'raw_text_adjusted':72,
                'birth_place':34,'birth_date':15,'birth_year':12,'birth_city':24,'birth_state':14,
                'birth_country':18,'is_us_birth':14,'is_us_geocoded':16,'geo_lat':14,'geo_lon':14,
                'geo_geoid':14,'geo_county_name':24,'geo_matched_name':30,'geocoding_status':20}
            for c,header in enumerate(headers,1):sheet.column_dimensions[get_column_letter(c)].width=widths[header]
        if styled:
            sheet.row_dimensions[1].height=24
            header_cells=[]
            for value in headers:
                cell=WriteOnlyCell(sheet,value=value);cell.data_type='s'
                cell.font=Font(name='Calibri',size=11,bold=True,color='FFFFFF')
                cell.fill=PatternFill('solid',fgColor='1F4E78')
                cell.alignment=Alignment(horizontal='center',vertical='center')
                cell.border=Border(bottom=Side(style='thin',color='A6A6A6'))
                header_cells.append(cell)
            sheet.append(header_cells)
        else:sheet.append(headers)
        count=1
        for values in reader:
            cells=[]
            for value in values:
                if value=='':cells.append(None)
                else:
                    cell=WriteOnlyCell(sheet,value=value);cell.data_type='s';cells.append(cell)
            sheet.append(cells);count+=1
            if count%20000==0:print(sheet_name,count-1,'rows',flush=True)
        sheet.auto_filter.ref=f'A1:{get_column_letter(len(headers))}{count}'
        xlsx_path.parent.mkdir(parents=True,exist_ok=True);wb.save(xlsx_path)
    print('Saved',xlsx_path,count-1,'rows',flush=True)


if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('csv');p.add_argument('xlsx');p.add_argument('sheet')
    p.add_argument('--reference');a=p.parse_args();export(a.csv,a.xlsx,a.sheet,a.reference)
