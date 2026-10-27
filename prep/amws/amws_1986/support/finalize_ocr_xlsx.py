"""Preserve plain Excel autofilters absent from artifact-tool's public API.

Values and styles are authored by artifact-tool; this changes only filter range
metadata after export, using the public Office Open XML file format.
"""
import csv
from pathlib import Path
import re
import sys
import zipfile


def finalize(csv_path,xlsx_path):
    csv.field_size_limit(10_000_000)
    with Path(csv_path).open(encoding='utf-8-sig',newline='') as f:
        reader=csv.reader(f);columns=next(reader);count=1+sum(1 for _ in reader)
    n=len(columns);letter=''
    while n: n,k=divmod(n-1,26);letter=chr(65+k)+letter
    target=Path(xlsx_path);temporary=target.with_suffix('.filtering.xlsx')
    with zipfile.ZipFile(target) as source,zipfile.ZipFile(temporary,'w',zipfile.ZIP_DEFLATED) as output:
        for member in source.infolist():
            data=source.read(member.filename)
            if member.filename=='xl/worksheets/sheet1.xml':
                data=re.sub(rb'<autoFilter\b[^>]*/>',b'',data)
                data=data.replace(b'</sheetData>',f'</sheetData><autoFilter ref="A1:{letter}{count}"/>'.encode(),1)
            output.writestr(member,data)
    temporary.replace(target)


if __name__=='__main__':finalize(*sys.argv[1:])
