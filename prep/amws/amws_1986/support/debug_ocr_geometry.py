import json,sys
from pathlib import Path
base=Path.home()/'ocr_runs/amws_ed16_paragraph_reviewed'
j=json.loads((base/sys.argv[1]/f'page_{int(sys.argv[2]):04d}.json').read_text(encoding='utf-8'))
for col in j['original_record']['prediction']['column_predictions']:
    o=col['prediction']['overall_ocr_res'];print('offset',col['x_offset'],'counts',len(o['rec_texts']),len(o['rec_boxes']),len(o.get('rec_polys',[])))
    for i,t in enumerate(o['rec_texts']):
        if sys.argv[3].upper() in t.upper():print(i,t,o['rec_boxes'][i],o.get('rec_polys',[None]*len(o['rec_texts']))[i])
