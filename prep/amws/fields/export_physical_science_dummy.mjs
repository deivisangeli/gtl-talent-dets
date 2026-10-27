// Author two machine-readable CSVs in bounded workbook batches.
import fs from 'node:fs/promises';
import {createReadStream} from 'node:fs';
import readline from 'node:readline';
import path from 'node:path';
import os from 'node:os';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';

const args = process.argv.slice(2);
const out = args.includes('--output') ? path.resolve(args[args.indexOf('--output')+1]) :
  path.join(process.env.TALENT_DETS_DATA_DIR ?? path.join(os.homedir(),'Globtalent Dropbox/gtl_talent_dets'),
    'output/amws/physical_science_dummy_20261020');
const staging = path.join(out,'_staging');
const config = JSON.parse(await fs.readFile(path.join(staging,'prepared.json'),'utf8'));
const modules = process.env.AMWS_ARTIFACT_NODE_MODULES ??
  path.join(os.homedir(),'.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules');
const req = createRequire(path.join(modules,'..','__amws_physical_dummy_loader.cjs'));
const {Workbook} = await import(pathToFileURL(req.resolve('@oai/artifact-tool')).href);
const paths = {
  dummy: path.join(staging,'amws_ed16_physical_science_dummy.csv.part'),
  audit: path.join(staging,'amws_ed16_physical_science_audit.csv.part'),
};
const handles = {dummy:await fs.open(paths.dummy,'w'),audit:await fs.open(paths.audit,'w')};
await handles.dummy.write('\uFEFF');await handles.audit.write('\uFEFF');
let written=0;
let first=true;
const quote = value => '"'+String(value ?? '').replaceAll('"','""')+'"';
const csv = rows => rows.map(row=>row.map(quote).join(',')).join('\r\n')+'\r\n';

async function writeBatch(batch) {
  const wb=Workbook.create();
  const sheets={};
  for (const type of ['dummy','audit']) {
    const columns=config[`${type}_columns`];
    const matrix=[columns,...batch.map(r=>r[type])];
    const sheet=wb.worksheets.add(type==='dummy'?'Dummy':'Audit');
    const range=sheet.getRangeByIndexes(0,0,matrix.length,columns.length);
    // The workbook consumes a leading apostrophe as a literal-text escape.
    // Read-back verification below requires the original, unescaped values.
    range.values=matrix.map(row=>row.map(value=>
      typeof value==='string' && value.startsWith('=') ? "'"+value : value));
    sheets[type]={sheet,range,matrix};
  }
  wb.recalculate();
  for (const type of ['dummy','audit']) {
    const {range,matrix}=sheets[type];
    const values=range.values;
    if (JSON.stringify(values)!==JSON.stringify(matrix)) throw new Error(`Cell preservation failed for ${type} batch ${written}`);
    await handles[type].write(csv(first?values:values.slice(1)));
  }
  if (first) {
    const dummy=sheets.dummy.sheet;
    dummy.showGridLines=false;
    dummy.getRange('A1:D9').format.font={name:'Arial',size:10};
    dummy.getRange('A1:D9').format.verticalAlignment='center';
    dummy.getRange('A1:D1').format={fill:'#23415A',font:{name:'Arial',size:10,bold:true,color:'#FFFFFF'},rowHeight:28};
    dummy.getRange('A2:D9').format.rowHeight=24;
    for (const [col,width] of Object.entries({A:18,B:14,C:48,D:22})) dummy.getRange(`${col}1:${col}9`).format.columnWidth=width;
    const audit=sheets.audit.sheet;
    audit.showGridLines=false;
    audit.getRange('A1:J6').format.font={name:'Arial',size:10};
    audit.getRange('A1:J6').format.wrapText=true;
    audit.getRange('A1:J6').format.rowHeight=44;
    audit.getRange('A1:J1').format={fill:'#23415A',font:{name:'Arial',size:10,bold:true,color:'#FFFFFF'},rowHeight:32,wrapText:true};
    for (const [col,width] of Object.entries({A:16,B:12,C:40,D:14,E:48,F:20,G:12,H:16,I:24,J:20})) audit.getRange(`${col}1:${col}6`).format.columnWidth=width;
    wb.recalculate();
    const previewDir=path.join(process.cwd(),'tmp/amws_physical_science_dummy_20261020');
    await fs.mkdir(previewDir,{recursive:true});
    for (const [sheetName,range,file] of [['Dummy','A1:D9','dummy_preview.png'],['Audit','A1:J6','audit_preview.png']]) {
      const image=await wb.render({sheetName,range,scale:1,format:'png'});
      await fs.writeFile(path.join(previewDir,file),new Uint8Array(await image.arrayBuffer()));
    }
    console.log((await wb.inspect({kind:'table',range:'Dummy!A1:D5',include:'values',tableMaxRows:5,tableMaxCols:4,maxChars:1000})).ndjson);
  }
  first=false;written+=batch.length;
}

try {
  let batch=[];
  for await (const line of readline.createInterface({input:createReadStream(path.join(staging,'classifications.jsonl'),{encoding:'utf8'}),crlfDelay:Infinity})) {
    if (!line.trim()) continue;
    batch.push(JSON.parse(line));
    if (batch.length===1000) {
      await writeBatch(batch);batch=[];
      global.gc?.();
      if (written%10000===0) console.log(`Authored ${written.toLocaleString()}/79,658 rows in each CSV.`);
    }
  }
  if (batch.length) await writeBatch(batch);
  if (written!==config.total_entries) throw new Error(`Unexpected output row count: ${written}`);
} finally {
  await handles.dummy.close();await handles.audit.close();
}
await fs.rename(paths.dummy,path.join(out,'amws_ed16_physical_science_dummy.csv'));
await fs.rename(paths.audit,path.join(out,'amws_ed16_physical_science_audit.csv'));
await fs.writeFile(path.join(staging,'csv_export.json'),JSON.stringify({rows_each:written,
  artifact_tool_value_preservation_passed:true,workbook_batch_rows:1000,encoding:'UTF-8 BOM',columns:{dummy:4,audit:20}},null,2)+'\n','utf8');
console.log(JSON.stringify({csvs:2,rows_each:written}));
