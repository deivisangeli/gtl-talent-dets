// Author the 500-row research CSV with exact artifact-tool value checks.
import fs from 'node:fs/promises';
import path from 'node:path';
import os from 'node:os';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';
const args=process.argv.slice(2);
const root=args.includes('--data-root') ? path.resolve(args[args.indexOf('--data-root')+1]) :
  process.env.TALENT_DETS_DATA_DIR ?? path.join(os.homedir(),'Globtalent Dropbox/gtl_talent_dets');
const out=path.join(root,'output/amws/field_classification_sample_20261023');
const modules=process.env.AMWS_ARTIFACT_NODE_MODULES ??
  path.join(os.homedir(),'.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules');
const req=createRequire(path.join(modules,'..','__cross_edition_sample_loader.cjs'));
const {Workbook}=await import(pathToFileURL(req.resolve('@oai/artifact-tool')).href);
const payload=JSON.parse(await fs.readFile(path.join(out,'audit_payload.json'),'utf8'));
const matrix=[payload.columns,...payload.rows];
const wb=Workbook.create();const sheet=wb.worksheets.add('Sample');
const range=sheet.getRangeByIndexes(0,0,matrix.length,matrix[0].length);
range.values=matrix.map(row=>row.map(v=>typeof v==='string'&&v.startsWith('=')?"'"+v:v));
range.format.font={name:'Arial',size:10};
range.format.verticalAlignment='center';
sheet.showGridLines=false;sheet.freezePanes.freezeRows(1);
sheet.getRange('A1:I8').format.wrapText=true;
sheet.getRange('A1:I8').format.rowHeight=46;
sheet.getRangeByIndexes(0,0,1,matrix[0].length).format={
  fill:'#23415A',font:{name:'Arial',size:10,bold:true,color:'#FFFFFF'},wrapText:true,rowHeight:34};
for(const [col,width] of Object.entries({A:12,B:17,C:12,D:47,E:48,F:48,G:21,H:12,I:17}))
  sheet.getRange(`${col}1:${col}501`).format.columnWidth=width;
wb.recalculate();
const values=range.values;
if(JSON.stringify(values)!==JSON.stringify(matrix)) throw new Error('CSV cell preservation failed');
console.log((await wb.inspect({kind:'table',range:'Sample!A1:I5',include:'values',tableMaxRows:5,tableMaxCols:9,maxChars:2200})).ndjson);
console.log((await wb.inspect({kind:'match',searchTerm:'#REF!|#DIV/0!|#VALUE!|#NAME\\?|#NUM!|#NULL!|#SPILL!|#CALC!',
  options:{useRegex:true,maxResults:10},summary:'formula error scan'})).ndjson);
const quote=v=>'"'+String(v??'').replaceAll('"','""')+'"';
await fs.writeFile(path.join(out,'amws_cross_edition_field_sample_500.csv'),
  '\uFEFF'+values.map(row=>row.map(quote).join(',')).join('\r\n')+'\r\n','utf8');
const previewDir=path.join(process.cwd(),'tmp/amws_cross_edition_sample_20261023');
await fs.mkdir(previewDir,{recursive:true});
const preview=await wb.render({sheetName:'Sample',range:'A1:I8',scale:1,format:'png'});
await fs.writeFile(path.join(previewDir,'preview.png'),new Uint8Array(await preview.arrayBuffer()));
await fs.writeFile(path.join(out,'export_validation.json'),JSON.stringify({rows:500,columns:matrix[0].length,
  artifact_tool_cell_preservation_passed:true,encoding:'UTF-8 BOM'},null,2)+'\n');
console.log('CSV export verified: 500 rows, all cells preserved.');
