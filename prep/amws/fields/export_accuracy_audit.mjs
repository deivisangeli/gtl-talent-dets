// Create the requested audit CSV through the bundled artifact workbook engine.
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';

const args=process.argv.slice(2);
const root=args.includes('--data-root') ? path.resolve(args[args.indexOf('--data-root')+1]) :
  process.env.TALENT_DETS_DATA_DIR ?? path.join(os.homedir(),'Globtalent Dropbox/gtl_talent_dets');
const out=path.join(root,'output/amws/physical_science_accuracy_audit_20261020');
const modules=process.env.AMWS_ARTIFACT_NODE_MODULES ??
  path.join(os.homedir(),'.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules');
const req=createRequire(path.join(modules,'..','__amws_accuracy_audit_loader.cjs'));
const {Workbook}=await import(pathToFileURL(req.resolve('@oai/artifact-tool')).href);
const payload=JSON.parse(await fs.readFile(path.join(out,'audit_payload.json'),'utf8'));
const wb=Workbook.create();
const sheet=wb.worksheets.add('Audit');
const matrix=[payload.columns,...payload.rows];
const range=sheet.getRangeByIndexes(0,0,matrix.length,matrix[0].length);
range.values=matrix.map(row=>row.map(v=>typeof v==='string'&&v.startsWith('=')?"'"+v:v));
sheet.showGridLines=false;
sheet.freezePanes.freezeRows(1);
range.format.font={name:'Arial',size:10};
range.format.verticalAlignment='top';
range.format.rowHeight=30;
sheet.getRangeByIndexes(0,0,1,matrix[0].length).format={
  fill:'#23415A',font:{name:'Arial',size:10,bold:true,color:'#FFFFFF'},rowHeight:44,wrapText:true};
for(const [col,width] of Object.entries({A:12,B:17,C:42,D:23,E:20,F:18,G:16,H:55,I:60,J:12,K:24}))
  sheet.getRange(`${col}1:${col}501`).format.columnWidth=width;
wb.recalculate();
const values=range.values;
if(JSON.stringify(values)!==JSON.stringify(matrix)) throw new Error('Audit cell preservation failed');
console.log((await wb.inspect({kind:'table',range:'Audit!A1:G5',include:'values',tableMaxRows:5,tableMaxCols:7,maxChars:1800})).ndjson);
const formulaErrors=await wb.inspect({kind:'match',searchTerm:'#REF!|#DIV/0!|#VALUE!|#NAME\\?|#NUM!|#NULL!|#SPILL!|#CALC!',
  options:{useRegex:true,maxResults:10},summary:'formula error scan'});
console.log(formulaErrors.ndjson);
const quote=v=>'"'+String(v??'').replaceAll('"','""')+'"';
await fs.writeFile(path.join(out,'amws_physical_science_accuracy_audit_500.csv'),
  '\uFEFF'+values.map(row=>row.map(quote).join(',')).join('\r\n')+'\r\n','utf8');
const previewDir=path.join(process.cwd(),'tmp/amws_physical_science_accuracy_audit_20261020');
await fs.mkdir(previewDir,{recursive:true});
const preview=await wb.render({sheetName:'Audit',range:'A1:G8',scale:1,format:'png'});
await fs.writeFile(path.join(previewDir,'preview.png'),new Uint8Array(await preview.arrayBuffer()));
await fs.writeFile(path.join(out,'export_validation.json'),JSON.stringify({rows:500,columns:matrix[0].length,
  artifact_tool_cell_preservation_passed:true,encoding:'UTF-8 BOM'},null,2)+'\n');
console.log('Exported and cell-verified 500 audit rows.');
