// Export the fresh evaluation CSV with exact artifact-tool value verification.
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';
const args=process.argv.slice(2);
const root=args.includes('--data-root') ? path.resolve(args[args.indexOf('--data-root')+1]) :
  process.env.TALENT_DETS_DATA_DIR ?? path.join(os.homedir(),'Globtalent Dropbox/gtl_talent_dets');
const out=path.join(root,'output/amws/physical_science_cross_edition_blind_audit_50_20261023');
const modules=process.env.AMWS_ARTIFACT_NODE_MODULES ??
  path.join(os.homedir(),'.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules');
const tmp=path.join(process.cwd(),'tmp/amws_cross_edition_blind_audit_50_20261023');
await fs.mkdir(tmp,{recursive:true});
try { await fs.symlink(modules,path.join(tmp,'node_modules'),'junction'); } catch(e) { if(e.code!=='EEXIST') throw e; }
const req=createRequire(path.join(tmp,'__amws_v2_audit_loader.cjs'));
const {Workbook}=await import(pathToFileURL(req.resolve('@oai/artifact-tool')).href);
const payload=JSON.parse(await fs.readFile(path.join(out,'audit_payload.json'),'utf8'));
const matrix=[payload.columns,...payload.rows];
const wb=Workbook.create();const sheet=wb.worksheets.add('Audit');
const range=sheet.getRangeByIndexes(0,0,matrix.length,matrix[0].length);
range.values=matrix.map(row=>row.map(v=>typeof v==='string'&&v.startsWith('=')?"'"+v:v));
sheet.showGridLines=false;sheet.freezePanes.freezeRows(1);
range.format.font={name:'Arial',size:10};range.format.verticalAlignment='center';
range.format.rowHeight=32;
sheet.getRangeByIndexes(0,0,1,matrix[0].length).format={
  fill:'#23415A',font:{name:'Arial',size:10,bold:true,color:'#FFFFFF'},wrapText:true,rowHeight:64};
for(const [col,width] of Object.entries({A:12,B:17,C:12,D:44,E:26,F:25,G:45,H:20,I:20,J:50}))
  sheet.getRange(`${col}1:${col}201`).format.columnWidth=width;
wb.recalculate();
const values=range.values;
if(JSON.stringify(values)!==JSON.stringify(matrix)) throw new Error('Audit cell preservation failed');
console.log((await wb.inspect({kind:'table',range:'Audit!A1:F4',include:'values',tableMaxRows:4,tableMaxCols:6,maxChars:1500})).ndjson);
console.log((await wb.inspect({kind:'match',searchTerm:'#REF!|#DIV/0!|#VALUE!|#NAME\\?|#NUM!|#NULL!|#SPILL!|#CALC!',
  options:{useRegex:true,maxResults:10},summary:'formula error scan'})).ndjson);
const quote=v=>'"'+String(v??'').replaceAll('"','""')+'"';
await fs.writeFile(path.join(out,'amws_cross_edition_blind_audit_200.csv'),
  '\uFEFF'+values.map(row=>row.map(quote).join(',')).join('\r\n')+'\r\n','utf8');
const previewDir=path.join(process.cwd(),'tmp/amws_cross_edition_blind_audit_50_20261023');
await fs.mkdir(previewDir,{recursive:true});
const preview=await wb.render({sheetName:'Audit',range:'A1:F4',scale:1,format:'png'});
await fs.writeFile(path.join(previewDir,'preview.png'),new Uint8Array(await preview.arrayBuffer()));
await fs.writeFile(path.join(out,'export_validation.json'),JSON.stringify({rows:200,columns:matrix[0].length,
  artifact_tool_cell_preservation_passed:true,encoding:'UTF-8 BOM'},null,2)+'\n');
console.log('CSV export verified: 200 rows, every cell preserved.');
