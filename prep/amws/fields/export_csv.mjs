// Export the requested 500-row CSV using the bundled artifact tool.
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';

const home = os.homedir();
const moduleDir = process.env.AMWS_ARTIFACT_NODE_MODULES ??
  path.join(home, '.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules');
const req = createRequire(path.join(moduleDir, '..', '__amws_artifact_loader.cjs'));
const {Workbook} = await import(pathToFileURL(req.resolve('@oai/artifact-tool')).href);
const root = process.env.TALENT_DETS_DATA_DIR ?? path.join(home, 'Globtalent Dropbox/gtl_talent_dets');
const out = path.join(root, 'output/amws/field_regex_pilot_20261020');
const payload = JSON.parse(await fs.readFile(path.join(out, 'audit_payload_with_guard.json'), 'utf8'));
const wb = Workbook.create();
const sheet = wb.worksheets.add('Audit');
const matrix = [payload.columns, ...payload.rows];
const range = sheet.getRangeByIndexes(0, 0, matrix.length, matrix[0].length);
range.values = matrix;
range.format.font = {name:'Arial',size:10};
range.format.wrapText = true;
range.format.verticalAlignment = 'top';
range.format.rowHeight = 40;
sheet.getRangeByIndexes(0,0,1,matrix[0].length).format = {
  fill:'#23415A',font:{name:'Arial',size:10,bold:true,color:'#FFFFFF'},rowHeight:32,wrapText:true};
for (const [col,width] of Object.entries({A:10,B:16,C:16,D:10,E:38,F:12,G:50,H:25,I:24,J:22})) {
  sheet.getRange(`${col}1:${col}501`).format.columnWidth = width;
}
sheet.showGridLines = false;
sheet.freezePanes.freezeRows(1);
wb.recalculate();
const values = range.values;
if (JSON.stringify(values)!==JSON.stringify(matrix)) throw new Error('Artifact value preservation check failed');
console.log((await wb.inspect({kind:'table',range:'Audit!A1:J4',include:'values',tableMaxRows:4,tableMaxCols:10,maxChars:2000})).ndjson);
const quote = x => '"' + String(x ?? '').replaceAll('"','""') + '"';
await fs.writeFile(path.join(out,'amws_field_regex_pilot_500.csv'),
  '\uFEFF'+values.map(row=>row.map(quote).join(',')).join('\r\n')+'\r\n','utf8');
const previewDir = path.join(process.cwd(),'tmp/amws_field_regex_pilot_20261020');
await fs.mkdir(previewDir,{recursive:true});
const preview = await wb.render({sheetName:'Audit',range:'A1:J5',scale:1,format:'png'});
await fs.writeFile(path.join(previewDir,'audit_preview.png'),new Uint8Array(await preview.arrayBuffer()));
console.log(JSON.stringify({rows:payload.rows.length,columns:payload.columns.length,format:'CSV',encoding:'UTF-8 BOM'}));
