import fs from 'node:fs/promises';
import path from 'node:path';
import {createRequire} from 'node:module';
import {pathToFileURL} from 'node:url';
const [stage, csvPath, xlsxPath, sheetName='amws_ed86_final', previewOnly='false'] = process.argv.slice(2);
const runtime = process.env.AMWS_ARTIFACT_RUNTIME || path.join(stage, 'runtime');
const require = createRequire(path.join(runtime, 'package.json'));
const {Workbook, SpreadsheetFile} = await import(pathToFileURL(require.resolve('@oai/artifact-tool')).href);
const wb = await Workbook.fromCSV((await fs.readFile(csvPath,'utf8')).replace(/^\uFEFF/,''), {sheetName});
const sheet = wb.worksheets.getItemAt(0);
if(previewOnly==='help') {
  console.log(wb.help('worksheet.autoFilter',{include:'index,examples,notes',maxChars:2500}).ndjson);
  process.exit(0);
}
sheet.freezePanes.freezeRows(1);
if (sheetName.endsWith('final') || sheetName==='amws_all_editions_persons') {
  sheet.freezePanes.freezeColumns(1);
  sheet.showGridLines=false;
  const cols = sheet.getUsedRange().columnCount;
  sheet.getRangeByIndexes(0,0,1,cols).format.fill='#1F4E78';
  sheet.getRangeByIndexes(0,0,1,cols).format.font={bold:true,color:'#FFFFFF'};
  sheet.getRangeByIndexes(0,0,1,cols).format.rowHeightPx=32;
  const widths={entry_id:90,edition:90,source_record_id:160,name_raw:220,raw_text_adjusted:550,
    birth_place:270,birth_date:120,birth_year:90,birth_city:180,birth_state:110,birth_country:135,
    is_us_birth:120,is_us_geocoded:125,geo_lat:100,geo_lon:100,geo_geoid:110,
    geo_county_name:190,geo_matched_name:230,geocoding_status:160};
  const headers=sheet.getRangeByIndexes(0,0,1,cols).values[0];
  for(let c=0;c<cols;c++) sheet.getRangeByIndexes(0,c,1,1).format.columnWidthPx=widths[headers[c]]??160;
}
console.log((await wb.inspect({kind:'region',sheetId:sheetName,range:'A1:C4',maxChars:1000,tableMaxCellChars:80})).ndjson);
if(previewOnly==='true') {
  const png=await wb.render({sheetName,range:'A1:H5',scale:1,format:'png'});
  await fs.writeFile(xlsxPath,new Uint8Array(await png.arrayBuffer()));
} else {
  const xlsx=await SpreadsheetFile.exportXlsx(wb);await xlsx.save(xlsxPath);
}
console.log(`Saved ${xlsxPath}`);
