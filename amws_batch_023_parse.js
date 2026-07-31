const fs = require('fs');

const inputPath = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets/output/amws/regex_all_docs/codex_subagents_20w_city_year_missing/batch_023_input.json';
const outputPath = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets/output/amws/regex_all_docs/codex_subagents_20w_city_year_missing/batch_023_output.json';

const input = JSON.parse(fs.readFileSync(inputPath, 'utf8'));

const stateMap = new Map([
  ['ala', 'AL'], ['alab', 'AL'], ['alabama', 'AL'],
  ['ariz', 'AZ'], ['arizona', 'AZ'],
  ['ark', 'AR'], ['arkansas', 'AR'],
  ['calif', 'CA'], ['california', 'CA'],
  ['colo', 'CO'], ['colorado', 'CO'],
  ['conn', 'CT'], ['connecticut', 'CT'],
  ['del', 'DE'], ['delaware', 'DE'],
  ['dc', 'DC'], ['d c', 'DC'], ['dist col', 'DC'],
  ['fla', 'FL'], ['florida', 'FL'],
  ['ga', 'GA'], ['georgia', 'GA'],
  ['ill', 'IL'], ['illinois', 'IL'],
  ['ind', 'IN'], ['indiana', 'IN'],
  ['iowa', 'IA'],
  ['kans', 'KS'], ['kansas', 'KS'],
  ['ky', 'KY'], ['kentucky', 'KY'],
  ['la', 'LA'], ['louisiana', 'LA'],
  ['maine', 'ME'],
  ['md', 'MD'], ['maryland', 'MD'],
  ['mass', 'MA'], ['massachusetts', 'MA'],
  ['mich', 'MI'], ['michigan', 'MI'],
  ['minn', 'MN'], ['minnesota', 'MN'],
  ['miss', 'MS'], ['mississippi', 'MS'],
  ['mo', 'MO'], ['missouri', 'MO'],
  ['mont', 'MT'], ['montana', 'MT'],
  ['neb', 'NE'], ['nebraska', 'NE'],
  ['nev', 'NV'], ['nevada', 'NV'],
  ['nh', 'NH'], ['new hampshire', 'NH'],
  ['nj', 'NJ'], ['new jersey', 'NJ'],
  ['nm', 'NM'], ['new mexico', 'NM'],
  ['ny', 'NY'], ['new york', 'NY'],
  ['nc', 'NC'], ['north carolina', 'NC'],
  ['nd', 'ND'], ['north dakota', 'ND'],
  ['oh', 'OH'], ['ohio', 'OH'],
  ['okla', 'OK'], ['oklahoma', 'OK'],
  ['ore', 'OR'], ['oregon', 'OR'],
  ['pa', 'PA'], ['penn', 'PA'], ['pennsylvania', 'PA'],
  ['ri', 'RI'], ['rhode island', 'RI'],
  ['sc', 'SC'], ['south carolina', 'SC'],
  ['sd', 'SD'], ['south dakota', 'SD'],
  ['tenn', 'TN'], ['tennessee', 'TN'],
  ['tex', 'TX'], ['texas', 'TX'],
  ['utah', 'UT'],
  ['vt', 'VT'], ['vermont', 'VT'],
  ['va', 'VA'], ['virginia', 'VA'],
  ['wash', 'WA'], ['washington', 'WA'],
  ['wv', 'WV'], ['west virginia', 'WV'],
  ['wis', 'WI'], ['wisconsin', 'WI'],
  ['wyo', 'WY'], ['wyoming', 'WY']
]);

const countryMap = new Map([
  ['eng', 'England'], ['england', 'England'],
  ['scot', 'Scotland'], ['scotland', 'Scotland'],
  ['wales', 'Wales'],
  ['ire', 'Ireland'], ['ireland', 'Ireland'],
  ['can', 'Canada'], ['canada', 'Canada'],
  ['poland', 'Poland'],
  ['germany', 'Germany'],
  ['netherlands', 'Netherlands'], ['holland', 'Netherlands'],
  ['india', 'India'],
  ['sri lanka', 'Sri Lanka'], ['ceylon', 'Sri Lanka'],
  ['cyprus', 'Cyprus'],
  ['arg', 'Argentina'], ['argentina', 'Argentina'],
  ['mex', 'Mexico'], ['mexico', 'Mexico'],
  ['us', 'USA'], ['usa', 'USA']
]);

function clean(text) {
  return String(text || '').replace(/\s+/g, ' ').trim();
}

function normalizeYear(y) {
  if (!/^\d{1,4}$/.test(y)) return null;
  const n = parseInt(y, 10);
  if (n >= 1000 && n <= 1986) return n;
  if (n >= 0 && n <= 25) return 1900 + n;
  if (n >= 26 && n <= 99) return 1800 + n;
  return null;
}

function normState(s) {
  if (!s) return null;
  const key = s.toLowerCase().replace(/\./g, '').replace(/\s+/g, ' ').trim();
  if (stateMap.has(key)) return stateMap.get(key);
  if (/^[A-Z]{2}$/.test(s)) return s;
  return null;
}

function normCountry(s) {
  if (!s) return null;
  const key = s.toLowerCase().replace(/\./g, '').replace(/\s+/g, ' ').trim();
  if (countryMap.has(key)) return countryMap.get(key);
  return null;
}

function parseRow(raw) {
  const text = clean(raw);
  const out = { city: null, state: null, country: null, year: null, confidence: 'low', note: 'no_birth_info' };
  if (!text) return out;

  const m = text.match(/(?:^|[\s,;])b\s+([^;]+)/);
  if (!m) {
    return out;
  }

  const segment = m[1].replace(/^\s+/, '');
  const seg = segment.replace(/\s+/g, ' ');
  const dateMatch = seg.match(/(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|January|February|March|April|June|July|August|September|October|November|December)\.?\s+\d{1,2}[,\.]?\s+(\d{2,4})\b/i);
  if (dateMatch) out.year = normalizeYear(dateMatch[1]);
  else {
    const date2 = seg.match(/\b(?:May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|Aug|Sept|Sep|Oct|Nov|Dec)\.?\s+\d{1,2}[,\.]?\s+(\d{2})\b/i);
    if (date2) out.year = normalizeYear(date2[1]);
    else {
      const date3 = seg.match(/\b(?:\d{1,2}\s+)?(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)[a-z]*\.?\s*,?\s*(\d{2})\b/i);
      if (date3 && /b\s+(?:[A-Za-z]+,?\s*)?\d/.test(text)) out.year = normalizeYear(date3[1]);
    }
  }

  let place = seg.split(/;|\./)[0].trim();
  place = place.replace(/^(?:born|b)\s+/i, '').trim();
  place = place.replace(/^(?:nat us|us citizen|can citizen)\b.*$/i, '').trim();
  const parts = place.split(',').map(s => s.trim()).filter(Boolean);
  if (parts.length === 1) {
    const c = normCountry(parts[0]);
    const st = normState(parts[0]);
    if (c) out.country = c;
    else if (st) { out.state = st; out.country = 'USA'; }
    else out.city = parts[0];
  } else if (parts.length >= 2) {
    const first = parts[0];
    const second = parts[1];
    const c2 = normCountry(second);
    const s2 = normState(second);
    const s1 = normState(first);
    const c1 = normCountry(first);
    if (c2) {
      out.city = first;
      out.country = c2;
      if (s1) out.state = s1;
    } else if (s2) {
      out.city = first;
      out.state = s2;
      out.country = 'USA';
    } else if (c1) {
      out.country = c1;
    } else {
      out.city = first;
      if (parts.length === 3) {
        const maybeState = normState(parts[1]);
        const maybeCountry = normCountry(parts[2]);
        if (maybeState) out.state = maybeState;
        if (maybeCountry) out.country = maybeCountry;
      }
    }
  }

  if (out.country === 'USA' && !out.state && out.city && /^(US|USA)$/i.test(out.city)) out.city = null;
  if (out.city && /^(US|USA)$/i.test(out.city)) { out.country = 'USA'; out.city = null; }
  if (out.city && out.state && !out.country) out.country = 'USA';
  if (out.country === 'USA' && !out.state && out.city && /^[A-Za-z]+$/.test(out.city) && out.city.length <= 2) {
    out.city = null;
  }

  if (out.city && out.state && out.year != null) { out.confidence = 'high'; out.note = 'city_state_year'; }
  else if (out.city && out.country && out.year != null) { out.confidence = 'high'; out.note = 'city_country_year'; }
  else if (out.country && out.year != null) { out.confidence = 'medium'; out.note = 'country_year'; }
  else if (out.city && out.state) { out.confidence = 'high'; out.note = 'city_state'; }
  else if (out.city && out.country) { out.confidence = 'high'; out.note = 'city_country'; }
  else if (out.city) { out.confidence = 'medium'; out.note = 'city_only'; }
  else if (out.state) { out.confidence = 'medium'; out.note = 'state_only'; }
  else if (out.country) { out.confidence = 'medium'; out.note = 'country_only'; }
  else if (out.year != null) { out.confidence = 'medium'; out.note = 'year_only'; }

  return out;
}

const rows = input.rows.map(r => {
  const parsed = parseRow(r.raw_text_20_words);
  return {
    row_id: r.row_id,
    birth_city: parsed.city,
    birth_state: parsed.state,
    birth_country: parsed.country,
    birth_year: parsed.year,
    confidence: parsed.confidence,
    note: parsed.note
  };
});

fs.writeFileSync(outputPath, JSON.stringify(rows, null, 2));
console.log(`Wrote ${rows.length} rows to ${outputPath}`);
