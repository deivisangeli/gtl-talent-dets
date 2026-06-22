#!/usr/bin/env python3
import json
import os
import re
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
TALENT_DETS_DATA_DIR = Path(os.environ.get(
    "TALENT_DETS_DATA_DIR",
    r"C:\Users\deivi\Globtalent Dropbox\gtl_talent_dets"
))
batch_dir = TALENT_DETS_DATA_DIR / "output" / "amws" / "amws_1955_batches"
input_file = batch_dir / "in" / "00136.tsv"
output_file = batch_dir / "out" / "00136.jsonl"

state_map = {
    'N. Y': 'NY', 'N.Y': 'NY', 'NY': 'NY', 'N. Y.': 'NY',
    'N. H': 'NH', 'N.H': 'NH', 'N.H.': 'NH', 'NH': 'NH',
    'MASS': 'MA', 'Mass': 'MA', 'Maas': 'MA', 'MA': 'MA',
    'PA': 'PA', 'Pa': 'PA', 'p': 'PA',
    'CALIF': 'CA', 'Calif': 'CA', 'CA': 'CA',
    'ILL': 'IL', 'Ill': 'IL', 'M': 'IL', '111': 'IL', 'IL': 'IL',
    'CONN': 'CT', 'Conn': 'CT', 'CT': 'CT',
    'NEBR': 'NE', 'Nebr': 'NE', 'NE': 'NE',
    'IND': 'IN', 'Ind': 'IN', 'IN': 'IN',
    'WASH': 'WA', 'Wash': 'WA', 'WA': 'WA',
    'MICH': 'MI', 'Mich': 'MI', 'MI': 'MI',
    'N MEX': 'NM', 'N. Mex': 'NM', 'N Mex': 'NM', 'NM': 'NM',
    'ONT': '', 'Ont': '', 'O. N. T': '', 'O.N.T.': '', 'Ontario': '',
    'DEL': 'DE', 'Del': 'DE', 'DE': 'DE',
    'KY': 'KY', 'Ky': 'KY', 'KENTUCKY': 'KY', 'Kentucky': 'KY',
    'MAINE': 'ME', 'Maine': 'ME', 'ME': 'ME',
    'KANS': 'KS', 'Kans': 'KS', 'KS': 'KS',
    'COLO': 'CO', 'Colo': 'CO', 'CO': 'CO',
    'MINN': 'MN', 'Minn': 'MN', 'MN': 'MN',
    'OREGON': 'OR', 'Oregon': 'OR', 'OR': 'OR',
    'OHIO': 'OH', 'Ohio': 'OH', 'OH': 'OH',
    'W. VA': 'WV', 'W Va': 'WV', 'W.Va': 'WV', 'W. Va': 'WV', 'WV': 'WV',
    'D. C': '', 'D.C': '', 'D.C.': '', 'DC': '', 'D. C.': '',
    'H. I': 'HI', 'HI': 'HI', 'H. I.': 'HI',
    'CAN': '', 'Can': '', 'CANADA': '', 'Canada': '',
    'N. B, Can': '', 'N B Can': '', 'N. B': '',
    'P. I': '', 'Philippines': '', 'P.I.': '',
}

# OCR error corrections for common city misreads
ocr_fixes = {
    'Bralnard': 'Brainard',
    'Prlneville': 'Prineville',
    'Palnesvllle': 'Painesville',
    'Martlnsburg': 'Martinsburg',
}

# Read input
with open(input_file, 'r', encoding='utf-8') as f:
    lines = f.readlines()[1:]  # Skip header

results = []
row_count = 0

for line in lines:
    line = line.strip()
    if not line:
        continue

    row_count += 1

    parts = line.split('\t')
    if len(parts) < 2:
        continue

    lineid = int(parts[0])
    birthplace = parts[1]

    # Initialize output object
    obj = {
        'lineid': lineid,
        'city': '',
        'state': '',
        'country': '',
        'date': '',
        'nat': '',
        'flag': ''
    }

    # Extract date (stops at first date pattern)
    date_pattern = r'([A-Za-z]+\.?\s+\d{1,2},?\s*\d{2}|\w+\s+\d{1,2},\s*\d{2})'
    date_match = re.search(date_pattern, birthplace)

    birthplace_for_parsing = birthplace

    if date_match:
        obj['date'] = date_match.group(0).strip()
        birthplace_for_parsing = birthplace[:date_match.start()].strip()

    # Extract nat info from original (can appear after date)
    nat_match = re.search(r'nat\.?\s*(\d{2})?', birthplace)
    if nat_match:
        if nat_match.group(1):
            obj['nat'] = f"nat. {nat_match.group(1)}"
        else:
            obj['nat'] = "nat"

    # Clean birthplace string
    birthplace_for_parsing = re.sub(r'^\s*[@&\s]+', '', birthplace_for_parsing)
    birthplace_for_parsing = re.sub(r'\s*[-;,.\s]*$', '', birthplace_for_parsing)

    # Rejoin hyphen-broken words (e.g., "Mar- tlnsburg" -> "Martinsburg")
    birthplace_for_parsing = re.sub(r'(\w+)-\s+(\w+)', r'\1\2', birthplace_for_parsing)

    # Check for garbled data (too many numbers in wrong places, institution names)
    if re.search(r'\bCOL\b|ASSOC\.|PROF|Physics Teachers|Underwater', birthplace_for_parsing):
        obj['flag'] = 'garbled'
        results.append(obj)
        continue

    # Check if this contains "Can" anywhere - indicates Canada
    is_canada = bool(re.search(r'\bCan\b|\bCAN\b|N\. B|N\.B|Ont\.?(?:\s|,|$)', birthplace_for_parsing))

    # Split by comma to get city, state, country parts
    parts = [p.strip() for p in birthplace_for_parsing.split(',') if p.strip()]

    if not parts:
        obj['flag'] = 'garbled'
        results.append(obj)
        continue

    # Fix obvious OCR errors in first part (city)
    if parts[0] in ocr_fixes:
        parts[0] = ocr_fixes[parts[0]]

    # Strip trailing country abbreviation from city if present
    parts[0] = re.sub(r'\s+(Can|Can\.|Ont\.?|Ontario|P\.?I\.?)\s*$', '', parts[0]).strip()

    obj['city'] = parts[0]

    # Determine state and country
    if len(parts) == 1:
        # Just city
        if is_canada:
            obj['state'] = ''
            obj['country'] = 'Canada'
        else:
            obj['state'] = ''
            obj['country'] = 'USA'
    elif len(parts) == 2:
        state_or_country = parts[1]

        # If we detected Canada anywhere in the original string, use it
        if is_canada:
            obj['state'] = ''
            obj['country'] = 'Canada'
        # Check if this part is a known country designation
        elif re.search(r'(?:Can|Ont|N\.?\s*B)', state_or_country):
            obj['state'] = ''
            obj['country'] = 'Canada'
        elif re.search(r'P\.?\s*I', state_or_country):
            obj['state'] = ''
            obj['country'] = 'Philippines'
        else:
            # Check state map
            if state_or_country in state_map:
                mapped = state_map[state_or_country]
                if mapped == '':
                    # This was a country/DC abbreviation
                    obj['state'] = ''
                    obj['country'] = 'USA'
                else:
                    obj['state'] = mapped
                    obj['country'] = 'USA'
            else:
                # Unknown - could be state abbreviation or city name
                # If it looks like a state (2-3 chars, mostly letters), treat as state
                if len(state_or_country) <= 3 and re.match(r'^[A-Z\.]{1,3}$', state_or_country):
                    obj['state'] = state_or_country
                    obj['country'] = 'USA'
                else:
                    # Otherwise treat as part of city or location info
                    obj['city'] = f"{parts[0]}, {state_or_country}"
                    obj['state'] = ''
                    obj['country'] = 'USA'

    elif len(parts) >= 3:
        # City, intermediate, country/state
        state_or_country = parts[1]
        final = parts[2]

        # Check if final part is Canada indicator
        if re.search(r'(?:Can|Ont)', final) or is_canada:
            obj['state'] = ''
            obj['country'] = 'Canada'
        else:
            # Use second part as state
            if state_or_country in state_map:
                mapped = state_map[state_or_country]
                obj['state'] = mapped if mapped else ''
            else:
                obj['state'] = state_or_country
            obj['country'] = 'USA'

    # Set flags
    if not obj['date']:
        obj['flag'] = 'no_date'

    results.append(obj)

# Write output
with open(output_file, 'w', encoding='utf-8') as f:
    for obj in results:
        f.write(json.dumps(obj, separators=(',', ':')) + '\n')

print(f"done {row_count}")
