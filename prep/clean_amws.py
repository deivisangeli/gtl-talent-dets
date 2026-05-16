import json
import re
import sys

# Read input
input_path = r"prep\output\amws_1955_batches\in\00775.tsv"
output_path = r"prep\output\amws_1955_batches\out\00775.jsonl"

with open(input_path, 'r', encoding='utf-8') as f:
    lines = f.readlines()

# Skip header
data_lines = lines[1:]

# State mappings
state_map = {
    "Wis": "WI", "Wisc": "WI", "Wisco": "WI",
    "Ill": "IL", "m": "IL", "111": "IL",
    "N. Y": "NY", "NY": "NY", "N.Y": "NY",
    "Mo": "MO", "Miss": "MS", "Md": "MD",
    "La": "LA", "Calif": "CA", "Cal": "CA",
    "Pa": "PA", "Penn": "PA", "Penna": "PA",
    "Kans": "KS", "Kansas": "KS",
    "Ohio": "OH", "Tenn": "TN", "Mass": "MA", "Maas": "MA",
    "R. I": "RI", "RI": "RI", "R.I": "RI",
    "Texas": "TX", "Tex": "TX", "Utah": "UT",
    "Nebr": "NE", "Neb": "NE", "Ind": "IN",
    "N Mex": "NM", "Wash": "WA", "Mich": "MI",
    "Conn": "CT", "N. H": "NH", "N.H": "NH",
    "N. J": "NJ", "N.J": "NJ", "N J": "NJ",
}

def clean_city(city):
    """Strip stray punct and rejoin hyphen-broken words."""
    city = re.sub(r'\s+-\s+', '', city)  # rejoin hyphen breaks
    city = re.sub(r'[&@]', '', city)     # strip stray chars
    city = city.strip()
    return city

def parse_birthplace(raw):
    """Parse OCR'd birthplace string."""
    result = {
        "city": "",
        "state": "",
        "country": "",
        "date": "",
        "nat": "",
        "flag": ""
    }

    # Completely garbled
    if re.match(r'^\s*@\s*;?\s*$', raw):
        result["flag"] = "garbled"
        return result

    # Extract date: Month DD, YY format
    date_pattern = r'(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec)\.?\s+\d{1,2},\s*\d{2}(?:\d{2})?'
    date_match = re.search(date_pattern, raw)

    before_date = raw
    if date_match:
        result["date"] = date_match.group(0)
        before_date = raw[:date_match.start()]
    else:
        result["flag"] = "no_date"

    # Extract naturalization info (only "nat" or "nat. NN" where NN is year)
    nat_pattern = r'nat\.?\s*(\d{2})?'
    nat_match = re.search(nat_pattern, raw)
    if nat_match:
        if nat_match.group(1):
            result["nat"] = f"nat. {nat_match.group(1)}"
        else:
            result["nat"] = "nat"

    # Parse location part (before date)
    location_part = before_date.strip()

    # Check for foreign countries
    if 'Poland' in location_part:
        result["country"] = "Poland"
        city_part = re.sub(r'Poland.*', '', location_part)
        result["city"] = clean_city(city_part)
        return result

    if 'Russia' in location_part:
        result["country"] = "Russia"
        city_part = re.sub(r'Russia.*', '', location_part)
        result["city"] = clean_city(city_part)
        return result

    if 'Denmark' in location_part:
        result["country"] = "Denmark"
        city_part = re.sub(r'Denmark.*', '', location_part)
        result["city"] = clean_city(city_part)
        return result

    if 'Dominican Republic' in location_part:
        result["country"] = "Dominican Republic"
        city_part = re.sub(r'Dominican Republic.*', '', location_part)
        result["city"] = clean_city(city_part)
        return result

    if re.search(r'Philippines|P\.\s*I\.?', location_part):
        result["country"] = "Philippines"
        city_part = re.sub(r'(Philippines|P\.\s*I\.?).*', '', location_part)
        result["city"] = clean_city(city_part)
        return result

    if re.search(r'Canada|N\.\s*B\.\s*Can', location_part):
        result["country"] = "Canada"
        city_part = re.sub(r'(Canada|N\.\s*B\.\s*Can).*', '', location_part)
        result["city"] = clean_city(city_part)
        return result

    # US location: "City, STATE"
    match = re.match(r'^(.+?),\s*([A-Za-z\.\s]+)$', location_part)
    if match:
        city = match.group(1).strip()
        state_raw = match.group(2).strip()

        # Resolve state
        if state_raw in state_map:
            result["state"] = state_map[state_raw]
        else:
            result["state"] = state_raw

        result["city"] = clean_city(city)
        result["country"] = "USA"

        if not result["city"] or len(result["city"]) < 2:
            result["flag"] = "garbled"

        return result

    # Just state, no city
    match = re.match(r'^([A-Za-z\.\s]+)$', location_part)
    if match:
        state_raw = location_part.strip()
        if state_raw in state_map:
            result["state"] = state_map[state_raw]
        else:
            result["state"] = state_raw
        result["country"] = "USA"
        result["flag"] = "garbled"
        return result

    # Fallback
    if not result["flag"]:
        result["flag"] = "garbled"

    return result

# Process all rows
json_lines = []
for line in data_lines:
    line = line.rstrip('\n')
    if not line.strip():
        continue

    parts = line.split('\t', 1)
    if len(parts) != 2:
        continue

    lineid = int(parts[0])
    birthplace_orig = parts[1]

    parsed = parse_birthplace(birthplace_orig)
    parsed["lineid"] = lineid

    json_lines.append(json.dumps(parsed, separators=(',', ':')))

# Write output
with open(output_path, 'w', encoding='utf-8') as f:
    for line in json_lines:
        f.write(line + '\n')

print(f"done {len(json_lines)}")
