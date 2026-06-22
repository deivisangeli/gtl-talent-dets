#!/usr/bin/env python3
import json
import re
import sys

# State abbreviation mappings (historic -> USPS)
STATE_ABBREV = {
    'n. y': 'NY', 'n.y': 'NY', 'ny': 'NY',
    'n. j': 'NJ', 'n.j': 'NJ', 'nj': 'NJ',
    'mass': 'MA', 'maas': 'MA', 'ma': 'MA',
    'pa': 'PA', 'penn': 'PA',
    'calif': 'CA', 'cal': 'CA', 'ca': 'CA',
    'ill': 'IL', 'm': 'IL', '111': 'IL', 'ills': 'IL',
    'conn': 'CT', 'ct': 'CT',
    'nebr': 'NE', 'ne': 'NE',
    'ind': 'IN', 'in': 'IN',
    'wash': 'WA', 'wa': 'WA',
    'mich': 'MI', 'mi': 'MI',
    'n mex': 'NM', 'n. mex': 'NM',
    'wis': 'WI', 'wls': 'WI', 'wi': 'WI',
    'dak': 'ND', 'n. dak': 'ND', 'n d': 'ND', 'n dak': 'ND',
    's. dak': 'SD', 's dak': 'SD', 's. dak': 'SD',
    'minn': 'MN', 'mn': 'MN',
    'iowa': 'IA', 'ia': 'IA',
    'ohio': 'OH', 'o': 'OH',
    'kan': 'KS', 'kans': 'KS',
    'mo': 'MO', 'missouri': 'MO',
    'texas': 'TX', 'tex': 'TX',
    'tenn': 'TN', 'tn': 'TN',
    'virginia': 'VA', 'va': 'VA',
    'maryland': 'MD', 'md': 'MD',
    'nc': 'NC', 'n.c': 'NC', 'n. c': 'NC',
    'sc': 'SC', 's.c': 'SC', 's. c': 'SC',
    'georgia': 'GA', 'ga': 'GA',
    'florida': 'FL', 'fla': 'FL', 'fl': 'FL',
    'louisiana': 'LA', 'la': 'LA',
    'alabama': 'AL', 'ala': 'AL', 'al': 'AL',
    'mississippi': 'MS', 'miss': 'MS',
    'arkansas': 'AR', 'ark': 'AR', 'ar': 'AR',
    'oklahoma': 'OK', 'okla': 'OK', 'ok': 'OK',
    'colorado': 'CO', 'colo': 'CO', 'col': 'CO',
    'wyoming': 'WY', 'wy': 'WY',
    'montana': 'MT', 'mont': 'MT', 'mt': 'MT',
    'idaho': 'ID', 'ida': 'ID', 'id': 'ID',
    'utah': 'UT', 'ut': 'UT',
    'nevada': 'NV', 'nev': 'NV', 'nv': 'NV',
    'new mexico': 'NM', 'new mex': 'NM',
}

# US city corrections with known states
US_CITIES = {
    'hailey': {'state': 'ID', 'correct': 'Hailey'},
    'halley': {'ignore': True},  # not a real US town
    'delavan': {'state': 'WI', 'correct': 'Delavan'},
    'hoopeston': {'state': 'IL', 'correct': 'Hoopeston'},
    'chicago': {'state': 'IL', 'correct': 'Chicago'},
    'bloomington': {'states': ['CA', 'IL', 'IN'], 'correct': 'Bloomington'},
    'maquoketa': {'state': 'IA', 'correct': 'Maquoketa'},
    'mazeppa': {'state': 'MN', 'correct': 'Mazeppa'},
    'fond du lac': {'state': 'WI', 'correct': 'Fond du Lac'},
    'scandinavia': {'state': 'WI', 'correct': 'Scandinavia'},
    'south manchester': {'state': 'CT', 'correct': 'South Manchester'},
}

# Country mappings
COUNTRY_ABBREV = {
    'can': 'Canada', 'canada': 'Canada',
    'p. i': 'Philippines', 'p.i': 'Philippines',
    'england': 'England', 'eng': 'England',
    'scotland': 'Scotland', 'scot': 'Scotland',
    'ireland': 'Ireland', 'ire': 'Ireland',
    'france': 'France', 'fr': 'France',
    'germany': 'Germany', 'ger': 'Germany',
    'italy': 'Italy', 'ital': 'Italy',
    'spain': 'Spain',
    'japan': 'Japan',
    'china': 'China',
    'india': 'India',
    'russia': 'Russia', 'ussr': 'Russia',
}

def clean_birthplace_string(birthplace_orig):
    """Parse and clean a birthplace string."""

    # Stop at date indicator and everything after
    # Pattern: month name or number, day, year (2-digit)
    date_pattern = r'(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec|\d{1,2})?[\s,]?(\d{1,2})?[\s,]?(\d{2})'

    # Find where the date starts
    match = re.search(date_pattern, birthplace_orig, re.IGNORECASE)
    if match:
        # Extract date and everything before it
        date_start = match.start()
        main_part = birthplace_orig[:date_start].strip()
        date_part = birthplace_orig[date_start:].strip()
    else:
        main_part = birthplace_orig.strip()
        date_part = ""

    # Extract city and state/country from main_part
    # Format: "City, State" or "City, Country"
    parts = [p.strip() for p in main_part.split(',')]

    city = ""
    state = ""
    country = ""
    flag = ""

    if len(parts) >= 1:
        city = parts[0].strip()

    if len(parts) >= 2:
        location = parts[1].strip()
        location_lower = location.lower()

        # Check if it's a US state abbreviation
        if location_lower in STATE_ABBREV:
            state = STATE_ABBREV[location_lower]
            country = "USA"
        elif location_lower in COUNTRY_ABBREV:
            country = COUNTRY_ABBREV[location_lower]
            state = ""
        else:
            # Try to match full state names
            if location_lower in ['idaho', 'id']:
                state = 'ID'
                country = 'USA'
            elif location_lower in ['california', 'calif', 'ca', 'cal']:
                state = 'CA'
                country = 'USA'
            elif location_lower in ['wisconsin', 'wis', 'wls', 'wi']:
                state = 'WI'
                country = 'USA'
            elif location_lower in ['iowa', 'ia']:
                state = 'IA'
                country = 'USA'
            elif location_lower in ['minnesota', 'minn', 'mn']:
                state = 'MN'
                country = 'USA'
            elif location_lower in ['new jersey', 'n. j', 'n j', 'nj']:
                state = 'NJ'
                country = 'USA'
            elif location_lower in ['new york', 'n. y', 'n y', 'ny']:
                state = 'NY'
                country = 'USA'
            elif location_lower == 'dakota':
                # Could be ND or SD - flag as ambiguous
                flag = "state_ambiguous"
                state = ""
            else:
                # Unknown location initially - will check if city lookup fixes it
                country = ""
                flag = "foreign_no_country_obvious"

    # Clean city name: fix hyphen breaks, OCR errors, strip punct
    city = clean_city_name(city, state)

    # If city is a known US city, override state and clear flag if needed
    city_lower = city.lower()
    if city_lower in US_CITIES:
        city_info = US_CITIES[city_lower]
        if 'state' in city_info:
            state = city_info['state']
            country = 'USA'
            flag = ""  # Clear the foreign flag since we've identified it as US
        if 'correct' in city_info:
            city = city_info['correct']

    # Extract date
    date_str = extract_date(date_part)

    # Extract naturalization info
    nat_str = extract_naturalization(birthplace_orig)

    return {
        'city': city,
        'state': state,
        'country': country,
        'date': date_str,
        'nat': nat_str,
        'flag': flag
    }

def clean_city_name(city, state):
    """Clean city name: fix breaks, OCR errors, etc."""
    city = city.strip()

    # Remove stray punctuation/symbols
    city = re.sub(r'[@&\*]', '', city)

    # Fix obvious OCR misreads
    city = fix_ocr_errors(city)

    # Rejoin hyphen-broken words
    city = re.sub(r'-\s+', '', city)  # "Fond- du Lac" -> "Fond du Lac"

    # Title case
    city = ' '.join(word.capitalize() for word in city.split())

    return city.strip()

def fix_ocr_errors(city):
    """Fix known OCR misreads."""
    city_lower = city.lower()

    # Common OCR substitutions
    fixes = {
        'halley': 'Hailey',  # Idaho town, OCR confusion
        'hoopeston': 'Hoopeston',
        'bloomington': 'Bloomington',
        'maquoketa': 'Maquoketa',
        'mazeppa': 'Mazeppa',
        'fond du lac': 'Fond du Lac',
        'fond- du lac': 'Fond du Lac',
        'scandinavia': 'Scandinavia',
        'south manchester': 'South Manchester',
    }

    for k, v in fixes.items():
        if city_lower == k or city_lower.strip() == k:
            return v

    return city

def extract_date(date_part):
    """Extract month/day/2-digit-year from date part, as written."""
    if not date_part:
        return ""

    # Find the first occurrence of a digit pattern that looks like a date
    # Pattern: optional month, optional day, required year (2-digit)
    date_pattern = r'(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec)?\s*(\d{1,2})?\s*[,\s]*(\d{2})'

    match = re.search(date_pattern, date_part, re.IGNORECASE)
    if match:
        month = match.group(1) or ""
        day = match.group(2) or ""
        year = match.group(3) or ""

        # Build date string: Month Day, Year
        parts = []
        if month:
            parts.append(month)
        if day:
            if month:
                parts.append(day + ",")
            else:
                parts.append(day + ",")
        if year:
            parts.append(year)

        # Return as written, properly formatted
        result = " ".join(parts).replace(" ,", ",").strip()
        return result

    return ""

def extract_naturalization(text):
    """Extract naturalization info (nat. NN or just nat)."""
    # Look for "nat" or "nat." followed by optional year
    # IGNORE: m. (married), c. (children), wid. (widowed)

    nat_pattern = r'\bnat\.?\s*(\d{2})?'
    match = re.search(nat_pattern, text, re.IGNORECASE)

    if match:
        year = match.group(1)
        if year:
            return f"nat. {year}"
        else:
            return "nat"

    return ""

def main():
    import os
    _db = os.environ.get("TALENT_DETS_DATA_DIR", r"C:\Users\deivi\Globtalent Dropbox\gtl_talent_dets")
    _batches = os.path.join(_db, "output", "amws", "amws_1955_batches")
    input_path  = os.path.join(_batches, "in",  "00095.tsv")
    output_path = os.path.join(_batches, "out", "00095.jsonl")

    # Read input
    with open(input_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Skip header
    data_lines = lines[1:]

    # Count rows before processing
    row_count = len([l for l in data_lines if l.strip()])

    # Process each row
    results = []
    for line in data_lines:
        line = line.strip()
        if not line:
            continue

        parts = line.split('\t')
        if len(parts) >= 2:
            lineid = int(parts[0])
            birthplace_orig = parts[1]

            cleaned = clean_birthplace_string(birthplace_orig)
            cleaned['lineid'] = lineid
            results.append(cleaned)

    # Write JSONL output
    with open(output_path, 'w', encoding='utf-8') as f:
        for result in results:
            f.write(json.dumps(result) + '\n')

    print(f"done {row_count}")

if __name__ == '__main__':
    main()
