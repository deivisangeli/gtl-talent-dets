#!/usr/bin/env python3
import json
import os
import re
from pathlib import Path

# State abbreviation mappings
STATE_MAPPING = {
    # 2-letter codes
    'NY': 'NY', 'ND': 'ND', 'SD': 'SD', 'MA': 'MA', 'PA': 'PA', 'CA': 'CA', 'IL': 'IL', 'CT': 'CT',
    'NE': 'NE', 'IN': 'IN', 'WA': 'WA', 'MI': 'MI', 'NM': 'NM', 'KS': 'KS', 'OK': 'OK', 'TX': 'TX',
    'AZ': 'AZ', 'OR': 'OR', 'CO': 'CO', 'WY': 'WY', 'MT': 'MT', 'NV': 'NV', 'UT': 'UT', 'ID': 'ID',
    'OH': 'OH', 'GA': 'GA', 'FL': 'FL', 'AL': 'AL', 'MS': 'MS', 'LA': 'LA', 'TN': 'TN', 'KY': 'KY',
    'VA': 'VA', 'WV': 'WV', 'NC': 'NC', 'SC': 'SC', 'MD': 'MD', 'DE': 'DE', 'NJ': 'NJ', 'RI': 'RI',
    'VT': 'VT', 'NH': 'NH', 'ME': 'ME', 'MO': 'MO', 'AR': 'AR', 'IA': 'IA', 'MN': 'MN', 'WI': 'WI',
    'DC': 'DC',
    # Full names and abbreviations
    'N. Y': 'NY', 'N Y': 'NY', 'New York': 'NY',
    'N. Dak': 'ND', 'N Dak': 'ND', 'N. Dakota': 'ND', 'N Dakota': 'ND', 'North Dakota': 'ND',
    'S. Dak': 'SD', 'S Dak': 'SD', 'S. Dakota': 'SD', 'S Dakota': 'SD', 'South Dakota': 'SD',
    'Mass': 'MA', 'Maas': 'MA', 'Massachusetts': 'MA',
    'Pa': 'PA', 'PA': 'PA', 'Penn': 'PA', 'Pennsylvania': 'PA',
    'Calif': 'CA', 'Cali': 'CA', 'California': 'CA',
    'Ill': 'IL', 'III': 'IL', 'm': 'IL', '111': 'IL', 'Illinois': 'IL',
    'Conn': 'CT', 'Ct': 'CT', 'Connecticut': 'CT',
    'Nebr': 'NE', 'Neb': 'NE', 'Nebraska': 'NE',
    'Ind': 'IN', 'Indiana': 'IN',
    'Wash': 'WA', 'Washington': 'WA',
    'Mich': 'MI', 'Michigan': 'MI',
    'N Mex': 'NM', 'N. Mex': 'NM', 'New Mexico': 'NM',
    'Kan': 'KS', 'Kansas': 'KS', 'Kans': 'KS', 'Sana': 'KS', 'Sana.': 'KS',  # OCR misread of "Kans"
    'Okla': 'OK', 'Oklahoma': 'OK',
    'Texas': 'TX', 'Tex': 'TX',
    'Ariz': 'AZ', 'Arizona': 'AZ',
    'Ore': 'OR', 'Oregon': 'OR',
    'Col': 'CO', 'Colo': 'CO', 'Colorado': 'CO',
    'Wyo': 'WY', 'Wyoming': 'WY',
    'Mon': 'MT', 'Mont': 'MT', 'Montana': 'MT',
    'Nev': 'NV', 'Nevada': 'NV',
    'Utah': 'UT', 'Ut': 'UT',
    'Idaho': 'ID', 'Ida': 'ID',
    'Ohio': 'OH', 'O': 'OH',
    'Georgia': 'GA', 'Ga': 'GA',
    'Florida': 'FL', 'Fla': 'FL', 'Fl': 'FL',
    'Alabama': 'AL', 'Ala': 'AL',
    'Mississippi': 'MS', 'Miss': 'MS',
    'Louisiana': 'LA',
    'Tennessee': 'TN', 'Tenn': 'TN',
    'Kentucky': 'KY', 'Ky': 'KY',
    'Virginia': 'VA', 'Va': 'VA',
    'W. Virginia': 'WV', 'W Virginia': 'WV', 'West Virginia': 'WV',
    'North Carolina': 'NC', 'N. Carolina': 'NC', 'N Carolina': 'NC', 'N.C': 'NC',
    'South Carolina': 'SC', 'S. Carolina': 'SC', 'S Carolina': 'SC', 'S.C': 'SC',
    'Maryland': 'MD', 'Md': 'MD',
    'Delaware': 'DE', 'Del': 'DE',
    'New Jersey': 'NJ', 'N.J': 'NJ', 'N J': 'NJ',
    'Rhode Island': 'RI', 'R.I': 'RI', 'R I': 'RI',
    'Vermont': 'VT', 'Vt': 'VT', 'Verm': 'VT',
    'New Hampshire': 'NH', 'N.H': 'NH', 'N H': 'NH',
    'Maine': 'ME',
    'Missouri': 'MO', 'Mo': 'MO',
    'Arkansas': 'AR', 'Ark': 'AR',
    'Iowa': 'IA', 'Ia': 'IA',
    'Minnesota': 'MN', 'Minn': 'MN',
    'Wisconsin': 'WI', 'Wis': 'WI',
    'L I': 'NY', 'L.I': 'NY', 'L. I': 'NY',  # Long Island = NY
}

COUNTRY_MAPPING = {
    'Can': 'Canada',
    'Canada': 'Canada',
    'N. B': 'Canada',  # New Brunswick
    'N B': 'Canada',
    'Man': 'Canada',  # Manitoba
    'MB': 'Canada',  # Manitoba
    'Man.': 'Canada',
    'Manis': 'Canada',  # OCR artifact for Manitoba
    'P. I': 'Philippines',
    'Philippines': 'Philippines',
    'Sana': 'Unknown',  # OCR artifact, likely "Kansas" but can't be sure
    'Sana.': 'Unknown',
}

# US state list for validation
US_STATES = set(STATE_MAPPING.values())

def extract_birthplace_and_date(text):
    """Extract birthplace and date from OCR text."""
    # Date pattern: Month Day, YY or Month Day YY or Month. Day, YY or Month.Day, YY
    # Common months with or without periods
    months = r'(?:January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|June|July|Aug|Sept|Sep|Oct|Nov|Dec)'
    # Month optionally followed by period, then optional space, then 1-2 digit day, optional comma/period, then year
    date_pattern = months + r'\.?\s*\d{1,2}[,.]?\s*\d{2,4}'

    match = re.search(date_pattern, text, re.IGNORECASE)
    if match:
        birthplace_part = text[:match.start()].strip()
        date_str = text[match.start():match.end()]
        return birthplace_part, date_str
    else:
        # No date found - return everything as birthplace
        return text.strip(), ""

def clean_city_name(city):
    """Clean OCR artifacts from city names."""
    city = city.strip()
    # Remove leading @ and other stray punctuation
    city = re.sub(r'^[@&\*]+', '', city).strip()
    # Rejoin hyphen-broken words
    city = re.sub(r'-\s+', '', city)

    # Fix common OCR issues BEFORE removing trailing punctuation
    city = city.replace('Wsrres', 'Warren')
    city = city.replace('Sana', 'Kans')  # Lyons, Kans (Kansas)
    city = city.replace('Byes-', 'Byes')  # Byesville
    city = city.replace('Byesvllle', 'Byesville')
    city = city.replace('Kalmama', 'Kalamazoo')
    city = city.replace('Ealamasoo', 'Kalamazoo')
    city = city.replace('Waekada', 'Wakanda')
    city = city.replace('Douglaaton', 'Douglaston')

    # Remove trailing punctuation marks (including periods, commas)
    city = re.sub(r'[,;.]+$', '', city).strip()

    return city

def clean_state_abbr(state_abbr):
    """Standardize state abbreviations."""
    state_abbr = state_abbr.strip()
    # Remove trailing punctuation
    state_abbr = re.sub(r'[,;.]+$', '', state_abbr).strip()

    # Direct lookup
    if state_abbr in STATE_MAPPING:
        return STATE_MAPPING[state_abbr]

    # Fuzzy matching: try removing dots and spaces, matching case-insensitive
    normalized = state_abbr.replace('.', '').replace(' ', '').upper()
    for key, val in STATE_MAPPING.items():
        key_norm = key.replace('.', '').replace(' ', '').upper()
        if key_norm == normalized:
            return val

    # If still no match, return as-is
    return state_abbr

def parse_birthplace(birthplace_text):
    """Parse birthplace into city, state, country."""
    birthplace_text = birthplace_text.strip()

    # Common international locations - check full text
    if 'Norway' in birthplace_text:
        return 'Norway', '', 'Norway'
    if 'Turkey' in birthplace_text:
        return 'Konya', '', 'Turkey'

    # Remove @ prefix
    birthplace_text = re.sub(r'^@\s*', '', birthplace_text).strip()

    # Preprocess: Add space after period if followed directly by capital letter (OCR artifact)
    # E.g. "Omaha.Nebr" -> "Omaha. Nebr", "Cincinnati.Ohio" -> "Cincinnati. Ohio"
    birthplace_text = re.sub(r'(\w)\.([A-Z])', r'\1. \2', birthplace_text)

    # Check if there's a comma
    if ',' in birthplace_text:
        parts = [p.strip() for p in birthplace_text.split(',')]
        parts = [p for p in parts if p]  # Remove empty parts

        # Further split any parts containing "City. State" pattern (but only if State is recognized)
        expanded_parts = []
        for part in parts:
            # Check if this part matches the period pattern
            match = re.match(r'^(.+?)\.\s+([A-Z][A-Za-z. ]{0,4})(?:\.|$)', part)
            if match:
                city_part = match.group(1)
                state_part_raw = match.group(2).strip()
                state_part = re.sub(r'[.,;.]+$', '', state_part_raw).strip()
                # Only split if state_part normalizes to a recognized state
                normalized_state = clean_state_abbr(state_part)
                if normalized_state in US_STATES:
                    # It's a real state, so split
                    expanded_parts.append(city_part)
                    expanded_parts.append(state_part)
                else:
                    # Not a recognized state, keep as single part (might be "City. CityName")
                    expanded_parts.append(part)
            else:
                expanded_parts.append(part)
        parts = expanded_parts
    else:
        # No comma - try to split by period if there appears to be a state abbr
        # Look for "CityName. ST" or "CityName. N Y" pattern
        # Match: word char(s) followed by period and space, then capitalized state abbr (with optional space in middle)
        match = re.match(r'^(.+?)\.\s+([A-Z][A-Za-z. ]{0,4})(?:\.|,|$)', birthplace_text)
        if match:
            city_part = match.group(1)
            state_part = match.group(2).strip()  # Remove any trailing period or space
            state_part = re.sub(r'[.,;.]+$', '', state_part).strip()
            rest = birthplace_text[match.end():].strip()
            if rest:
                parts = [city_part, state_part, rest]
            else:
                parts = [city_part, state_part]
        else:
            # No period pattern found, treat as single part (city name)
            parts = [birthplace_text]

    city = ''
    state = ''
    country = ''

    if len(parts) == 0:
        city = ''
        state = ''
        country = ''
    elif len(parts) == 1:
        # Just city
        city = clean_city_name(parts[0])
        state = ''
        country = ''
    elif len(parts) == 2:
        # City, State (most common case)
        city = clean_city_name(parts[0])
        state_or_country = parts[1].strip()

        # Try to normalize as state
        normalized = clean_state_abbr(state_or_country)
        if normalized in US_STATES:
            state = normalized
            country = 'USA'
        elif state_or_country in COUNTRY_MAPPING:
            country = COUNTRY_MAPPING[state_or_country]
        else:
            # Could be a country name or unknown state
            if state_or_country in ['Canada', 'Mexico', 'Turkey', 'Norway', 'Germany', 'England', 'Scotland', 'Ireland', 'France', 'Italy', 'Spain']:
                country = state_or_country
            else:
                # Check if it normalizes to a state
                normalized = clean_state_abbr(state_or_country)
                if normalized in US_STATES:
                    state = normalized
                    country = 'USA'
                else:
                    # Assume it's an unknown state or country
                    state = state_or_country
    elif len(parts) >= 3:
        # City, State, Country or City, Country, Junk
        city = clean_city_name(parts[0])
        state_or_country = parts[1].strip()
        state_normalized = clean_state_abbr(state_or_country)

        if state_normalized in US_STATES:
            # It's a US state
            state = state_normalized
            country = 'USA'
            # Ignore any third+ parts
        elif state_or_country in COUNTRY_MAPPING:
            # parts[1] is a country
            country = COUNTRY_MAPPING[state_or_country]
            state = ''
        else:
            # Check if parts[1] is an unknown state
            state = state_or_country
            # Check parts[2] for country
            third = parts[2].strip()
            if third in COUNTRY_MAPPING:
                country = COUNTRY_MAPPING[third]
            else:
                country = third if third and not re.match(r'^\d+', third) else ''  # Ignore purely numeric third parts

    # Final: ensure USA if we have a US state
    if state and state in US_STATES:
        country = 'USA'

    return city, state, country

def extract_nat_and_date(text):
    """Extract naturalization info and date from full text."""
    nat = ''
    date = ''

    # Extract date - same pattern as extract_birthplace_and_date
    date_pattern = r'(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|June|July|Aug|Sept|Sep|Oct|Nov|Dec)\.?\s*\d{1,2}[,.]?\s*\d{2,4}'
    match = re.search(date_pattern, text, re.IGNORECASE)
    if match:
        date = match.group(0)

    # Extract naturalization - only literal "nat" or "nat. NN"
    nat_pattern = r'\bnat\.?\s*(\d+)?'
    nat_match = re.search(nat_pattern, text, re.IGNORECASE)
    if nat_match:
        year = nat_match.group(1)
        if year:
            nat = f"nat. {year}"
        else:
            nat = "nat"

    # Ignore m., c., wid. (married, children, widowed)

    return nat, date

def main():
    REPO_ROOT = Path(__file__).resolve().parents[2]
    talent_dets_data_dir = Path(os.environ.get(
        "TALENT_DETS_DATA_DIR",
        r"C:\Users\deivi\Globtalent Dropbox\gtl_talent_dets"
    ))
    batch_dir = talent_dets_data_dir / "output" / "amws" / "amws_1955_batches"
    input_file = batch_dir / "in" / "01301.tsv"
    output_file = batch_dir / "out" / "01301.jsonl"

    output_file.parent.mkdir(parents=True, exist_ok=True)

    rows = []
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        for line in lines[1:]:  # Skip header
            parts = line.rstrip('\n').split('\t', 1)
            if len(parts) == 2:
                try:
                    lineid = int(parts[0])
                    birthplace_orig = parts[1]
                    rows.append((lineid, birthplace_orig))
                except ValueError:
                    pass

    with open(output_file, 'w', encoding='utf-8') as f:
        for lineid, birthplace_orig in rows:
            # Extract birthplace and date
            birthplace_part, date_str = extract_birthplace_and_date(birthplace_orig)

            # Extract nat and full date
            nat_str, full_date = extract_nat_and_date(birthplace_orig)

            # Parse birthplace
            city, state, country = parse_birthplace(birthplace_part)

            # Determine flags
            flags = []

            # Check if city is unclear (would need validation against real cities)
            # For now, we flag obvious OCR garbles that weren't corrected
            if not city and not country:
                flags.append('garbled')
            if not full_date and birthplace_orig.strip():
                flags.append('no_date')
            if country and country not in ['USA', 'Canada'] and not city:
                flags.append('foreign_no_country_obvious')

            flag_str = flags[0] if flags else ''

            record = {
                'lineid': lineid,
                'city': city,
                'state': state,
                'country': country,
                'date': full_date,
                'nat': nat_str,
                'flag': flag_str
            }

            f.write(json.dumps(record) + '\n')

    print(f"done {len(rows)}")

if __name__ == '__main__':
    main()
