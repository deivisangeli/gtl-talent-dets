---
name: worlds-fairs-visits
description: Manual internet research workflow for estimating total visits/attendance/admissions and recording venues with coordinates for historical world's fairs and industrial expositions. Use when Codex needs to find or audit visit counts, venues, and venue latitude/longitude for fairs using only Fair_name, City, and Year, especially for GTL Talent Determinants world-fairs datasets or tabular fair records.
---

# Worlds Fairs Visits

## Core Rule

Use only `Fair_name`, `City`, `Year`, and `source_url` to identify and search for a fair. `source_url` may be opened as an initial source or search lead when it exists, but it does not replace identity verification against fair name, city, and year. Do not use country, coordinates, notes, `Fair_observation`, existing classifications, or other dataset fields in queries or subagent prompts.

Treat `visits` as the total reported visits, attendance, admissions, or entries for the fair. Do not convert to unique visitors. If a source distinguishes paid admissions from total admissions, record the source's measure in `visits_measure`.

Also record the specific venue, site, grounds, building, palace, park, or exhibition precinct where the fair was held when a reliable source reports it. `venue` is an output field: do not use pre-existing venue-like dataset fields as inputs unless they are part of `Fair_name`, `City`, or `Year`.

For the identified `venue`, record latitude and longitude in decimal degrees using WGS84. Coordinates are output fields: do not use existing dataset coordinates as inputs. If a reliable source does not provide explicit coordinates, geocode the identified venue using the venue name, city, and year context, then document the coordinate source and uncertainty.

## Before Searching

Use a two-stage workflow when working on a batch: the main agent handles subagent spawning, batching, and CSV file I/O, while worker subagents handle only web search, source-tier evaluation, and extraction for assigned fairs. Do not ask worker subagents to manage files or batching.

Ask the user how many subagents to use before any internet research. If the user says `0`, research locally in the main agent. If the user gives `N > 0`, spawn exactly `N` subagents using your subagent tool.

When delegating, give each subagent only:

```text
Fair_name
City
Year
source_url
```

Also give the required output schema. Do not provide extra dataset fields. If `source_url` is missing for a row, pass it as blank or omit it for that row.

If reliable sources indicate the fair was canceled and never opened, set `search_status` to `not_found`, set `visits` to `NA`, and explicitly state that the fair was canceled in `source_note`.

If a subagent fails, experiences a timeout, or returns invalid data, record its batch output with `search_status` as `not_found`, `visits` as `NA`, and detail the failure in `source_note`.

## Source Priority

Prefer sources in this order:

1. BIE / Bureau International des Expositions and centralized international-exposition datasets.
2. Archives, libraries, museums, universities, and academic institutions in the fair city.
3. Official exposition reports, historical catalogues, digitized books, municipal archives, and public records.
4. Academic books/articles, theses, book chapters, commented catalogues, and reputable historical databases.
5. Local newspapers from the fair period, newspaper archives, local retrospective news stories, historical magazines, cultural periodicals, and city-history publications.
6. Local history blogs or exposition compendia only as low-confidence fallback when they report an explicit total.
7. Wikipedia or Wikidata only as fallback, or as a path to stronger cited sources.

If sources conflict, keep the conflict visible. Prefer the higher-tier source as the recommended value only when the fair match is unambiguous.

## Search Procedure

For each fair, search manually on the internet using combinations of:

```text
open source_url and inspect it as an initial source or search lead
"<Actual Fair Name>" "<Actual City>" <Actual Year> visitors
"<Actual Fair Name>" "<Actual City>" <Actual Year> attendance
"<Actual Fair Name>" "<Actual City>" <Actual Year> admissions
"<Actual Fair Name>" "<Actual City>" <Actual Year> visits
"<Actual Fair Name>" "<Actual City>" <Actual Year> newspaper attendance
"<Actual Fair Name>" "<Actual City>" <Actual Year> reported attendance
"<Actual Fair Name>" "<Actual City>" <Actual Year> gate receipts
"<Actual Fair Name>" "<Actual City>" <Actual Year> admission receipts
"<Actual Fair Name>" "<Actual City>" <Actual Year> retrospective
"<Actual Fair Name>" "<Actual City>" <Actual Year> archive
"<Actual Fair Name>" "<Actual City>" <Actual Year> venue
"<Actual Fair Name>" "<Actual City>" <Actual Year> site
"<Actual Fair Name>" "<Actual City>" <Actual Year> grounds
"<Actual Fair Name>" "<Actual City>" <Actual Year> "exhibition building"
"<Actual Fair Name>" "<Actual City>" <Actual Year> palace
"<Actual Fair Name>" "<Actual City>" <Actual Year> park
"<Actual Fair Name>" "<Actual City>" <Actual Year> coordinates
"<Venue Name>" "<Actual City>" coordinates
"<Venue Name>" "<Actual City>" latitude longitude
"<Venue Name>" "<Actual City>" map
site:bie-paris.org "Fair_name" "Year"
site:bie-paris.org "City" "Year" "Visitors"
```

Replace the placeholder variables with the actual fair data before searching. If `source_url` is available, open it before broad search and use it only if it matches the same fair, city, and year or provides a credible citation trail to a matching source. If a reliable source identifies the fair venue, you may then search that venue name together with the city for venue and coordinate lookup.

Use native-language terms when search results indicate the relevant language:

```text
visiteurs, entrees, frequenti, visiteurs totaux
journal, journaux, archives, billets, recettes
Besucher, Besuch, Eintritt, Eintritte, Besucherzahl
Zeitung, Archiv, Eintrittskarten, Einnahmen
visitatori, ingressi, presenze, giornale, archivio
visitantes, visitas, asistencia, entradas, periodico, archivo
visitas, visitantes, ingressos, entradas, jornal, arquivo
venue, site, grounds, exhibition building, palace, park
lieu, site, palais, parc
Ausstellungsgelaende, Palast, Park
sede, recinto, local, palacio, parque
sede, recinto, local, pavilhao, parque
coordinates, latitude, longitude, map
coordonnees, latitude, longitude, carte
Koordinaten, Breite, Laenge, Karte
coordenadas, latitude, longitud, mapa
coordenadas, latitude, longitude, mapa
```

Verify that the source matches all three identity fields as closely as possible: fair name, city, and year. Also verify that `venue` and venue coordinates refer to the venue for that same fair, city, and year. If the match is only partial, record `ambiguous_match`.

If `source_url` points to a generic, incorrect, or ambiguous page, do not use its facts directly. Continue searching from `Fair_name`, `City`, and `Year`, and mention the ambiguity in `source_note` or `venue_note` only if it affects the chosen result.

## Output

If the user requests research for 1 fair, return a compact note with:

```text
Fair_name | City | Year
venue
venue_source_title
venue_source_url
venue_note
venue_latitude
venue_longitude
venue_coordinates_source_title
venue_coordinates_source_url
venue_coordinates_note
visits
visits_measure
confidence
source_title
source_url
source_note
search_status
```

If the user requests research for 2 or more fairs, add or fill these columns:

```text
row_id
Fair_name
City
Year
venue
venue_source_title
venue_source_url
venue_note
venue_latitude
venue_longitude
venue_coordinates_source_title
venue_coordinates_source_url
venue_coordinates_note
visits
visits_measure
source_tier
confidence
source_title
source_url
source_note
search_status
```

## Batch Files

For table research with subagents, write one intermediate CSV per batch before
consolidation. Use stable filenames such as:

```text
_agent_batch1.csv
_agent_batch2.csv
_agent_batch3.csv
_agent_batch4.csv
```

Each batch file must use the full table schema, including `row_id`, and must be
saved to disk before the final merge. Consolidate by reading the batch files
from disk, not by constructing one very long shell command from all results.

Before writing final CSV/XLSX outputs, validate that:

```text
all expected row_id values are present
row_id is unique across all batch files
venue is a venue string or NA
venue_source_title, venue_source_url, and venue_note document venue evidence when venue is not NA
venue_latitude and venue_longitude are decimal-degree numeric values or NA
venue_coordinates_source_title, venue_coordinates_source_url, and venue_coordinates_note document coordinate evidence when coordinates are not NA
visits is an integer or NA; if sources conflict, keep one recommended integer in `visits`, put alternate values and citations in `source_note`, and set `search_status` to `conflicting_sources`
search_status uses only the allowed values
```

This batch-file step is required to avoid command-length failures and to keep an
auditable trail of each subagent's work.

Allowed `search_status` values:

```text
found
conflicting_sources
ambiguous_match
not_found
```

Allowed `confidence` values:

```text
high
medium
low
```

Use `high` for BIE, official exposition pages, official reports, or direct historical records. Use `medium` for academic or institutional secondary sources. Use `low` for tertiary sources or sources that provide a number without a clear citation trail.

## Recording Standards

Normalize `visits` as an integer when the source provides a precise count. If the source gives a rounded value, keep the rounded integer and explain rounding in `source_note`.

Use `NA` for `visits` when no reliable count is found. Do not infer visits from site area, participants, revenue, ticket price, or daily attendance unless a source explicitly reports total visits.

Use `venue = NA` when no reliable source identifies the specific venue, site, grounds, building, palace, park, or exhibition precinct. Do not infer venue from city alone. If a source gives several sites, keep the main or official venue when clear; otherwise record the named sites in `venue` and explain the issue in `venue_note`.

Use `source_title`, `source_url`, and `source_note` for visits evidence. Use `venue_source_title`, `venue_source_url`, and `venue_note` for venue evidence when the venue comes from a different source.

If the input `source_url` is used to find the venue, record that page in `venue_source_title`, `venue_source_url`, and `venue_note`. If it only points to a stronger cited source, record the stronger source as `venue_source_url` and mention the citation path in `venue_note`.

Use `venue_latitude = NA` and `venue_longitude = NA` when there is no reliable venue, when the venue cannot be geocoded, or when several plausible venue locations cannot be resolved. Do not use the city centroid as a substitute for venue coordinates.

When coordinates are not explicitly reported by the venue source, geocode the identified venue and document the geocoding source. Prefer official or municipal pages for the current site, institutional archives, Wikidata, GeoNames, or OpenStreetMap. If the historic venue has disappeared but the modern site is unambiguous, record the modern site's coordinates and explain this in `venue_coordinates_note`.

If the fair used multiple sites, record coordinates for the main or official venue when clear. Otherwise set coordinate fields to `NA` and explain the ambiguity in `venue_coordinates_note`.

Keep `source_note` short: quote only a small phrase when needed, otherwise paraphrase the relevant evidence and mention any ambiguity.
