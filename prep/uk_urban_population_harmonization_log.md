# UK Historical Urban Population Harmonization Log

This note documents the harmonization used to build the UK historical urban-unit
population and inventor panel for the world's fairs analysis.

## Objective

The panel combines Law-Robson-Bennett settlement populations for 1801-1911 with
Nomis historical census district populations for 1921-1961. The target geography
is fixed to 1921 Urban Districts, Municipal Boroughs, and County Boroughs, plus a
synthetic Greater London unit.

The main goal is to keep population denominators and Wikipedia inventor outcomes
on the same fixed territorial definitions.

## Main Inputs

- Law-Robson-Bennett settlement population, 1801-1911.
- CAMPOP settlement points used to spatially assign Law-Robson settlements.
- Nomis historical census CR03 district population, 1921, 1931, 1951, and 1961.
- Historical 1921-1961 boundary files downloaded from the ONS Open Geography
  portal and linked to Nomis metadata.
- Nomis 1911 benchmark populations used to weight polygon overlaps by
  population density.
- GISCO LAU boundaries used only to construct the synthetic Greater London
  definition.
- Laouan et al. cross-verified Wikipedia people database for inventor/scientist
  birth outcomes.

## Baseline Harmonization

Law-Robson settlements are assigned to the target geography using only their
geocoded settlement points and the fixed target polygons. No name matching is
used for this assignment.

Nomis post-1921 district populations are assigned to the same target geography by
polygon intersection. When one Nomis district intersects more than one target
unit, the population is allocated by intersection area weighted by each target
unit's 1911 population density. This keeps the allocation spatial and avoids
using names to force matches.

Greater London is handled as a special synthetic unit. Historical urban units
overlapping the Greater London geography are removed from the ordinary target set
to avoid overlapping target geometries.

## Manual Territorial Adjustments

Several Law-Robson settlements combine adjacent places that Nomis reports as
separate urban authorities after 1921. For high- and medium-confidence cases, the
panel now uses an explicit manual harmonization overlay:

- The primary unit keeps the Law-Robson pre-1921 population.
- For 1921-1961, the primary unit receives the sum of its own Nomis population
  and the component units' Nomis populations.
- Component units are suppressed in the population panel to avoid double
  counting.
- Wikipedia inventor/scientist counts are also merged from component units into
  the primary unit, so per-capita outcomes use consistent numerators and
  denominators.

Applied adjustments:

| Confidence | Primary unit | Component units | Reason |
| --- | --- | --- | --- |
| High | Newcastle upon Tyne | Gateshead | Law-Robson reports `NEWCASTLE & GATESHEAD`; Nomis separates adjacent authorities. |
| High | Manchester | Salford | Law-Robson reports `MANCHESTER & SALFORD`; Nomis separates adjacent authorities. |
| High | Liverpool | Birkenhead | Law-Robson reports `LIVERPOOL & BIRKENHEAD`; Nomis separates adjacent Mersey authorities. |
| High | Sale | Ashton-upon-Mersey | The combined Nomis 1911 population reproduces Law-Robson Sale almost exactly. |
| Medium | Altrincham | Bowdon; Hale | Contiguous associated places; combined Nomis 1911 population is materially closer to Law-Robson Altrincham than Altrincham alone. |

The audit file for these decisions is written to:

`input/worlds_fairs/city_census/GBR/uk_historical_urban_units_manual_harmonization_audit.csv`

## Cases Not Adjusted

The following potential combinations were not applied because they were lower
confidence or could not be justified cleanly from the spatial evidence:

- Wider Liverpool/Mersey combinations including Bootle or Wallasey.
- Wolverhampton plus Bilston.
- Widnes.
- Rowley Regis.
- Urmston.

These cases remain under the baseline spatial allocation.

## Outputs

The UK population and inventor panel is written to:

`Data/processed/uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv`

The combined UK+US panel is written by the next prep step to:

`Data/processed/uk_historical_urban_units_us_county_inventor_panel_1800_1960_nhgis_us.csv`

