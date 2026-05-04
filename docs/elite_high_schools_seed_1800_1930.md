# Elite US High Schools Founded 1800-1930

This is a phase-1 seed list for identifying historically elite US secondary schools founded between 1800 and 1930. It is not yet a full national census. The goal is to create a defensible, reproducible shortlist that can later be expanded state by state.

## Working Definition

For this project, an "elite high school" should satisfy both conditions below:

1. It was created as, or clearly evolved into, a continuous secondary-school institution between 1800 and 1930.
2. It shows at least two elite signals:
   - repeated appearance in state-level prestige/search queries;
   - explicit selectivity or academic gatekeeping (exam admission, merit scholarship, nationally known college-prep boarding/day model);
   - outsized historical reputation for placing students into highly selective colleges or producing nationally visible alumni/scientists.

Search prominence is only a candidate-generation tool. It is not enough by itself.

## Founding-Date Rules

- Use the earliest continuous secondary-school date.
- Do not treat a later rename as a new school.
- Do not treat a campus move as a new school.
- Do not reset the clock for later coeducation if the institution is continuous.
- Do separate "current school date" from "institutional roots" when a school was substantively reorganized.
- Do flag mergers explicitly. A merger is not the same thing as a rename.

Examples:

- `Lawrenceville`: use `1810`; the 1883 rename from Maidenhead Academy is not a refounding.
- `Lowell High School`: use `1856`; the 1894 name change to Lowell is not a refounding.
- `Baltimore Polytechnic`: use `1883`; the 1893 rename from Baltimore Manual Training School is not a refounding.
- `Mercersburg Academy`: use `1893` for the current academy model, but keep `1836` as institutional roots.
- `Choate Rosemary Hall`: keep the predecessor dates visible (`Choate School` 1896, `Rosemary Hall` 1890) and note the 1974 merger instead of pretending the merger created a 19th-century school.

## Pilot States

I used `New York`, `Massachusetts`, and `Pennsylvania` as pilot states because they stress-test the rules with very different cases:

- `New York`: public exam schools and elite private schools in the same market.
- `Massachusetts`: many canonical schools are too early and must be excluded.
- `Pennsylvania`: good examples of public magnets, private day schools, and reorganized boarding schools.

## Phase-1 Seed List By State

The school-level dataset is in [elite_high_schools_seed_1800_1930.csv](/abs/path/C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_seed_1800_1930.csv).

| State | Schools kept in phase 1 | Notes |
| --- | --- | --- |
| New York | Stuyvesant (1904); Horace Mann (1887); Regis (1914); Brooklyn Tech (1922) | Mix of elite public exam schools and elite independent schools. Exclude Bronx Science because it opened in 1938. |
| Massachusetts | Groton (1884); Noble & Greenough (1866) | Many famous Massachusetts schools are excluded for being too early: Boston Latin (1635), Roxbury Latin (1645), Andover (1778). |
| Pennsylvania | Central High (1836); Haverford (1884); Mercersburg Academy (1893, roots 1836) | Mercersburg is the best example of why "roots" and "current academy date" should both be recorded. |
| Connecticut | Hotchkiss (1891); Taft (1890); Choate School (1896) / Rosemary Hall (1890) | Current Choate Rosemary Hall is the 1974 merger; keep predecessor dates for feeder-history work. |
| New Jersey | Lawrenceville (1810); Pingry (1861); Blair Academy (1848) | Lawrenceville is a rename case, not a refounding case. |
| Maryland | Baltimore City College (1839); Baltimore Polytechnic (1883); Gilman (1897); McDonogh (1873) | Good mix of selective public and private college-prep institutions. |
| Virginia | Episcopal High (1839); Woodberry Forest (1889) | Episcopal is continuous despite Civil War interruption; Woodberry is a clean founding case. |
| Illinois | UChicago Lab Schools (1896); Latin School of Chicago (1888); Lane Tech (1908) | Lane Tech started as a manual-training school and later broadened into a selective college-prep model. |
| California | Lowell (1856); Thacher (1889); Cate (1910) | Lowell is a rename case; Cate is a multi-name continuity case (Miramar School -> Santa Barbara School -> Cate). |
| Ohio | Western Reserve Academy (1826); University School (1890); Hawken (1915) | Western Reserve requires a continuity judgment because the college and academy histories later diverged. |

## What To Exclude

Exclude schools that are famous but outside the date window:

- Pre-1800: Collegiate School, Trinity School, Boston Latin, Hopkins School, Phillips Academy Andover, Roxbury Latin, Newark Academy.
- Post-1930: Bronx High School of Science, Thomas Jefferson High School for Science and Technology, many modern magnets and science academies.

## Recommended Expansion Rule For The Remaining States

For each remaining state:

1. Run four search families:
   - `"elite high school" + state`
   - `"best private high school" + state`
   - `"historic preparatory school" + state`
   - `"famous alumni" or "Nobel" + school/state`
2. Keep schools that recur across at least two search families, or one search family plus a hard elite signal like exam admission or a nationally recognized boarding-school model.
3. Verify founding date and continuity on an official history page.
4. Store both the `used founding year` and the `lineage note`.
5. Keep a separate file of exclusions so the boundary decisions stay transparent.

