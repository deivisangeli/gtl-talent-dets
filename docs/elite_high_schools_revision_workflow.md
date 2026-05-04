# Elite High School Revision Workflow

This note documents the completed second-pass revision of the national elite-school list so the school universe is better aligned with the research question:

`Do elite, historically accessible schools allow local talent to flourish?`

## Files

- [elite_high_schools_revision_additions.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/input/elite_high_schools_revision_additions.csv>)
- [elite_high_schools_state_review_batches.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/input/elite_high_schools_state_review_batches.csv>)
- [elite_high_schools_revision_queue.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/input/elite_high_schools_revision_queue.csv>)
- [elite_high_schools_revision_decisions.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/input/elite_high_schools_revision_decisions.csv>)
- [build_elite_high_schools_national.py](</C:/Users/deivi/github/gtl-talent-dets/prep/build_elite_high_schools_national.py>)
- [elite_high_schools_national_1800_1930.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_national_1800_1930.csv>)
- [elite_high_schools_core_1800_1930.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_core_1800_1930.csv>)
- [elite_high_schools_robustness_only_1800_1930.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_robustness_only_1800_1930.csv>)
- [elite_high_schools_benchmarks_and_boundaries.csv](</C:/Users/deivi/github/gtl-talent-dets/prep/output/elite_high_schools_benchmarks_and_boundaries.csv>)

## Why This Revision Exists

The first national pass is usable, but it is still biased toward schools that are famous today and toward private-school types that are easy to recognize quickly.

The main issues are:

- some historically important schools were omitted
- some merger-root schools are not clean causal units
- some special institutional models are not comparable to ordinary elite high schools
- most historical access coding is still rule-based rather than directly source-checked

## Batch Structure

The revision is now organized by state batches.

- `state_review_batches` tracks which states are being reviewed together and why
- `revision_queue` tracks candidate additions, boundary cases, and existing rows that need redesign decisions
- `revision_additions` stores only confirmed rows that are ready to merge into the main national build

This keeps the main output reproducible while letting the review process stay transparent.

## What The Revision Now Produces

The revision now distinguishes four objects:

- the full in-frame candidate universe
- a `core` sample for the main treatment analyses
- a `robustness_only` sample for merger-root or special-model schools
- a benchmark/boundary file for historically central schools that are outside the frame or still unresolved

The main output now carries these revision fields directly:

- `sample_role`
- `include_in_core_sample`
- `comparability_class`
- `lineage_risk`
- `local_access_relevance`
- `review_batch`
- `revision_note`

## First Batches Completed

`B01` and `B02` are the first active batches.

- `B01`: Connecticut, Massachusetts, New York, Virginia
  Focus: elite girls' academies omitted from the first pass, plus frame boundaries like Boston Latin
- `B02`: Kansas, Missouri
  Focus: historically important Black public academic schools and public-access pathways

The first confirmed additions merged into the main national build are:

- `Miss Porter's School`
- `Dana Hall School`
- `Emma Willard School`
- `Foxcroft School`
- `Sumner Academy of Arts and Science`
- `Sumner High School`

## Boundary Rules

Some schools are historically central but should not automatically enter the main `1800-1930` list.

- `Boston Latin School` is a core benchmark school, but it was founded in `1635`, so it is outside the creation-year frame
- `Dunbar` / `M Street` are historically central but sit outside the current 50-state frame because they are in DC

Those cases belong in the queue as explicit benchmark or boundary rows, not as silent omissions.

## Current Boundary Decisions

The main decisions encoded in the revision are:

- keep historically important but out-of-frame schools like `Boston Latin School` in the benchmark file rather than the main `1800-1930` universe
- keep DC benchmark schools like `Dunbar` and `M Street` outside the 50-state main file but preserve them explicitly
- move later-merger schools like `Choate Rosemary Hall`, `Ransom Everglades`, `Westminster`, `MICDS`, `Pembroke Hill`, `Catlin Gabel`, `Porter-Gaud`, and `University School of Milwaukee` into `robustness_only`
- move special institutional models like `University of Chicago Laboratory Schools` and `New Mexico Military Institute` into `robustness_only`
- keep historically important Black public academic schools like `Sumner Academy` and `Sumner High School` in the `core` sample
