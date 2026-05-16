# SES extraction from Wikipedia text — plan to revisit

Status: **paused**. Document the path for when we pick this back up.

## Why we want this

The decade and year event-study results show effects on STEM-births rates per
county. We have no household-SES variable for the scientists themselves, so
we can't say whether the elite high schools lifted lower-SES talent into
science (the interesting mechanism) or just moved higher-SES kids' college
choices around. Whoever the marginal student was, we can't see them.

## Why the obvious paths don't work

- **Wikidata structured fields (P22 father / P25 mother)**: 6% / 3%
  coverage on a 500-scientist sample of US STEM births. When the parent
  *is* listed, the parent is themselves Wikipedia-notable — strong
  selection on parental fame.
- **Wikidata P69 (educated at)**: 78% overall but ~95% of those are
  universities, not high schools. Zero of 17 sampled scientists had
  one of our elite HSs in P69.
- **IPUMS census linkage**: we (PI) decided not to pursue it.

## Path we tested with 20 sampled US STEM scientists

1. R script `analysis/fetch_wiki_articles_for_ses_probe.R`:
   - resolves each Wikidata Q-code to its English-Wikipedia title via
     Special:EntityData
   - pulls plain-text article via the Wikipedia REST API
   - saves to `analysis/ses_probe_articles/<Q>.txt`
   - 17 of 20 had English-Wikipedia articles
2. 17 Claude Code subagents fired in parallel (`Agent` tool,
   `general-purpose`), each reading one .txt file and returning a
   pipe-delimited SES record (father occupation, mother occupation,
   family_ses class, schools, summary).
3. Aggregated into `analysis/ses_probe_results.csv`.

### Coverage on the 17-scientist probe

| Field | Filled | % |
|---|---|---|
| Father occupation | 4/17 | 24% |
| Mother occupation | 1/17 | 6% |
| Family SES class (any non-unknown) | 7/17 | 41% |
| University named | 12/17 | 71% |
| Mentions one of our elite HSs | 0/17 | 0% |

SES distribution where derivable: 1 working / 3 middle / 3 upper-middle.

### Read

- ~40% of articles have *some* family-SES content (concrete enough to
  classify into working / middle / upper-middle / etc.).
- Coverage is denser for pre-1900-born scientists than for 20th-century
  ones — fortunate for our research design.
- Selection bias: among the families that are described, they skew
  middle/upper-middle. Either real (notable scientists tend to come from
  more documented households) or Wikipedia rounds "his father was a
  laborer" less reliably than "his father was a physicist".
- Elite-HS attendance is essentially never in the article text — won't
  serve as first-stage validation.

## Cost model

Subagents go against the Claude Code plan quota, not paid API. So:

- Free in $ terms until hitting the plan's 5-hour window cap.
- Each agent in the probe used ~26k tokens. At ~260k tokens per 10
  scientists, the 5-hour Max-plan budget would clear roughly 1k-2k
  scientists per window depending on other usage.
- Doing the pre-1900 subset (estimated 3-4k US STEM births) is feasible
  across 2-3 plan windows. Spread over a couple of days.

Alternative: Haiku via direct API at ~$0.50 per 1M input tokens. 10k
scientists × ~5k tokens of article text = 50M tokens ≈ **$25-50**.
Faster, deterministic, no rate windows. Tradeoff: actual dollars.

## When we come back, things to decide

1. **Scope**: pre-1900 US STEM births only (~3-4k) vs full 10k.
2. **Engine**: subagents (free, slow) vs Haiku direct API (cheap, fast).
3. **Sampling vs full pass**: a 1k random sample is enough to estimate
   the SES distribution of treated-county scientists vs control-county
   scientists. Full pass only if we want SES as a regression covariate
   on every observation.
4. **Output schema**: the 17-row probe used pipe-delimited K=V. For
   scale this should be a JSON-Lines schema with a fixed set of fields
   so parsing is robust.
5. **Validation**: spot-check 20-50 LLM extractions by hand to confirm
   the SES coding isn't systematically off.

## Existing artifacts

- `analysis/probe_wikidata_ses.R` — initial Wikidata SPARQL coverage check.
- `analysis/fetch_wiki_articles_for_ses_probe.R` — Wikipedia article puller.
- `analysis/ses_probe_articles/` — 17 sample article .txt files.
- `analysis/ses_probe_records.csv` — sample metadata.
- `analysis/ses_probe_results.csv` — 17-row LLM-extracted SES.
