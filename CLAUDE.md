# CLAUDE.md

Guidance for Claude Code (or any agent) working in this repo.

## What this is

DCCEEW Threat Spatial Planner project — plant-susceptibility component. Scores
Australian native/threatened plant taxa for susceptibility to 7 threats:
**Phytophthora cinnamomi, Myrtle rust, Goats, Deer, Pigs, C4 tropical grasses,
Hymenachne**. Author/domain scientist is Elizabeth Wenk (also author of the
`APCalign` R package, used here for taxonomic name alignment and native-status
lookups). Output feeds DCCEEW Word reports, one per threat.

This repo also contains an unrelated `traits.build`/`austraits` database-build
scaffold (`build.R`, `remake.yml`, `config/`) — that machinery is vestigial
boilerplate for this project and not part of the active susceptibility
pipeline described below.

## The master pipeline

**`scripts/threatened_script_20250813.qmd`** is the single source of truth —
a Quarto document with R chunks sharing one persistent environment. It is
edited directly by both Elizabeth and Claude; there is no separate "helper
functions" file for the scoring logic itself (only generic utilities live in
`R/`, e.g. `plot_trait_values.R`).

Older/parallel `.qmd` files in `scripts/` (`threatened_script.qmd`,
`threatened_script_20250514*.qmd`, `threatened_script_C4_grasses.qmd`, etc.)
are prior iterations or scratch branches — **don't assume they're current**;
always confirm which file is being asked about.

Structure, top to bottom:
1. **Setup** — load libraries (`tidyverse`, `austraits`, `APCalign`,
   `traits.build`), read `data_extras/*_scores.csv` lookup tables (one CSV per
   trait/attribute, e.g. `woodiness_scores.csv`, `plant_goat_toxicity_scores.csv`).
2. **Taxon universe construction** (`native_species`, `threatened`) — built via
   `APCalign::native_anywhere_in_australia()` + `APCalign::create_taxonomic_update_lookup()`,
   then `full_join()`ed (not `left_join`) so threatened taxa that are
   APC-accepted but fall outside `native_species`'s own filters (e.g.
   presumed-extinct species) aren't silently dropped. `full_join()` with no
   `by=` joins on every shared column name — verified clean here, but worth
   re-checking if new shared columns are ever introduced.
3. **Threat-independent scoring** — regeneration ability, species
   precariousness (range-size based, no habitat/taxonomic component).
4. **Per-threat sections** (Phytophthora → Myrtle rust → Goats → Deer → Pigs →
   C4 tropical grasses → Hymenachne), each following the same pattern:
   - trait-effect tribble (`trait`, `effect_score`) → `add_trait_risks()`
   - taxonomic effect score (family/genus-level, only where a threat has a
     training dataset — Phytophthora, Myrtle rust host orchids, Goats, Pigs)
   - `total_trait_effect_score`, `total_individual_susceptibility`,
     `population_risk`, then `create_risk_bins()` into 5 ordinal categories
   - export to `export/{threat}_risk_scores.csv` (also
     `export/{threat}_assigned_risk_scores.csv` for Phytophthora)

## Key conventions / gotchas

- **`add_trait_risks(data, trait_effects, condition = TRUE)`** replaces what
  used to be one hand-written `{trait}_risk = calculate_risk(...)` line per
  trait. `condition` must be recycled to `nrow(data)` *before* use in
  `ifelse()` — passing the length-1 default straight into `ifelse()` silently
  truncates/recycles every risk column to one value. Already handled inside
  the function; don't reintroduce a bare `ifelse(condition, ...)` elsewhere
  without the same recycling.
- **Taxonomic weighting** — `taxonomic_risk`/`taxonomic_effect_score` is
  pulled out of the plain `rowSums(across(c(...)))` trait sum and added back
  as `+ N * coalesce(taxonomic_risk, 0)`, where `N` is a **per-threat
  multiplier** (higher for threats with more traits, so taxonomy carries
  comparable relative weight): Phytophthora 5x, Goats 3x, Pigs 3x, C4 grasses
  2x, Hymenachne 1x. Myrtle rust uses a multiplicative
  `habitat_taxonomy_risk = taxonomic_risk * habitat_risk` instead (already
  undiluted, don't also apply an additive multiplier). Deer has no taxonomic
  term at all. Check which pattern a threat uses before changing its weighting.
- **Family/genus taxonomic effect scores need an `n > 5` filter** before
  being trusted enough to apply to every (including untested) species in that
  group — otherwise a single tested taxon (n=1) can swing an entire
  family/genus to the scoring ceiling/floor. Also carry an `n_tested` column
  (`coalesce(taxa_scored_per_family, taxa_scored_per_genus)`) wherever a
  taxonomy score is training-data-derived, so reports can show it (Table
  showing n per family/genus — keep in every report going forward).
- **Known-susceptibility overrides** (Phytophthora only) — species with a
  known susceptibility of 0/5 or 1/5 get their scaled individual
  susceptibility multiplicatively downweighted (`0.1x` for known=0, `0.3x`
  for known=1) via `case_when()`, ahead of the pre-existing upscale/downscale
  branches. `case_when()` is first-match-wins; `NA == value` evaluates to
  `NA` (safely skipped), not an error.
- **Habitat combines with trait/taxonomy risk multiplicatively, never
  additively — this has already been decided and rejected once, don't
  re-propose additive.** `total_individual_susceptibility = habitat_risk *
  trait_effect_score_scaled` (and each threat's analogous combination) must
  stay a product: `habitat_risk = 0` (species definitively outside a threat's
  relevant habitat) has to force the total to exactly `0`, and multiple
  downstream code paths rely on that invariant. Additive combination breaks
  it (a `0` habitat_risk would still leave a nonzero total from the trait
  term). If a pairwise ranking looks wrong (e.g. a species with a much higher
  trait score ending up with a lower total than one with a much lower trait
  score but higher habitat_risk), that's an artifact of two bounded factors
  being multiplied — look at `sigmoidal_rescale`'s `k`/ceiling, or the
  habitat score lookup table itself, not the combination operator.
- **`sigmoidal_rescale(x, to, center=mean(x), spread=sd(x), k=1)`** is
  centered on the *population's own* mean/SD — if the population distribution
  is itself skewed, even a modestly negative raw score can map into the upper
  half of the output range. This was the root cause of at least one scoring
  anomaly (a species with known-mild susceptibility scoring "extremely high");
  worth checking first whenever a single taxon's score looks wrong despite
  sane-looking inputs.
- **Per-threat bin thresholds** — `create_risk_bins(x, bin_thresholds)`
  converts continuous risk to 5 ordinal categories. Threats can define their
  own `{threat}_bin_thresholds` vector instead of the default. **If a threat
  uses custom thresholds, `add_risk_range(risk_col, confidence_col, ...,
  bin_thresholds=)` must also be passed that same vector explicitly** — the
  lowest/highest-plausible columns silently fall back to the default
  thresholds otherwise, producing incoherent rows (e.g. best-estimate
  "Moderate" but lowest/highest both "Extremely high").
- **Plot functions**: `make_8_panel_risk_density_plot(data, threat)` (and the
  older `make_2_panel_risk_density_plot`) are the pre-existing diagnostic
  plots rendered **inline** in Elizabeth's knitted output — treat as
  load-bearing, don't alter without being asked. `make_4_panel_risk_density_plot(data,
  threat, panel_cols, panel_labels, panel_colors, listed_only=TRUE, x_min=-0.2,
  x_max=2.1)` is a separate, additive function for the cleaner figure used in
  Word reports — it must only be called from its own `include=FALSE`,
  PNG-saving chunks (`figures/{threat}_4panel.png`), **never** substituted for
  or rendered inline alongside the 8-panel diagnostic chunks.
- **`all_traits` is a `traits.build` S3 object** — its long-format trait
  records live in `all_traits$traits`; plain `filter(all_traits, ...)` has no
  method and errors.
- **Validate qmd edits from a clean environment**, not just by editing the
  chunk in place: `knitr::purl(qmd, documentation = 0)` to extract pure R,
  truncate to the relevant chunk boundary, run via a background `Rscript`,
  and inspect the log. Chunk-level edits can look correct while silently
  depending on stale state left over from a previous partial run.

## Word report generation

Reports are Microsoft Word docs, one per threat, built either fresh (docx-js,
`report_helpers.js`-style shared components — `t/it/bd/bit/h1/h2/h3/p/bullet/
caption/calloutBox/cellText/dataTable/img/parseCSV/buildDocx`) or — once
Elizabeth has hand-finalized a report's formatting — by editing/cloning her
finished `.docx` directly via raw XML surgery
(unzip → edit `word/document.xml` → rezip; validate with
`python3 -c "import xml.etree.ElementTree as ET; ET.parse(...)"` and visually
via `soffice --headless --convert-to pdf` + `pdftoppm`). See the `docx` skill
for the general mechanics.

Format constraints, established through direct correction and now standing
policy for every threat report:
- **Concise**: 12–15 pages max (8–10 for threats without an explicit training
  dataset). No vertebrate-report comparisons, no "this is the same as
  vertebrates" filler, no meta-commentary about why species counts changed
  between versions.
- **Trait-table evidence column**: cite literature-review sources when the
  evidence is strong; otherwise just say "Expert elicitation" — never
  characterise a correlation as "weak."
- Keep the **population-risk-by-category count table (with %)** in every
  report — explicit positive feedback to preserve this.
- Keep the **n-tested table** wherever a threat's taxonomy score is
  training-data-derived (see n>5 filter above).
- **Never strip/replace Elizabeth's own report prose when only asked to add
  citations** — augment existing text with the citations that support each
  specific claim (e.g. add a citation next to "mixed field evidence," don't
  rewrite the sentence).
- **Never let generated/edited cells fall back to Word's default font** —
  when adding or editing table cells or headers via raw XML, always copy the
  exact `<w:rPr>` (font, size, color) from an adjacent, already-styled run.
  Elizabeth has standardised fonts/colors in tables and headers to match a
  vertebrate-team template; regenerating content without explicitly carrying
  those properties forward is a repeat failure mode. When in doubt, clone an
  already-correctly-styled finished report rather than generating fresh
  styled content from scratch.
- `dataTable()`/table row helpers should set `cantSplit: true` on `<w:tr>` so
  table rows don't split across page boundaries.

## Directory map

- `scripts/threatened_script_20250813.qmd` — active pipeline (see above)
- `data_extras/*.csv` — per-trait/attribute scoring lookup tables, plus
  `threatened_flora.csv` (the listed-taxa source list)
- `export/*_risk_scores.csv` — per-threat scored output, one row per taxon
- `R/` — generic utilities only (not the scoring logic)
- `docs/`, `datasets_from_manuscripts/` — supporting reference material

## Written material lives outside this repo (OneDrive, not git)

All report/manuscript-level material for this project is on OneDrive under
`~/Library/CloudStorage/OneDrive-UNSW(2)/Documents/threatened_species/threatened species susceptibility/`,
not in this git repo:

- **`plant outputs for DCCEEW threats project/`** — what was submitted to
  DCCEEW in June 2025.
- **`plant reports 2026-08/`** — current/active copies of the per-threat Word
  reports (e.g. `Phytophthora_Plants_20260817_final copy.docx`,
  `Myrtle_rust_Plants_20260818_final.docx`,
  `Deer_Plants_20260818_final_v2.docx`). **Save new/updated report drafts
  here, not to `~/Downloads`.**
- **`vertebrate files 2026-07/`** — the vertebrate team's equivalent reports;
  useful as the formatting/style reference these plant reports are matched
  against.
