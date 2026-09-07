# data_from_profiles/

Trait data extracted from species profile / conservation advice / recovery plan
PDFs, one species at a time, via the `species-profile-traits` skill
(`.claude/skills/species-profile-traits/SKILL.md` — the full extraction process
and every convention below is documented there in more depth; this file is a
quick column reference for anyone reading the CSVs directly).

## Files

- **`<species_name>.csv`** — Stage 1: a faithful, plain-language summary of
  every trait mentioned in the source PDF, one row per fact.
- **`<species_name>_apd.csv`** — Stage 2: the same rows, cross-referenced
  against the AusTraits Plant Dictionary (APD) and this project's own
  `config/traits.yml` trait dictionary, one row per Stage 1 row (same order).
- **`list_species_trait_data_apd.csv`** — every species' `_apd.csv` file
  concatenated into one table. This is the file downstream processing should
  read; it's a derived rebuild of the per-species files, not hand-maintained.
- **`APD_reference/`** — cached copies of the APD export
  (`APD_traits.csv`, `APD_categorical_values.csv`), plus two project-specific
  ledgers: `project_approved_extensions.csv` (new allowed *values* for
  existing APD traits, approved by a project domain expert) and
  `new_traits.yml` (whole new *traits*, proposed during extraction and
  since accepted — see `match_confidence` below).

## Stage 1 columns (`<species_name>.csv`)

`taxon_name, trait_category, trait, value, value_type, context, source_section, source, notes`

- **`context`** — blank on most rows. Populated only when the *same* `trait`
  genuinely needs more than one row, and each row needs a short qualifier to
  be read correctly on its own. Covers several distinct situations, all using
  the same mechanism:
  - **Usual vs. occasional**: `context = "typical"` / `"occasional extreme"`
    for a value the source itself flags as the normal case vs. a rarer
    extreme (e.g. a leaf-length range mostly `6–11 mm` but occasionally down
    to `4 mm` or up to `14 mm`).
  - **Geographic/subpopulation split**: e.g.
    `context = "north of Cue subpopulations"` vs.
    `"southeast of Perenjori subpopulations"` for a trait (flowering time,
    habitat, soil type) that differs by location — kept as the *same* trait
    name across rows rather than invented per-population trait names.
  - **Disagreeing sources**: when a document's main description and its
    formal taxonomic appendix give different numbers for the same
    measurement, `context` names which source each row came from (e.g.
    `"current description (Brown et al 1998)"` vs.
    `"taxonomic-description appendix (Leigh et al 1984, older/conflicting
    source)"`) — both are kept rather than silently reconciled, with the
    older/secondary one usually at lower `match_confidence` in the `_apd`
    file.
  - **Life stage**: `context = "juvenile"` / `"adult"` for a *non-size*
    characteristic that genuinely differs by life stage (leaf shape, habit,
    grazing susceptibility). Note: juvenile/regrowth *size* measurements
    (height, width — anything allometric) are deliberately **not** recorded
    this way at all, since a juvenile's size is just a snapshot partway
    through growth, not a stable trait value — see SKILL.md for the full
    reasoning.
  - **Single-row provenance note**: occasionally used on a single row (no
    paired second row) just to record which specific population, year, or
    study a number came from, e.g. `context = "1998 estimate, two localities,
    0.4 km2 area"`.

  `context` carries straight through into the `_apd.csv` file unchanged
  (same column, same meaning) — it's not stage-2-specific.

- **`value`**: never a range. A source range becomes two rows with the same
  `trait`, one `value_type = min` and one `max`. A categorical value the
  source states as a genuine plausible range/mix of terms (not just an
  underspecified single value) is recorded as one row with a
  space-delimited multi-value string (e.g. `narrowly_elliptical
  narrowly_oblanceolate`), carried through unchanged into `_apd.csv`.

## Stage 2 columns (`<species_name>_apd.csv`)

`taxon_name, source, raw_trait, raw_value, value_type, context, value, units, apd_trait, apd_trait_type, apd_units, match_confidence, notes`

- **`apd_trait`**: the name of a real trait `value` is genuine data for —
  either from the released APD, or from this project's own
  `config/traits.yml` (noted as such in `notes` whenever used, since it's a
  separate, less-reviewed vocabulary). **This column never names a real
  trait unless the row's `value` is actually reporting data for that
  trait** — a proposed trait that merely *reuses* another trait's allowed-value
  vocabulary (e.g. a proposed `petal_shape` reusing `leaf_shape`'s terms)
  gets its own name here, never the donor trait's name.
- **`match_confidence`** — one of:
  - `high` / `medium` / `low`: a real trait and a real allowed value, graded
    by how confident the specific mapping is.
  - `new_trait`: a trait that started as a proposal during extraction and has
    since been reviewed and accepted — defined in `APD_reference/new_traits.yml`,
    pending merge into `config/traits.yml` proper.
  - `proposed_new_trait`: a candidate trait (and often a candidate value)
    proposed during extraction, not yet reviewed.
  - `proposed_new_value`: the trait is real, but none of its allowed values
    cover the source's stated value — `value` is a proposed new one, in that
    trait's own naming style.
  - `no_apd_trait`: no matching trait found anywhere (released APD, project
    `config/traits.yml`, or already-proposed); `value` may still be populated
    even though `apd_trait` is blank.
  - `no_match`: a real trait exists and is the right concept, but no
    allowed/proposed value can be committed with any confidence.
