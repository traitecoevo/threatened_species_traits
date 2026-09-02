---
name: species-profile-traits
description: Extract trait data (habitat, morphology, life history — numeric and categorical) from a threatened-species conservation advice, recovery plan, or listing-advice PDF, and cross-reference it against the AusTraits Plant Dictionary (APD) controlled vocabulary. Use this whenever the user hands over a species profile/conservation-advice/recovery-plan/listing-advice PDF for the threatened_species_traits project and wants trait data pulled out of it — even if they just say "read this species PDF and pull out the traits" or "build a trait table for X" without naming the skill, APD, or mentioning both output files explicitly. Also use it if asked to "fill in" a species' traits, to prep data for the traits.build pipeline, or to check what a PDF's description says about a species' habitat/leaf/flower/seed measurements.
---

# Species profile → trait table (+ APD crosswalk)

Two-stage pipeline for one species PDF: **(1)** read the PDF's description/habitat
sections and record every trait mentioned, in plain language, as its own row; then
**(2)** map each of those rows onto the AusTraits Plant Dictionary's controlled trait
names and allowed values, where a mapping genuinely exists.

Keep the two stages separate and produce two files. Stage 1 is reading comprehension
— nothing about it is mechanical. Stage 2 is term-matching against a fixed reference
list — treat a shaky match as *no match* rather than a guess, because these files may
end up feeding the actual scoring pipeline later and a wrong-but-confident value is
worse than a blank one.

**This file covers the workflow and the procedural rules. Specific "trait X vs
trait Y" pitfalls and vocabulary gotchas — the kind of thing a new species keeps
turning up — live in `references/trait_notes.md`, organized as a lookup by
biological domain (leaves, flowers, fire response, ...), not here. Check it
whenever you're deciding where a row belongs and the answer isn't obvious from
the rules below; add a new entry there (not here) whenever a species surfaces
something not yet covered.**

**Re-read this file and `references/trait_notes.md` in full at the start of
every session that does this work — don't rely on your own memory of past
sessions, even a recent one.** On 2026-09-02, most of a session's extraction
work (naming a fire-response trait `fire_response` instead of the documented
`resprouting_capacity`, treating `associated_species` as a real trait despite
that being corrected here four separate times already, skipping the entire
pollination cluster, converting units this file already says not to convert)
turned out to already be documented correctly in these two files — the
mistakes happened because the work was done from recalled context rather than
from actually opening and reading the current version of these files first.
These files exist specifically so this doesn't have to be re-discovered or
re-explained every session across what will eventually be thousands of
species profiles; treat "I already know this from before" as a reason to
double-check against the file, not a reason to skip reading it.

## File locations (updated 2026-09-01)

Per-species working files and the two large read-only APD reference caches
moved out of the git repo to keep the repo from filling up with clutter —
don't write per-species files under the repo's `data_from_profiles/` anymore:

- **Source PDFs**: `/Users/z3524079/Library/CloudStorage/OneDrive-UNSW(2)/Documents/threatened_species/threatened species susceptibility/threatened species profiles/approved_conservation_advice/`
- **Per-species stage-1/stage-2 files** (`<species>.csv`, `<species>_apd.csv`) —
  write these to `/Users/z3524079/Library/CloudStorage/OneDrive-UNSW(2)/Documents/threatened_species/threatened species susceptibility/threatened species profiles/scraped convseration advice files/`
  (yes, "convseration" — that's the real folder name, a typo baked into the
  path; don't "fix" it). Not tracked by git.
- **APD reference — read-only caches** (`APD_traits.csv`,
  `APD_categorical_values.csv`): also at `.../scraped convseration advice
  files/APD_reference/`, alongside the per-species files. Not tracked by git.
- **APD reference — trait definitions this project maintains**
  (`new_traits.yml`, `project_approved_extensions.csv`): stay in the git repo
  at `data_from_profiles/APD_reference/` — the maintainer wants these
  version-controlled since they're actively-edited decisions, not a static
  cache. **Read and write these at the git repo path, not the OneDrive one**,
  even though a copy also happens to exist on OneDrive from an earlier move —
  the git copy is authoritative; don't let the two drift.
- **Combined table** (`list_species_trait_data_apd.csv`) — stays at the git
  repo path, `data_from_profiles/list_species_trait_data_apd.csv`. This is
  "the overall table" the maintainer refers to — the only per-run output that
  belongs in git besides the trait definitions above.

## Workflow cadence (maintainer directives, 2026-09-01/02)

- **Do not use sub-agents for this work.** Process species one at a time,
  yourself, directly. Sub-agent batches for this task have previously died
  mid-run after burning significant tokens re-reading reference files, with
  no partial results saved along the way — the maintainer has explicitly
  asked this not happen again.
- **After each species, append its `_apd.csv` rows to the bottom of the
  combined file** — don't regenerate the whole combined file sorted
  alphabetically by species name each time. Newly-added species should be
  easy to find at the end of the file for a quick review pass, not buried
  wherever they'd alphabetically sort.
- **Stop after every 10 species and check in** with: a table of the new
  values captured (anything scored `new_trait`/`proposed_new_trait`/
  `proposed_new_value`), and the single trait most in need of being added to
  `new_traits.yml` (the most frequent, well-defined `no_apd_trait` gap from
  that batch — see the corpus-wide `no_apd_trait` tally pattern used in past
  check-ins for how to identify this). Exclude site/community descriptors
  from that nomination even when they dominate the tally — `family`
  (taxonomic metadata), and `associated_vegetation_community`/
  `associated_species` (site context, not a plant trait — see
  `trait_notes.md`'s "Associated species & vegetation community" section)
  are never candidates, no matter how often they recur.
- **Commit (git) only every 50 species**, not every 10 — batch several
  check-ins' worth of work into one commit. Still commit only the four core
  files per the existing commit-scope rule (`SKILL.md`, `trait_notes.md`,
  `new_traits.yml`, the combined table) — never the per-species files, which
  live outside the repo anyway (see File locations above).

## Stage 1 — extract raw traits from the PDF

**Locate the right section before reading everything.** These PDFs run 5–40+ pages.
Read the first ~10 pages first — almost all of them have a table of contents that
tells you exactly which page the species description and habitat sections start on
(commonly headed "Description", "Species information", "Habitat", "Ecology", or
similar — numbering and exact headings vary by document type: NSW recovery plans,
Commonwealth conservation advices, and state action plans are all laid out
differently). Then jump straight to those pages rather than reading page-by-page.
The `Read` tool caps large PDFs at 20 pages per call and requires the `pages`
parameter — plan your reads around that rather than fighting it.

Some documents don't have a numbered "Description" section at all (e.g. a short
conservation advice) — in that case just read enough of the document to find whatever
prose describes the plant's form, and whatever describes where it grows. Habitat
information in particular is often scattered outside a section literally titled
"Habitat" (e.g. folded into "Distribution" or "Ecology") — don't stop at the first
matching heading if the description continues elsewhere.

**What counts as a trait worth a row**: anything with a plant-biology value —
growth form, woodiness, life history, leaf/flower/fruit/seed measurements and shapes,
reproductive biology (pollination, flowering time, seed set, seed bank), vegetative
spread, and everything about habitat (landform, substrate, water chemistry, soil,
fire response, flood tolerance, competing species, disturbance relationships). Skip
anything that's pure conservation-status/legislative bookkeeping (listing category,
Act citations, review dates) — that's not a trait.

**Write `<species_name_snake_case>.csv`** (taxon name lowercased, spaces
→ underscores, e.g. `Eriocaulon carsonii` → `eriocaulon_carsonii.csv`) in the
per-species folder (see "File locations" above) with columns:

```
taxon_name,trait_category,trait,value,value_type,context,source_section,source,notes
```

- `taxon_name`: the full scientific name as given in the document (e.g.
  `Eriocaulon carsonii`), same on every row. Each species gets its own file, so this
  looks redundant within one file — but it's what lets rows from many species' files
  be safely concatenated into one combined table later without losing track of which
  row came from which species.
- `source`: what *kind* of document this is — `SPRAT Conservation Advice`,
  `NSW Recovery Plan`, `State Action Plan`, etc. Same value on every row within one
  file (it describes the document, not the individual claim — `source_section`
  already covers where *within* the document a value came from). Look at the
  document itself to determine this rather than assuming — a Commonwealth SPRAT
  conservation advice, a state recovery plan, and a state listing/action plan are
  differently structured and differently authoritative, and that's worth being able
  to filter on once multiple species are combined.
- `trait_category`: a handful of buckets, typically `taxonomy`, `leaf`,
  `reproductive`, `habitat` — group traits the way a reader would expect to find
  them, don't invent a new category per row.
- `trait`: short snake_case name for what you're recording (doesn't need to match
  any controlled vocabulary yet — that's stage 2's job).
- `value`: a single bare value (a number, or a categorical term/phrase) — **never a
  range**. When the source gives a range (`"1.5-6 cm"`), split it into **two rows**
  with the same `trait` name: one with `value_type = minimum` and the bounding value,
  one with `value_type = maximum` and the other bounding value. Don't collapse a range
  to its midpoint or average — that discards real information (the actual spread)
  and invents a number the source never stated. Keep the unit *with* the number in
  this file (e.g. `"1.5 cm"`) since this file is a faithful human-readable summary,
  not yet a machine schema.
  - If a single sentence bundles two genuinely different measurements together
    (e.g. "mounds 1–10 m high, 2 to >100 m diameter"), that's two distinctly-named
    traits (`mound_height`, `mound_diameter`), each with its own min/max row pair
    — that's a difference in *what's being measured*, not the same trait recorded
    twice. Contrast this with the same trait genuinely varying by site or
    subgroup (see `context`, below), which should stay one `trait` name.
  - A single stated bound that isn't part of a two-sided range (a threshold like
    "most abundant below 2000 µS/cm", or an upper limit like "up to 30°C") is not a
    range to split — keep it as one row, `value_type = minimum` or `maximum`
    as appropriate (a "below X"/"at most X" ceiling is `maximum`; an "at least
    X"/"X or more" floor is `minimum`).
- `value_type`: **exactly one of `minimum`, `maximum`, `raw`, `mode`, `mean`** —
  this is the traits.build schema's own fixed vocabulary (confirmed against
  `config/traits.yml` 2026-09-02), not a free choice. `mode` covers every
  categorical/qualitative value (the term traits.build uses where you might
  reach for "categorical"); `raw` covers a single specific reported number that
  isn't a min/max/mean (a point measurement, "n = 9" chromosome count, etc.).
  **A retrofit is needed**: this project used a much larger, non-conforming set
  (`categorical`, `numeric`, `min`, `max`, `numeric_threshold`,
  `numeric_upper_bound`, `numeric_lower_bound`, `numeric_mean`, `point`,
  `single`, `NA`) for most of its history before this was caught 2026-09-02 —
  map old→new as: `categorical`→`mode`, `max`/`numeric_upper_bound`→`maximum`,
  `min`/`numeric_lower_bound`/`numeric_threshold`→`minimum`,
  `numeric`/`point`/`single`→`raw`, `numeric_mean`→`mean`, `NA`→blank (empty
  string, not the literal text "NA" — these are `no_match` rows with no real
  value to type). The full corpus retrofit is done as of 2026-09-02 (see the
  combined file and every per-species `_apd.csv`) — this note exists so it
  doesn't regress. If genuinely nothing fits, that's a sign the row itself
  needs rethinking, not a sign to add a sixth value to this list.
- `context`: leave blank on most rows. Use it when the *same* trait genuinely
  needs more than one row and each row needs a short qualifier to be read
  correctly on its own — a source giving different values for different
  subpopulations, geographic zones, life stages, or source documents, or flagging
  one value as usual and another as occasional. Keep the `trait` name the same
  across those rows and put what varies in `context` instead of inventing a
  differently-named trait per case — e.g. a flowering-time range that differs
  between two disjunct subpopulations is `trait = flowering_time` twice, with
  `context = "north of Cue subpopulations"` on one row and
  `context = "southeast of Perenjori subpopulations"` on the other, not
  `flowering_time_subpop_ne` / `flowering_time_subpop_se` as two different trait
  names. The same pattern covers "usually X, occasionally Y" (`context = "usually"`
  / `context = "occasionally"`), a value specific to one named population versus
  the species generally, and a measurement given by two disagreeing source
  documents (`context` naming which document each row came from). This keeps every
  row about the same underlying trait actually discoverable as the same trait when
  someone filters or aggregates the combined file later, instead of scattering
  under a growing list of ad hoc suffixed names.
  - **Don't report a juvenile or regrowth-stage measurement of an allometric size
    trait** (height, width, or any dimension that keeps changing as the plant
    grows towards its mature size) as if it stood on its own — a juvenile's
    height today isn't a trait value, it's a snapshot partway through growth
    that the adult plant_height/plant_width rows already describe the endpoint
    of. This applies equally to a "regrowth N years post-fire" height/width
    measurement, which is the same in-progress-growth situation. Non-size
    juvenile characteristics are a different matter and are worth recording —
    leaf shape, hairiness, colour, or growth habit can genuinely differ between
    a juvenile and an adult in a way that doesn't just resolve itself with more
    growth — record those with `context = "juvenile"` (and the corresponding
    adult-stage row, if the source gives one, as `context = "adult"`) on the
    same trait name, following the pattern above.
- `source_section`: the section/subsection number (or page, if the document has no
  numbered sections) the value came from — this is what makes the row auditable
  later.
- `notes`: anything a reader would need to interpret the value correctly —
  attribution to a specific study/site the document itself flags as atypical,
  caveats the document states, which end of a split range this row represents, and
  anything uncertain. `context` is for a short, structured qualifier that the row
  needs to be read correctly; `notes` is for everything else about the row (why a
  value was chosen, what's uncertain, what the source actually said).

**`references/example_raw_traits.csv`** in this skill is a complete worked example
(*Eriocaulon carsonii*, extracted from a 2002 NSW recovery plan) — read it before
writing your own file so the granularity and grouping match. Notably it includes a
`plant_height` row for a population the source document explicitly flags as
possibly a different, undescribed species (`context = "Edgbaston (Qld) population -
possibly a distinct, undescribed taxon..."`) alongside the species' regular
`plant_height` rows — kept in the table but clearly marked via `context` as not
representative of the species as a whole, rather than either silently averaged in,
dropped, or given a separate trait name of its own. It also shows the same pattern
for water chemistry recorded at two different site types (`water_conductivity`,
`water_temperature`, each appearing twice with a different `context`). Follow that
pattern whenever a document flags a value as an outlier, disputed, site-specific,
or from an atypical population.

## Stage 2 — cross-reference against the APD

### Vocabulary sources, in order of authority

The APD (AusTraits Plant Dictionary) reference files are cached locally in the
per-species folder's own `APD_reference/` subfolder (see "File locations"
above — this is on OneDrive, not the git repo) at `APD_traits.csv` (the trait
dictionary — trait names, `trait_type`, `units`, `label`, `trait_groupings`)
and `APD_categorical_values.csv` (allowed values, one per
row, in the `allowed_values_levels` column — **not** a column called `value`;
double-check the header before concluding a term is "missing", since a wrong
column name silently returns nothing and reads exactly like a genuine absence).
Use these local copies — they're fast and reproducible, and re-fetching ~1,400
rows from GitHub on every run buys nothing. If the user explicitly asks for a
refresh (or the cache is missing), re-download from
`https://raw.githubusercontent.com/traitecoevo/APD/master/export/` and say so
out loud — don't refresh silently, since a newer APD version can rename traits
(`deprecated_trait_name` column) in ways worth flagging rather than papering
over.

Three more places to check, in order, before declaring a concept unmatched — a
concept being real-but-not-in-the-first-place-you-looked is the normal state of
things here (see the last bullet), not a sign it doesn't exist:

- `data_from_profiles/APD_reference/project_approved_extensions.csv` — trait values
  this project's own domain expert has approved that aren't in the released
  APD yet (e.g. `leaf_base_shape: clasping`). Check it first; if what you need
  is already there, use it directly at `high` confidence rather than
  re-proposing it. When the user approves a new value in conversation, add a
  row here so the next species benefits without needing this conversation's
  history.
- `references/nvis_growth_form_traits.md` (bundled with this skill) — several
  whole-plant architecture traits (`plant_canopy_form`, `stem_branching_form`,
  `stem_base_form`, `stem_constitution`) are drafted on the NVIS branch of
  `austraits.build` and not yet in the released APD export cached above. A
  plain-language "habit" sentence (e.g. "dense to open; domed; erect or
  spreading") is exactly the kind of thing that used to have nowhere to go —
  read that file when a habit/architecture description doesn't cleanly fit
  `plant_growth_form` or `stem_growth_habit` alone (see
  `references/trait_notes.md`'s "Growth form & habit" section for the specific
  gotcha this produces).
- **`config/traits.yml` at the repo root** — this project's own trait
  dictionary (the one its wider traits.build pipeline actually reads), and it
  covers real ground the plant-trait-only released APD deliberately doesn't:
  `habitat` (a very large controlled vocabulary — `rainforest_tropical_lowland`,
  `forest_monsoon`, `heath_coastal`, `mallee_heath`, and hundreds more),
  `soil_type` (`peat`, `loam_sandy`, `clay_red`, `gravel`, ...), plus
  `habitat_moisture`, `soil_moisture`, `soil_colour`, `geologic_substrate`,
  the population/range metrics (`population_count`, `subpopulation_count`,
  `individual_count`, `extent_of_occurrence`, `area_of_occupancy` — see
  `references/trait_notes.md` for how to score these), and others. Site/
  habitat/soil rows have no core-APD equivalent almost by design (APD
  measures the plant, not the site) — check this file before defaulting every
  `habitat`/`soil_type` row to `no_apd_trait`. **The trait existing and the
  value existing are two separate questions — don't default to `no_apd_trait`
  just because your specific value isn't in the allowed list.** `habitat`,
  `soil_type`, `soil_colour`, and `geologic_substrate` are large, deliberately
  growing controlled vocabularies (the maintainer adds new allowed values as
  real data surfaces new terms) — if the *trait* is real but your value isn't
  yet one of its allowed values, that's `match_confidence = proposed_new_value`
  with your candidate term in `value`, **not** `no_apd_trait`. Write the
  candidate value the way every other value in that trait's list is written:
  snake_case, underscores joining words, never a literal space (`sedimentary
  rocks` → `sedimentary_rocks`) — the maintainer's workflow auto-flags any
  value it doesn't recognise for review when the data is loaded into the
  database, and that only works if the value follows the vocabulary's own
  formatting. `no_apd_trait` is for when no trait covers the concept at all
  (e.g. water chemistry, which really has no plant- or site-vocabulary
  equivalent anywhere) — not for "this trait exists but my value is missing".
  Two caveats worth flagging in
  `notes` when you use one: most of these entries are largely undocumented
  (the `label`/`description` fields are still literal `XX` placeholders), so a
  match here is a term-matching judgment call, not a verified-definition one
  — hold confidence at `medium` unless the fit is unambiguous (the five
  population/range metrics — `population_count`, `subpopulation_count`,
  `individual_count`, `extent_of_occurrence`, `area_of_occupancy` — are
  fully-defined numeric traits with no vocabulary-matching ambiguity, so an
  unambiguous value match there scores `high`, same as any other unambiguous
  fit; **don't** score these at `new_trait` — that tier is reserved for
  traits still living only in `new_traits.yml`, pending merge into
  `config/traits.yml`, and these five are already merged); and always say
  "this project's own trait (`config/traits.yml`), not core APD" in `notes`,
  so nobody mistakes it for a released-APD match.
- **`/Users/z3524079/GitHub/austraits.build/config/traits.yml`** — a separate,
  newer, and noticeably richer copy of the trait dictionary than the one
  cached in this project's own `config/traits.yml` (last touched 2025-06-07).
  This isn't a one-off staleness bug to fix and forget — it's the normal
  state of things here. Traits get proven out on feature branches (an NVIS
  branch, a pollinator branch, etc.), sit there through review, only merge to
  `austraits.build`'s main branch some time later, and only make it into an
  actual *released* APD version later still. So a concept being
  real-but-not-yet-in the released APD, or real-but-only-on-this-newer-copy,
  is expected — treat "not found in the copy I checked" as "check the next
  copy," not as "doesn't exist." Several real traits (`leaf_phyllotaxis`,
  `pollination_vector_possible`, `dispersers`,
  `seedling_establishment_conditions`, `flowering_cues`,
  `post_fire_flowering`, `life_history_ephemeral_class`, `flower_diameter`)
  exist only here (or, in `flower_diameter`'s case, were simply
  under-searched-for) and were missed for a while as a result — see
  `references/trait_notes.md` for what each one is for and how it differs
  from the trait it's easy to confuse it with. If a species profile's
  vocabulary genuinely doesn't match anything in the reference file either,
  it's still worth a direct search of this newer copy before falling back to
  `proposed_new_trait`.

### For every row in the stage-1 file, decide:

1. **Does an APD trait exist for this concept at all?** Many won't — this project's
   habitat/ecology/competitor traits are project-specific and have no APD analogue
   (APD covers plant traits, not site descriptions). Search `APD_traits.csv`'s
   `trait` and `label` columns for a real match; don't force one that's only
   thematically adjacent. **If no trait exists, leave `apd_trait` blank and set
   `match_confidence` to `no_apd_trait` — but still fill in `value` and `units`
   from the raw data.** Don't go hunting for the closest-sounding existing trait to
   attach the value to instead (e.g. a per-inflorescence viable-seed-count has no
   APD equivalent — don't fold it into a *flower*-count trait just because both are
   "a number of reproductive structures counted per inflorescence"; that's a
   different measurement of a different thing, and mapping it there would make the
   crosswalk actively misleading rather than just incomplete). A populated
   `value`/`units` with a blank `apd_trait` is the correct, complete answer here —
   it's flagging a genuine gap in APD that might get a new trait added later, not a
   failure on your part to find one.
   - **For categorical concepts with no APD trait, still propose a best-guess
     value** when asked to (or once the raw-extraction pass is otherwise done) —
     don't stop at flagging the gap. Use `match_confidence = proposed_new_trait`,
     and put a *candidate* snake_case trait name in `apd_trait` even though it
     doesn't exist in `APD_traits.csv` yet (make that clear in `notes` — e.g.
     "PROPOSED trait, not currently in APD"). Commit to a sensible term rather than
     hedging on whether the concept "deserves" to become a trait — that call
     belongs to a human with real domain expertise (the APD dictionary only gets
     expanded that way, deliberately), so your job here is just to make their
     review easy by proposing something reasonable, not to pre-filter which
     proposals are worth making. Two things make a proposal much more useful than
     an invented-from-scratch guess:
     - **Check whether this project already has its own controlled vocabulary for
       the concept** before inventing one — several `data_extras/*_scores.csv`
       files (e.g. `habitat_scores.csv`, `plant_growth_substrate.csv`) predate
       this skill and already carry project-specific terms for exactly this kind
       of thing (habitat type, substrate, etc.). Reusing an existing project term
       (and saying so) beats coining a new one from nothing.
     - **Check whether an existing, structurally-similar APD trait suggests the
       right vocabulary style** — e.g. APD has `leaf_base_shape` but no
       `leaf_apex_shape`; proposing the latter with a value drawn from the
       former's own allowed-value list (`rounded`, `acute`, `attenuate`, ...) is a
       far stronger proposal than inventing unrelated terminology.
     - Not everything reduces to one clean term — a list of named competing
       species, for instance, isn't naturally a single categorical value. Say so
       in `notes` rather than forcing a fit, and give the list itself as `value`
       (semicolon-separated) so the information isn't lost even though it doesn't
       fit a controlled-vocabulary structure.
   - **`apd_trait` must never hold the name of a real APD trait unless the row is
     genuinely reporting data for that trait.** This is a hard rule, not a style
     preference: this file is meant to be read straight into the traits.build
     workflow, and any consumer has to be able to trust that a populated
     `apd_trait` column means "this row's `value` belongs to that trait's data,"
     full stop — a human isn't going to check each row individually. Borrowing an
     existing trait's *allowed-value vocabulary* for a different concept (e.g.
     using `leaf_base_shape`'s `acute`/`attenuate`/`rounded` terms to describe a
     petal tip, or `stem_hairs`'s `glabrous`/`hairy` to describe a corolla
     surface) is a legitimate, encouraged move — but the row is proposing a *new*
     trait that happens to reuse that vocabulary, not reporting data for the
     donor trait itself. Put the new trait's own candidate name in `apd_trait`
     (e.g. `petal_apex_shape`, `corolla_hairs`) — or leave `apd_trait` blank if
     you haven't settled on a good new name yet — but never the donor trait's own
     name. If you catch yourself writing "reuses X's vocabulary" in `notes`
     while `apd_trait` still says `X`, that's the bug: fix `apd_trait`, not just
     the sentence explaining it.
2. **For categorical traits**, which single entry in
   `APD_categorical_values.csv`'s `allowed_values_levels` (filtered to that
   `trait`) does the raw value map to? Some mappings need real semantic judgment
   (a prose phrase like "small hairless herb" implies `plant_growth_form: herb`
   *and* a separate hairiness trait, not one combined value) — that judgment is the
   point of doing this with a model rather than a lookup table. If the trait exists
   but nothing in its allowed values fits confidently, **propose a new value**
   rather than stopping at `no_match` — the same reasoning as proposing a whole
   new trait (above) applies to a single missing value within a real trait:
   `seed_colour`'s eight allowed values, for instance, are nowhere near
   exhaustive (a source saying "tan" or "shining brown" has nowhere to go yet).
   Use `match_confidence = proposed_new_value`, put your best-guess term in
   `value` in APD's own naming style for that trait (lowercase, underscores,
   matching the granularity of its existing values — e.g. `seed_colour`'s
   existing `red_brown`/`blue_purple` pattern suggests `tan` could stand alone
   or extend to `tan_brown`; use your judgment), and say in `notes` that it's
   proposed. Only fall back to `no_match` with `value` left blank when you
   genuinely can't commit to any reasonable term (rare — usually you can at
   least propose *something* sensible, even if a human later reshapes it).
   - **A source describing a plausible range or mix of values is not the same
     problem as a source describing one unclear value.** When the source states
     (or implies) that more than one allowed value genuinely applies — a shape
     given as a range ("narrowly elliptic to narrowly oblong-oblanceolate"), a
     colour given as two possibilities ("white to pale pink"), a mechanism the
     source itself can't narrow down ("gravity or wind, unconfirmed") — record
     **all** the plausible allowed values in `value` as a space-delimited string
     (e.g. `narrowly_elliptical narrowly_oblanceolate`, `white_cream pink`,
     `barochory anemochory`) rather than picking only the first term or leaving
     it unmatched. This is different from the case above (one specific,
     underspecified value you have to guess at) — here the source is telling you
     the true value could genuinely be any of several things, so recording all of
     them is the accurate answer, not a hedge. Only do this when the source
     actually supports more than one value; don't pad a single confident value
     into a list.
   - **Flowering/fruiting time is a special case**: APD records both
     `flowering_time` and `fruiting_time` as a 12-character month-flag string,
     one character per calendar month (`Y`=occurs that month, `N`=doesn't), Jan
     first — score `fruiting_time` exactly the same way, it's a real APD trait
     too, not just a flowering-time-adjacent gap. If the source states a season
     rather than months (e.g. "summer to late autumn"), translate to Australian
     seasons → months yourself and say in `notes` that you did — don't leave it
     unmatched just because the source wasn't already month-by-month. Flag if
     you're not certain the month-order convention (Jan-first) matches what APD
     actually expects — the local cache doesn't carry an example of this trait's
     values to confirm against, only note the assumption.
3. **For continuous traits, do not convert units.** Record `value` exactly as
   given in the source (same number, unconverted), and record its actual unit in
   `units`. Separately record what unit APD itself expects for that trait in
   `apd_units` (straight from `APD_traits.csv`) — this is documentation for
   whoever consumes the file next, not an instruction to reconcile the two
   yourself. There's a separate downstream workflow that handles unit conversion;
   redoing that conversion here risks introducing an error nobody's positioned to
   catch, since the crosswalk file itself is the record of what was actually
   measured. If the raw value is a range that stage 1 already split into `min`/
   `max` rows, carry that split through here too (one crosswalk row per stage-1
   row) — never collapse back to a midpoint.

**Write `<species_name_snake_case>_apd.csv`** in the same per-species folder,
with columns:

```
taxon_name,source,raw_trait,raw_value,value_type,context,value,units,apd_trait,apd_trait_type,apd_units,match_confidence,notes,evidence_level,reference,source_pdf
```

`taxon_name` and `source` carry the same meaning and values as in the stage-1 file
(same species, same document) — repeated here too so this file stands alone and
survives being combined with other species' crosswalk files later.

**`source_pdf`** (added 2026-09-02, maintainer directive) is the actual source
PDF's filename (e.g. `82665-conservation-advice-06022026.pdf`) — not the
derived per-species `_apd.csv` filename that `source_file` (below, on the
combined table) already holds. Before this column existed there was no way to
trace a row back to the literal document it came from, only the intermediate
file this project generated — populate it on every new species going forward.
Historical rows (everything before 2026-09-02) have this column blank since
the original PDF filename usually isn't recoverable after the fact — don't
backfill by guessing.

**`evidence_level`** (added 2026-09-01, maintainer directive) records how the
*source itself* backs the raw fact — independent of `match_confidence`, which
scores the APD vocabulary match, not the underlying evidence. One of:
- `stated` — the source gives this as a directly observed/measured/reported fact
  with no hedging language.
- `estimated` — the source itself flags the figure as an approximation
  ("approximately", "estimated", "up to", "~").
- `modelled` — the source states the figure was derived by calculation/GIS/
  modelling (common for EOO/AOO, e.g. "calculated using a minimum convex
  polygon").
- `assumed` — genuinely vague sourcing (a `pers. comm.` with no number, "a few
  hundred") or a plausible inference the extractor made rather than a value
  the source stated outright (e.g. mapping "recurved" to the nearest allowed
  `leaf_margin_posture` value).
- `unknown` — can't be determined from the source language; use sparingly,
  prefer actually reading the surrounding sentence first.

**This is a fixed five-value enum — `stated`/`estimated`/`modelled`/`assumed`/
`unknown`, nothing else.** A 6th value, `inferred`, crept into this project's
extractions across ~460 rows/~130 species (most heavily in the
ConservationAdvice_2026-folder batch) before being caught by the maintainer
2026-09-03 — always for a hedged, non-numeric categorical judgement the source
itself flagged as likely/probable ("likely an obligate seeder", "probably a
product of disjunct speciation divergence"). That's exactly what `assumed`
already covers ("a plausible inference... rather than a value the source
stated outright") — score it there, never invent `inferred` as a sixth value.
Also watch for the more common mirror-image mistake this same audit found:
`evidence_level = stated` on a row whose `raw_value` itself contains a hedge
word next to a number ("about 6 mm long", "c. 3 mm diam.", "estimated to be
120 km2", "~500 individuals") — a hedged number is `estimated` even when the
sentence otherwise reads like a normal taxonomic description; formal
botanical descriptions routinely use "c."/"about" as their own standard
approximation notation, and that still counts as the source flagging the
figure as approximate, not as a fully precise stated fact. 245 rows had this
exact mismatch, corrected in the same sweep. Rule of thumb: a hedge word
next to a *number* → `estimated`; a hedge word on a *category/conclusion*
with no number → `assumed`; only a fact with zero hedging language at all is
`stated`.

**Use `context` to distinguish current vs. historical, and per-site vs.
whole-species, values — for every trait, not just population/range metrics**
(maintainer feedback, 2026-09-02: "context is so important! For all
traits!"). A newer listing assessment's Table 4 giving both a current
estimate and a historical/pre-disturbance maximum plausible value is the
clearest case (`context="current estimate"` vs. `context="maximum plausible
(historical)"`), but the same instinct applies everywhere a source gives a
figure that only means what it means alongside a caveat — a per-population
count vs. a species-wide total, a single-site habitat description vs. a
species-wide generalisation, a pre-fire vs. post-fire estimate. Don't let a
qualifier like this collapse into the bare number.

**`reference`** (added 2026-09-01, maintainer directive) is the specific
in-text citation the source itself attributes to that fact, verbatim (e.g.
`Halford & Henderson, 2002`, `DPIWE 2006`), when the document gives one **for
that specific fact** — not the generic `source` document type already
captured in the `source` column. Leave blank when the advice states the fact
as its own assessment with no inline citation, or when it isn't cheaply
determinable from context already at hand — don't guess or fabricate a
citation, and don't reopen a species' source PDF solely to backfill this
retroactively unless asked; fill it in going forward as you read each PDF.

**Write this file's rows as dicts keyed by column name, not positional tuples.**
With 15 columns and most rows leaving several blank (a `no_apd_trait` row has six
blanks in a row between `value_type` and `match_confidence`), a positional tuple
is one miscounted comma away from silently shifting every field after it —
`match_confidence` ending up in the `notes` column and vice versa, invisible
until something greps for it. A small `row(raw_trait, raw_value, value_type,
context="", value="", ...)` helper that returns a dict (defaulting the columns
most rows leave blank) removes the whole failure mode, since a missing argument
just keeps its named default instead of shifting every field after it. After
writing, verify every row actually has all 15 fields — `len(row) == len(header)`
for each row via `csv.reader` — before trusting the file; a positional-tuple
version of this exact mistake happened twice in this project before this
row-count check caught it.

**Before calling any species done, run all three checks below — not as a
"looks right" skim, but as an actual script.** The `apd_trait`-mismatch check
in particular (item 3) has caught a real violation of the rule above — a
proposed trait reusing another trait's vocabulary, with that donor trait's
*name* left in `apd_trait` at `medium`/`high` confidence, exactly the mistake
the rule exists to prevent — recurring across multiple different species in
this project (`petal_shape`→`leaf_shape` alone recurred three times, and it's
recurred since in other structure-shape rows). That it keeps recurring despite
being documented means eyeballing the file isn't catching it — only actually
running this check does:

```python
import csv
SPECIES_DIR = "/Users/z3524079/Library/CloudStorage/OneDrive-UNSW(2)/Documents/threatened_species/threatened species susceptibility/threatened species profiles/scraped convseration advice files"
with open(f"{SPECIES_DIR}/APD_reference/APD_traits.csv") as f:
    real_traits = set(row['trait'] for row in csv.DictReader(f))

fp_raw, fp_apd = f"{SPECIES_DIR}/<species>.csv", f"{SPECIES_DIR}/<species>_apd.csv"

# 1. field-count integrity (every row has all 15 columns)
with open(fp_apd, newline='') as f:
    rd = csv.reader(f); header = next(rd); n = len(header)
    bad = [(i, len(row)) for i, row in enumerate(rd, start=2) if len(row) != n]
    print("malformed rows:", bad or "none")

# 2. raw <-> crosswalk row alignment (same trait, same order, every row)
r1 = list(csv.DictReader(open(fp_raw)))
r2 = list(csv.DictReader(open(fp_apd)))
print("raw:", len(r1), "apd:", len(r2))
mism = [i for i,(a,b) in enumerate(zip(r1,r2)) if a['trait']!=b['raw_trait']]
print("trait mismatches:", mism or "none")

# 3. apd_trait-mismatch bug (real trait name used for a vocabulary-reuse proposal)
for r in r2:
    at = r['apd_trait'].strip()
    if at and at in real_traits and r['match_confidence'] != 'new_trait':
        notes = r['notes'].lower()
        if r['match_confidence'] == 'proposed_new_trait' or ('reus' in notes and 'not core apd' not in notes):
            print("POSSIBLE MISMATCH:", r['raw_trait'], at, r['match_confidence'])
```

Run this on every new species file before reporting it done, and re-run it
across every existing file after any bulk edit (a `new_trait` sweep, a
retroactive fix) that touches many rows at once.

**Common cause of check 2 failing — a range split where only one file got
split.** When a source gives a range ("1–2 m") and you split it into
separate min/max rows, you must split it in *both* files identically: the
stage-1 raw row's own `value` column has to become the individual bound
("1 m" / "2 m" in two rows), not stay as the shared range string while only
the stage-2 crosswalk's `raw_value` holds the individual bound. This exact
mistake — reusing the combined range text in both raw rows while the
crosswalk rows hold the split bounds — recurred many times across a single
session before check 2 caught it each time. Whenever you split a range,
immediately re-run check 2 on that one species rather than assuming the
split was done correctly on both sides.

**Periodic corpus-wide audit — run this across the whole combined file every
so often, not just per-species.** The per-species checks above catch
structural bugs in one file; they won't catch a real trait match that was
simply missed (the row is well-formed and internally consistent, just wrong).
Group every row with a blank `apd_trait` by `raw_trait`, most-common first —
a `raw_trait` recurring across many species with zero real matches is a
strong signal either that no such trait genuinely exists (worth confirming
once, then leaving alone) or that it does exist under a different name you
haven't found yet (see "Recurring raw_trait aliases" in `trait_notes.md` for
examples this exact audit turned up on 2026-08-25 — `population_size` for
`individual_count`, `inflorescence_axis_length` for `inflorescence_length`,
etc.):

```python
import csv
from collections import Counter
with open("data_from_profiles/list_species_trait_data_apd.csv") as f:
    rows = list(csv.DictReader(f))
no_apd = [r for r in rows if not r["apd_trait"].strip()]
counts = Counter(r["raw_trait"] for r in no_apd)
for trait, n in counts.most_common(30):
    print(n, trait)
```

For each high-count `raw_trait`, grep the released `APD_traits.csv`, this
project's `config/traits.yml`, and the newer `austraits.build/config/traits.yml`
copy for the concept under plausible alternate names before concluding it's
genuinely unmatchable — a surprising number turn out to be real traits hiding
under a name the source's own wording didn't suggest (e.g. a "labellum
length" measurement is really `flower_petal_length`). When a fix applies to
many rows across many species, don't guess at the mapping alone if the right
call is genuinely ambiguous or would require adding a new allowed value —
ask first, the same way you would for any other `proposed_new_value`.

**Any time the maintainer gives you new information — approves a value,
adds a trait, extends a definition, corrects a mapping — sweep the whole
combined file for every row that information now resolves, not just the
row that prompted it.** This is a standing instruction, not a one-off: the
value of a fix compounds across every species it applies to, and a gap the
maintainer just closed will look exactly like a gap they haven't seen yet
if it's still sitting there unmatched in twenty other species. Search
`list_species_trait_data_apd.csv` for the same `raw_trait`/concept under
its known aliases (not just an exact string match — a synonym extension to
an allowed value, for instance, might resolve rows filed under several
different `raw_trait` names), apply the fix to each per-species `_apd.csv`
it touches, re-run the three integrity checks on every file you change, and
only then regenerate the combined file. Report back which species were
affected — the maintainer is tracking coverage and wants to know the fix
actually landed everywhere it could, not just in the file open in front of
you.

`raw_trait`/`raw_value`/`value_type`/`context` are carried straight over from the
stage-1 file (one crosswalk row per stage-1 row, same order). `value`/`units` are
this stage's own output — un-converted, as described above. `match_confidence` is one of `high` /
`medium` / `low` / `no_match` / `no_apd_trait` / `proposed_new_trait` /
`proposed_new_value` / `new_trait`:
- `high`/`medium`/`low`: a real, currently-existing `apd_trait` **and** a real,
  currently-existing allowed `value` are both set; these grade how confident the
  specific mapping is.
- `new_trait`: a trait that started as a proposal during extraction and has since
  been reviewed and accepted — defined in `new_traits.yml`, pending merge into
  `config/traits.yml`/a future released APD version.
- `no_match`: a real `apd_trait` is set (the concept is right) but you genuinely
  can't commit to any allowed value or reasonable proposal, so `value` is blank.
  Rare in practice now that a shaky value should usually become
  `proposed_new_value` instead (see below) — reserve `no_match` for when even a
  proposal isn't defensible.
- `proposed_new_value`: a real, currently-existing `apd_trait` is set, but none of
  its allowed values cover the raw value — `value` carries a best-guess new term
  in that trait's own naming style (e.g. a `seed_colour` the existing eight
  options don't cover). `notes` should say it's proposed, not an existing allowed
  value. Check `data_from_profiles/APD_reference/project_approved_extensions.csv` first —
  if the user has already approved this exact term, use it at `high` confidence
  instead of re-proposing.
- `no_apd_trait`: no APD trait exists for this concept, and no best-guess value has
  been proposed either — `apd_trait` and `value` are both blank, `units` may still
  be populated. This is the right resting state right after stage 1, before anyone's
  asked for proposals yet.
- `proposed_new_trait`: no APD trait exists yet, but `apd_trait` carries a
  *candidate* name and `value` a best-guess value anyway, per the categorical-gap
  guidance above — `notes` should make clear it's proposed, not real.

Use `low` rather than omitting a shaky-but-real match, so a human reviewer knows
where to look first, not just which rows are entirely unmapped.

**After writing a species' `_apd.csv`, append it to the combined file**
`data_from_profiles/list_species_trait_data_apd.csv` (the same 16 columns —
including `source_pdf` — plus a 17th, `source_file`, holding the basename of
the per-species `_apd.csv` it came from — added 2026-08-25 so rows can be
traced back to their species file without re-matching on `taxon_name`; header
once at the top) rather than leaving each species' crosswalk to be found and
concatenated later — this is the file downstream processing should actually
read.
Keep the per-species `_apd.csv` files too (they're the auditable,
one-document-at-a-time working copy, and the user has confirmed they want both);
the combined file is a derived rebuild of all of them concatenated, not a separate
hand-maintained thing. If you're not sure it's current, regenerate it from every
`*_apd.csv` in the per-species folder on OneDrive (see "File locations" above —
not the git repo's `data_from_profiles/`, which no longer holds per-species
files) rather than trust a hand-edit to have kept it in sync.

**`references/example_apd_crosswalk.csv`** in this skill is the stage-2 counterpart
to the worked example above — same *Eriocaulon carsonii* case, showing the min/max
row splitting, the month-flag string for `flowering_time`, several genuine
`no_apd_trait` rows with `value`/`units` still populated, and unconverted units
sitting alongside APD's documented units for comparison. Read both example files
together before writing your own — they're meant to be read as one worked case, not
two unrelated samples.

## A note on being conservative in stage 2

The two files exist for different reasons: file 1 is a faithful summary of what the
source document says, file 2 is a best-effort translation into a specific controlled
vocabulary that other tooling might consume later. It's fine — expected, even — for
file 2 to have real gaps: a `no_apd_trait` or `no_match` row with an honest note is
far more useful downstream than a `high`-confidence row that's actually a guess or a
force-fit into a trait that measures something adjacent but different, since someone
reviewing this data has no way to tell the difference unless you're honest about
which is which. The same reasoning is why unit conversion doesn't belong here either
— a downstream workflow already owns that step, and doing it twice (once wrong, in
two different places) is worse than doing it once, correctly, in the place set up
for it.
