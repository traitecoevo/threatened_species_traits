# Trait-specific notes

A lookup reference for specific trait pitfalls, trait-pairs, and vocabulary
gotchas discovered while extracting species profiles — not a tutorial to read
start to finish. Skim the heading for the row you're working on; add a new
entry under the right heading (create a heading if none fits) whenever a
species surfaces a new one, rather than appending to the bottom.

Every trait named here that isn't explicitly marked "project trait" or
"NVIS-branch" is a real trait in the cached `APD_traits.csv` (see SKILL.md's
"File locations" section for its current path — it moved off the git repo
2026-09-01) (the released APD) or in `/Users/z3524079/GitHub/austraits.build/config/traits.yml`
(the newer, richer copy — see SKILL.md's vocabulary-sources list for why
several traits below live only there). Check both before concluding a concept
has no real trait.

## Growth form & habit

- **A `growth_form`/`habit` row is almost never fully captured by
  `plant_growth_form` alone** — check it against `stem_growth_habit`'s full
  allowed-value list every time, not just when nothing else fits.
  `plant_growth_form` only gets the coarse category (`shrub`/`herb`/`tree`/
  ...); the descriptive words that usually come with it in the same sentence
  — `erect`, `spreading`, `open`, `dense`, `sprawling`, `prostrate`, `bushy`,
  `spindly`, `decumbent`, `tufted`, `compact`, and more — are themselves
  direct `stem_growth_habit` allowed values (the *released* trait, not the
  NVIS-branch ones) and were missed across four different species in this
  project before this was caught, always because only `plant_growth_form` was
  checked. When a leftover word still doesn't fit `stem_growth_habit`, check
  `plant_canopy_form` (crown silhouette — `rounded`, `open-crowned`,
  `spreading`, ...) and `stem_constitution` (mechanical character —
  `spindly`, `wiry`, `robust`, `straggly`, ...), both NVIS-branch traits (see
  `nvis_growth_form_traits.md`). `spindly` in particular is a real allowed
  value of *both* `stem_growth_habit` and `stem_constitution` and can
  genuinely warrant a row on each, since they're recording different facets
  (architecture vs. rigidity) of the same word.
- **A climbing plant's stem-length measurement scores on *two* real traits at
  once: `stem_length` (continuous, m — "maximum length or extent of a
  plant's stems", any growth form) and `plant_height_climbing_plant`
  (continuous, m — specifically "maximum vertical height of the vegetative
  shoot system of a non-self-supporting taxon"). Both genuinely apply to the
  same source figure for a vine/liana/climber (e.g. "climbing stems are 3-5 m
  long") — score the same value on both rows rather than picking one, same
  one-clause-many-real-traits pattern as elsewhere in this file. Only use
  `plant_height_climbing_plant` when the growth form is actually a climber
  (`plant_growth_form = climber`/`climber_herbaceous`/`climber_woody`);
  `stem_length` alone is the right call for a non-climbing plant with a
  notably long stem.
- **`plant_growth_substrate`'s hedge words matter for whether you score one
  value or two.** "May grow in rock crevices" (or similar "sometimes"/
  "occasionally" phrasing) implies the taxon is *not exclusively* lithophytic
  — it's normally `terrestrial` with an *additional*, optional `lithophyte`
  habit, so score **both** values (`terrestrial lithophyte`), not `lithophyte`
  alone. Read the hedge as information about how many substrate values apply,
  not just noise to discard.
- **`lithophyte` means the plant is physically rooted on/in a rock surface**
  (the way some orchids and ferns grow directly on boulders or cliff faces)
  — it does **not** mean "grows in an area with rocky/bouldery terrain."
  A tree or shrub rooted in ordinary soil that happens to sit among granite
  boulders or on a stony rise is `terrestrial`, not `lithophyte`, even when
  the source's habitat description is full of rock language (corrected
  2026-09-03, Brachychiton guymeri) — `geologic_substrate`, `soil_type`, and
  `habitat` already exist specifically to carry that rocky-terrain context,
  so don't reach for `plant_growth_substrate` to do the same job a second
  time with the wrong value.
- **A "basal rosette" leaf arrangement is `stem_growth_habit=rosette`, not
  `leaf_arrangement`** — `leaf_arrangement`'s own vocabulary
  (`decussate`/`distichous`/`spiral`/`crowded`/...) has no rosette value;
  `stem_growth_habit` is the real, released trait that carries it (corrected
  2026-09-03, two Drosera species mis-scored this the same way).
- **`stem_branching_form`** (real trait, newer austraits.build copy) records
  stem/trunk *count and branching pattern* — `single_basal_stem`/
  `multiple_basal_stems`/`multiple_near_basal_stems`/`unbranched`/`branched`/
  `few-stemmed`/`multi-stemmed`/`many-stemmed`, plus branching-density and
  -pattern terms (`openly-branched`, `much-branched`, `divaricately-branched`,
  etc.). Distinct from `stem_growth_habit` (erect/spreading/prostrate/...) —
  a "single trunked or multi-stemmed" description needs *both* traits, one
  for stem count/origin, one for overall carriage; don't let scoring one
  substitute for the other (missed for Brachychiton guymeri, 2026-09-03,
  despite the source stating it plainly in the very first description
  sentence).
- **"Epiphyte" (and "lithophyte"/"terrestrial"/etc.) scores on
  `plant_growth_substrate`, not `life_form`.** Both traits happen to share an
  `epiphyte` allowed value, but they're different concepts — `life_form` is
  a Raunkiaer-style classification (`phanerophyte`, `chamaephyte`,
  `geophyte`, `therophyte`, `epiphyte`, ...) about where a plant's
  perennating buds sit relative to ground/soil surface across seasons;
  `plant_growth_substrate` is specifically what the plant is rooted in or
  attached to (`epiphyte`, `lithophyte`, `terrestrial`, `aquatic`,
  `semiaquatic`, `marine`, `hemiepiphyte`). A source saying a plant "grows on
  the bark of trees" or "is a terrestrial herb" is describing substrate —
  score `plant_growth_substrate`, not `life_form` (corrected 2026-09-02
  after two epiphytic orchids were mis-scored against `life_form`). Only use
  `life_form` when the source is actually making a Raunkiaer-style claim
  (e.g. an explicitly tuberous/bulbous terrestrial species genuinely
  supports `life_form = geophyte`, which is a distinct fact worth its own
  row *alongside*, not instead of, `plant_growth_substrate = terrestrial`).
- **`plant_canopy_form`'s allowed values cover more whole-plant silhouette
  descriptions than you'd guess from the term list alone** — a shrub
  described as "rush-like" or similarly sparse/upright/broom-shaped is
  `broom-like`, not `no_apd_trait`. Check the full NVIS-branch list
  (`broom-like`/`columnar`/`dense-crowned`/`diffuse`/`flat-topped`/`globose`/
  `hemispherical`/`obconical`/`open-crowned`/`mounded`/`pine-like`/
  `pyramidal`/`rounded`/`spreading`/`squat`) before giving up on a
  silhouette-shaped description.
- **`plant_height` is vegetative-only — never score an inflorescence/
  flowering-stem height there.** Its own APD definition is explicit: "maximum
  vertical height of the *vegetative* shoot system... not the height reached
  by reproductive shoots." APD already has the exact machinery for the
  common case where a source *only* gives a flowering-stem height (orchids
  with a single leaf and one flowering stem, leafless mycoheterotrophs,
  corm/bulb/tuber geophytes with no persistent aboveground parts, rosette or
  leaf-tuft plants): score `plant_height_type = na_geophyte` (or
  `na_rosette`/`na_leaf_tuft`, whichever fits the described habit) to
  document *why* `plant_height` isn't used, and put the actual numeric
  height on `plant_height_reproductive` (m, whole reproductive shoot) —
  **and**, from the same clause, `inflorescence_length` (cm, just the
  inflorescence/flower-cluster portion) is very often *also* a correct,
  distinct row (both apply to a single-flowering-stem plant, since the
  entire stem essentially *is* the inflorescence there). **This project
  scored a "flowering stem as a proxy for whole-plant height" pattern
  directly onto `plant_height` for multiple orchid/geophyte species before
  this was caught by the maintainer 2026-09-02** (confirmed and fixed for
  *Danhatchia copelandii* and *Thelymitra variegata* within the current
  session's batch) — **this is flagged as a likely systemic, corpus-wide
  issue needing a dedicated retrospective sweep across earlier sessions'
  orchid/geophyte/rosette species, which the maintainer will scope**; don't
  assume it's already been swept just because these two instances were
  fixed.
- **`plant_photosynthetic_organ = phyllode` cross-maps to almost every Acacia
  s.l. (wattle) species** — "leaf-like phyllodes" in a description is a
  near-automatic high-confidence row, not something to check case by case;
  score it whenever Acacia phyllodes are mentioned. A *leafless* Acacia (or
  similar) that still photosynthesises through green stems instead is
  `plant_photosynthetic_organ = cladode`, not `non-photosynthetic_plant` —
  that value is for taxa with *no* photosynthetic tissue at all
  (achlorophyllous parasites/saprophytes), not a photosynthetic-stemmed,
  leafless plant.

## Leaves

- **Compound leaves — leaflet dimensions have their own real, dedicated
  traits: `leaflet_length`/`leaflet_width` (continuous, **cm**, not mm) and
  `leaflet_count` (numeric, `{count}`).** Don't force leaflet length/width
  onto `leaf_length`/`leaf_width` (those are whole-leaf traits, mm) — this
  happened at least twice in this project (most recently *Zieria odorifera
  subsp. copelandii*, 2026-09-02, corrected in place) despite the raw_trait
  label already correctly saying "leaflet_length"/"leaflet_width" — check
  the `apd_trait`, not just the raw label. A named leaflet-number word
  ("trifoliate" = 3, "quinquefoliate"/"digitately 5-foliolate" = 5, etc.)
  should always be scored as an explicit `leaflet_count` row at `high`
  confidence alongside `leaf_compoundness = compound` — don't record the
  compoundness fact without also recording the count it directly implies. **Leaflet *shape*, by
  contrast, has no dedicated trait — map it directly to `leaf_shape` with
  `context = "leaflet"`,** not a proposed new trait; this is a legitimate
  direct use of `leaf_shape`'s own concept (2D outline) applied to a
  leaflet, not an unrelated-structure vocabulary reuse the "never name the
  donor trait" rule is meant to catch (that rule is about borrowing a
  trait's *vocabulary style* for a genuinely different concept, like reusing
  `leaf_base_shape`'s terms for a petal tip — not about the same concept
  measured on a smaller leaf-like organ). **When a compound leaf's overall
  length is given as the length of its rachis** (the axis the leaflets
  attach to — sometimes stated as its own figure alongside individual
  leaflet dimensions), **that rachis length is the real APD trait
  `leaf_length`** (whole-leaf length, base to tip) — score both the raw
  `rachis_length` observation and `leaf_length` from the same figure, rather
  than treating rachis length as merely a proposed trait with no released
  equivalent. Rachis *width*, by contrast, doesn't have a `leaf_width`
  equivalent (leaf width is normally the spread across the blade/leaflets,
  not the diameter of the central axis) — keep that as its own proposed trait.
- **`leaf_phyllotaxis` (categorical: `alternate`/`opposite`/`whorled`)** is
  the real trait for leaf position at a *single stem node* — **confirmed in
  the released `APD_traits.csv` cache (re-checked 2026-08-25), not
  newer-copy-only as this note used to say** — score a direct match at
  `high`. Easy to conflate with `leaf_arrangement` (a genuinely different trait: the
  3D pattern along the *whole shoot* — `decussate`/`distichous`/`spiral`/
  `crowded`/etc. — with no alternate/opposite/whorled values of its own). A
  source description often warrants a row on *both*: "opposite pairs,
  alternate pairs at right angles to each other" is `leaf_arrangement =
  decussate` **and** `leaf_phyllotaxis = opposite` (decussate is by
  definition opposite phyllotaxis rotated 90° between nodes).
- **A stated leaf angle is `leaf_axil_angle` or `leaf_inclination_angle`, not
  prose to leave unmatched.** `leaf_axil_angle` (continuous, deg, 0-180) is
  the angle between the leaf and the stem at the axil — "spreading to
  slightly more than 90° to the stem" is a direct, numeric match (record the
  stated bound; "slightly more than 90" is a `min` of 90, not a range).
  `leaf_inclination_angle` (continuous, deg, -90 to 90) is a different
  measurement — the slope of the leaf blade itself relative to
  horizontal/the sun — don't conflate the two just because both are called
  an "angle"; check which one the source is actually describing (angle *at
  the stem attachment* vs angle *of the blade*).
- **`petiole_length`/`petiole_width` (continuous, cm) are real APD traits.**
  A "sessile"/"stalkless" leaf is a genuine `petiole_length = 0` match, not a
  gap — a sessile/stalkless structure is a real measurement (zero), not an
  absence of one. A sessile *inflorescence* or *pedicel* is a different
  structure with no real trait found so far — propose one (e.g.
  `pedicel_length`) rather than reusing `petiole_length` for a non-leaf
  stalk.
- **"Egg-shaped" is the everyday name for both `ovate` and `obovate`** (an
  inverted egg) — not a shape `leaf_shape` fails to cover, just described in
  lay terms. Translate a plain-language synonym rather than defaulting to
  `no_match`; record both plausible values as a space-delimited multi-value
  string if nothing else in the source settles which one.
- **`leaf_cross_section_shape`** (project trait, not APD; renamed from
  `leaf_cross_section` — a cross-section is a "thing", a trait needs a
  measured property) covers `keeled`/`conduplicate`/`terete`/`subterete`/
  `flat`/`concave_convex`. See `new_traits.yml`.
- **`leaf_apex_shape` is a real, already-released APD trait** (`acuminate`/
  `acute`/`obtuse`/`rounded`/`apiculate`) — **not** a pending project trait.
  This project incorrectly carried it in `new_traits.yml` as a `new_trait`
  from 2026-08-24 until 2026-09-02, when a full read-through of
  `austraits.build/config/traits.yml` found it was real all along; that
  incorrect `new_traits.yml` entry has been removed. Score a direct match at
  `high`, not `new_trait` — species previously scored `new_trait` for this
  trait (a batch of ~29, listed in git history for the removed entry) still
  need retroactively correcting to `high` (not yet done as of 2026-09-02).
  None of its 5 values cover a notched/bilobed apex ("truncate" doesn't fit
  either) — a source describing one is a genuine `proposed_new_value` against
  this *real* trait (e.g. `emarginate` for notched, `truncate` for
  flat/squared-off), not something to add to `new_traits.yml`, since that
  file is for traits pending merge, not values pending merge into an
  already-merged trait.
- **`leaf_margin_posture`** (real APD trait: `flat`/`involute`/`revolute`/
  `undulate` — margin curvature/curling, distinct from `leaf_margin`'s
  entire/toothed edge-shape concept) is easy to miss and easy to reinvent as
  a project trait instead. "Margins rolled upward" → `involute`; "rolled
  under/downward" → `revolute`; "wavy" → `undulate`. A grass leaf described
  as "stiffly involute" is this trait directly at `high` — don't propose
  `involute` as a new value of `leaf_cross_section_shape` (a different,
  project-only trait) instead, a mistake this project made once before the
  real trait was found.

## Flowers & reproduction

- **A part count for sepals, petals, *or* tepals is all the same APD trait**:
  `flower_perianth_merism` (continuous, `{count}`) is explicitly "the count
  of perianth parts (sepals, petals, and tepals) in each floral whorl" — not
  tepal-specific despite that being its first use in this project.
- **A "tubular" flower/corolla is a fusion measurement**: `flower_perianth_fusion`
  (0-1 fusion-proportion; a stated partial fusion like "connate for 1/4 of
  length" is `0.25`) scores a tubular corolla as `1` — the tube shape *is*
  what full perianth-part fusion looks like. Check whether a fused-sounding
  shape word ("tubular"/"funnel-shaped") is actually describing fusion
  before leaving it `no_apd_trait`.
- **`flower_length` (continuous, cm) is for the *whole flower*** — a single
  length for an entire corolla/perianth (tube + limb, base to tip), even
  when the source calls it "corolla length" or "perianth length". Does
  *not* apply to one petal/tepal/sepal among several (a Fabaceae standard/
  wing/keel measurement is a part measurement, not the whole flower).
- **`flower_petal_length` (continuous, mm) is real** — petal *length* always
  goes there directly, no need to propose anything. `flower_petal_width` has
  no APD equivalent — that's `flower_petal_width` (project trait, mm) sibling
  in `new_traits.yml`. A source describing multiple petal types on one
  flower (Fabaceae standard/wing/keel) still maps each to
  `flower_petal_length`/`flower_petal_width`, with a single-word `context`
  naming which petal (`standard`, `wing`, `keel` — not phrases).
- **`flower_diameter` (continuous, **cm**) is real** and has been in the
  local `APD_traits.csv` cache the whole time — don't trust an older row's
  `no_apd_trait` note at face value for common concepts like flower size,
  re-search yourself. Watch the unit: cm, not mm, unlike most other floral
  length/width traits.
- **`inflorescence_type` is a real APD trait, not yet in the released
  version, confirmed 2026-09-01 to be present (with all ten allowed values)
  in the newer austraits.build copy of `config/traits.yml`** — this
  supersedes an earlier, incorrect run where it was treated as this
  project's own invention and scored `new_trait`/`proposed_new_value`; a
  corpus-wide sweep corrected every prior row to `medium` (the same
  treatment given to `plant_canopy_form` and `rhizome_form`). All ten
  values — `raceme`/`cyme`/`head`/`solitary`/`axillary`/`spike`/`panicle`/
  `umbel`/`terminal`/`corymb` — score at `medium` going forward; don't
  reintroduce `new_trait`/`proposed_new_value` for any of them.
  `axillary`/`terminal` are position descriptors ("one per axil"/"at the
  branch tip"), not architecture terms — either can and should be scored as
  a second `inflorescence_type` row alongside whatever architecture term
  (e.g. `head`, `umbel`) already covers the same source clause, rather than
  being left unmatched as a seemingly-redundant position detail. Note: the
  trait's own description field references a sibling `inflorescence_shape`
  trait for silhouette (globose/cylindrical/ovoid) — that trait does exist
  in the same newer austraits.build copy (values: `spherical`/`cylindrical`/
  `elongated`/`narrow`), also not yet released, also score at `medium`; a
  shape-only description like "open elliptic panicle" should score the
  shape half `no_apd_trait` only if it doesn't fit one of those four terms.
- **`flower_count_maximum` (numeric, `{count}`) is real, but it's a
  whole-plant/whole-season total flower output — not a per-inflorescence
  count.** "Up to 35 flowers" on one raceme/spike/head/capitulum is a
  *different* concept, and it turns out APD already has a real, released
  trait for exactly this: **`buds_per_inflorescence`** (continuous,
  `{count}/{count}`, entity_URI `trait_0012351` — "the count of buds in an
  inflorescence, where an inflorescence can be either a single cluster of
  flowers or the entire reproductive shoot system"). This project spent a
  long stretch treating the concept as an invented project trait,
  `flowers_per_inflorescence` (numeric, `{count}`, allowed range 1–1000),
  described as "the number of flowers borne on a single inflorescence
  (raceme, spike, head, panicle, corymb, umbel, etc.), regardless of
  inflorescence architecture" — before discovering, on 2026-08-25, that this
  whole concept was already a real, released APD trait. `flowers_per_inflorescence`
  is now **superseded** and must never be proposed again for a new species;
  every species previously scored under it was retroactively corrected to
  score `buds_per_inflorescence` directly at high confidence instead. Its
  entry has been removed from `new_traits.yml` (this note is now the sole
  record of the correction, kept here rather than there since it's a closed
  historical fix, not a pending trait). A composite-head floret count (ray florets, disc florets) is also a
  `buds_per_inflorescence` count, with `context` naming which floret type —
  a "head" is one of `inflorescence_type`'s accepted architectures, so a
  per-head count is a per-inflorescence count. The trait's own "entire
  reproductive shoot system" framing is flexible enough to also cover a
  *heads-per-compound-cluster* count (e.g. "2-4 heads, each with 25-40
  flowers" scores **two** `buds_per_inflorescence` rows — one for the
  flowers-per-head, one for the heads-per-shoot-system — at appropriately
  different confidence levels since the second is a looser reading).
  This bug (forcing a per-structure count onto `flower_count_maximum`, or
  inventing a project trait that duplicated a real one) recurred across many
  species in this project before being caught — always double-check which
  concept a source's flower count actually is, and re-search
  `APD_traits.csv` for a real trait before proposing a new one.
- **Correction to the above, 2026-09-02**: `flowers_per_inflorescence` has
  reappeared as its own real (unreleased, entity_URI `XX`) APD trait in the
  newer austraits.build copy of `config/traits.yml`, immediately alongside
  `buds_per_inflorescence` — same `{count}/{count}` units, same "single
  cluster or entire reproductive shoot system" framing, but counting
  *flowers* rather than *buds*. These are legitimately two different
  concepts that the dictionary now separately supports. Going forward,
  score whichever concept the source actually describes (an explicit flower
  count → `flowers_per_inflorescence`; a bud count → `buds_per_inflorescence`)
  at `high` confidence, rather than forcing every per-inflorescence count
  onto `buds_per_inflorescence` as the note above once required — that
  blanket rule is now superseded for this specific pairing (the rest of the
  note above, on not inventing a duplicate project trait and not conflating
  with `flower_count_maximum`, still stands).
- **`reproductive_maturity`'s own definition is the age of *first*
  flowering/first ability to set any seed at all** ("this trait will often
  be scored as when plants first produce flowers, as this is easier to
  score" — per the trait's own comments). Watch for sources that use the
  words "reproductive maturity" for something else — a later, secondary
  milestone (full/maximum reproductive output), distinguished from an
  earlier "age of first flowering" figure in the same paragraph. The
  *earlier* figure maps to APD's `reproductive_maturity`; the *later* one is
  `reproduction_time_to_maximum_seeding` — a real, merged trait in
  `austraits.build/config/traits.yml` as of 2026-09-02 (this project
  originally proposed it as `age_maximum_reproductive_capacity`, still
  findable under that old name in *Darwinia carnea*'s extraction notes and
  git history, but score new species directly under the current name at
  `high` confidence, not `new_trait`) — don't default to `reproductive_maturity`
  just because the source happens to use that exact phrase for the later milestone.
- **`senescence_onset` / `reproductive_maturity_to_senescence`** (project
  traits, both age/duration in years) — age at which crown/whole-plant
  senescence begins, and the sibling duration from reproductive maturity to
  that point. See `new_traits.yml`. When both are given alongside a
  `lifespan`/`max_lifespan` figure for the same species, sanity-check the
  arithmetic: `reproductive_maturity` (age) + `reproductive_maturity_to_senescence`
  (duration) sets a lower bound on when senescence begins, which itself sets
  a lower bound on total lifespan — a `lifespan` figure smaller than that sum
  is an internal inconsistency worth flagging (or correcting, with the
  derivation shown) rather than silently recording as-is.
- **`generation_time`** (raw-extraction label; not `generation_length` —
  length is a spatial dimension, not a duration) — **accepted as a project
  trait 2026-09-02** (numeric, `a`, 1-500) after recurring in 15+ species'
  IUCN-criteria assessments with nowhere to be recorded; see `new_traits.yml`.
  Score a direct match at `high` once a species gives an explicit generation-
  length figure or formula-derived estimate (IUCN assessments often show the
  derivation, e.g. "age at first reproduction + [0.5 x reproductive length]"
  — that's still `generation_time`, not `no_apd_trait`, even though it's a
  calculated rather than directly-observed figure; note the derivation in
  `notes`).
- **`lifespan` is numeric, but a source stating a species "can live
  indefinitely" (typically because it resprouts rather than senescing) still
  gets a row — write `indefinite` into `value` verbatim rather than leaving
  the fact unscored just because it won't parse as a number** (maintainer
  instruction, 2026-09-03, Brachychiton guymeri) — the maintainer will decide
  downstream how to encode it numerically. Don't invent a large placeholder
  number (e.g. "100 years") to force it to look numeric; that fabricates
  precision the source doesn't support.
- **Pollination — a whole cluster of real traits was missed for most of this
  project's history (found 2026-09-02 on a full read-through of
  `austraits.build/config/traits.yml` after a maintainer caught a
  sexually-deceptive orchid species with no pollination data recorded at
  all). Check every reproductive-ecology section against all of these, not
  just `pollination_syndrome`/`pollination_vector_possible`/`dispersers`
  below (which were already documented) — pick by what kind of evidence the
  sentence reports, not just whether a pollinator taxon is named:**
  - `pollination_syndrome` — a *flower-morphology-based inference* about
    what probably pollinates a flower shaped a certain way.
  - `pollination_vector_possible` (values incl. `wasp`/`bee`/`bird`/`ant`/
    `autonomous`/etc.) — an *actual or likely floral visitor* a source names
    (observed, or "potential pollinators recorded interacting with"). If a
    source names a wasp genus, "beetles directly observed," "at least 12
    genera of bee recorded interacting with," etc., that's vector-possible
    data, not syndrome data, even when the wording sounds similar. Also
    covers "pollen removed/deposited observed" language even when the
    visiting animal itself wasn't identified — that's still evidence of an
    (unidentified) vector's activity, not full `pollination_vector_known`.
  - `pollination_vector_known` — direct pollen-transfer/seed-set evidence
    from a pollinator-exclusion experiment or explicit identified-species
    observation. Rarer in these profiles than `_possible`, but don't default
    everything to `_possible` just because it's more common — check whether
    the source's evidence is actually this strong. A source phrase like
    "obligate insect pollinator" (a confirmed, named relationship) maps here
    directly, not to `_possible`.
  - `pollination_system` (values incl. `biotic_specialised`/
    `biotic_unspecialised`/named taxa/`self`/`abiotic`) — a field-study-based
    (preferred) or morphology-plus-observation-based classification of the
    *system*, distinct from `pollination_syndrome` (morphology-only
    inference). "Almost certainly has an obligate pollination relationship
    with one or few species of thynnine wasp" is `biotic_specialised` here,
    **and** `pollination_syndrome: wasp` from the same sentence — score both,
    they're different facets of the same fact, not competing options.
  - `flower_pollinator_reward` (values: `deceit`/`food`/`heat`/`nectar`/
    `oil`/`pollen`/`reproduction`/`scent`/`stigmatic_exudate`/
    `sugary_exudate`) — what (if anything) the pollinator gets. **Sexually
    deceptive pseudocopulation (thynnine wasp orchids, hammer orchids,
    Chiloglottis, Caladenia, Drakaea, etc.) is always `deceit`** — the male
    wasp gets nothing, a fact this project missed for every sexually
    deceptive orchid species done before 2026-09-02.
  - `flower_scent_production` / `flower_nectar_production` (both simple
    presence/absence binaries: `scent_produced`/`scent_absent`,
    `nectar_produced`/`nectar_absent`) — score whenever the source states or
    clearly implies either. Pheromone-mimicking chemical compounds (sexual
    deception) count as `scent_produced`.
- **`dispersers`** (newer-copy only; categorical, values incl. `ants`/
  `wind`/`birds`/`water`/`abiotic`/etc.) is the disperser-side counterpart to
  `pollination_vector_possible` — the actual/likely dispersal agent, distinct
  from `dispersal_syndrome`'s morphology-based mechanism classification.
  Whenever a source names an actual disperser ("dispersed by ants," "wind
  dispersed"), score **both** `dispersal_syndrome` (e.g. `myrmecochory`/
  `anemochory`) **and** `dispersers` (the named agent) from the same clause
  — same cross-mapping relationship as `dispersal_syndrome`/
  `dispersal_appendage` below.
- **Dispersal structure vs dispersal mechanism are two different real
  traits from one source clause**: an aril/elaiosome is `dispersal_appendage`
  (the physical structure); the resulting dispersal syndrome (e.g.
  `myrmecochory`) is `dispersal_syndrome` (the mechanism) — score both when
  a source describes an appendage that explains a dispersal syndrome, rather
  than picking one.

- **Araceae spathe-spadix structures are not the "flower" of `flower_colour`/
  `perianth_colour`** — a spathe is a bract subtending the true, tiny
  florets on the spadix, and an appendix is a sterile zone of the spadix,
  neither a perianth part. Colour/shape/size detail for these structures
  (e.g. "dark purple-black spathe," "maroon-black appendix," "septate tube")
  has no equivalent AusTraits floral-organ trait and should be documented in
  `raw_value`/`notes` only, not force-mapped onto `flower_colour`. By
  contrast, `flower_scent_production` (a whole-inflorescence-level
  presence/absence trait) and `inflorescence_type` (`solitary` is one of its
  ten allowed values) still apply normally to an Araceae inflorescence —
  don't over-generalise the "no equivalent trait" reasoning to every
  Araceae-specific noun in the same sentence (established while extracting
  *Typhonium praetermissum*/*Typhonium* sp. Cox Peninsula, 2026-09-02).

## Fruit & seed

- **A stated fruit "diameter" (round/spherical fruits — drupes, berries,
  cones) scores on *both* `fruit_length` and `fruit_width`, not a missing
  `fruit_diameter` trait** (maintainer directive, 2026-09-02). There's no
  dedicated diameter trait in APD, but for a fruit where width and length
  are the same measurement (a round fruit has no separate long/short axis),
  the single stated diameter value genuinely satisfies both real traits —
  score the identical value on both, at `high` confidence, rather than
  treating it as `no_apd_trait` or picking only one. This is the same
  one-fact-many-real-traits pattern as elsewhere in this file (e.g. the
  climbing-plant stem-length case above); the corpus had several
  `fruit_diameter` rows mis-scored `no_apd_trait` or mapped to only
  `fruit_width` before this was caught.
- **A "pod" is a fruit, not a separate structure**: Acacia/legume "pod"
  dimensions and shape are `fruit_length`/`fruit_width`/`fruit_type`
  (`legume`/`legume_indehiscent`) — the same traits any other fruit maps to.
- **Spines, thorns, and prickles have their own dedicated trait —
  `plant_physical_defence_structures`** (`absent`/`sharp_pointed_defence`/
  `prickle`/`pungent_leaf_apex`/`spine`/`stinging_or_irritant_hairs`/
  `thorn`) — check this before reaching for `plant_spinescence`, whose
  allowed values are density-*and*-size buckets (e.g.
  `high_density_hard_spines_to_5mm`) that a plain qualitative statement like
  "spine-tipped branches" usually can't support with any confidence.
  `plant_physical_defence_structures` also distinguishes *which structure*
  is sharp: a modified stem is `thorn`, a modified leaf/leaf-part (petiole,
  midrib, stipule) is `spine`, a whole leaf ending sharply is
  `pungent_leaf_apex`, a bark/epidermis outgrowth that's neither is
  `prickle` — read which structure the source actually calls sharp rather
  than defaulting to `spine` as the generic-sounding option.

- **`fruit_shape`** (categorical, same allowed-value vocabulary as the real
  APD trait `seed_shape` — `cylindrical`/`hemispheric`/`ovoid`/`globoid`/
  `polyhedral`/`polyhedral_inflated`/etc.) is a project new_trait (accepted
  2026-08-25, see `new_traits.yml`) — check it before defaulting a fruit-shape
  description to `no_apd_trait`. "Cup-shaped"→`hemispheric`, "barrel-shaped"→
  `cylindrical`, "egg-shaped"→`ovoid`, an inflated multi-lobed capsule→
  `polyhedral_inflated`, a ribbed/angular capsule→`polyhedral`. A bare "flat"
  (e.g. a legume pod with no further shape detail) genuinely doesn't map to
  any of these rounded/faceted-solid terms — score that as `no_match` (real
  trait, no defensible value) rather than force a guess or fall back to
  `no_apd_trait`.
- **`stem_cross_section_shape`** (categorical: `terete`/`winged`/`angular`/
  `quadrangular`) is a project new_trait (accepted 2026-08-25) for stems,
  branches, or branchlets described as "four-sided" (→`quadrangular`),
  "angular", "winged", or "round/terete" in cross-section — also reusable
  with a context note (e.g. `context="inflorescence axis"`) for other
  elongated axis-like structures described the same way.

## Bark

- **`bark_texture`** (categorical: `smooth`/`flaky`/`fibrous`/`furrowed`/
  `fissured`/`corky`/`curling`/`scaly`/`rough`/`papery`) and **`bark_colour`**
  (categorical, `soil_colour`-style earthy palette: `black`/`brown`/
  `brown_dark`/`brown_light`/`green`/`grey`/`orange`/`pink`/`red`/`white`/
  `yellow`) are project new_traits (accepted 2026-08-25, see `new_traits.yml`).
  `bark_morphology_eucalyptus` (the real APD trait) is explicitly restricted
  to Eucalyptus/Corymbia/Angophora's named bark types (stringybark/box/gum/
  ribbonbark/ironbark/peppermint/stocking) and shouldn't be force-applied to
  other genera, even when a term like "smooth" would technically fit one of
  its values. **`bark_texture` is not restricted the other way** (2026-09-02,
  maintainer-confirmed) — it's the appropriate trait for all non-eucalypt
  genera, but can *also* be scored on a Eucalyptus/Corymbia/Angophora species
  alongside `bark_morphology_eucalyptus`, for texture detail the named
  eucalypt type doesn't capture (e.g. "stringybark ... with included thin
  scales" → `bark_morphology_eucalyptus=eucalypt_stringybark` AND
  `bark_texture=scaly`, both scored). A combined description ("furrowed, dark
  grey bark") is a texture clause plus a colour clause — split into two raw
  rows, one per trait, rather than picking just one.
- A curling/peeling Acacia bark commonly called "minniritchi" in source text
  is `bark_texture=curling`.

## Stipules

- **`stipule_presence`** (categorical: `present`/`absent`/`spine`/`scale`/
  `leafy`) and **`stipule_length`** (numeric, mm) are project new_traits
  (accepted 2026-08-25, see `new_traits.yml`). Stipules modified specifically
  into sharp defensive spines should *also* get a `plant_physical_defence_structures`
  row (value `spine`) alongside `stipule_presence=spine` — the two traits
  capture different things (defence structure identity vs. the stipule's own
  presence/form) from the same clause.

## Non-photosynthetic / parasitic plants

- **A saprophyte/mycoheterotroph/parasite gets three separate real traits,
  not one**: `plant_alternative_energy_and_nutrient_acquisition_strategy`
  (`saprophyte`/`carnivorous`/`nutrient_mining` — use the source's own word
  directly, it's usually a literal allowed value), `plant_photosynthetic_organ`
  (`leaf`/`cladode`/`phyllode`/`non-photosynthetic_plant` — a leafless,
  achlorophyllous, or "translucent white" plant is `non-photosynthetic_plant`),
  and `parasitic` (whose `not_parasitic` value explicitly, deliberately
  covers a fungal-partner symbiosis — a mycoheterotroph parasitises fungi,
  not another plant, so it's `not_parasitic` here). All three are usually
  worth a row when a source describes a non-photosynthetic plant.
- **Carnivorous plants (Drosera/sundews, Utricularia, Nepenthes, etc.) are a
  separate case from the non-photosynthetic plants above — they're still
  fully photosynthetic, just supplementing nutrition via prey capture — but
  still need `plant_alternative_energy_and_nutrient_acquisition_strategy=
  carnivorous` scored.** Missed for two Drosera species in a row
  (2026-09-03, maintainer-caught) despite one of them stating "As an unusual
  carnivorous plant..." directly in its Threats section, and despite the
  other's own row *notes* already saying "glandular hairs expected for this
  carnivorous genus" without ever actually scoring the trait. If the genus is
  Drosera (or another well-known carnivorous genus) and the source describes
  sticky/glandular trapping leaves, score this even when the document itself
  never uses the word "carnivorous" — that's common enough taxonomic
  knowledge to support `evidence_level=assumed` at `medium` confidence.

## Foliage time (deciduous geophytes especially)

- `foliage_time` is a REAL, already-released APD trait (entity_URI
  `trait_0030030`): "months during which taxon has leaves," keyed as the same
  12-character Jan-start Y/N string as `flowering_time`/`fruiting_time`. Easy
  to miss because it isn't one of the "obvious" flower/fruit phenology traits,
  but any source that gives seasonal leaf-emergence/leaf-dieback wording is
  scoring material for it — not just explicit deciduous orchids/geophytes.
- Watch for the info showing up in two different places that need combining:
  a direct statement ("leaf emerges in autumn... shrivels by mid-late
  spring") vs. an indirect one (a stated dormant period for controlled burns,
  or a fire-vulnerability window for "above-ground parts") — both describe
  the same foliage-present/absent cycle and are usually mutually
  corroborating. When only the indirect fire/dormancy framing is given, still
  score `foliage_time` from it (don't wait for an explicit leaf-phenology
  sentence) — held at high confidence when the fire-vulnerable window and the
  dormant window are two independently-stated, non-overlapping periods that
  complement each other to fill all 12 months; medium when only one
  loosely-worded window ("autumn", "mid-late spring") is given and month
  boundaries require interpretation.
- Pairs naturally with `leaf_phenology` (`deciduous`/`drought_deciduous`/
  etc.) when the source also uses that word directly — score both rather than
  picking one, they're not redundant (`leaf_phenology` is the categorical
  cause/pattern, `foliage_time` is the specific month-by-month timing).

## Vegetative reproduction & ploidy

- **`ploidy`** (numeric, count of chromosome sets, 1-4) is real - score directly
  for "diploid"/"triploid"/"tetraploid" statements (triploid=3, etc.).
- **`vegetative_reproduction_ability`** (categorical: `vegetative`/
  `not_vegetative`) is a real, binary trait for whether a taxon can reproduce
  asexually at all - distinct from `clonal_spread_mechanism` (categorical:
  `root_buds`/`rhizome`/`stolon`/`bulb`/`corm`/etc. - the *specific structure*
  used) which is a sibling trait for the mechanism itself. Both are in this
  project's `config/traits.yml`, not yet in the released APD cache.
- **`bud_bank_location`** (categorical: `bud-bearing_root` for root suckering,
  `basal_stem_buds`/`epicormic_buds`/`rhizome`/etc.) records *where* the buds
  that allow post-disturbance resprouting are located - overlapping in
  concept with `clonal_spread_mechanism` but framed around disturbance
  survival rather than vegetative spread; score both when the source
  supports it, since they capture related but distinct angles on the same
  underlying regeneration structure.

## Fire & disturbance response

- **`fire_response` is not a trait at all — never write it into `apd_trait`.**
  It's a reasonable *raw_trait* label, but the real trait for what happens to
  a plant after fire is always `resprouting_capacity` (or occasionally
  `bud_bank_location`/`post_fire_flowering`/`seed_germination_treatment`,
  depending what the sentence actually describes — see the rest of this
  section). This exact invented-trait mistake recurred across 7 species
  (Drakaea micrantha, Drakaea isolata, Acacia atrox, Acacia lumholtzii,
  Aristida thompsonii, Asperula tetraphylla, Acacia pubifolia, Brachychiton
  guymeri) before being maintainer-caught 2026-09-03 — despite this section
  already documenting the correct trait name right below. If you catch
  yourself typing `apd_trait="fire_response"`, stop: that string is never
  correct no matter how the raw_trait column reads.
- **A source describing genuinely different fire responses in different
  seasons is two `resprouting_capacity` rows, not one** — common in
  terrestrial Orchidaceae, whose above-ground parts and replacement tuber
  are only actively growing (and therefore vulnerable) for part of the year.
  "Fire between June and early October, when its above ground parts and
  replacement tuber are actively growing" is the species' main threat implies
  `fire_killed` with `context` naming that growing-season window, **and** a
  second row, `resprouts`, with `context` naming the rest of the year
  (dormant season, mature unexposed tuber) — even when the source only
  explicitly states the vulnerable half and the survival half has to be
  inferred (`evidence_level=assumed`) from the same sentence (maintainer
  example, 2026-09-03: Drakaea micrantha). The maintainer may eventually
  define a dedicated trait for this pattern pending expert input — don't
  pre-empt that, just use the two-row `context`-split approach for now.
  **Also score `fire_exposure_level=seasonal_fire_avoidance`** (real,
  released APD trait — other values: `aquatic_taxon`,
  `fire_avoidance_among_rocks`, `fire_avoidance_in_swamp`,
  `fire_avoidance_near_water`) alongside the two-row split above — it's a
  direct, dedicated match for exactly this "vulnerable above ground only
  part of the year, otherwise safely dormant" pattern (maintainer addition,
  2026-09-03, after being initially missed for both Drakaea species). Check
  `plant_tolerance_fire` too when a source describes a *morphological*
  fire-avoidance mechanism (thick bark, fire-retardant tissue chemistry) —
  a different concept again from either of the traits above.
- **`post_fire_recruitment` (binary: `post_fire_recruitment`/
  `post_fire_recruitment_absent`) is the primary trait for "germinates from
  seed following fire" — score it whenever a source states or clearly
  implies post-fire seedling recruitment was observed, not just
  `seed_germination_treatment`** (maintainer correction, 2026-09-03,
  Eucalyptus beardiana — "this is a really important one to score whenever
  applicable"). `seed_germination_treatment` (`heat`/`smoke`/`heat+smoke`/
  ...) is still worth adding alongside it for the specific breaking-cue
  detail when given, but isn't itself the right primary match. When the
  source doesn't specify whether heat, smoke, or both are the actual cue,
  record `heat smoke` as two space-delimited plausible values (the standard
  "source doesn't narrow it down" convention) rather than the compound
  `heat+smoke` value, which asserts both are jointly required.
- **A basal/epicormic/lignotuberous resprouting-location detail is
  `bud_bank_location`, not a second `resprouting_capacity` row.**
  "Re-sprouted from basal shoots" is `bud_bank_location=basal_buds`
  *alongside* the already-scored `resprouting_capacity=resprouts` fact from
  the same source, not a duplicate/competing `resprouting_capacity` row with
  an invented value like `basal_resprouting` (not a real value — corrected
  2026-09-03, Brachychiton guymeri).
- **`plant_tolerance_fire` and `resprouting_capacity` are two different real
  traits — don't reach for the first when you mean the second** (corrected
  2026-09-02, after 8 rows across 8 species were mis-scored this way).
  `plant_tolerance_fire`'s allowed values are about fire-*resistance*
  mechanisms (`fire_retardant`, `fire_retardant_bark`, `thick_bark`) — a
  species with fire-retardant foliage chemistry or protective bark, nothing
  to do with what happens to the plant after fire. Whether the plant is
  **killed by fire and regenerates from seed** vs. **resprouts** is
  `resprouting_capacity` (`fire_killed`/`resprouts`/`partial_resprouting`),
  covered next. A source saying a species is "fire sensitive" or "an
  obligate seeder" or "killed by fire" scores `resprouting_capacity:
  fire_killed` — never `plant_tolerance_fire`.
- **Fire survival is a standing convention**: a source stating a species
  "survives fire" or "survives burning" maps to `resprouting_capacity:
  resprouts`, always — don't treat this as under-specified just because the
  source doesn't spell out the mechanism. Only mark it unmatched if the
  source is silent on fire response entirely.
- **`resprouting_capacity` is specifically *post-fire* resprouting
  capacity** ("fewer than 30% of plants resprout following a fire with 100%
  leaf scorch" is its `fire_killed` definition, for scale) — don't blend it
  with resprouting after a *different* disturbance (browsing, cyclone,
  drought, mechanical damage). Those get `resprouting_capacity_non_fire_disturbance`
  (`resprouts_non_fire_disturbance`/`resprouts_after_cyclone`/
  `resprouts_after_drought`) — an obligate-seeding (fire-killed) species that
  still shows some resprouting after heavy browsing is `fire_killed` on
  `resprouting_capacity` **and** `resprouts_non_fire_disturbance` on
  `resprouting_capacity_non_fire_disturbance`, two rows, never one blended
  `partial_resprouting` guess. Narrower siblings worth checking when a
  source gives that level of detail: `resprouting_capacity_juvenile`,
  `resprouting_capacity_proportion_individuals`,
  `resprouting_capacity_time_from_germination`,
  `resprouting_capacity_stem_ratio`.
- **`seedling_establishment_conditions`** (newer-copy only; categorical:
  `establish_anytime`/`establish_post_disturbance`/`establish_post_fire`/
  `establish_intermediate_to_mature_vegetation`) — the successional
  conditions under which seedlings establish. Distinct from
  `post_fire_recruitment` (binary: was post-fire germination observed at
  all) and worth adding *alongside* it whenever a fire-recruitment sentence
  is precise enough to also support `establish_post_fire`. General
  disturbance language that isn't fire-specific ("responds to disturbance,"
  "early recruit in newly disturbed areas," "pioneer species") maps to
  `establish_post_disturbance` — watch for this especially in "Fluctuating
  populations"/"Disturbance"/habitat-ecology prose, which is easy to skim
  past without extracting a row at all.
- **`flowering_cues` and `post_fire_flowering`** (newer-copy only) — for
  "flowering is promoted/stimulated by fire" language. `flowering_cues`
  (categorical: `fire`/`floods`/`rain`/`rain_obligate`/etc.) is the simple
  presence check; `post_fire_flowering` (categorical:
  `fire_dependent_flowering`/`fire_enhanced_flowering`/
  `fire_independent_flowering`/`fire_suppressed_flowering`) is more precise
  when before/after counts are given (usually `fire_enhanced_flowering`).
  Neither is `post_fire_recruitment` — that's specifically about seed
  germination/seedling establishment, not increased flowering in
  already-established plants.
- **`life_history_ephemeral_class`** (newer-copy only; categorical:
  `disturbance_ephemeral`/`fire_ephemeral`/`fire_ephemeral_obligate`/
  `fire_ephemeral_facultative`/`rain_ephemeral`) — for genuinely short-lived,
  disturbance/fire/rain-triggered species. Not every species with *a*
  disturbance response is ephemeral — check the plant itself germinates,
  completes its life cycle, and dies within roughly a single
  disturbance-to-disturbance interval before using this.
- **Always record the fire season/timing as `context` on fire-cued rows**
  (`post_fire_flowering`, `flowering_cues`, `resprouting_capacity`, etc.)
  whenever the source states or implies it (maintainer directive,
  2026-09-02) — e.g. `context = "summer fire"`, not just a bare categorical
  value. This matches these traits' own documented guidance (fire severity,
  season, and frequency should all be captured as context properties
  wherever available) and matters a lot ecologically: spring/out-of-season
  fire and summer fire have opposite effects on the same species in several
  documented cases in this project.
- **Fire-stimulated/enhanced flowering from a dormant underground organ
  implies `resprouting_capacity: resprouts`, even without a separate
  explicit resprouting statement** (maintainer-confirmed inference rule,
  2026-09-02) — a plant that flowers *because of* a fire necessarily
  survived that fire underground; don't leave `resprouting_capacity`
  unscored just because the source's language is all about flowering, not
  survival. Score both rows from the same fact. Don't over-apply this to a
  case where the source explicitly casts doubt on individual-level survival
  (as opposed to population-level uncertainty, which doesn't contradict the
  inference) — read the actual caveat before applying the rule mechanically.
  When the underlying organ is specifically a tuber (or bulb/corm/stem
  tuber/root tuber/belowground caudex), `bud_bank_location` should be scored
  `fleshy_underground_organ` — that value's own definition explicitly covers
  tubers (listed synonym: "geophyte"); there is no literal `tuber` token in
  `bud_bank_location`'s allowed values, so don't invent one.
- **`resprouting_capacity`'s real `clonal_spread_mechanism` sibling value for
  root-suckering is `root_buds`** (synonym: `root_suckers`), **not**
  `root_suckering` — a plausible-sounding but non-existent value this
  project used for several species before 2026-09-02.

## Other whole-plant traits

- **Salt tolerance has two distinct APD traits** — pick based on what the
  source actually states: `plant_tolerance_salt` (categorical:
  `glycophyte`/`halophyte`/`halophyte_moderate`/`hydrohalophyte`/
  `salt_spray_tolerant`/`xerohalophyte`/`salinity_tolerance_undefined`) for a
  qualitative statement ("not salt tolerant" → `glycophyte`, the standard
  term for a salt-*intolerant* plant); reserve `plant_tolerance_soil_salinity`
  (continuous, dS/m) for when the source gives an actual numeric conductivity
  threshold. Don't force a qualitative statement onto the continuous trait
  just because it sounds like the same topic.

## Ferns (non-flowering species)

- **A fern's fronds are its leaves** — map frond length/width straight onto
  `leaf_length`/`leaf_width` (high confidence, not a stretch reuse); the stipe
  (frond stalk) is the petiole-equivalent structure. Flower/fruit/seed traits
  obviously don't apply at all to ferns; don't force them.
- **`plant_growth_substrate`** (categorical: `aquatic`/`epiphyte`/
  `hemiepiphyte`/`lithophyte`/`marine`/`semiaquatic`/`terrestrial`) — **now
  confirmed in the released `APD_traits.csv` cache (re-checked 2026-08-25)**,
  not just `config/traits.yml`; score a direct match at `high`, not
  `medium`/`new_trait`, and drop the "this project's own trait" framing in
  `notes`. Exactly the right trait for "epiphyte on trees" / "lithophyte on
  rocks" statements, which come up constantly for ferns and orchids — it's
  easy to miss since it's a whole-plant substrate trait, distinct from
  `habitat`.
- `plant_growth_form`'s allowed values include `fern` and `lycophyte`
  directly — use them rather than falling back to `herb`.

## Dioecy / plant sex

- **`sex_type`** (categorical: `dioecious`/`monoecious`/`hermaphrodite`/
  `androdioecious`/`gynodioecious`/and other mixed-system terms) — **now
  confirmed in the released `APD_traits.csv` cache (re-checked 2026-08-25)**,
  not just `config/traits.yml` — score it directly at `high` (not
  `medium`/`new_trait`) whenever a source states a species is
  dioecious/monoecious/etc., rather than treating it as `no_apd_trait`.

## Population & range metrics

`population_count`, `subpopulation_count`, `individual_count` (all numeric,
`{count}`), `extent_of_occurrence`, `area_of_occupancy` (both numeric, km² —
the standard IUCN EOO/AOO metrics; use these exact names) live in this
project's own `config/traits.yml`, not the released APD. A source's "known
from N populations" and "approximately X mature individuals" are two
distinct real traits, not one blended `population_size` prose row — keep
them as separate rows even when the source states them in the same
sentence. Unlike the largely-undocumented habitat/soil vocabulary in
`config/traits.yml` (labels/descriptions still literal `XX` placeholders,
term-matching judgment calls, hold at `medium`), these five are
fully-defined, unambiguous project traits already merged into
`config/traits.yml` — score an unambiguous value match at `high` confidence,
not `medium`. **Do not score these at `new_trait`** — despite living outside
core APD, they are not pending-merge proposals (that's what `new_trait`
means; see the crosswalk-scoring section) — they're already-established
traits this project's pipeline reads today. (Corrected 2026-09-01 — a
maintainer caught 507 rows mis-scored `new_trait` across 140 species files
under an earlier, incorrect version of this guidance; all have been swept
back to `high`.)

**`extent_of_occurrence`/`area_of_occupancy` specifically need consistent,
active extraction on every species (maintainer directive, 2026-09-01) — treat
this as a checklist item, not an incidental catch.** Most conservation
advices state at least one of the two explicitly (often in a "Distribution
and Habitat" or listing-assessment section, sometimes only in an attachment/
appendix with the formal IUCN criteria) — actively look for "extent of
occurrence", "EOO", "area of occupancy", "AOO", or a stated km² figure tied
to either concept, rather than waiting for it to surface incidentally. Always
use the exact raw_trait names `extent_of_occurrence` and `area_of_occupancy`
(never the bare abbreviations `EOO`/`AOO` as the raw_trait label — that's a
recurring alias problem, see below) so rows are findable and comparable
across species without a corpus-wide grep for synonyms. If the source
genuinely states neither figure, that's fine — but don't skip the search
just because a species' profile is short.

**Always fill in `apd_trait` (`extent_of_occurrence`/`area_of_occupancy`,
`apd_trait_type=numeric`, `apd_units=km2`) on these two rows even when the
figure is genuinely absent and `match_confidence=no_match` with `value`
blank** (maintainer directive, 2026-09-02). This is a deliberate exception to
the usual `no_match`/`no_apd_trait` convention of leaving `apd_trait` blank —
because both are real, fully-defined project traits (unlike a genuine
`no_apd_trait` gap where no trait exists to name), keeping `apd_trait`
populated lets a `group_by(apd_trait)` on the combined file directly count
how many species have vs. lack each metric, without having to separately
filter on `raw_trait` for just these two rows. 20 pre-existing rows across
10 species were missing this and have been backfilled.

**This same "keep `apd_trait` populated, use `no_match`" pattern applies to
*any* real trait a source explicitly says is unknown/unavailable** — not
just EOO/AOO. `reproductive_maturity` in particular was found scored
`apd_trait` blank / `match_confidence=no_apd_trait` for a source stating
"the time taken for this species to reach maturity is unknown" (maintainer
correction, 2026-09-03, Eucalyptus beardiana and Thismia clavarioides) —
`reproductive_maturity` **is** a real, released APD trait (numeric, years);
a stated data gap for a real trait's *value* is `no_match`, never
`no_apd_trait` (that verdict is reserved for when no matching trait exists
at all, a different and much rarer situation than "the source didn't say").

- **When a source gives both a per-site/per-population breakdown and an
  overall total, record both — always, not just when it's convenient.** A
  source that says "Population A: 350 individuals, Population B: 2,000
  individuals... approximately 2,500 individuals in total" is reporting two
  distinct, both-worth-keeping facts: several `individual_count` rows, one
  per named population (`context` = the population/site name), **and** a
  separate `individual_count` row for the overall total (`context` = e.g.
  "total across all populations", or naming which census/estimate the total
  came from if the source gives more than one over time). The per-population
  rows let a reader see the actual distribution across sites (which one
  dominates, which are small and vulnerable); the total is what most
  consumers of the combined file will actually want to filter on. Don't
  drop the total because the per-population rows already imply it (the
  source did the summing, not you — recording both is more faithful than
  making a reader re-derive the total, and a source's stated total sometimes
  doesn't exactly equal the simple sum of the parts given, e.g. because of
  unsampled areas), and don't drop the per-population rows because the total
  is more concise. Same pattern applies to `population_count`/
  `subpopulation_count` when a source gives both a count of fine-grained
  units and a count of the broader groupings they fall into — record
  both levels, never only the one that seems more "official".
- **A source's "locality"/"area"/"region" count is still `population_count`
  — map it there, don't leave it unmatched.** A source frequently uses its
  own looser vocabulary ("two general localities", "three disjunct areas")
  for what is, functionally, a population-level breakdown, even when the
  same document separately gives a finer-grained count under the word
  "population" itself. Both are genuine `population_count` facts at
  different granularities — map both to `population_count`, and use
  `context` to say which grouping each row counts (e.g. `context =
  "localities"` for the broader figure, blank or naming the finer unit for
  the other) so a reader can tell them apart without guessing. Don't invent
  a reason to leave the looser figure unmatched (it is not a
  `subpopulation_count`, which is normally a *finer* subdivision than
  population, not a broader one) and don't silently pick only one of the
  two granularities to record.

## Threats

- **`threatened_species_key_threatening_processes`** (project trait, real,
  categorical — values include `fire`/`fire_suppression`/`fire_increase`/
  `fire_regime`/`disease`/`chytrid`/`phytophthora_cinnamomi`/`myrtle_rust`/
  `invasive_species`/`invasive_plant`/`livestock`/`ungulate`/`grazer`/`cats`/
  `cane_toads`/`foxes`/`goats`/`pigs`/`rabbits`/`rats_offshore_islands`/
  `population_small`) went unused for most of this project's history despite
  every species profile having a Threats section — found 2026-09-02.
  **Actively score this for every species from its Threats section**, not
  just narrate the threats in prose/notes elsewhere. Space-delimited
  multi-value when several apply (most species have 2-4). `ungulate` covers
  deer/goats/pigs/cattle/horses/donkeys collectively when a source doesn't
  distinguish; rabbits are deliberately **not** covered by `ungulate` (same
  convention as `plant_response_to_grazing`'s ungulate/native-mammal split
  in `new_traits.yml`) — a rabbit-specific threat has no value here yet,
  leave it unscored rather than forcing it under `ungulate` or `grazer`.
- **`plant_response_to_grazing` needs the source to actually describe the
  *plant itself* being grazed/browsed and responding poorly — not merely
  that stock or feral herbivores are present, could access the site, or have
  degraded the surrounding habitat.** "Stock have degraded the habitat...the
  potential threat exists for 12 populations...which may be accessed by
  stock and feral goats" is habitat-degradation-plus-access-risk, not a
  grazing-intolerance fact — it does not support this trait at all
  (maintainer correction, 2026-09-03, Eucalyptus beardiana); score
  `threatened_species_key_threatening_processes` for the herbivore/habitat
  threat instead (as was already correctly done in the same row-pair here).
  Reserve `plant_response_to_grazing` for language that actually states or
  strongly implies an effect *on the plant* — observed browsing/defoliation/
  death, reduced recruitment attributed to grazing, exclusion-fencing
  experiments showing a difference, etc.

## Habitat, soil & substrate — naming gotchas

- **A rock/granite/sandstone/basalt substrate is `geologic_substrate`
  (`granitic`/`sandstone`/`basaltic`/`limestone`/`metamorphic`/etc.), not
  `soil_type`** — `soil_type`'s vocabulary has no plain `rock` value (it has
  soil-texture/rockiness terms like `skeletal`, `gravel`, `stony`, but not a
  bare "the substrate is rock" token). A source saying a species grows
  "among rocks" or "on granite" is `geologic_substrate`, not a forced
  `soil_type: rock` (used incorrectly by this project before 2026-09-02).
- **Soil colour is a separate real project trait, `soil_colour`**
  (`black`/`brown`/`brown_dark`/`brown_light`/`grey`/`orange`/`red`/`white`/
  `yellow`) — don't fold a stated soil colour ("red sandy soil", "black clay
  soils", "grey silt") into the `soil_type` value string; score both traits
  separately from the same clause.
- **`topographic_position`'s real vocabulary has no `summit`/`crest`/`rise`
  value** — the closest real terms are `hills`/`mountains`/`ridges`/
  `rocky_outcrop`/`slopes` (habitat itself has a separate `mountain_summit`
  value if that's specifically what's needed instead). Note the spelling is
  `rocky_outcrop`, not `rock_outcrop` — both `habitat` and
  `topographic_position` use the `rocky_` form.
- **`associated_species`/`associated_vegetation_community` are documented,
  never scored as real traits — see "Associated species & vegetation
  community" below. This has been corrected/re-explained to this project at
  least 4 times (2026-09-01, then three more times on 2026-09-02) — if you
  find yourself scoring either at `high`/`medium` confidence as if it were
  an established project trait, stop and re-read that section.**

## Site climate (rainfall, temperature)

**Don't propose a project trait for annual rainfall, temperature, or other
site-climate figures a source states — this is a deliberate decision (2026-08-25),
not an oversight.** A conservation advice routinely states something like
"average annual rainfall 350-400 mm" as a habitat descriptor, and it's worth
extracting into the stage-1 raw file for completeness (it's genuine, useful
context) — but score it `no_apd_trait` in the crosswalk and stop there, the
same as water chemistry. These are environmental/climate conditions at a
site, not a property of the plant, and unlike `habitat`/`soil_type`/
`geologic_substrate` (which fill a genuine gap for site descriptors this
project *does* want scored), climate variables are expected to come from a
separate gridded-climate-data workflow rather than being hand-extracted and
scored per species here. Two fire-response-adjacent site metrics
(fire-return-interval requirements, and phyllode/gland micro-morphology
traits) surfaced repeatedly in the same 2026-08-25 batch and are still open
questions, not yet resolved either way — check back here for an update
before deciding how to score them, rather than assuming either resolution.

## Associated species & vegetation community

**Don't propose a project trait for `associated_vegetation_community` or
`associated_species` either — this is also a deliberate decision (maintainer
correction, 2026-09-01), not a gap to fill.** A conservation advice routinely
lists the community a species grows within ("open scrubland with sparse
stunted *Eucalyptus* spp.") or the other species it co-occurs with
("Associated species include *Melaleuca scabra*..."). Keep extracting this
into the stage-1 raw file — it's good background context worth having on
record — but score it `no_apd_trait` in the crosswalk and stop there, the
same as site climate above. This describes the community/site, not a trait
of the species itself, so it should never appear as a candidate when
nominating "the trait most needing addition to `new_traits.yml`" in a batch
check-in, no matter how often it recurs in the `no_apd_trait` tally.

## Project-proposed traits (see `new_traits.yml` for the definitive, current list)

`new_traits.yml` is the single source of truth for every trait this project
has proposed and (at least tentatively) accepted — don't duplicate its
allowed-value lists here (a static list here goes stale the moment
`new_traits.yml` gains another trait or another allowed value, which happens
often); check it directly and re-check it fresh each session, since a
parallel session may have added to it since you last looked.

## Recurring raw_trait aliases — recognize these immediately, don't leave unmapped

A 2026-08-25 corpus-wide audit (grouping every row with a blank `apd_trait`
by `raw_trait` across the whole combined file, see the periodic-audit script
in `SKILL.md`) turned up ~50 rows across ~25 species where the *real* trait
match was missed — not because it didn't exist, but because the extraction
used a different label than the one the real trait is keyed under. Recognize
these aliases on sight rather than waiting for the next audit to catch them:

- **A cluster of invented, never-real trait names found together in two old
  (pre-2026-08) Drosera files during a 2026-09-03 maintainer review** —
  a reminder that `apd_trait` names from early sessions in this project
  cannot be assumed correct just because they read plausibly; verify every
  one, not just the ones that feel uncertain. Aliases found:
  `root_type` → `root_system_type` (allowed value `fibrous_roots` for
  "fibrous roots" - not a bare `fibrous`), `stamen_number` →
  `flower_fertile_stamens_count`, `carpel_number` →
  `flower_structural_carpels_count`, `bract_length` → `flower_bract_length`,
  `petal_length` → `flower_petal_length`, `petal_shape` → `leaf_shape` with
  `context="petal"` (the same legitimate cross-organ vocabulary reuse as
  leaflet shape, not the donor-trait-name-misuse pattern). `leaf_number`
  (count of leaves in a rosette/on a plant) has no matching trait anywhere
  and is a genuine `no_apd_trait` gap, not an alias.
- **`population_size`** → this is always `individual_count` (or occasionally
  needs splitting into separate `individual_count` rows per the guidance
  above) — never leave a raw `population_size` label unmapped or unsplit.
- **`inflorescence_axis_length`** → the same concept as the real APD trait
  `inflorescence_length` (the length of the whole inflorescence axis).
- **`labellum_length` / `labellum_width`** (orchid-specific) → real APD
  traits `flower_petal_length` / `flower_petal_width`, held at `medium`
  since a labellum is a specialised, structurally distinct petal rather than
  a typical undifferentiated one (established convention, first used for
  Caladenia ovata/Sarcochilus hartmannii).
- **`leaf_orientation`** described in terms of "pendulous" / "not pendulous"
  / "hanging" → the real, binary APD trait `leaf_pendulousness`
  (`pendulous`/`not_pendulous`), not `leaf_axil_angle` or
  `leaf_inclination_angle` (both of those are numeric-degree traits that
  need an actual angle figure, which sources essentially never give — check
  for `leaf_pendulousness` first before concluding a qualitative leaf
  direction/angle description is unmatchable). A description of upward
  orientation ("antrorse", "ascending") can sometimes be inferred as
  `not_pendulous` at `medium`/`low` if the source gives no more direct word,
  but don't stretch this inference further than "clearly incompatible with
  hanging downward".
- **`flower_scent` / scent descriptions** ("honey-perfumed", "musky",
  "fragrant") → the real, binary project trait `flower_scent_production`
  (`scent_produced`/`scent_absent`, in the newer `austraits.build` config,
  not yet in the released APD cache) — it only captures presence/absence,
  not the specific scent character, so don't hold back a match just because
  the qualitative description ("musky" vs "sweet") isn't itself captured.
- **`flower_orientation`** described as "erect"/"spreading horizontally"/
  "pendulous"/"reversed" → the real, released APD trait `flower_orientation`
  (`up`/`lateral`/`down`/`mixed`) usually covers erect→`up`,
  horizontal/spreading→`lateral`, pendulous/hanging→`down`. Watch for the
  genuine non-match case though: orchid labellum *resupination* ("borne
  with the lip uppermost, unlike most orchids") is a different concept
  (internal floral-part orientation, not the whole flower's up/lateral/down
  orientation) and doesn't fit any of the four allowed values — `no_match`.
- **`landform`** described as ridges, gullies, slopes, plateaus, boulders,
  rock outcrops/platforms/ledges, cliffs, plains, valleys, watercourses, or
  similar → the real project trait `topographic_position`
  (`config/traits.yml`), not `no_apd_trait`. This one recurred across an
  entire batch of species (2026-08-25) despite most other species correctly
  finding it — a plain search for "landform" as a trait name won't surface
  it, since the project's own name for the concept is `topographic_position`.
  Several compound terms already exist (`slopes_rocky`, `cliffs_coastal`,
  `gullies_sheltered`) — check for the more specific compound value before
  defaulting to the generic one. A description naming more than one facet
  (e.g. "gently undulating slopes and ridges") is a genuine multi-value case
  (`undulating slopes ridges`), same pattern as elsewhere in this file — not
  a reason to pick only one term. `boulders` itself isn't yet an allowed
  value (the closest existing terms, `rocky`/`rocky_outcrop`, don't
  specifically capture it) — propose it (`proposed_new_value`) rather than
  force-fitting or leaving it unmatched.
- **`plant_tolerance_inundation`** — a real, released APD trait, but its
  allowed values are *duration buckets* (`less_than_1_month`/`1-6_months`/
  `greater_than_6_months`/`aquatic`/`not_applicable`), not a qualitative
  tolerant/intolerant scale — don't propose this as a new trait (it nearly
  happened, 2026-08-25) and don't force a qualitative "tolerates flooding"
  statement onto it without a duration; if the source gives no duration,
  that's a genuine `no_match`, not `no_apd_trait`.
- **`fruit_colour`** — a real, released APD trait with a plain colour
  palette (confirm current allowed values before assuming a shade is
  missing) — don't default a fruit-colour description to `no_apd_trait`
  without checking it first, the same way `flower_diameter` was
  under-searched-for previously.
- **`bud_length` / `bud_width`** — real traits in the newer austraits.build
  copy (units: cm), for a *flower bud's* own length/width — distinct from
  `flower_length` (open flowers only). Missed for a whole batch of Eucalyptus
  species (2026-09-03, maintainer-caught) by assuming no bud-dimension trait
  existed at all and filing every eucalypt bud measurement as `no_apd_trait`
  — the mistake this section exists to prevent, and a direct repeat of the
  `flower_diameter`/`fruit_colour` under-search pattern above. Buds are a
  primary Eucalyptus diagnostic character (length, width, shape, cap/operculum
  form all routinely given together) — always check `bud_length`/`bud_width`
  by name before reaching for `no_apd_trait` on a eucalypt bud measurement.
- **`pedicel_length`** (accepted 2026-08-25) — the stalk of a single flower
  within a multi-flowered inflorescence, explicitly distinct from
  `peduncle_length` (the stalk of the whole inflorescence; that trait's own
  definition excludes pedicels). A pedicel length of 0 mm records a sessile
  flower. Don't force a pedicel measurement onto `peduncle_length` just
  because it's already an accepted trait — score `pedicel_length` instead.
- **`EOO`/`AOO`** as raw_trait abbreviations, or a raw_trait already named
  `extent_of_occurrence`/`area_of_occupancy` — these sometimes show up with
  an empty `apd_trait` in older files (written before this project trait was
  consistently applied). If you see the real figure sitting in `raw_value`
  with a blank `apd_trait`, that's a missed mapping, not a genuine absence —
  fix it in place rather than treating the row as already-handled.
- **`inflorescence_type` scoring guidance lives earlier in this file**
  (search for `inflorescence_type` — it's the entry right after the
  `flower_diameter` bullet): all ten values score `medium`, confirmed
  against the newer austraits.build copy of `config/traits.yml`. Don't
  reintroduce a `spike`/`panicle`-specific `high` exception here — that was
  an earlier, since-corrected scoring.
- **A "glaucous"/"not glaucous" description is `leaf_glaucousness`
  (categorical: `glaucous`/`not_glaucous`), NOT `leaf_wax`** (a numeric mg/mg
  wax-content-by-mass measurement trait, a completely different concept -
  confused once this session, 2026-09-02, for Eucalyptus boliviana and
  Lordhowea pilosicrista, both retroactively corrected). `leaf_glaucousness`
  is a real, already-well-used APD trait in this project's own history
  (already scored high/medium confidence for many prior species) - check it
  first for any "glaucous"/"waxy bloom"/"pruinose"/greyish-or-silvery-leaf
  description before reaching for `leaf_wax`.
- **Non-vascular-plant species (marine algae, and presumably lichens/fungi
  if they ever appear) need a fundamentally more conservative extraction**
  (first hit 2026-09-02, *Nereia lophocladia*, a Fisheries Scientific
  Committee CAM assessment for a brown alga). All three trait dictionaries
  are scoped to vascular land plants — leaf/flower/fruit/seed morphology,
  fire response, pollination, `habitat`'s terrestrial vocabulary, and
  `geologic_substrate`'s soil-forming-rock vocabulary are categorical scope
  mismatches for algae, not merely unmatched values, and should be
  documented in `raw_value` only (`no_apd_trait`) rather than force-mapped
  to the nearest-sounding vascular-plant trait (e.g. don't score thallus
  length under `plant_height` - that trait's own definition is specifically
  about a vegetative shoot system). What *does* transfer cleanly: the
  taxon-agnostic IUCN-style population parameters
  (`individual_count`/`subpopulation_count`/`extent_of_occurrence`/
  `area_of_occupancy`/`generation_time`) and `threatened_species_key_threatening_processes`'
  generic categories that happen to apply (e.g. `population_small` for a
  genetically-bottlenecked, wildly-fluctuating population) - score those at
  their usual confidence levels. A native species undergoing a
  climate-driven range expansion into the habitat (e.g. the sea urchin
  *Centrostephanus rodgersii* grazing algal beds) is not `invasive_species`
  (that value is for introduced organisms) - document as `no_apd_trait`
  instead of stretching the value to fit.
- **`soil_type`, `topographic_position` and `geologic_substrate` (this
  project's own habitat traits, config/traits.yml) are strict controlled
  vocabularies with specific tokens - always check `allowed_values_levels`
  before inventing a value.** Found 2026-09-03 (Eucalyptus hallii,
  langleyi, parramattensis subsp. decadens): scored made-up
  prefixed-compound values (`soil_sandy`, `soil_skeletal`) that don't
  exist - the real tokens are bare (`sand`, `skeletal`, `loam_sandy`,
  `loam_gravelly`, `gravel`, `clay`, `silt`, etc; some are literal
  multi-word compounds like `soil_shallow soil_rocky` - these are single
  values, not two space-delimited ones). Similarly `topographic_position`
  uses `flats`/`hills`/`mountains`/`slopes`/`slopes_steep`/`plateaus`/
  `undulating`/`streams`/`watercourses` (not `flat`, not `plateau`
  singular). `geologic_substrate` has no bare `sedimentary` value (only
  specific rock types: `sandstone`/`mudstone`/`shale`/`limestone`/etc) -
  when a source only says "sedimentary rock" without specifying which,
  score `proposed_new_value` with `sedimentary`, matching the established
  Endiandra hayesii precedent, not `medium` confidence against a
  near-enough-sounding real value. All 5 wrong values found this session
  were corrected in both the combined table and the per-species files.
- **`leaf_glaucousness` (real APD trait) is the match for "glaucous" /
  "waxy bloom" / "pruinose" / "blue-grey" / "grey-blue" leaf colour
  descriptions** - but its definition is explicitly scoped to *mature,
  fully-expanded leaves on adult plants only*. A source describing
  glaucous *juvenile* leaves, stems, buds, or fruit does not fit this
  trait (no sibling trait exists for those structures) - documented in
  `raw_value` only, `no_apd_trait`. (Eucalyptus pulverulenta had all four:
  glaucous juvenile leaves, young stems, buds and fruit, but only the
  glaucous *adult* leaves scored under `leaf_glaucousness`.)
- **`leaf_phyllotaxis` (not `leaf_arrangement`) is the real APD trait for
  "opposite"/"alternate" leaf attachment at a single stem node.**
  `leaf_arrangement`'s own definition explicitly defers this to
  `leaf_phyllotaxis` and instead covers multi-node patterns (decussate,
  distichous, spiral, rosette, clustered, etc). Found 2026-09-03
  (Eucalyptus pulverulenta, rhodantha).
- **No altitude/elevation trait exists anywhere** (real APD, this
  project's config/traits.yml, or new_traits.yml) despite elevation
  ranges being routinely reported in these conservation advice documents
  (first flagged 2026-09-03, Eucalyptus pulverulenta; recurred at least
  7 more times the same day across Hicksbeachia pinnatifolia, Homoranthus
  decumbens/montanus, Huperzia lockyeri/marsupiiformis/prolifera,
  Hydriastele costata). **Maintainer decision 2026-09-03: keep recording
  it every time it's given (same as `family`/`associated_species`), but
  it is not itself a trait needing a `new_traits.yml` addition and must
  not be nominated as a batch's top candidate, however often it recurs**
  - see SKILL.md's check-in exclusion list.
  Score `apd_trait=altitude`, `match_confidence=proposed_new_trait`,
  `apd_units=m`, value populated - per SKILL.md's "propose a best-guess
  value/trait name, don't just flag the gap" instruction - rather than
  leaving it `no_apd_trait`.
- **`seed_colour`'s `allowed_values_levels` descriptions in
  austraits.build/config/traits.yml appear internally scrambled/corrupted
  relative to their own key names** (e.g. the `black` key's description
  reads "Seed coat is white.", `red_brown` reads "...is yellow.") - found
  2026-09-03, Eucalyptus rhodantha. Trust the key name itself (it reads
  as a sensible colour vocabulary: black/blue_purple/green/grey/pink/
  red_brown/white_cream/yellow_orange), not the paired description text,
  until this is confirmed fixed upstream; note the discrepancy in `notes`
  when scoring this trait.
- **`bark_transition_height`** (new project-pending trait, maintainer-
  specified 2026-09-03; see new_traits.yml) - for trees, particularly
  eucalypts, with a "half-bark" pattern (rough persistent bark low on the
  trunk, smooth shedding bark above), the height above ground where the
  bark changes from rough to smooth. Numeric, units m. Only score this
  when the source states an actual height for the transition - a plain
  two-zone rough-trunk/smooth-branches description with no height given
  (e.g. Eucalyptus raveretiana, this project, 2026-09-03) still only
  supports `bark_texture`/`bark_morphology_eucalyptus` scored separately
  by context ("trunk and largest branches" vs "branches"), not this
  trait. Name is provisional - maintainer may rename later.
- **The "already done?" check against the combined table can miss a
  species filed under a taxonomic synonym.** Found 2026-09-03: about to
  extract "Euphorbia carissoides" (the current name, per
  `threatened_flora.csv`) as new, when it was already fully done (35
  rows, high quality) from an earlier session - just filed under its
  older synonym "Chamaesyce carissoides" (the name the source PDF itself
  uses in its header). A simple lowercase taxon_name match against
  `threatened_flora.csv`'s current name won't catch this. Caught here
  only because the PDF's own header said "Chamaesyce carissoides" and
  that name rang a bell in `new_traits.yml`'s `fruit_shape` used_in list.
  **Before starting a species whose PDF header uses an unfamiliar or
  older-looking genus/epithet, grep the combined table for that name too,
  not just the `threatened_flora.csv` name** - a mismatch between the
  master list's current name and the source document's name is the
  tell. No corpus-wide sweep has been done to check for other
  already-done synonym duplicates; flagging as a possible gap, not
  something to chase down unprompted.
- **Some `threatened_flora.csv` rows marked "Profile Available: Yes" have
  no PDF in the local `approved_conservation_advice` folder** (verified
  against the complete 1221-file first-page index, and also checked the
  sibling `ConservationAdvice_2024/2025/2026` folders - not there
  either). Found 2026-09-03: Euphorbia obliqua, Euphrasia amphisysepala,
  Euphrasia phragmostoma all skipped for this reason and replaced with
  the next available species in the alphabetical sweep (Fontainea
  australis, Gastrolobium lehmannii, Gastrolobium modestum respectively)
  to keep the batch at 10. These three remain outstanding - their PDFs
  need to be sourced (from SPRAT online or elsewhere) and added to the
  corpus before they can be extracted; not something to fetch from the
  web unprompted mid-sweep.
- **Root-suckering / rhizome-clonal species routinely carry an explicit
  counting-reliability caveat** that's worth preserving whenever present:
  a source will give a population estimate but flag that connected
  ramets/suckers were often counted as one individual rather than
  separately, making the true count likely higher than stated (seen
  2026-09-03 for Grevillea celata, Grevillea infecunda, and Grevillea
  kennedyana - the last via rhizome "clumps" rather than root suckers).
  Score the number as given but always carry the caveat into `notes`.
- **`plant_height_type=na_geophyte` + `plant_height_reproductive`
  applies cleanly beyond eucalypt-adjacent orchids** - confirmed again
  2026-09-03 for Genoplesium vernale (a terrestrial tuberous orchid with
  one leaf and a separate flowering stem), reinforcing the convention
  already noted for Diuris/Drakaea-type geophytes.
- **A species can have no known viable seed production at all - this is
  exactly the case `vegetative_reproduction_ability=vegetative_obligate`
  exists for, don't just leave it as a raw-only curiosity.** Grevillea
  infecunda (2026-09-03, corrected after maintainer review) produces no
  confirmed viable seed and reproduces only by root-suckering. Score all
  of: `vegetative_reproduction_ability=vegetative_obligate` (real trait,
  matches "never or very rarely regenerates from seed" exactly),
  `clonal_spread_mechanism=root_buds` ("root-suckering" is a listed
  synonym), and `seed_viability=0` (real trait, a proportion 0-1 - "no
  viable seed appears to have been produced" is a stated 0, not merely
  an unmatched observation). `dispersal_syndrome` still stays
  `no_match`/`unknown` for the species itself - `atelochory` describes
  seed whose dispersal is prevented (implying seed exists), which is a
  weaker condition than never producing viable seed at all.
- **`leaf_lamina_division` has precise pinnate-division terms worth
  checking for** beyond the broad lobed/compound terms first noticed:
  `pinnatifid` (lobes <halfway to midrib), `pinnatipartite` (>halfway),
  `pinnatisect` (nearly to midrib), and their `bipinnatifid` /
  `bipinnatipartite` / `bipinnatisect` doubled forms - a source giving
  "pinnatisect to pinnatifid" or "bipinnatisect" should be scored with
  these exact terms, not the coarser `pinnately_lobed`/`pinnately_compound`.
- **`sex_type` (not "sexual_system") is the real APD trait for
  dioecious/monoecious/hermaphrodite etc.** - found 2026-09-03 (Fontainea
  australis).
- **Two genuinely opposite fire-interval threats can both be evidenced
  for the same species** - too-frequent fire depleting the soil seedbank
  before it replenishes (`fire_increase`) AND too-infrequent fire failing
  to trigger the germination cue at all while unburnt mature plants
  senesce (`fire_suppression`). Score both when the source gives concrete
  evidence for each side, rather than picking one (seen 2026-09-03 for
  Grevillea floripendula and Grevillea raybrownii).
- **Proposing a new `geologic_substrate` value beyond the "sedimentary"
  gap**: found 2026-09-03 that "silcrete" (a specific, well-known
  Australian substrate term) has no allowed value either (closest is the
  more generic "siliceous") - proposed as `proposed_new_value` for
  Grevillea kennedyana. Also confirmed `plant_width` (real APD trait,
  units m) as the correct trait for a stated plant spread/breadth
  dimension, sibling to `plant_height`.
- **`threatened_species_key_threatening_processes` has no allowed value
  for illegal collection/harvesting** (of seed, cuttings, whole plants,
  or spores, for the horticultural trade or by plant enthusiasts) -
  this is one of the single most frequently recurring `no_apd_trait`
  gaps in the whole corpus so far, having come up for Hicksbeachia
  pinnatifolia, Homoranthus decumbens, Homoranthus montanus (as
  "removal of specimens"), and all three Huperzia species processed
  2026-09-03 alone. Score it `no_apd_trait` with a note (`"no matching
  \"collection\"/harvesting key threatening process value..."`) and
  flag it as the top new_traits.yml candidate at the next check-in
  until a maintainer decision is made - a plausible new value name is
  `collection` or `illegal_collection`.
- **Lycophytes ("fern allies" - Huperzia/Phlegmariurus) need a distinct
  extraction approach**: no flower/fruit/seed traits apply (they
  reproduce via spores, not seeds); `plant_growth_form=fern` is the
  closest available value (its own definition covers spore-reproducing,
  seedless/flowerless vascular plants generally, not just true ferns);
  `plant_growth_substrate=epiphyte`/`lithophyte` covers the growth
  substrate (a separate real trait from plant_growth_form, easy to
  miss); `leaf_axil_angle` (real trait, deg) fits a stated leaf-to-stem
  angle exactly; `leaf_cross_section_shape=keeled` is already an
  existing allowed value for this project's pending trait, not a new
  proposal, and comes up often for these strongly keeled leaves;
  `leaf_phyllotaxis` covers both `whorled` and `spiral` arrangements.
  Sporophylls (spore-bearing leaves) are morphologically distinct from
  vegetative leaves and were scored under the same APD traits
  (`leaf_length`, `leaf_margin`, `leaf_cross_section_shape`) with
  `context`/`context_type` marking them as the sporophyll, not the
  vegetative leaf - there is no separate sporophyll-specific trait.
  The sterile-to-fertile-zone transition and the spore-body-to-leaf
  length ratio have no matching traits anywhere. Found 2026-09-03,
  Huperzia carinata/dalhousieana/filiformis.
- **`Jacksonia` (Fabaceae) is aphyllous - "leaves" described in these
  sources are actually phyllodes** (flattened, leaf-like petioles/stems
  that take over photosynthesis) or the ribbed/angular branches
  themselves acting as cladodes. No dedicated phyllode/cladode trait
  exists in APD or this project's config - resist the temptation to
  force phyllode descriptions (apex shape, colour) onto ordinary
  leaf_apex_shape/leaf_surface_colour traits, since those traits'
  definitions are specifically about true leaf lamina. `stem_cross_section_shape`
  (this project's pending trait) DOES apply correctly here, since it is
  explicitly stem-scoped - e.g. "angular, prominently ribbed" branches
  scored as `angular`. Pea-flower (Faboideae) colour is often described
  per-petal-type (standard/wings/keel), each a different colour -
  score `flower_colour` three times with `context`/`context_type`
  identifying which petal, rather than picking just one. Found
  2026-09-03, Jacksonia sp. Collie (C.J.Koch 177).
- **Maintainer directive 2026-09-03: be more thorough on floral
  morphology, not less** - the project is actively building out this
  part of the dictionary, so a source's floral description deserves the
  same row-by-row treatment as leaves/fire ecology, not a quick pass.
  Caught missing `flower_shape=tubular` for Homoranthus montanus's "1-6
  small tubular flowers" despite `flower_shape` being a REAL APD trait
  (allowed values: `bell-shaped`, `tubular` - only two values, so
  narrow applicability, but check every flower description against it).
  Also check for `flower_style_differentiation` (categorical: absent/
  continuous/present_long_and_narrow/present_long_and_wide/
  present_petaloid/present_short_and_narrow/present_short_and_thick/
  present_length_and_shape_unknown - style presence and shape *relative
  to ovary size*, not to sepals), `flower_stigma_length` (numeric, mm),
  and `flower_style_fusion` (numeric 0-1, proportion of style length
  fused) whenever style/stigma detail is given. When a described detail
  (floral tube presence/absence, a specific colour pattern, a stigma
  shape) has no matching trait at all even after checking, still
  document it in `raw_value`/`notes` as `no_apd_trait` rather than
  skipping the row outright - per SKILL.md's standing instruction, and
  per this correction.
- **A sparse allowed-values list on a REAL (core APD) trait is not a
  ceiling - propose a new value rather than force-fitting a poor match,
  the same as for this project's own pending traits.** Maintainer
  correction 2026-09-03: `flower_shape` currently has only two defined
  values (`bell-shaped`, `tubular`) because that part of APD is still
  being actively built out, not because those are the only flower
  shapes that exist. Initially scored Lambertia echinata's "trumpet-
  shaped flowers" as `tubular` at `medium` confidence reasoning it was
  "closest of only two options" - wrong instinct. Fixed to
  `proposed_new_value=funnelform` (a tube gradually flaring to a wide
  mouth, meaningfully distinct from tubular's uniform width). Apply
  this same openness to any other core trait that turns out to have a
  thin value list for the structure being described - the trait being
  "real APD" doesn't mean its value list is exhaustive.
- **`stem_hairs` and `leaf_hairs_adult_leaves` are both binary
  (glabrous/hairy only) - propose `hairs_branched` (stellate/branched
  hairs) as a new value when a source specifies "star-shaped"/stellate
  hairs**, rather than collapsing to the generic `hairy`. APD already
  uses exactly this vocabulary for `seed_surface_hairs`
  (`hairs_branched`, synonym stellate/plumose, vs `hairs_simple`,
  synonym tomentose/velutinous/sericeous) - reuse those same value
  names for consistency when proposing the same distinction on other
  hair traits. Stellate hairs are common and often diagnostic in
  Malvaceae/Sterculiaceae (e.g. Lasiopetalum) and several other
  families - expect this to recur. Found 2026-09-03, Lasiopetalum
  longistamineum. Do NOT, however, force a floral/bracteole hair
  observation onto `leaf_hairs_adult_leaves` just to attach a proposed
  value to *something* - that trait is leaf-specific by definition, and
  scoring a different structure against it is a structural mismatch,
  not a shaky-but-defensible value match. When no trait exists for the
  structure at all (e.g. bracteole/flower hairs specifically), document
  in `raw_value`/`notes` as `no_apd_trait` with no `apd_trait` assigned,
  same as any other true gap.
- **More `flower_shape` proposals**: `turbinate` (top-shaped/inverted
  cone - already used as a cross-reference inside fruit_shape's own
  "conical" definition, so reuse the term for consistency) for a
  "top-shaped" calyx (Lawrencia buchananensis). Keep proposing distinct
  values here rather than collapsing everything to bell-shaped/tubular
  - this trait is one of the sparsest in APD relative to how often
  flower shape is actually described in these sources.
- **`fruit_type` DOES already include `schizocarp`** ("fruits derived
  from a compound pistil that separate into individual carpellary
  constituents at maturity... includes mericarps, nutlets, fruitlets")
  - don't miss it in a quick scan of the fruit_type list (it's easy to
  stop scanning after nut/nutlet/pyrene and miss schizocarp further
  down). Matches Malvaceae-style fruits that split into several
  segments at maturity (Lawrencia buchananensis, "breaking apart into
  five segments when ripe") - distinct from `mericarp`, which is the
  individual segment, not the whole pre-split fruit.
- **No `flower_petal_fusion`/`flower_sepal_fusion` proportion trait
  exists** (unlike the style, which has `flower_style_fusion`, 0-1
  proportion) - a stated fusion fraction for sepals/petals ("fused for
  up to three quarters of their length") has no matching trait;
  document in `raw_value` only. Found 2026-09-03, Lawrencia
  buchananensis.
- **Goodeniaceae corolla lobes routinely have "wings"** (flattened,
  membranous flanges along the lobe margins) - no matching trait exists
  in APD for this. Recurred twice same day (Lechenaultia chlorantha,
  Leschenaultia laricina), alongside the pollen presenter gap already
  noted above - both genus-wide, taxonomically standard structures.
  Keep documenting in `raw_value` only for now; if it keeps recurring
  across more Goodeniaceae genera (Scaevola, Dampiera, Goodenia,
  Velleia), it may be worth nominating as a genuinely new trait (not
  just a new value) at a future check-in, rather than a value on an
  existing trait.
