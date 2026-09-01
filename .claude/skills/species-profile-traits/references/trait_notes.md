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
  traits: `leaflet_length`/`leaflet_width` (continuous, cm) and
  `leaflet_count` (numeric, `{count}`).** Don't force leaflet length/width
  onto `leaf_length`/`leaf_width` (those are whole-leaf traits) — this
  happened once in this project and was corrected. **Leaflet *shape*, by
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
  length is a spatial dimension, not a duration) — left as `no_apd_trait`/
  unformalized for now, not yet a project trait.
- **Pollination — three distinct traits, pick by what kind of evidence the
  sentence reports, not just whether a pollinator taxon is named:**
  - `pollination_syndrome` — a *flower-morphology-based inference* about
    what probably pollinates a flower shaped a certain way.
  - `pollination_vector_possible` (newer-copy only; values incl. `wasp`/
    `bee`/`bird`/`ant`/`autonomous`/etc.) — an *actual or likely floral
    visitor* a source names (observed, or "potential pollinators recorded
    interacting with"). If a source names a wasp genus, "beetles directly
    observed," "at least 12 genera of bee recorded interacting with," etc.,
    that's vector-possible data, not syndrome data, even when the wording
    sounds similar.
  - `pollination_vector_known` — direct pollen-transfer/seed-set evidence.
    Rarer in these profiles, but keep it in mind for exclusion-experiment or
    explicit-observation language.
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
  `yellow`) are project new_traits (accepted 2026-08-25, see `new_traits.yml`)
  for **non-Eucalyptus** bark description — `bark_morphology_eucalyptus` (the
  real APD trait) is explicitly restricted to Eucalyptus/Corymbia/Angophora's
  named bark types (stringybark/box/gum/ribbonbark/ironbark/peppermint/
  stocking) and shouldn't be force-applied elsewhere, even when a term like
  "smooth" would technically fit one of its values. A combined description
  ("furrowed, dark grey bark") is a texture clause plus a colour clause —
  split into two raw rows, one per trait, rather than picking just one.
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
