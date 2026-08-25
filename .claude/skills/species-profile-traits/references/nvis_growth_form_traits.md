# NVIS-branch growth-form traits (not yet in the released APD export)

The cached `data_extras/APD_reference/APD_traits.csv` /
`APD_categorical_values.csv` files reflect the released AusTraits Plant
Dictionary. A handful of whole-plant architecture traits exist only on the
`NVIS_2026_1` branch of `github.com/traitecoevo/austraits.build`
(`config/traits.yml`) — drafted but not yet merged. They're real, well-defined
candidates and worth using; just note in `notes` that the trait is
"NVIS-branch, not yet in the released main APD" rather than treating it as an
ordinary high-confidence match to a citable release.

**Why this matters**: a source's plain-language "habit" description (e.g.
"dense to open; domed; erect or spreading") routinely bundles several
*distinct* architecture concepts that used to have nowhere to go except a
vague `no_apd_trait`. These four traits give each concept its own home — when
a habit description has multiple independent clauses like this, split it into
multiple stage-1 rows (same pattern as a min/max range split) rather than
forcing it into one row or leaving it unmatched. `stem_growth_habit` (already
in the released APD — position/3D extent, e.g. `erect`, `spreading`, `dense`,
`open`) often overlaps with `stem_branching_form` below; pick whichever is the
more specific fit for the actual clause and say so.

## stem_branching_form
Number, position, and branching density/geometry of a plant's stems.
Distinct from `stem_growth_habit` (position + 3D extent of the whole shoot
system) and `stem_constitution` (rigidity/texture, below).

Allowed values: `unbranched`, `branched`, `few-stemmed`, `multi-stemmed`,
`many-stemmed`, `single_basal_stem`, `multiple_basal_stems`,
`multiple_near_basal_stems`, `much-branched`, `openly-branched`,
`sparsely-branched`, `moderately-branched`, `compactly-branched`,
`densely-branched`, `intricately-branched`, `monopodial`,
`horizontally-branched`, `tangled`, `gnarled`, `divaricately-branched`,
`dichasially-branched`, `virgate`.

## plant_canopy_form
The silhouette/outline of an individual plant's crown — shape and density,
not the broad growth-form category (that's `plant_growth_form`) or stem
architecture (`stem_growth_habit`).

Allowed values: `broom-like`, `columnar`, `dense-crowned`, `diffuse`,
`flat-topped`, `globose`, `hemispherical` (synonym: **domed**), `obconical`,
`open-crowned`, `mounded`, `pine-like`, `pyramidal`, `rounded`, `spreading`,
`squat`.

## stem_base_form
Shape of the base of a plant's main stem/trunk. Allowed values: `buttressed`,
`fluted`.

## stem_constitution
Mechanical character of stems — rigidity, sturdiness, flexibility. Distinct
from branching pattern (`stem_branching_form`) or 3D arrangement
(`stem_growth_habit`).

Allowed values: `rigid`, `robust`, `wiry`, `slender`, `straggly`, `flexible`,
`wispy`, `spindly`.

---
Fetched from `https://raw.githubusercontent.com/traitecoevo/austraits.build/NVIS_2026_1/config/traits.yml` on 2026-08-24. Re-check that branch if these traits still aren't showing up in a refreshed `APD_traits.csv` cache and the mapping matters for a review.
