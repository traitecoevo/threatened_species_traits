# ============================================================================
# Trait/habitat gap-filling priority model
# ============================================================================
#
# Answers one question: of everything still missing from the trait and
# habitat layers behind the threat-susceptibility model, which gaps - if
# filled - would move the most calculated risk scores?
#
#   impact(trait) = (exposure-weighted % of threatened species missing it)
#                   x  sum(|effect_score|) across every threat that uses it
#
# - effect size: the expert-elicited |effect_score| for the trait, summed
#   across every threat_effects tribble that includes it - so a trait used
#   in six threats scores ~6x higher than an identical one used in one.
#   Breadth is folded directly into this term, not treated separately.
# - exposure-weighted missingness: % of the ~1400 currently-threatened
#   species (the population the model actually scores, not the full
#   ~30,000-taxon native flora) that lack the trait, weighted by each
#   species' habitat_risk - so a gap concentrated in taxa that already look
#   exposed counts more than the same gap spread across taxa that habitat
#   alone already rules low-risk.
#
# Deliberately excluded:
# - extent_of_occurrence/area_of_occupancy (precariousness's only inputs) -
#   complete layers are already incoming, so gap-filling effort there is moot.
# - taxonomy (family/genus-level overrides) - family and genus are known for
#   essentially every species, so there's no missing-DATA question there,
#   only a curated-list-coverage one (a literature/expert-elicitation task,
#   not a data-sourcing one) - out of scope for this model.
#
# Run this AFTER rendering threatened_script_20250514.qmd (it reads the
# exported CSVs in export/, not the live data), e.g.:
#   Rscript scripts/trait_gap_priority_model.R
#
# Each run is saved as a dated snapshot in export/gap_priority_model/, and
# automatically compared against the most recent previous snapshot so you
# can see exactly how much progress each new round of data collection made.
#
# ---- keeping this in sync with the qmd ----
# `trait_tables` below is a hand-maintained copy of every *_trait_effects
# tribble in threatened_script_20250514.qmd. There's no way to avoid this
# duplication short of sourcing the qmd itself (heavyweight - it rebuilds
# the whole database), so whenever a trait or effect_score changes in one of
# those tribbles, update the matching entry below to match. Search the qmd
# for "_trait_effects <- tribble(" to find all of them at once.
# ============================================================================

suppressMessages(library(tidyverse))

# ---- config ---------------------------------------------------------------

export_dir <- "~/GitHub/threatened_species_traits/export"
snapshot_dir <- file.path(export_dir, "gap_priority_model")
habitat_scores_path <- "~/GitHub/threatened_species_traits/data_extras/habitat_scores.csv"

threat_csv_files <- c(
  phytophthora        = "phytophthora_assigned_risk_scores.csv",
  myrtle_rust         = "myrtle_rust_risk_scores.csv",
  goats                = "goats_risk_scores.csv",
  deer                 = "deer_risk_scores.csv",
  pigs                 = "pigs_risk_scores.csv",
  c4_grasses           = "tropical_C4_grasses_risk_scores.csv",
  hymenachne           = "hymenachne_risk_scores.csv",
  threat_independent   = "threat_independent_risk_scores.csv"
)

# habitat_risk column used as the exposure weight, per threat (~97% complete,
# computed upstream of the trait gaps being audited, so using it as a weight
# isn't circular). threat_independent has no habitat term, so it's weighted
# equally (every species counts once) rather than by exposure.
habitat_risk_col <- c(
  phytophthora = "habitat_risk", myrtle_rust = "habitat_risk", goats = "habitat_risk",
  deer = "habitat_risk", pigs = "habitat_risk", c4_grasses = "habitat_risk",
  hymenachne = "habitat_risk"
)

# the habitat_scores.csv column + the exported habitat-effect-score column
# name, per threat - used only to report habitat's own gap for context
habitat_score_col   <- c(phytophthora = "phytophthora", myrtle_rust = "myrtle_rust", goats = "goats",
                          deer = "deer", pigs = "pigs", c4_grasses = "C4_grasses", hymenachne = "hymenachne")
habitat_effect_col  <- c(phytophthora = "phytophthora_habitat_effect_score", myrtle_rust = "myrtle_rust_habitat_effect_score",
                          goats = "goat_habitat_effect_score", deer = "deer_habitat_effect_score",
                          pigs = "pig_habitat_effect_score", c4_grasses = "C4_grasses_habitat_effect_score",
                          hymenachne = "hymenachne_habitat_effect_score")

# ---- trait tables (hand-synced copy of the qmd's *_trait_effects tribbles) ----
# traits with effect_score = NA in the qmd (handled manually elsewhere, e.g.
# folded into another trait's scoring) are omitted here - they don't have
# their own effect size to weight by. dry_season_flowering is the one
# exception: it's NA in the tribble but genuinely scored (with two different
# hardcoded coefficients) elsewhere in the C4 grasses calc, so it's added
# back in via `extra_direct_traits` below.

trait_tables <- list(
  phytophthora = tribble(
    ~trait,                          ~effect_score,
    "woodiness",                      0.3,
    "resprouting_capacity",          -0.25,
    "storage_organ_fleshy",          -0.2,
    "leaf_P_per_dry_mass_scaled",    -0.15,
    "leaf_N_per_dry_mass_scaled",    -0.15,
    "reproductive_maturity_scaled",  -0.3
  ),
  myrtle_rust = tribble(
    ~trait,                              ~effect_score,
    "leaf_length_scaled",                -0.2,
    "plant_height_scaled",               -0.4,
    "resprouting_capacity",              -0.3,
    "bud_bank_location_scaled",          -0.3,
    "leaf_lignin_per_dry_mass_scaled",   -0.2
  ),
  goats = tribble(
    ~trait,                          ~effect_score,
    "woodiness",                      0.3,
    "life_history",                   0.4,
    "forb",                          -0.15,
    "resprouting_capacity",          -0.25,
    "plant_height_4m_peak_scaled",   -0.1,
    "bud_bank_location_scaled",      -0.2,
    "bark_thickness_scaled",         -0.2,
    "reproductive_maturity_scaled",  -0.3
  ),
  deer = tribble(
    ~trait,                                ~effect_score,
    "woodiness",                           -0.1,
    "life_history",                        -0.2,
    "plant_physical_defence_structures",   -0.2,
    "resprouting_capacity",                -0.2,
    "plant_height_4m_peak_scaled",         -0.1,
    "leaf_mass_per_area_scaled",           -0.2,
    "bark_thickness_scaled",               -0.1
    # reproductive_maturity_scaled deliberately excluded - not enough data,
    # commented out of deer_trait_effects in the qmd
  ),
  pigs = tribble(
    ~trait,                    ~effect_score,
    "woodiness",               -0.4,
    "resprouting_capacity",    -0.2,
    "plant_height_scaled",     -0.2,
    "fruit_length_scaled",      0.25,
    "storage_organ_fleshy",     0.4
    # fruit_fleshiness excluded - NA effect_score, folded into fruit_length_scaled
  ),
  c4_grasses = tribble(
    ~trait,                          ~effect_score,
    "woodiness",                      0.3,
    "life_history",                   0.2,
    "plant_height_scaled",           -0.1,
    "resprouting_capacity",          -0.2,
    "storage_organ_present",         -0.3,
    "bud_bank_location_scaled",      -0.1,
    "reproductive_maturity_scaled",  -0.3
    # dry_season_flowering excluded here - NA effect_score in the tribble;
    # added back via extra_direct_traits below
  ),
  hymenachne = tribble(
    ~trait,                    ~effect_score,
    "woodiness",               -0.4,
    "life_history",            -0.3,
    "plant_height_scaled",     -0.3,
    "pollination_syndrome",    -0.3
    # plant_growth_substrate_hymenanche excluded - it's a habitat signal
    # (aquatic vs not), not a trait, scored directly into habitat_risk
  ),
  threat_independent = tribble(
    ~trait,                  ~effect_score,
    "life_history",           0.5,
    "woodiness",               0.5,
    "herb_shrub_tree",         0.5,
    "plant_height_scaled",     0.5
  )
)

# traits scored directly in the calc chunk rather than through a
# trait_effects tribble - see the tribble comment for why each is excluded
# above. Each row is one distinct use with its own coefficient.
extra_direct_traits <- tribble(
  ~threat,       ~trait,                   ~effect_score,
  "c4_grasses",  "dry_season_flowering",   -0.3,   # in direct_competition_risk
  "c4_grasses",  "dry_season_flowering",   -0.1    # in fire_season_risk
)

# ---- functions --------------------------------------------------------------

#' Load every threat's exported risk-score CSV into a named list
load_threat_csvs <- function(dir = export_dir) {
  map(threat_csv_files, ~ suppressWarnings(read_csv(file.path(dir, .x), show_col_types = FALSE)))
}

#' Exposure-weighted (or flat, if no habitat term) fraction of a threat's
#' scored species missing `trait`. Missing habitat weights (~3% of rows) are
#' imputed at the median exposure rather than dropped, so a handful of
#' missing weights can't silently bias the result.
weighted_missing_fraction <- function(data, trait, weight_col = NULL) {
  if (!(trait %in% names(data))) return(NA_real_)
  missing <- is.na(data[[trait]])
  if (is.null(weight_col) || is.na(weight_col) || !(weight_col %in% names(data))) {
    w <- rep(1, nrow(data))
  } else {
    w <- data[[weight_col]]
    w[is.na(w)] <- median(w, na.rm = TRUE)
  }
  sum(w[missing]) / sum(w)
}

#' Build the full ranked trait/habitat gap-priority table from a list of
#' already-loaded threat CSVs (see load_threat_csvs()).
build_gap_priority_model <- function(dats, habitat_scores_file = habitat_scores_path) {

  h <- read_csv(habitat_scores_file, show_col_types = FALSE)

  trait_rows <- map_dfr(names(trait_tables), function(th) {
    trait_tables[[th]] %>%
      mutate(
        threat = th,
        flat_pct_missing = map_dbl(trait, ~ weighted_missing_fraction(dats[[th]], .x, NULL)),
        weighted_pct_missing = map_dbl(trait, ~ weighted_missing_fraction(dats[[th]], .x, habitat_risk_col[th]))
      )
  })

  extra_rows <- extra_direct_traits %>%
    rowwise() %>%
    mutate(
      flat_pct_missing = weighted_missing_fraction(dats[[threat]], trait, NULL),
      weighted_pct_missing = weighted_missing_fraction(dats[[threat]], trait, habitat_risk_col[threat])
    ) %>%
    ungroup()

  trait_summary <- bind_rows(trait_rows, extra_rows) %>%
    group_by(trait) %>%
    summarise(
      input_type = "trait",
      breadth = n_distinct(threat),
      threats = paste(sort(unique(threat)), collapse = ", "),
      sum_abs_effect = sum(abs(effect_score)),
      flat_pct_missing = mean(flat_pct_missing),
      weighted_pct_missing = mean(weighted_pct_missing),
      .groups = "drop"
    )

  habitat_summary <- map_dfr(names(habitat_effect_col), function(th) {
    tibble(
      threat = th,
      pct_missing = mean(is.na(dats[[th]][[habitat_effect_col[[th]]]])),
      score_range = diff(range(h[[habitat_score_col[[th]]]], na.rm = TRUE))
    )
  }) %>%
    summarise(
      trait = "habitat (all threats)",
      input_type = "habitat",
      breadth = n_distinct(names(habitat_effect_col)),
      threats = "all 7 threats",
      sum_abs_effect = sum(score_range),
      flat_pct_missing = mean(pct_missing),
      weighted_pct_missing = mean(pct_missing)   # habitat is the weight source, so it can't weight itself
    )

  bind_rows(trait_summary, habitat_summary) %>%
    mutate(
      impact_flat = flat_pct_missing * sum_abs_effect,
      impact_weighted = weighted_pct_missing * sum_abs_effect
    ) %>%
    arrange(desc(impact_weighted))
}

#' Save this run as a dated snapshot and print how it compares to the most
#' recent previous snapshot, if one exists - this is the "did our data
#' collection actually help" check.
save_and_compare_snapshot <- function(model, dir = snapshot_dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # normalizePath() so today_file matches list.files()'s expanded paths below
  # even when `dir` contains a literal "~" - otherwise a same-day rerun never
  # gets excluded from "previous" and ends up compared against itself
  today_file <- normalizePath(file.path(dir, paste0("gap_priority_model_", Sys.Date(), ".csv")), mustWork = FALSE)
  previous_files <- sort(list.files(dir, pattern = "^gap_priority_model_.*\\.csv$", full.names = TRUE))
  previous_files <- setdiff(previous_files, today_file)  # ignore a same-day rerun as "previous"

  write_csv(model, today_file)
  message("Saved snapshot: ", today_file)

  if (length(previous_files) == 0) {
    message("No previous snapshot to compare against - this is the baseline.")
    return(invisible(model))
  }

  prev_file <- tail(previous_files, 1)
  prev <- read_csv(prev_file, show_col_types = FALSE)

  comparison <- model %>%
    select(trait, impact_weighted, weighted_pct_missing) %>%
    full_join(
      prev %>% select(trait, prev_impact = impact_weighted, prev_pct_missing = weighted_pct_missing),
      by = "trait"
    ) %>%
    mutate(
      impact_change = round(impact_weighted - prev_impact, 4),
      pct_missing_change = round((weighted_pct_missing - prev_pct_missing) * 100, 1)
    ) %>%
    arrange(impact_change)

  message("\nCompared against: ", prev_file)
  message("(negative impact_change / pct_missing_change = a gap got smaller = progress)\n")
  print(comparison %>% select(trait, weighted_pct_missing, pct_missing_change, impact_weighted, impact_change), n = 40)

  invisible(comparison)
}

# ---- run --------------------------------------------------------------------

if (sys.nframe() == 0) {
  dats <- load_threat_csvs()
  model <- build_gap_priority_model(dats)

  cat("\n=== Trait/habitat gap-filling priority model ===\n")
  cat("(sorted by exposure-weighted impact; see header comment for the formula)\n\n")
  print(model %>% mutate(across(where(is.numeric), ~round(.x, 3))), n = 40)

  save_and_compare_snapshot(model)
}
