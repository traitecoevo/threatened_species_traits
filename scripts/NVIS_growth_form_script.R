library(austraits)
library(dplyr)
# open v6.0.0. from Zenodo

# extract data for key traits
trait_data_raw <- (all_traits %>%
  austraits::extract_trait(c("plant_growth_form", "plant_growth_substrate", "plant_height", "succulence", "life_history", "woodiness_detailed", "leaf_length", "stem_length")) %>%
  austraits::join_taxa(vars = c("genus", "family", "taxonomic_status")) %>%
  austraits::join_location_coordinates())$traits %>%
  dplyr::filter(taxonomic_status == "accepted")

# retain tallest height measurement for each species
trait_data_height <- trait_data_raw %>%
  dplyr::filter(trait_name == "plant_height") %>%
  dplyr::select(dataset_id, taxon_name, trait_name, value, genus, family, location_name, `latitude (deg)`, `longitude (deg)`) %>%
  dplyr::arrange(taxon_name, value) %>%
  dplyr::group_by(taxon_name) %>%
    dplyr::mutate(value = first(value)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(taxon_name, value, .keep_all = TRUE)  %>%
  rename(plant_height_m = value, dataset_id_height = dataset_id) %>%
  dplyr::select(-trait_name)

trait_data_categorical <- trait_data_raw %>%
  dplyr::filter(trait_name != "plant_height") %>%
  dplyr::select(dataset_id, taxon_name, trait_name, value, genus, family) %>%
  tidyr::separate_longer_delim(cols = value, delim = " ") %>%
  dplyr::arrange(value) %>%
  dplyr::group_by(taxon_name, trait_name) %>%
    dplyr::mutate(dataset_id = paste0(dataset_id, collapse = " ")) %>%
    dplyr::distinct(value, .keep_all = TRUE) %>%
    dplyr::mutate(value = paste0(value, collapse = " ")) %>%
    dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(taxon_name) %>%
  dplyr::mutate(dataset_id = paste0(dataset_id, collapse = " ")) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  tidyr::separate_longer_delim(cols = dataset_id, delim = " ") %>%
  dplyr::arrange(dataset_id) %>%
  dplyr::distinct() %>%
  dplyr::group_by(taxon_name, trait_name) %>%
    dplyr::mutate(dataset_id = paste0(dataset_id, collapse = "; ")) %>%
    dplyr::distinct() %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = trait_name, values_from = value)

trait_data_formatted <- trait_data_categorical %>%
  left_join(trait_data_height)

trait_data_formatted %>% write_csv("ignore/life_history_data.csv")
