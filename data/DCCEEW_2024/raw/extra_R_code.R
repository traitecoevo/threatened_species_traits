read_csv("data/DCCEEW_2024/raw/threatened_species_data_oldnames.csv") -> old_names
read_csv("data/DCCEEW_2024/raw/threatened_species_through_APCalign.csv") -> through_APC

old_names %>%
  left_join(through_APC %>% select(`EPBCA sp. name` = original_name, suggested_name, accepted_name, taxon_distribution)) %>%
  mutate(
    taxon_name = sub("\\[.*", "", suggested_name)
    ) %>%
  select(taxon_name, suggested_name, `EPBCA sp. name`, `EPBCA status`, taxon_distribution, everything()) %>%
  write_csv("data/DCCEEW_2024/data.csv")