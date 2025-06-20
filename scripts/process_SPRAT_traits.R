library(tidyverse)

SPRAT_traits <- read_csv("data_extras/threatened_species_traits_to_fill3.csv")

SPRAT_traits_longer <- SPRAT_traits %>%
  select(-6, -c(29:38)) %>%
  tidyr::pivot_longer(cols = 6:27) %>%
  filter(!is.na(value)) %>%
  filter(!str_detect(value, "XXX")) %>%
  filter(!str_detect(value, "ZZZ")) 

SPRAT_traits_longer %>%
  rename(plant_trait = name) %>%
  separate_wider_delim(cols = value, delim = ";", names_sep = "_", too_few = "align_start") %>%
  tidyr::pivot_longer(cols = contains("value_"), values_drop_na = TRUE) %>%
  select(-name) %>%
  mutate(
    value = str_trim(value),
    plant_trait = ifelse(str_detect(value, "\\["), str_extract(value, "\\[.+?\\]"), plant_trait),
    plant_trait = str_replace_all(plant_trait, "\\[", ""),
    plant_trait = str_replace_all(plant_trait, "\\]", ""),
    value = str_replace(value, "\\[.+?\\]", "")
  ) %>%
  separate_wider_delim(cols = value, delim = "{", names_sep = "_", too_few = "align_start") %>%
  rename(reference = value_2, value = value_1) %>%
  mutate(
    reference = str_replace(reference, "\\}", ""),
    reference = ifelse(is.na(reference), "SPRAT_2025_2", reference)
    ) %>%
  separate_wider_delim(cols = value, delim = "(", names_sep = "_", too_few = "align_start") %>%
  rename(notes = value_2, value = value_1) %>%
  mutate(
    value = str_trim(value),
    units = ifelse(str_detect(value, "^[:digit:]"), str_extract(value, "[:alpha:]+"), NA),
    value = ifelse(str_detect(value, "^[:digit:]"), str_replace(value, " [:alpha:]+", ""), value),
    units = ifelse(str_detect(value, "^[:digit:]") & str_detect(value, "\\%"), str_extract(value, "\\%"), units),
    value = ifelse(str_detect(value, "^[:digit:]"), str_replace(value, " \\%", ""), value)
    ) %>%
  separate_wider_delim(cols = value, delim = "--", names_sep = "_", too_few = "align_start") %>%
  mutate(
    value_2 = ifelse(!is.na(value_2), paste0(value_2, " x__maximum"), NA),
    value_1 = case_when(
      is.na(units) ~ paste0(value_1, " x__mode"),
      is.na(value_2) & !is.na(units) ~ paste0(value_1, " x__mean"),
      !is.na(value_2)  & !is.na(units) ~ paste0(value_1, " x__minimum")
    )
  ) %>% 
  tidyr::pivot_longer(cols = c(value_1, value_2), values_drop_na = TRUE) %>%
  separate_wider_delim(cols = value, delim = "x__", names_sep = "_", too_few = "align_start") %>%
  rename(value_type = value_2, value = value_1) %>%
  mutate(value = str_trim(value)) %>%
  mutate(notes = str_replace(notes, "\\)", "")) %>%
  write_csv("data/SPRAT_2025_2/data.csv")
