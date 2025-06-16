library(tidyverse)
library(APCalign)

extract_genus <- function(taxon_name) {
  
  taxon_name <- APCalign::standardise_names(taxon_name)
  
  genus <- stringr::str_split_i(taxon_name, " |\\/", 1) %>% stringr::str_to_sentence()
  
  # Deal with names that being with x, 
  # e.g."x Taurodium x toveyanum" or "x Glossadenia tutelata"
  i <- !is.na(genus) & genus =="X"
  
  genus[i] <- 
    stringr::str_split_i(taxon_name[i], " |\\/", 2) %>% stringr::str_to_sentence() %>%  paste("x", .)
  
  genus
}

#### extract_categorical_trait

function to extract categorical traits

```{r, message=FALSE, warning=FALSE}
extract_categorical_trait <- function(database = database, categorical_trait, trait_scores, output_name) {
  
  extracted_data <- (database %>%
                       austraits::extract_trait(categorical_trait))$traits %>%
    tidyr::separate_longer_delim(value, delim = " ") %>%
    dplyr::rename(trait_value = value) %>%
    dplyr::left_join(trait_scores %>% rename(output_name = 3), by = join_by(trait_name, trait_value)) %>%
    dplyr::filter(!is.na(output_name)) %>%
    dplyr::select(dataset_id, observation_id, taxon_name, output_name) %>%
    dplyr::group_by(taxon_name, dataset_id, observation_id) %>%
    dplyr::mutate(output_name = mean(as.numeric(output_name))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(taxon_name, output_name) %>%
    group_by(taxon_name) %>%
    dplyr::mutate(output_name = mean(as.numeric(output_name))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    rename(!!output_name := output_name)
  
  extracted_data
}
```

resources <- APCalign::load_taxonomic_resources()

accepted_taxa <- resources$APC %>% 
  dplyr::filter(taxonomic_status == "accepted") %>%
  dplyr::filter(taxon_rank %in% c("species", "subspecies", "variety")) %>%
  dplyr::select(taxon_name = canonical_name, family, taxon_distribution, higher_classification, class) %>% 
  filter(str_detect(taxon_distribution, "[:alpha:]$") | 
           str_detect(taxon_distribution, "[:alpha:]\\,") |
           str_detect(taxon_distribution, "native and naturalised")) %>%
  mutate(genus = extract_genus(taxon_name = taxon_name))


chenopod_shrub_genera <- c("Atriplex", "Chenopodium", "Rhagodia", "Maireana", "Enchylaena", "Sclerostegia", 
                          "Tecticornia", "Sclerolaena", "Salsola", "Einadia", "Threlkeldia", "Nitraria", 
                          "Sarcocornia", "Halosarcia", "Sueda", "Frankenia", "Lawrencia", "Wilsonia", "Trianthema")


#polypodiopsida_families <- read_csv("ignore/NVIS_GrowthForm_AusTraits/australian_polypodiopsida_families.csv")

samphire_shrub_genera <- read_csv("ignore/NVIS_GrowthForm_AusTraits/samphire_shrub_genera.csv")

read_csv("~/Github/austraits.build/config/NSL/APC-taxon-2023-08-16-4847.csv") %>%
  filter(str_detect(higherClassification, "Poales")) %>%
  distinct(family) %>% 
  filter(!is.na(family)) %>% write_csv("Poales_families.csv")

polypodiopsida_families <- read_csv("~/Github/austraits.build/config/NSL/APC-taxon-2023-08-16-4847.csv") %>%
  filter(str_detect(higherClassification, "Polypodiidae")) %>%
  distinct(family) %>% 
  filter(!is.na(family)) %>% write_csv("Polypodiidae_families.csv")

poales_families <- read_csv("ignore/NVIS_GrowthForm_AusTraits/Poales_families.csv")

AusTraits_life_history_data <- read_csv("ignore/NVIS_GrowthForm_AusTraits/life_history_data.csv")

conversion_table <- read_csv("ignore/NVIS_GrowthForm_AusTraits/NVIS-AusTraits conversion.csv")

taxa <- list()

taxa[["to_allocate"]] <- accepted_taxa %>%
  left_join(AusTraits_life_history_data) %>%
  mutate(checked = FALSE,
         NVIS_growthform = NA_character_)

taxa[["allocated"]] <- 
  dplyr::tibble(
    taxon_name = NA_character_,
    genus = NA_character_,
    family = NA_character_,
    dataset_id = NA_character_,
    plant_growth_substrate= NA_character_,
    plant_growth_form = NA_character_,
    woodiness_detailed = NA_character_,
    life_history = NA_character_,
    life_history_ephemeral_class = NA_character_,
    plant_succulence = NA_character_,     
    dataset_id_height = NA_character_,     
    plant_height_m = NA_real_,     
    location_name = NA_character_,     
    `latitude (deg)` = NA_real_,     
    `longitude (deg)` = NA_real_,
    checked = NA,
    NVIS_growthform = NA_character_
  )

redistribute <- function(taxa) {
  taxa[["allocated"]] <- dplyr::bind_rows(taxa[["allocated"]],
                                          taxa[["to_allocate"]] %>% 
                                          dplyr::filter(checked))
  
  taxa[["to_allocate"]] <-
    taxa[["to_allocate"]] %>% dplyr::filter(!checked)
  taxa
}

# 01,02. allocate plant_growth_substrate - aquatic, epiphytic

# XX- QUESTION: What about lithophytes? Some ferns, orchids that are lithophytes
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_substrate, "aquatic"), "aquatic", NVIS_growthform),
    NVIS_growthform = ifelse(str_detect(plant_growth_substrate, "epiphyte"), "epiphyte", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 03. map NVIS growth forms: tree-fern
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "palmoid") & family %in% polypodiopsida_families$family, "tree-fern", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 04. map NVIS growth forms: fern
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(family %in% c(polypodiopsida_families$family, "Lycopodiaceae", "Marattiaceae", "Ophioglossaceae", "Selaginellaceae", "Equisetaceae"), 
                             "fern", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 05. map NVIS growth forms: cycad
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(family %in% c("Cycadaceae", "Zamiaceae"), "cycad", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 06. map NVIS growth forms: palm
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "palmoid") & (family %in% c("Arecaceae")|genus %in% c("Pandanus")), "palm", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 07. map NVIS growth forms: grass-tree
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "palmoid") & genus %in% c("Xanthorrhoea"), "grass-tree", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 08. map NVIS growth forms: rush
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(family %in% c("Juncaceae", "Typhaceae", "Liliaceae", "Iridaceae", "Xyridaceae") |
                               genus %in% c("Lomandra"), 
                             "rush", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 09. map NVIS growth forms: sedge
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(family %in% c("Cyperaceae", "Restionaceae"), 
                             "sedge", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)


# 10. map NVIS growth forms: hummock grass
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(genus %in% c("Triodia", "Plectrachne"), 
                             "hummock grass", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 11. map NVIS growth forms: tussock grass
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(plant_growth_form == "tussock" & family %in% poales_families$family, "tussock grass", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 12. map NVIS growth forms: other grass
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(family %in% poales_families$family, "other grass", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 13. map NVIS growth forms: vine
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "climber"), "vine", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 14. map NVIS growth forms: forb
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "herb")|str_detect(plant_growth_form, "geophyte"), "forb", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 15. map NVIS growth forms: mallee shrub
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "mallee") & plant_height_m <8, "mallee shrub", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 16. map NVIS growth forms: tree mallee
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "mallee"), "tree mallee", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 17. map NVIS growth forms: tree
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "tree"), "tree", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 18. map NVIS growth forms: samphire shrub
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(genus %in% samphire_shrub_genera$genus, "samphire shrub", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 19. map NVIS growth forms: chenopod shrub
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(family %in% c("Chenopodiaceae"), "chenopod shrub", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

# 20. map NVIS growth forms: shrub
taxa[["to_allocate"]] <- taxa[["to_allocate"]] %>%
  mutate(
    NVIS_growthform = ifelse(str_detect(plant_growth_form, "shrub"), "shrub", NVIS_growthform),
    checked = ifelse(!is.na(NVIS_growthform), TRUE, checked)
  )

taxa <- redistribute(taxa)

taxa <- bind_rows(taxa$to_allocate, taxa$allocated)

taxa %>% write_csv("ignore/NVIS_GrowthForm_AusTraits/scoring_NVIS_growthform.csv")
