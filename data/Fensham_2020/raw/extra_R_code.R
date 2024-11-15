Table2 <- read_csv("data/Fensham_2020/raw/Fensham_Table2.csv") %>%
  mutate(
    Species = str_replace(Species, "^E. ", "Eucalyptus "),
    Species = str_replace(Species, "^C. ", "Corymbia "),
    Species = str_replace(Species, "^A. ", "Angophora ")
    )

Table3 <- read_csv("data/Fensham_2020/raw/Fensham_Table3.csv") %>%
  mutate(
    Species = str_replace(Species, "^E. ", "Eucalyptus "),
    Species = str_replace(Species, "^C. ", "Corymbia "),
    Species = str_replace(Species, "^A. ", "Angophora "),
    Locations = as.character(Locations)
  )

Table4 <- read_csv("data/Fensham_2020/raw/Fensham_Table4.csv") %>%
  mutate(
    Species = str_replace(Species, "E. ", "Eucalyptus "),
    Species = str_replace(Species, "C. ", "Corymbia "),
    Species = str_replace(Species, "A. ", "Angophora "),
    Locations = as.character(Locations)
  )


Combined <- Table2 %>% 
  full_join(Table3) %>% 
  full_join(Table4) %>%
  mutate(Species = str_replace(Species, "\\*", ""))

View(Combined)

setdiff(names(Combined), names(Fensham_data))

Fensham_data %>% 
  full_join(Combined %>% select(Species, TEC, `Mature individuals`, `Largest sub-population`, `Accepted threats`, `Putative threats`)) %>%
  write_csv("data/Fensham_2020/data.csv")

read_csv("data/Fensham_2020/data.csv") %>% write_csv("data/Fensham_2020/data.csv", na = "")

Fensham_data %>% distinct(Habitat)
