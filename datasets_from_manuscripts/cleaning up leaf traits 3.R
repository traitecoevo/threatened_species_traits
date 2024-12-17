# clean up leaf traits
library(tidyverse)
files = list.files(pattern = "extracted_leaves_3", recursive = T)



for (j in 1:length(files)){
  
  d = read_csv(files[j])
  
  
  d$test = NA
  for (i in 1:nrow(d)){
    
    d$test[i] = ifelse(length(unlist(str_extract_all(d$text[i], "flat"))) > 1 & str_detect(d$leaf_margin_posture_a[i], "flat"), "lamina_flat", NA)
    
  }
  
  
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "involute"), gsub("involute", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "revolute"), gsub("revolute", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "undulate"), gsub("undulate", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "recurved"), gsub("recurved", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "concave"), gsub("concave", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "convex"), gsub("convex", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  
  d$leaf_lamina_posture_a = ifelse(str_detect(d$leaf_margin_posture_a, "flat") & is.na(d$test), gsub("flat", "", d$leaf_lamina_posture_a), d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = str_squish(d$leaf_lamina_posture_a)
  d$leaf_lamina_posture_a = str_trim(d$leaf_lamina_posture_a)
  
  write.csv(d, files[j], row.names = F)
  }

files = list.files(pattern = "extracted_leaves_3", recursive = T)



####


#
#out$leaf_base_shape_a = gsub("base.*", "", out$leaf_base_shape_a)
#out = out %>% mutate(plant_photsynthetic_organ_a = ifelse(str_detect(plant_photsynthetic_organ_a, "phyllode") & str_detect(plant_photsynthetic_organ_f, "blade"), "phyllode", plant_photsynthetic_organ_a))

write.csv(out, "230515_leaf_traits_set_2.csv", na = "", row.names = F)                  




x = read_csv("C:/Users/dcol2804/Downloads/leaf_dims_for_checking1-Lizzy.csv")

x$code = str_c(x$source, x$taxon_name, x$category)

test = x %>% group_by(source, taxon_name, code) %>% summarise(n = n()) %>% filter(n > 1)

dups = x %>% filter(code %in% test$code)
unique(dups$category)
dups$text = gsub("[0-9]frond", "frond", dups$text)
dups$text = gsub("[0-9]leaves", "leaves", dups$text)
dups$text = gsub("[0-9]leaflet", "leaflet", dups$text)
dups$text = gsub("[0-9]pinnae", "pinnae", dups$text)
dups$text = gsub("[0-9]pinnule", "pinnule", dups$text)
dups$text = gsub("[0-9]leaf", "leaf", dups$text)


#write.csv(dups, "dup_leaves.csv", na = "", row.names = F)

dups_new = read_csv("dup_leaves.csv")
new = rbind(x %>% filter(!code %in% dups_new$code), dups_new)
test = dups %>% filter(!code %in% dups_new$code)
new = new %>% filter(!code %in% test$code)

write.csv(new, "C:/Users/dcol2804/Downloads/leaf_dims_for_checking1-Lizzy1.csv", na = "", row.names = F)
