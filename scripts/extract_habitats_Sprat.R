library(tidyverse)

SPRAT_data <- read_csv("datasets_from_manuscripts/SPRAT_habitats_needed.csv", guess_max = 100000) %>%
  select(contains(c("taxon_name", "distribution")))

habitats_misc <- read_csv("datasets_from_manuscripts/habitats_needed_final.csv", guess_max = 100000) %>%
  select(contains(c("taxon_name", "habitat")))

data <- remaining_habitats %>%
  rename(text = habitat) %>%
  select(taxon_name, text) %>%
  mutate(original_text = text,
         text = stringr::str_replace_all(text, "watershed[s]", ""),
         text = stringr::str_replace_all(text, "[:punct:]", ""),
         text = stringr::str_replace_all(text, "less common", "lesscommon"),
         text = stringr::str_replace_all(text, "Not ", "Not"),         
         text = stringr::str_replace_all(text, "not ", "not"),
         text = stringr::str_replace_all(text, "not in ", "notin"),
         text = stringr::str_replace_all(text, "rarely in ", "rarelyin"),
         text = stringr::str_trim(text),
         #text = stringr::str_to_lower(text),
         text = stringr::str_replace_all(text, "$", " "),
         text = stringr::str_replace_all(text, "^", " "),
         text = ifelse(text=="", NA, text)
  ) %>%
  filter(stringr::str_detect(taxon_name, " "))


# prep definitions
definitions <- read_csv("data_extras/habitat_lookup_table.csv") %>%
  mutate(regex = str_c("[\\s[:punct:]]", match_term, "[\\s[:punct:]s]")) %>% 
  filter(match_term != "")

# go through text and replace terms with variants in the definitions list
for (i in 1:nrow(definitions)) {
  
  data$text <- str_replace_all(data$text, definitions$flora_term[i], definitions$match_term[i])
  
}

# generate a template of all possible trait_names
trait_table = data.frame(trait_name = c(str_c(unique(definitions$trait_name), "_a"), 
                                        str_c(unique(definitions$trait_name), "_f")), blank = NA) %>% arrange(trait_name)

trait_table_wide = trait_table %>% pivot_wider(names_from = trait_name, values_from = blank)

# create table to extract into
new <- tibble()

# begin run
# cycle through each row of data
for (i in 1:length(data$text)){
#for (i in 1:100){  # for testing
  
  test_out <- data.frame()
  
  # cycle through each trait value
  for (j in 1:length(definitions$regex)){
    
    # is there a match to any of the trait_name categories (T or F)?
    test = str_detect(data$text[i], definitions$regex[j])
    
    # what was the match?
    test2 = str_extract(data$text[i], definitions$regex[j])
    
    #clean the output
    test2 = str_trim(test2)
    test2 = gsub("[[:punct:]]", "", test2)
    
    # make a dataframe
    test3 = data.frame(match = test, flora_term = test2)
    
    # glue it to the end to make a dataframe of equal length to t
    test_out = rbind(test_out, test3)
    
  }
  
  # use the logical vector (match column) and the extracted terms to create a dataframe of just trait_names where there was a match
  temp <- definitions %>%
    dplyr::filter(test_out$match) %>%
    dplyr::select(-regex) %>% 
    dplyr::mutate(flora_term = test_out$flora_term[!is.na(test_out$flora_term)]) %>% 
    dplyr::distinct()
  
  # If no traits exist, make a blank row with just the original text if no words at all were extracted and start the for loop again (go to the next taxon).
  if (length(temp$flora_term) == 0){
    
    out_final = cbind(data.frame(taxon_name = data$taxon_name[i], original_text = data$original_text[i]), text = data$text[i], trait_table_wide)
    
    new = rbind(new, out_final)
    
    # go to the next taxon_name and description
    next()
    
  }
  
  # Split into dataframes of different traits
  out_list <- split(temp, f = temp$trait_name)
  
  #create a blank dataframe
  out = data.frame()
  
  # cycle through lists of values for each trait
  for (k in 1:length(out_list)){
    
    trait = out_list[[k]]
    
    # create a dataframe with the traits_name, a classification column labeling the austraits term (a) or the flora term (f) and the trait values
    df = data.frame(trait_name = names(out_list[k]), 
                    value_type = c("a", "f"), 
                    values = c(str_c(unique(trait$austraits_trait_value), collapse = " "), str_c(trait$flora_term, collapse = " "))
    )
    
    out = rbind(out, df)
  }
  
  # assign each row as either the austraits trait_name: "trait_name_a" or the flora trait_name: "trait_name_f", ready for pivot_wider
  out <- out %>%
    dplyr::mutate(trait_name = str_c(trait_name, "_", value_type)) %>%
    dplyr::select(-value_type)
  
  # merge with a template containing all possible traits so as to preserve the number of columns even if some categories will be blank
  traits <- merge(trait_table %>% select(trait_name), out, by = "trait_name", all.x = T)
  
  # turn it into a wide table for attaching to the original text. 
  out_final <- traits %>%
    tidyr::pivot_wider(names_from = trait_name, values_from = values)
  
  out_final <- cbind(taxon_name = data$taxon_name[i], 
                    original_text = data$original_text[i], 
                    text = data$text[i],
                    out_final)
  
  # store the finished row of data
  new <- rbind(new, out_final)
  
}

# checking output - not needed
new <- new %>% filter(!is.na(text)) %>% 
  select(-original_text) %>%
  filter(stringr::str_detect(taxon_name, " "))

# write to file
write_csv(new, "datasets_from_manuscripts/habitats_misc.csv", na = "")
