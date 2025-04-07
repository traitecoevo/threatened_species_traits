library(rvest)
library(dplyr)
library(purrr)

# Define a list of URLs to scrape
urls <- c(
  "https://en.wikipedia.org/wiki/Melaleuca_lazaridis",
  "https://en.wikipedia.org/wiki/Melaleuca_hemisticta",
  "https://en.wikipedia.org/wiki/Melaleuca_chisholmii"
)


resources$APC %>% 
  filter(taxonomic_status == "accepted") %>% 
  filter(name_type == "scientific") %>%
  filter(taxon_rank %in% c("species")) %>%
  select(taxon_name = canonical_name, family, taxon_distribution) %>%
  filter(family == "Myrtaceae") %>%
  mutate(
    name2 = stringr::str_replace(taxon_name, " ", "_"),
    url = paste0("https://en.wikipedia.org/wiki/",name2)
  ) -> urls_to_scrape

urls <- urls_to_scrape$url %>% as.vector()

process_page <- function(url) {
  # Try to read the page content, handle errors if page is not found
  page <- tryCatch({
    read_html(url)
  }, error = function(e) {
    # If an error occurs, print a message and return NULL
    message("Skipping URL: ", url, " - Page not found or cannot be accessed.")
    return(NULL)  # Return NULL to indicate failure
  })
  
  # If page is NULL (meaning the URL failed), skip further processing
  if (is.null(page)) {
    return(NULL)  # Skip this URL and return NULL
  }
  
  # Extract all h2 headers
  headers <- page %>% html_elements("h2") %>% html_text() %>% as.vector()
  
  # Extract all paragraphs (p elements)
  paragraphs <- page %>% html_elements("p") %>% html_text()
  
  
  data <- page %>% 
    html_elements("h2, p") %>% 
    html_text() %>% 
    as.data.frame() %>%
    rename(text = 1)
  
  headers2 <- data$text %in% headers
  
  # Step 2: Create a new data frame with two columns: "Header" and "Content"
  # Create an empty list to store the final result
  species_data <- tibble(
    Header = character(0), 
    Content = character(0)
  )
  
  # Step 3: Loop through the data and separate headers from content
  for (i in which(headers2)) {
    # Extract header (current row)
    header <- data$text[i]
    
    # Extract content (next rows until the next header is found)
    content_start <- i + 1
    content_end <- ifelse(i + 1 <= nrow(data) && headers2[i + 1], i, nrow(data))
    content <- paste(data$text[content_start:content_end], collapse = " ")
    
    # Add the header and content to the final table
    species_data <- rbind(species_data, data.frame(Header = header, Content = content))
  }
  
  species_data_t <- species_data %>% t() %>% as.data.frame()
  
  colnames(species_data_t) <- as.character(species_data_t[1,])
  
  species_data_t <- species_data_t %>% slice_tail %>% bind_cols(species_url = url)
  
  species_data_t
}

# Use lapply to apply the function to all URLs
species_data_all <-
  tibble(
    `Contents` = character(0),
    `Description` = character(0),
    `Taxonomy and naming` = character(0),
    `Distribution and habitat` = character(0),
    `References` = character(0),
    `Conservation status` = character(0),
    `Use in horticulture` = character(0),
    species_url = character(0),
  )

same as code below using lapply
for (i in 1:length(urls)) {

  species_data_t <- process_page(urls[i])

  if (!is.null(species_data_t)) {
    species_data_all <- species_data_all %>% bind_rows(species_data_t)
  }

}

results <- lapply(urls, process_page)
results <- results[!sapply(results, is.null)]

# Combine the results into a single data
Wikipedia_all_species_data <- bind_rows(results)
