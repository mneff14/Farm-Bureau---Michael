### Web Scraper - PDF URLs
### 
### These functions scrape all PDF URL's from a given website.




library(dplyr)
library(rvest)
library(tidyverse)




######################
## DEFINE FUNCTIONS ##
######################




scrape_farm_bureau_pdf_paths <- function(website_url) {
  
  ### Scrapes all PDF URLs for the Market Reports from the Farmer Bureau's Website 
  ###
  ###
  ### Example of a Farmer's Bureau pdf link element and what's being scraped: 
  ###
  ###   <a href="pdf/market_reports/2020/mprpt0514.pdf">May 14, 2020</a>
  ###           |____________________________________|  
  ###                             |                          
  ###                      PDF's URL Path               
  
   all_pdf_paths <- website_url %>% 
    read_html() %>% 
    xml_nodes('li') %>% 
    html_nodes(xpath = "./a") %>% 
    html_attr("href") %>% 
    stringr::str_subset("pdf$") %>% # Regex that keeps only urls that end with "pdf"
    data.frame() # Turns the list into a dataframe
  
  # Returns the dataframe
  return(all_pdf_paths)
}




complete_farm_bureau_pdf_urls <- function(pdf_paths) {
  
  ### Loops through the given dataframe of PDF Paths and adds a specific part of the protocol and/or domain 
  ### based on the beginning of the pdf paths.
  ###
  ###
  ### Example of result:      https://barley.idaho.gov/pdf/market_reports/2020/mprpt0514.pdf
  ###                        |_____| |_______________||____________________________________|
  ###                          |            |                           |                   
  ###                      protocol      domain                       path             
  ###
  ###
  ### Unique path beginnings:
  ###   - pdf/
  ###   - /pdf/
  ###   - ../pdf/                           
  ###   - /barley.idaho.gov_prod/pdf/       (Already contains the domain)
  
  # Copy the given dataframe
  clean_urls <- pdf_paths
  
  # Loop through all rows in the dataframe
  for (r in 1:nrow(clean_urls)) { 
    # Make variable for the current pdf path
    path <- clean_urls[r,1]
    # Retrieve all characters before (and including) the first pdf/
    first_part <- str_extract(path, "^(.*pdf/)")
    # Combine the path and protocol/domain into "result"
    result <- NULL
    switch (first_part,
            "pdf/"                        = result <- paste0("https://barley.idaho.gov/", path),
            "/pdf/"                       = result <- paste0("https://barley.idaho.gov", path),
            "../pdf/"                     = result <- paste0("https://barley.idaho.gov/", path) %>% 
              str_replace("\\.\\./", ""), # Remove the ../
            "/barley.idaho.gov_prod/pdf/" = result <- paste0("https:/", path)
    )
    # Change the current row in the dataframe to the complete PDF URL
    clean_urls[r,1] <- result
    # Increase the incrementor
    r <- r + 1
  }
  
  # Change the type of complete result to dataframe
  complete_result <- data.frame(clean_urls)
  
  # Return the complete dataframe
  return(complete_result)
}




####################
## TEST FUNCTIONS ##
####################




# Define the website URL
url <- "https://barley.idaho.gov/archived_reports.html"

# Use function to get all pdf paths
paths <- scrape_farm_bureau_pdf_paths(url)

# Use function to complete the pdf paths
complete_urls <- complete_farm_bureau_pdf_urls(paths)

# Show the results
print(complete_urls)

















