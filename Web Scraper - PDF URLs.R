### Web Scraper - PDF URLs
### 
###   This function scrapes all PDF URL's from a given website.



library(dplyr)
library(rvest)
library(tidyverse)



### Test by scraping all PDF URLs for the Market Reports from the Farmer Bureau's Website 
###
###
### Example of a link element and what's being scraped: 
###
###   <a href="pdf/market_reports/2020/mprpt0514.pdf">May 14, 2020</a>
###           |____________________________________|  |__________|
###                             |                          |
###                      PDF's URL Path               PDF's Date



# Define the website URL
website_url <- "https://barley.idaho.gov/archived_reports.html"


## PDF Paths

# Scrapes all PDFs' URL Path from the given website
all_pdf_paths <- website_url %>% 
  read_html() %>% 
  xml_nodes('li') %>% 
  html_nodes(xpath = "./a") %>% 
  html_attr("href") %>% 
  stringr::str_subset("pdf$") # Regex that keeps only urls that end with "pdf"

print(all_pdf_paths)


## PDF Dates


# Scrapes all PDFs' Date from the given website









