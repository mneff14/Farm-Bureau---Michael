###  Automation Script
###  
###  ALPHA VERSION
###
###  - Scrapes all PDFs from the website and puts them into a folder
###    
###
###
###
###  Here's where I found a solution to download a pdf to a folder with download.file:
###  https://www.r-bloggers.com/scraping-pages-and-downloading-files-using-r/
###  
###  Credit also goes here for getting it to work by using mode = "wb":
###  https://stackoverflow.com/questions/9280243/problems-with-downloading-pdf-file-using-r




### DEFINE GLOBAL VARIABLES

# Folder where all PDFs will be downloaded to
folder <- "C:/Users/mneff/Documents/Git/Farm_Bureau_F19/Personal/Michael/Automation/PDFs"




### DEFINE THE FUNCTION

get_pdf_urls <- function(url) {
  
  
  ### Scrapes all PDF URL's from the given Farmer's Bureau website and saves them into specified folder
  
  
  
  # Require some libraries before we start
  require(dplyr)
  require(rvest)
  require(tidyverse)
  
  
  scrape_farm_bureau_pdf_paths <- function(url) {
    
    ### Scrapes all PDF URLs for the Market Reports from the Farmer Bureau's Website 
    ###
    ###
    ### Example of a Farmer's Bureau pdf link element and what's being scraped: 
    ###
    ###   <a href="pdf/market_reports/2020/mprpt0514.pdf">May 14, 2020</a>
    ###           |____________________________________|  
    ###                             |                          
    ###                      PDF's URL Path               
    
    all_pdf_paths <- url %>% 
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
    
    ## Copy the given dataframe
    clean_urls <- pdf_paths
    
    ## Loop through all rows in the dataframe
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
                str_replace("\\.\\./", ""), # Replace the ../ with ""
              "/barley.idaho.gov_prod/pdf/" = result <- paste0("https:/", path) %>% 
                str_replace("_prod", "") # Replace the _prod with ""
      )
      ## Change the current row in the dataframe to the complete PDF URL
      clean_urls[r,1] <- result
      
      ## Increase the incrementor
      r <- r + 1
    }
    
    # Change the type of complete result to dataframe
    complete_result <- data.frame(clean_urls)
    
    # Return the complete dataframe
    return(complete_result)
  }
  
  
  # Use scrape function to get all pdf paths
  paths <- scrape_farm_bureau_pdf_paths(url)
  
  # Use complete function to turn the paths into complete URL's
  complete_urls <- complete_farm_bureau_pdf_urls(paths)
  
  # Return the dataframe
  return(complete_urls)
}




### USE THE FUNCTION

## Returns a dataframe of all of the pdf urls listed on the website
all_pdf_urls <- get_pdf_urls(url = "https://barley.idaho.gov/archived_reports.html")

## Download all pdfs and put them into the specified folder
for (i in 1:nrow(all_pdf_urls)) {
  
  ## Use the market report's date as its unique pdf name
  ## Example:
  ## 
  ## A PDF with this url:
  ## 
  ##   https://barley.idaho.gov/pdf/market_reports/2020/mprpt0116.pdf
  ##                                              |__|      |__|  
  ##                                               |         |                 
  ##                                             Year   Month & Day
  ## 
  ## Would be downloaded by the name of:
  ## 
  ##  "market_report_01_16_2020.pdf"
  ## 
  
  # Extract the year, month, and day of the report using regex
  pdf_year <- stringr::str_extract(all_pdf_urls[i,1], 
                                   "(?<=/)\\d\\d\\d\\d(?=/)") # Four digits surrounded by /'s
  pdf_month <- stringr::str_extract(all_pdf_urls[i,1], 
                                    "(?<=[[:alpha:]]{4,5})\\d\\d") # Two digits preceeded by 4-5 letters
  pdf_day <- stringr::str_extract(all_pdf_urls[i,1],
                                  "(?<=[[:alpha:]]{4,5}\\d\\d)\\d\\d") # Two digits preceeded by the month
  
  # Construct the name from the given url
  pdf_name <- paste0("market_report_", pdf_month, "_", 
                     pdf_day, "_", pdf_year, "_", ".pdf")
  
  # Make full path for download
  pdf_full_path <- paste(folder, pdf_name, sep = "/")
  
  # Download the url as a pdf
  download.file(pdf_url, pdf_full_path, method = "auto", mode = "wb", cacheOK = TRUE,
                extra = getOption("download.file.extra"))
  
  # Increae iterator
  i <- i + 1
}






