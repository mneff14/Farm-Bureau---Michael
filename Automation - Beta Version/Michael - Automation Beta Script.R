### Web Scraper - PDF URLs
### 
###   This script will scrape and save (as a dataframe) all non-existing PDF URLs within specified date range 
###   into specified folder 
### 
### 
###   https://barley.idaho.gov/archived_reports.html
### 
### 
### *** ORDER OF EVENTS ***
### 
### 
### 
### KYLE:
### 1. Create a new dataframe containing the names of all pdfs in specified folder
###       - Can use list.file to get pdf file names
###    
###    
### 2. Get desired date range from user (EX: today to 3 months back)
###    This will determine which pdf urls will be scraped
###    
### 
### 3. Create a lubridate interval of the user's desired date range
###    
### 
### 4. Create a dataframe containing the HTML Element href string and Date of all pdf url elements on website
### 
###    
### *5.Download all new, valid htmls
###
###    Filter out all htmls that:
###       - Are not within the desired date interval
###       - Aren't already in the read-in dataframe
### 
###    Then Download the htmls, after turning them into full PDF paths






######################
## DEFINE FUNCTIONS ##
######################



# Carry out ORDER OF EVENTS
main <- function(folder, url, date_from, date_to) {
  
  ### Calls all necessary functions to fulfill the 'Order of Events' listed at the top
  
  
  # Require some libraries before we start
  require(dplyr)
  require(rvest)
  require(tidyverse)
  require(lubridate)
  require(stringr)
  
  
  ### STEP 1
  # Read in an existing dataframe from specified file
  # If there is none, create a new dataframe with 0 rows and 3 columns
  orig_df <- read_in_df(folder)
  
  
  ### STEP 2
  # The user passes in the date range as main's agruments
  
  
  ### STEP 3
  # Create a lubridate interval of the user's desired date range
  # date_range <- lubridate::interval(start = date_from, end = date_to)
  date_range <- create_interval(date_from, date_to)
  # Make sure date range isn't NULL before continuing
  if (is.null(date_range)) {
    return()
  } else {
    
    ### STEP 4
    # Create a dataframe containing the HTML Element and Date of all pdf url elements on website
    df_dates_and_elements <- scrape_all_pdf_dates_and_elements(url)
    
    
    ### STEP 5
    # Will filter the HTML/Date dataframe and download any new, in-range pdfs
    suppressWarnings( download_all_valid_pdfs(orig_df, date_range, df_dates_and_elements, folder) )
  }
}






# Step 1
read_in_df <- function(location) {
  
  ### Reads in the names of all files in the specified folder (location)
  
  return(list.files(location, full.names = FALSE))
}



# Step 2 is inside of main()



# Step 3
create_interval <- function(from, to) {
  
  ### Returns an interval from the two given dates
  ### Returns NULL if an error occured
  
  interval <- tryCatch(
    expr = lubridate::interval(start = from, end = to),
    warning = function(cond) {
      message("Warning: Invalid dates. Returning NULL")
      return(NULL)
    }
  )
  
  # Return results from tryCatch
  return(interval)
}



# Step 4
scrape_all_pdf_dates_and_elements <- function(url) {
  
  ### Returns a dataframe with two columns containing:
  ###   - All pdf HTML Elements
  ###   - All pdf Dates
  
  
  # Call function to scrape all pdf elements from the website
  xml_nodesets <- scrape_all_xml_nodesets(url)
  
  
  # Create blank dataframe with two columns and number of rows equal to number of xml_nodesets
  df <- as.data.frame(matrix(nrow = length(xml_nodesets), ncol = 2))

  
  # If the xml nodeset refers to a pdf, then:
  #   - Put the href into the first column
  #   - Put the Date into the second column
  i <- 1
  for (i in 1:nrow(df)) {
    if (xml_is_a_pdf(xml_nodesets[i])) {
      
      # Put the href string from the xml nodeset into the first column of the dataset
      df[i,1] <- xml_nodesets[i] %>% 
        html_attr("href")

      # Extract the date and put it into the second column of the dataset
      df[i,2] <- as.character.Date(extract_date(xml_nodesets[i]))
    }
    i <- i + 1
  }

  # Return the complete dataframe
  return(df)
}



# Step 4 (helper)
scrape_all_xml_nodesets <- function(url) {
  
  ### Returns all xml nodesets scraped from the url's HTML <a> elements
  
  
  # Scrape all <a> xml from all HTML Elements from the given url
  all_xml_nodesets <- url %>% 
    read_html() %>% 
    xml_nodes('li') %>% 
    html_nodes(xpath = "./a")
  
  
  # Return the list of xml nodesets
  return(all_xml_nodesets)
}



# Step 4 (helper)
xml_is_a_pdf <- function(xml) {
  
  ### Returns true if the passed xml nodeset ends with ".pdf" in the href
  
  
  # Extract the href string from the xml
  href <- xml %>% 
    html_attr("href")
  
  # Returns true if the href ends with "pdf" and false if not
  if (stringr::str_detect(href, "pdf$")) {
    return(TRUE)
  } else { 
    return(FALSE) 
  }
}



# Step 4 (helper)
# This will work very well because it extracts the date from the href of the HTML Element (no typos)
extract_date <- function(xml_element) {
  
  ### Scrapes all pdfs' dates from a link HTML element 
  ###
  ###   <a href="pdf/market_reports/2020/mprpt0514.pdf">May 14, 2020</a>
  ###                              |__|      |__|  
  ###                               |         |                 
  ###                             Year   Month & Day
  
  # Define date varibales
  year <- NULL
  month <- NULL
  day <- NULL
  
  # Scrape the href from the given element
  href_string <- xml_element %>% 
    html_attr("href")
  
  # Extract the year from the href
  year <- tryCatch(
    expr = stringr::str_extract(href_string, "(?<=/)\\d\\d\\d\\d(?=/)"), # Four digits preceded and followed by /
    error = function(cnd) {
      # Returns NULL if an error is produced
      message(paste0("Cannot extract year from ", href_string))
      return(NULL)
    }
  )
  
  # Extract the year from the href
  month <- tryCatch(
    expr = stringr::str_extract(href_string, "(?<=[[:alpha:]]{4,5})\\d\\d"), # Two digits preceeded by "mprp" or "mprpt"
    error = function(cnd) {
      # Returns NULL if an error is produced
      message(paste0("Cannot extract month from ", href_string))
      return(NULL)
    }
  )
  
  # Extract the year from the href
  day <- tryCatch(
    expr = stringr::str_extract(href_string, "(?<=[[:alpha:]]{4,5}\\d\\d)\\d\\d"), # Two digits preceeded by the month
    error = function(cnd) {
      # Returns NULL if an error is produced
      message(paste0("Cannot extract day from ", href_string))
      return(NULL)
    }
  )
  
  # Return null if any of the extraction attempts returned NULL
  if (is.null(year) || is.null(month) || is.null(day)) {
    return(NULL)
  }
  
  # Combine the extracted year, month, and day into a date, if not NULL
  date_string <- paste0(month, "-", day, "-", year)
  href_date <- lubridate::mdy(date_string)

  # Returns the extracted date
  return(href_date)
}



# Step 5
download_all_valid_pdfs <- function(readin_df, date_range, html_date_df, dest_folder) {
  
  ### Downloads all valid htmls, which happens if:
  ###   - They are within the date range 
  ###   - They haven't been downloaded already
  
  
  # Extract all htmls and dates that are within the date range
  valid_htmls <- html_date_df %>% 
    filter(ymd(html_date_df[,1]) %within% date_range)

  # Extract all htmls and dates that haven't been downloaded already
  valid_htmls <- extract_htmls_not_downloaded(valid_htmls, readin_df)

  # Proceed if there are new pdfs to download
  if (nrow(valid_htmls) < 1) {
    message("All PDFs are up to date.")
  } else {
    
    # Complete the htmls (turn into complete pdf URLs for download)
    valid_htmls$V1 <- complete_farm_bureau_pdf_urls(as.data.frame(valid_htmls$V1))

    # Combine the dest_folder path to each report's name to make a complete path for download
    valid_htmls$V3 <- str_c(dest_folder, '\\', valid_htmls$V3)

    # Download all the new, valid pdfs into the specified folder
    i <- 0
    for (i in 1:nrow(valid_htmls)) {
      
      # The final step!
      download.file(url = as.character(valid_htmls[i,1]), destfile = valid_htmls[i,3], method = "auto", 
                    mode = "wb", cacheOK = TRUE, extra = getOption("download.file.extra"))
      
      # Increment incrementor
      i <- i + 1
    }
    # Display final success message
    message(paste0("Successfully downloaded ", i - 1, " PDFs"))
  }
}
  


# Step 5 (Helper)
extract_htmls_not_downloaded <- function(html_date_df, downloaded_pdfs_df) {
  
  ### Returns a dataframe containing htmls and dates that haven't been already downloaded
  ### 
  ### This function will filter out and return any rows in the html_date_df whose dates are 
  ### nested inside any of the names of the pdfs already downloaded in downloaded_pdfs_df
  
  
  # Create a new column which contains the would-be file name of the html_dates.
  # This will be used to check if it has already been downloaded.
  #   
  #   Example
  #     
  #     html_date_df row:
  #       pdf/market_reports/2020/mprpt0619.pdf , 2020-06-19
  #     
  #     potential file name:
  #       market_report_06_19_2020
  #
  html_date_df$V3 <- str_c("market_report_", 
                           str_pad(month(ymd(html_date_df$V2)), 2, pad = "0"), "_",
                           str_pad(day(ymd(html_date_df$V2)), 2, pad = "0"), "_",
                           year(ymd(html_date_df$V2)), "_.pdf")
  
  
  # Filter out any htmls whose potential file name DOESN'T MATCH any in the already downloaded pdfs df
  html_date_df <- filter(html_date_df, !(V3 %in% downloaded_pdfs_df))
  

  return(html_date_df)
}



# Step 5 (Helpter)
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
              str_replace("\\.\\./", ""), # Replace the ../ with ""
            "/barley.idaho.gov_prod/pdf/" = result <- paste0("https:/", path) %>% 
              str_replace("_prod", "") # Replace the _prod with ""
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





###################
## USE FUNCTIONS ##
###################





# Call the main function
main(folder = "Personal\\Michael\\Automation - Alpha Version\\PDFs",
     # NOTE: Make sure the folder has "\\" separating the path folders, NOT '\'
     url = "https://barley.idaho.gov/archived_reports.html",
     date_from = mdy("1/1/2009"),
     date_to = now())






















