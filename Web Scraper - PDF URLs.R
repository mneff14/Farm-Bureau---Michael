### Web Scraper - PDF URLs
### 
###   This script will scrape and save (as a dataframe) all non-existing PDF URLs within specified date range 
###   into specified folder 
### 
### 
### *** ORDER OF EVENTS ***
### 
### 
### 1. Read in an existing dataframe from specified folder (it will have a uniform name)
###    If there is none, create a new dataframe with 0 rows and 2 columns (see 5b)
### 
### 2. Get desired date range from user (EX: today to 3 months back)
###    This will determine which pdf urls will be scraped
### 
### 3. Create a lubridate interval of the user's desired date range
### 
### 4. Create a dataframe containing the HTML Element href string and Date of all pdf url elements on website
### 
### 5. Loop through the HTML/Date dataframe. 
###    a. For each, check if:
###       - The HTML Element's Date is within the desired date interval
###       - The HTML Element and its Date isn't already in the read-in dataframe
### 
###    b. If both are true, append the following to the read-in dataframe: 
###       - The completed pdf url extracted from the HTML Element (String)
###       - The Date (Date)
### 
### 6. If any changes were made, save the read-in dataframe with a uniform name to the specified folder
###    (This will update an existing dataframe in the specified folder)



library(dplyr)
library(rvest)
library(tidyverse)
library(lubridate)



########################
### GLOBAL VARIABLES ###
########################



# Uniform name for any completed dataframe
df_name <- "all_pdf_urls.RData"



######################
## DEFINE FUNCTIONS ##
######################



# Carry out ORDER OF EVENTS
# (Includes Step 2)
main <- function(folder, url, date_from, date_to) {
  
  ### Calls all necessary functions to fulfill the 'Order of Events' listed at the top

  
  # Read in an existing dataframe from specified file
  # If there is none, create a new dataframe with 0 rows and 3 columns
  orig_df <- read_in_df(location = folder)
  
  
  # Create a lubridate interval of the user's desired date range
  # date_range <- lubridate::interval(start = date_from, end = date_to)
  date_range <- create_interval(date_from, date_to)
  
  
  # Create a dataframe containing the HTML Element and Date of all pdf url elements on website
  df_dates_and_elements <- scrape_all_pdf_dates_and_elements(url)
  # print(df_dates_and_elements)
}






# Step 1
read_in_df <- function(location) {
  
  ### Reads in the dataframe matching the df_name from the specified folder
  ### If there are none, then creates a new dataframe with 0 rows and 3 columns
  
  
  df <- tryCatch(
    # Try to read the dataframe from given location
    expr = readRDS(paste0(location, "/", df_name)), 
    
    # Create blank dataframe if expression throws an error
    error = function(cond) {
      # Do nothing
    },
    warning = function(cond) {
      message(paste0("The dataframe '", df_name, "' does not exist in given folder '", location, "'")) 
      message("Created a new dataframe instead")        
      # Return a new dataframe with 0 rows and 2 columns
      return(as.data.frame(matrix(nrow = 0, ncol = 2)))
    })
  
  # Return the result from tryCatch
  return(df)
}



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
  # `colnames<-`("Href", "Date")
  
  
  # If the xml nodeset refers to a pdf, then:
  #   - Put the href into the first column
  #   - Put the Date into the second column
  i <- 1
  for (i in 1:nrow(df)) {
    print(xml_is_a_pdf(xml_nodesets[i]))
    i <- i + 1
  }
  
      
  # Scrape all pdf dates from the website
  # all_pdf_dates <- extract_date()
  
  
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
extract_date <- function(pdf_element) {
  
  ### Scrapes all pdfs' dates from a link HTML element 
  ###
  ###   <a href="pdf/market_reports/2020/mprpt0514.pdf">May 14, 2020</a>
  ###                              |__|      |__|  
  ###                               |         |                 
  ###                             Year   Month & Day
  
  
  # Scrape the href from the given element
  href_string <- pdf_element %>% 
    # read_html() %>% 
    # xml_nodes('li') %>% 
    # html_nodes(xpath = "./a") %>% 
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
    
  
  # Combine the extracted year, month, and day into a date, if not NULL
  print(year)
  
  
  # Returns the extracted date
  return(pdf_date)
}



# Will be implemented in Step 5b (completing URLs)
get_pdf_urls <- function(url, folder_path = "~", return_df = FALSE) {
  
  ### Scrapes all PDF URL's from the given Farmer's Bureau website and saves them into specified folder
  
  
  
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
  
  
  
  # Use scrape function to get all pdf paths
  paths <- scrape_farm_bureau_pdf_paths(url)

  # Use complete function to turn the paths into complete URL's
  complete_urls <- complete_farm_bureau_pdf_urls(paths)

  # Save the complete URLs to the given folder as an .RData file
  saveRDS(complete_urls, file = paste0(folder_path, "/all_pdf_urls.RData"))
  
  # Return the dataframe, if specified
  if (return_df) {
    # return(complete_urls)
    return(complete_urls)
  }
}



# DON'T USE
# Use extract_date()
# This one won't work well because of typos in HTML text (Ex: hyperlink with date "11/03/2106")
scrape_pdf_dates <- function(url) {
    
    ### Scrapes all pdfs' dates from the link HTML element 
    ###
    ###   <a href="pdf/market_reports/2020/mprpt0514.pdf">May 14, 2020</a>
    ###                                                  |___________|  
    ###                                                       |                          
    ###                                                  PDF's Date
    
    pdf_dates <- url %>% 
      # Extracts the text of all hyperlinks
      read_html() %>% 
      xml_nodes('li') %>% 
      html_nodes(xpath = "./a") %>% 
      html_text() %>%
      # Extracts from the HTML text String a Date, which has one of the following formats:  
      #   - mm/dd/yyyy 
      #   - month, dd yyyy
      stringr::str_extract("(\\d\\d/\\d\\d/\\d\\d\\d\\d)|([\\w]+\\s\\d\\d,\\s\\d\\d\\d\\d)") %>% 
      na.omit() %>% 
      mdy() %>% # Convert extracted Strings to Date
      # sort.POSIXlt() %>% 
      data.frame() # Turns the list into a dataframe
    
    # Return the results
    return(pdf_dates)
  }
  
  

###################
## USE FUNCTIONS ##
###################



# Call the main function
main(folder = "PDF URLs", 
     url = "https://barley.idaho.gov/archived_reports.html", 
     date_from = mdy("11/1/2009"), 
     date_to = now())



#################
### TEST CODE ###
#################



# USE THIS CODE TO PUT AN "all_pdf_urls.RData" DATAFRAME INTO A FILE
# Save all pdf url's from the Farmer's Bureau website to specified folder, returning the resulting dataframe
# get_pdf_urls(url = "https://barley.idaho.gov/archived_reports.html",
#              folder_path = "PDF URLs")


# # How to read in the df from the file
# read_in_df <- readRDS("PDF URLs/all_pdf_urls.RData") %>% print()


# # Use the return_df parameter of the get pdf's function
# df <- get_pdf_urls(url = "https://barley.idaho.gov/archived_reports.html",
#                    folder_path = "PDF URLs",
#                    return_df = TRUE) %>% print()


# # Test the scrape dates function
# dates <- scrape_pdf_dates(url = "https://barley.idaho.gov/archived_reports.html") %>% print()


# # Test %within% with date and intervals
# date_range <- interval(start = mdy("11/1/2009"), end = now())
# test_date1 <- mdy("2/13/2011")
# test_date2 <- mdy("2/13/2106")
# print(test_date1 %within% date_range)
# print(test_date2 %within% date_range)



















