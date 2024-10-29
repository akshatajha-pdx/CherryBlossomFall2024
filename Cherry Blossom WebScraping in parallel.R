# Cherry blossom web scraping, parallel
# first we try to parallelize over year and gender combinations 


#rm(list=ls())
install.packages(c("httr", "dplyr", "purrr", "furrr", "tibble"))
library(httr)
library(dplyr)
library(purrr)
library(furrr)
library(tibble)

{
  start_time <- Sys.time()
  # Set up parallel processing plan
  num_cores <- parallel::detectCores() - 1
  plan(multisession, workers = num_cores)
  
  # Helper function to convert "hh:mm:ss" into total seconds
  time_to_seconds <- function(time_str) {
    time_parts <- unlist(strsplit(time_str, ":"))
    # Convert to total seconds (hours * 3600 + minutes * 60 + seconds)
    as.numeric(time_parts[1]) * 3600 + as.numeric(time_parts[2]) * 60 + as.numeric(time_parts[3])
  }
  
  # Helper function to convert total seconds to "mm:ss" pace per mile
  seconds_to_pace <- function(seconds) {
    minutes <- floor(seconds / 60)
    remaining_seconds <- round(seconds %% 60)
    # Return in "mm:ss" format
    sprintf("%02d:%02d", minutes, remaining_seconds)
  }
  
  # Base URL template
  base_url <- function(year, gender, page) {
    paste0(
      "https://alltimerunner.caprunner.championsoftheweb.com/api/performances/?where%5Band%5D%5B0%5D%5Bevent.eventType%5D%5Bin%5D=10M",
      "&where%5Band%5D%5B1%5D%5Bevent.year%5D%5Bequals%5D=", year,
      "&where%5Bdivision%5D%5Bcontains%5D=", gender,
      "&where%5Brunner.hidden%5D%5Bequals%5D=false&depth=3&page=", page
    )
  }
  
  # Function to scrape race results for a given year and gender
  scrape_race_results <- function(year, gender) {
    # First request to get total pages
    res <- httr::GET(base_url(year, gender, 1))  # Request page 1
    content_data <- httr::content(res)
    
    # uncomment this line to load all pages
    #total_pages <- ifelse(!is.null(content_data$totalPages), content_data$totalPages, 1)
    total_pages <- 10
    
    message(paste0("Total pages to scrape for ", gender, ", ", year, ": ", total_pages))
    
    # Create a vector of pages
    pages <- 1:total_pages
    
    #parallelize over years and genders only
    all_data <- list()
    
    for (page in pages) {
      message(paste("Scraping page", page, "of", total_pages))
      
      # Make the request for the current page
      res <- httr::GET(base_url(year, gender, page))
      content_data <- httr::content(res)
      
      if (!is.null(content_data$docs)) {
        # Extract the relevant fields and append them to all_data
        page_data <- map_dfr(content_data$docs, function(person) {
          net_time_seconds <- time_to_seconds(person$netTime)  # Convert netTime to seconds
          pace_seconds_per_mile <- net_time_seconds / 10       # Calculate pace in seconds per mile
          pace <- seconds_to_pace(pace_seconds_per_mile)       # Convert pace to "mm:ss" format
          
          tibble(
            Name = paste(person$runner$firstName, person$runner$lastName, sep = " "),  # Combine firstName and lastName
            PiD_TiD = paste(person$pid, person$tid, sep = " / "),  # Combine PiD and TiD
            PiS_TiS = paste(person$pis, person$tis, sep = " / "),  # Combine PiS and TiS
            Age = person$age,                 
            Time = person$netTime,            # Net Time (original format)
            Pace = pace,                      # Pace in "mm:ss" format
            Division = person$division,      
            Hometown = person$homeTown,       
            Year = year,                      # Add Year for reference
            Gender = gender                   # Add Gender for reference
          )
        })
        
        all_data[[page]] <- page_data
      } else {
        message(paste("No docs found on page", page))
      }
    }
    
    # Combine all pages into a single data frame
    final_data <- bind_rows(all_data)
    return(final_data)
  }
  
  # Define the years and genders
  years <- 2024:2023
  genders <- c("W", "M")
  
  # Create all combinations of years and genders
  combinations <- expand.grid(year = years, gender = genders)
  
  # Use future_pmap_dfr to scrape data in parallel
  all_data <- future_pmap_dfr(combinations, function(year, gender, .options = furrr_options(seed = TRUE)) {
    scrape_race_results(year, gender)
  })
  end_time <- Sys.time()
  print(all_data)
  print(end_time - start_time)
}




# Alternatively, if you want separate data frames for males and females:

# Scrape female data in parallel
# allyears_female_data <- future_map_dfr(years, function(yr) {
#   scrape_race_results(yr, "W")
# })
# 
# # Scrape male data in parallel
# allyears_male_data <- future_map_dfr(years, function(yr) {
#   scrape_race_results(yr, "M")
# })
# 
# print(allyears_female_data)
# print(allyears_male_data)





# try it over pages instead of year-gender combinations. EDIT: No, don't.
# {
#   start_time <- Sys.time()
#   num_cores <- parallel::detectCores() - 1
#   plan(multisession, workers = num_cores)
#   
#   time_to_seconds <- function(time_str) {
#     time_parts <- unlist(strsplit(time_str, ":"))
#     as.numeric(time_parts[1]) * 3600 + as.numeric(time_parts[2]) * 60 + as.numeric(time_parts[3])
#   }
#   seconds_to_pace <- function(seconds) {
#     minutes <- floor(seconds / 60)
#     remaining_seconds <- round(seconds %% 60)
#     sprintf("%02d:%02d", minutes, remaining_seconds)
#   }
#   
#   base_url <- function(year, gender, page) {
#     paste0(
#       "https://alltimerunner.caprunner.championsoftheweb.com/api/performances/?where%5Band%5D%5B0%5D%5Bevent.eventType%5D%5Bin%5D=10M",
#       "&where%5Band%5D%5B1%5D%5Bevent.year%5D%5Bequals%5D=", year,
#       "&where%5Bdivision%5D%5Bcontains%5D=", gender,
#       "&where%5Brunner.hidden%5D%5Bequals%5D=false&depth=3&page=", page
#     )
#   }
# 
#   scrape_race_results <- function(year, gender) {
#     res <- httr::GET(base_url(year, gender, 1))
#     content_data <- httr::content(res)
#     
#     #total_pages <- ifelse(!is.null(content_data$totalPages), content_data$totalPages, 1)
#     total_pages <- 10
#   
#     message(paste0("Total pages to scrape for ", gender, ", ", year, ": ", total_pages))
#     
#     pages <- 1:total_pages
#     
#     # Use future_map_dfr to process pages in parallel
#     page_data_list <- future_map_dfr(pages, function(page) {
#       message(paste("Scraping page", page, "of", total_pages))
# 
#       res <- httr::GET(base_url(year, gender, page))
#       content_data <- httr::content(res)
#       
#       if (!is.null(content_data$docs)) {
#         # Extract the relevant fields
#         map_dfr(content_data$docs, function(person) {
#           net_time_seconds <- time_to_seconds(person$netTime)
#           pace_seconds_per_mile <- net_time_seconds / 10
#           pace <- seconds_to_pace(pace_seconds_per_mile)
#           
#           tibble(
#             Name = paste(person$runner$firstName, person$runner$lastName, sep = " "),
#             PiD_TiD = paste(person$pid, person$tid, sep = " / "),
#             PiS_TiS = paste(person$pis, person$tis, sep = " / "),
#             Age = person$age,
#             Time = person$netTime,
#             Pace = pace,
#             Division = person$division,
#             Hometown = person$homeTown,
#             Year = year,
#             Gender = gender
#           )
#         })
#       } else {
#         message(paste("No docs found on page", page))
#         NULL
#       }
#     }, .options = furrr_options(seed = TRUE))  # Ensure reproducibility
#     
#     return(page_data_list)
#   }
#   
#   # Define the years and genders
#   years <- 2024:2020  # Adjust the year range as needed
#   genders <- c("W", "M")
#   
#   # Create all combinations of years and genders
#   combinations <- expand.grid(year = years, gender = genders)
#   
#   # Use future_pmap_dfr to scrape data in parallel over years and genders
#   all_data <- future_pmap_dfr(combinations, function(year, gender) {
#     scrape_race_results(year, gender)
#   })
#   print(all_data)
#   end_time <- Sys.time()
#   print(end_time - start_time)
# }

