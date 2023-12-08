################################################################################
# Downloading Drinking Water Datasets 
# Author: EmmaLi Tsai 
# Date: 12/1/2023
################################################################################
# Function takes a vector of state acronyms and pulls national drinking water 
# data - data is then saved into the ./data/raw folder as a geojson file. 
# 
# in general - this takes about 10 minutes to run for two states: 
################################################################################
download_sabcrosswalk <- function(states = c("TX")) {
  # increase timeouts since some of these take a bit to download: 
  options(timeout=200)
  
  # commenting this out for now since this function does not need to run on a 
  # schedule: 
  # # setting working directory - CRON default is home directory so needed to 
  # # route it towards the location of my remote:
  # setwd("/Users/emmalitsai/state-drinking-water")
  # # printing for log troubleshooting purposes 
  # print(Sys.time()) 
  
  # required libraries - will need to manage these dependencies in some way 
  # to avoid having to load them each time the function is run (maybe an R package?)
  library(dplyr)
  library(arcpullr)
  library(sf)
  library(readr)
  library(magrittr)
  
  ## Service area boundaries: ##
  sab <- sf::st_read("https://www.hydroshare.org/resource/9ebc0a0b43b843b9835830ffffdd971e/data/contents/temm.gpkg")
  # minor cleaning and subsetting based on state pwsids
  sab_states <- sab %>% 
    dplyr::filter(state_code %in% states) %>%
    dplyr::mutate(area_miles = as.numeric(sf::st_area(.))) %>%
    dplyr::mutate(area_miles = area_miles/27880000) %>%
    dplyr::mutate(pop_density = population_served_count / area_miles)
  print("Pulling SABs")
  
  ## Crosswalk: ##
  # grabbing pwsids from dataset above for SQL query: 
  sab_pwsids <- unique(sab_states$pwsid)
  # querying from REST services: 
  idString <- toString(sprintf("'%s'", sab_pwsids)) 
  sql_fmt <- "pwsid in (%s)"
  sql <- sprintf(sql_fmt, idString)
  pws_cross <- arcpullr::get_table_layer(url = "https://services8.arcgis.com/SVa1M0pGzvtS1NP4/ArcGIS/rest/services/Public_Water_System_Census_Block_Crosswalk/FeatureServer/0", 
                                         where = sql)
  print("Pulling Crosswalk")
  
  # looping through states for merging and writing: 
  for(i in 1:length(states)){
    # subsetting for state data: 
    state_i <- states[i]
    sab_i <- subset(sab_states, state_code == state_i)
    pws_cross_i <- subset(pws_cross, pwsid %in% unique(sab_i$pwsid))
    
    ## Crosswalk x service area boundaries ##
    sab_cross <- merge(sab_i, pws_cross_i, by = "pwsid", all = TRUE)
    sab_cross <- st_as_sf(sab_cross)
    # saving locally: 
    # file_name <- paste0(state_i, "-sabcrosswalk-raw")
    # # NOTE: this should be a geojson but the file is so large that it times out 
    # # saving it as rds for now: 
    # write_rds(sab_cross, paste0("./data/raw/", file_name, ".rds"))
    # sf::st_write(sab_cross, paste0("./data/raw/", file_name, ".geojson"))
    
    # writing to aws bucket
    # NOTE that you need private credentials here 
    # file_name <- paste0(state_i, "-sabcrosswalk-raw.rds")
    # file path in aws bucket
    # aws_filepath <- paste0("state-drinking-water/", state_i, "/raw/", file_name)
    # writing to temporary location  
    # readr::write_rds(data, file.path(tempdir(), file_name))
    # storing temp file into a bucket - commented out for now until we decide 
    # data storage stucture for this project: 
    # put_object(
    #   file = file.path(tempdir(), file_name),
    #   object = aws_filepath,
    #   bucket = "tech-team-data",
    # )
    
    print(paste0("Data saved for ", state_i))
    
    # return this in the form of a data frame if only one argument was passed: 
    if(length(states) == 1){
      return(sab_cross)
    }
  } 
}


# download_data(states = c("TX"))

# SOP for automating R scripts on Macs: 
# install.packages("cronR")
# cronR::cron_rstudioaddin() -- use this interface to schedule batch-downloads 
# will use the the command line on your computer to execute -- consider this 
# more for collaborative data projects - maybe there's a different method 
# to consider here? -- I've set this up to run every hour using my command line
#
# alternative here using GitHub actions and wrapping all of this into a 
# package: 
# https://www.simonpcouch.com/blog/2020-12-27-r-github-actions-commit/

# alternatively - we could also have these as separate functions: 
download_sab <- function(states = ("TX")){
  library(magrittr)
  
  ## Service area boundaries: ##
  sab <- sf::st_read("https://www.hydroshare.org/resource/9ebc0a0b43b843b9835830ffffdd971e/data/contents/temm.gpkg")
  # minor cleaning and subsetting based on state pwsids
  sab_states <- sab %>% 
    dplyr::filter(state_code %in% states) %>%
    dplyr::mutate(area_miles = as.numeric(sf::st_area(.))) %>%
    dplyr::mutate(area_miles = area_miles/27880000) %>%
    dplyr::mutate(pop_density = population_served_count / area_miles)
  return(sab_states)
}

download_blockcrosswalk <- function(pwsids = c("AR0000360", "TX0610218", 
                                               "TX0610220", "TX0610223", "TX0610224")){
  library(magrittr)
  ## Crosswalk: ##
  # querying from REST services: 
  idString <- toString(sprintf("'%s'", pwsids)) 
  sql_fmt <- "pwsid in (%s)"
  sql <- sprintf(sql_fmt, idString)
  pws_cross <- arcpullr::get_table_layer(url = "https://services8.arcgis.com/SVa1M0pGzvtS1NP4/ArcGIS/rest/services/Public_Water_System_Census_Block_Crosswalk/FeatureServer/0", 
                                         where = sql)
  return(pws_cross)
}

