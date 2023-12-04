################################################################################
# Downloading Drinking Water Datasets 
# Author: EmmaLi Tsai 
# Date: 12/1/2023
################################################################################
# Function takes a vector of state acronyms and pulls national drinking water 
# data - data is then saved into the ./data/raw folder, with the potential 
# to write to an aws s3 bucket in the future. 

download_data <- function(states = c("TX", "OR")) {
  # increase timeouts since some of these take a bit to download: 
  options(timeout=180)
  
  # setting working directory - CRON default is home directory so needed to 
  # route it towards the location of my remote:
  setwd("/Users/emmalitsai/state-drinking-water")
  
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
    # subsetting state data: 
    state_i <- states[i]
    sab_i <- subset(sab_states, state_code == state_i)
    pws_cross_i <- subset(pws_cross, pwsid %in% unique(sab_i$pwsid))
    ## Crosswalk x service area boundaries ##
    sab_cross <- merge(sab_i, pws_cross_i, by = "pwsid", all = TRUE)
    # writing - this needs to be an rds object since some of the columns are 
    # lists -- this will of course write to S3 but storing locally for now 
    readr::write_rds(sab_cross, paste0("./data/raw/", state_i, 
                                       "-sabcrosswalk-raw.rds"))  
    print(paste0("Data saved for ", state_i))
  }
}


download_data(states = c("TX", "OR"))

# SOP for automating R scripts on Macs: 
# install.packages("cronR")
# library(cronR)
# cronR::cron_rstudioaddin() -- use this interface to schedule batch-downloads 
# will use the the command line on your computer to execute -- consider this 
# more for collaborative data projects - maybe there's a different method 
# to consider here? -- I've set this up to run every hour using my command line
#
# alternative here using GitHub actions and wrapping all of this into a 
# package: 
# https://www.simonpcouch.com/blog/2020-12-27-r-github-actions-commit/


