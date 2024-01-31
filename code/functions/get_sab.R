################################################################################
# Downloading SABs 
# Author: EmmaLi Tsai 
# Date: 12/1/2023
################################################################################
# download_sab(states = c("TX")) 
# 
# Function to download service area boundaries for a particular state. 
#
# Dependencies: 
#   dplyr filter, mutate, %>%
#   sf st_area, st_read, st_transform
# 
# Inputs: 
#   states:  character vector containing the abbreviation of the states of 
#            interest. Default is states = "TX"
#   crs: coordinate reference system to use 
# 
# Output: data frame containing pwsid data and population served based on 
#         SDWIS data
# 
# Example of use: download_sab(states = ("TX"), crs = ("ESRI:102296"))
################################################################################
get_sab <- function(states = ("TX"), crs = ("ESRI:102296")){
  library(dplyr)
  ## Service area boundaries: ##
  sab <- sf::st_read("https://www.hydroshare.org/resource/9ebc0a0b43b843b9835830ffffdd971e/data/contents/temm.gpkg")
  # minor cleaning and subsetting based on state pwsids
  sab_states <- sab %>%
    # filtering for both primacy agency code and state code
    filter(primacy_agency_code %in% states | state_code %in% states) %>%
    sf::st_transform(., crs = crs) %>%
    mutate(area_miles = as.numeric(sf::st_area(.))) %>%
    mutate(area_miles = area_miles/27880000) %>%
    mutate(pop_density = population_served_count / area_miles) %>%
    mutate(population_served_count = as.numeric(population_served_count))
  return(sab_states)
}


###
# DEPRECATED - moving to areal interpolation methods
###
################################################################################
# download_blockcrosswalk(pwsids = c("TX0610218", "TX0610220", "TX0610223"))
# 
# Function to download the block parcel crosswalk from Blue Conduit - used 
# for estimating pwsid characteristics (i.e., crosswalking census block 
# demographics with pwsid geometries)
#
# Dependencies: 
#   arcpullr get_table_layer
#   dplyr mutate, case_when
# 
# Inputs: 
#   pwsids:  character vector containing the pwsids of interest. Default is 
#            set to pwsids = c("TX0610218", "TX0610220", "TX0610223") as an 
#            example. 
# 
# Output: data frame containing the block parcel crosswalk for the pwsids 
#         provided. This includes the census block and the weight associated 
#         with it. 
# 
# Example of use: download_blockcrosswalk(pwsids = c("TX0610218", "TX0610220", "TX0610223"))
################################################################################
download_blockcrosswalk <- function(pwsids = c("TX0610218", "TX0610220", "TX0610223")){
  library(dplyr)
  # querying pwsids from REST services: 
  idString <- toString(sprintf("'%s'", pwsids)) 
  sql_fmt <- "pwsid in (%s)"
  sql <- sprintf(sql_fmt, idString)
  pws_cross <- arcpullr::get_table_layer(url = "https://services8.arcgis.com/SVa1M0pGzvtS1NP4/ArcGIS/rest/services/Public_Water_System_Census_Block_Crosswalk/FeatureServer/0", 
                                         where = sql)
  # fixing blocks missing a preceding zero: 
  pws_cross <- pws_cross %>%
    mutate(census_block = as.character(census_block),
           census_block = case_when(
             str_length(census_block) == 14 ~ paste0(0, census_block),
             TRUE ~ census_block
           ))
  return(pws_cross)
}

# IN THE FUTURE - would be cool of certain scripts could run on a schedule: 
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
