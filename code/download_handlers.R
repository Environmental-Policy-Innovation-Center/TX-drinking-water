################################################################################
# Downloading Drinking Water Datasets 
# Author: EmmaLi Tsai 
# Date: 12/1/2023
################################################################################
# Function takes a state acronym and pulls national drinking water data 
# the though here is that data will be uploaded to aws here into a larger 
# data lake 
download_data <- function(state = c("TX")) {
  # increase timeouts since some of these take a bit to download: 
  options(timeout=180)
  
  ## Service area boundaries: ##
  sab <- sf::st_read("https://www.hydroshare.org/resource/9ebc0a0b43b843b9835830ffffdd971e/data/contents/temm.gpkg")
  # minor cleaning: 
  sab_state <- sab %>% 
    dplyr::filter(state_code == state) %>%
    dplyr::mutate(area_miles = as.numeric(st_area(.))) %>%
    dplyr::mutate(area_miles = area_miles/27880000) %>%
    dplyr::mutate(pop_density = population_served_count / area_miles)

  ## Crosswalk: ##
  # grabbing pwsids from dataset above for querying: 
  sab_pwsids <- unique(sab_state$pwsid)
  # querying from REST services: 
  idString <- toString(sprintf("'%s'", sab_pwsids)) 
  sql_fmt <- "pwsid in (%s)"
  sql <- sprintf(sql_fmt, idString)
  pws_cross <- arcpullr::get_table_layer(url = "https://services8.arcgis.com/SVa1M0pGzvtS1NP4/ArcGIS/rest/services/Public_Water_System_Census_Block_Crosswalk/FeatureServer/0", 
                                         where = sql)
  
  ## Crosswalk x service area boundaries ##
  sab_cross <- merge(sab_state, pws_cross, by = "pwsid", all = TRUE)
  # writing - this needs to be an rds object since some of the columns are 
  # lists -- this will of course write to S3 but storing locally for now 
  readr::write_rds(sab_cross, paste0("./data/raw/", state, "_sabcrosswalk_raw.rds"))
  
}

# download_data(state = "TX")
# download_data(state = "OR")