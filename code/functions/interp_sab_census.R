################################################################################
# Crosswalking SABs and Census data using areal interpolation methods 
# Author: EmmaLi Tsai 
# Date: 12/1/2023
################################################################################
# interp_sab_census(census_df = census, sab_df = sab, 
#           total_pop_var = "estimate_total_pop", 
#           white_alone_var = "estimate_white_alone")
# 
# Function to interpolate SABs and census data using areal interpolation methods. 
# All variables estimated from the census are passed as spatially extensive 
# variables for inteprolation. 
#
# Dependencies: 
#   sf st_crs, st_transform
#   dplyr select, mutate, relocate
#   areal aw_interpolate
# 
# Inputs: 
#   census_df : census data from acs
#   
#   sab_df : service area boundaries 
#
#   total_pop_var: variable from census that holds total population 
#
#   white_alone_var: variable from census that holds white_alone to calculate
#             percent POC
#  
# Output: data frame containing crosswalked census values for each pwsid
# 
# Example of use: interp_sab_census(census_df = census, sab_df = sab, 
#                 total_pop_var = "estimate_total_pop", 
#                 white_alone_var = "estimate_white_alone")
# TODO: Flag for rural or non-rural here? 
# TODO: brainstorm how to recalculate moe and mhi.. summing them is wrong. 
# TODO: this needs to be edited if someone passes decennial census info
################################################################################
interp_sab_census <- function(census_df = census, sab_df = sab, 
                      total_pop_var = "estimate_total_pop", 
                      white_alone_var = "estimate_white_alone"){
  
  print("Tidying data")
  # adding counties
  census_tidy <- census_df
  counties <- unlist(strsplit(census_tidy$NAME, split = ","))
  census_tidy$counties <- trimws(counties[grepl("County", counties)])
  
  # sab copying: 
  sab_tidy <- sab_df 
  sab_crs <- sf::st_crs(sab_tidy)$input
  
  # tidying census data and crs transformation: 
  census_wide <- pivot_wider(census_tidy, 
                             names_from = c("variable"), 
                             values_from = c("estimate", "moe"))  %>%
    st_transform(crs= sab_crs)
  
  # grabbing variables to crosswalk
  census_vars <- names(census_wide)
  ext_varList <- census_vars[grepl("estimate", census_vars)]

  # sab and block group areal interpolation - note this takes quite a bit of time 
  # to run
  # Intensive interpolation also isn't right because they're not density or 
  # % values... this pub notes that this method for MHI doesn't exist yet
  # https://www.mdpi.com/2220-9964/8/7/302
  print("Interpolating")
  sab_bg_ext <- areal::aw_interpolate(sab_tidy, tid = "pwsid", 
                                      source = census_wide, 
                                      sid = "GEOID", 
                                      weight = "total", 
                                      output = "sf", 
                                      extensive = ext_varList)
  
  # calculating percentage across census vars: 
  sab_census_per <- sab_bg_ext %>%
    as.data.frame(.) %>%
    select(starts_with(c("estimate", "pwsid"))) %>%
    # select(-c("estimate_mhi")) %>%
    mutate_at(vars(!c(!!(sym(total_pop_var)), "pwsid")), 
              ~ ((./!!(sym(total_pop_var)))*100)) %>%
    relocate("pwsid") %>%
    mutate(estimate_poc_alone = (100 - !!sym(white_alone_var))) %>%
    select(-c(!!(sym(total_pop_var))))
  # renaming columns for merging purposes: 
  colnames(sab_census_per) <- paste0(colnames(sab_census_per), "_per")
  
  # merging this back into the original interpolation: 
  sab_census <- merge(sab_bg_ext, sab_census_per, 
                      by.x = "pwsid", by.y = "pwsid_per") 
  
  return(sab_census)
  
}