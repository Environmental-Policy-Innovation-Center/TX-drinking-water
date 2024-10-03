################################################################################
# Function: state_sab_crosswalk(sab, state, crs, census_col_order)
# Date: June 19th 2024 
# Author: EmmaLi Tsai 
# 
# TODO - update below documentation for doing this by year 
# Function that takes water utility service area boundaries for a state 
# and applies our crosswalk method (population weighted interpolation and 
# parcel crosswalk) to estimate pre-defined census variables for the service 
# area. This function takes 30 minutes to run for the entire U.S. 
# 
# Args: 
# - sab_f: sf object containing service area boundaries for a single state
# - state: state that census boundaries should be pulled from. Should be the 
#        exact same as the service areas 
# - crs: the CRS that should be used for spatial calculations
# - census_col_order: order of census columns. This has to be IDENTICAL to 
#        the order that is pulled from the data/ folder - future iterations of 
#        this function could handle various column orders, but this was needed
#        to make calculations and data universe handling easier. 
# 
# output: 
# - An sf object containing the original SAB columns along with crosswalked 
#   census variables. There are three tiers of crosswalk types that vary by 
#   confidence. Tier 1 is population weighted interpolation, and Tier 2 is using
#   the tract parcel crosswalk from Blue Conduit to estimate census values for
#   small geographies. This method either uses the crosswalk population, or uses
#   the population reported by SDWIS for scaling raw counts, since previous 
#   analyses have found that this crosswalk consistently overestimates values. 
################################################################################
state_sab_crosswalk_year <- function(sab_f, state, year, block_weights, 
                                     crs) {
  
  ##############################################################################
  # Downloading and organizing data for crosswalk tiers: 
  ##############################################################################
  message(paste0("Working on: ", state))
  # making sure the sab crs matches with census and block weights for 
  # interpolating
  sab <- sab_f %>%
    st_transform(., crs = crs) %>%
    st_cast(.) %>%
    # these geometries cannot be crosswalked at the moment - they are 
    # recombined as tier 3 - geometry issues at the end of the function 
    filter(st_geometry_type(.) != "GEOMETRYCOLLECTION")
  
  # turn off spherical geometry - otherwise, errors are thrown during pw 
  # interpolation 
  sf_use_s2(FALSE)
  
  if(sum(!st_is_valid(sab)) > 0) {
    # needed to make multipolygons from EPAs dataset which has disconnected
    # polyons for pwsids and/or intersecting vertices 
    # NOTE: this DOES change the spatial footprint of the SAB, but valid geoms
    # is required for pw interpolation... there are a few states that have only
    # 8 invalid geoms, but TX has ~380
    sab <- sab %>%
      st_make_valid(., geos_method = "valid_linework")
  }

  # vector of pwsids for querying the tract parcel crosswalk: 
  pwsids <- unique(sab$pwsid)
  
  message("Grabbing Census Data")
  # all_census_vars <- load_variables(2020, "acs5", cache = TRUE)
  
  # grabbing state census stats - NOT making this a function arg to control
  # for column names and data universe calculations 
  census_vars <- c(total_pop = "B01003_001",
                   # race and ethnicity stats: 
                   black_alone = "B02001_003", 
                   asian_alone = "B02001_005", 
                   white_alone = "B02001_002", 
                   AIAN_alone = "B02001_004", 
                   NAPI_alone = "B02001_006", 
                   other_alone = "B02001_007", 
                   mixed_alone = "B02001_008",
                   hisp_alone = "B03003_003", 
                   # MHI
                   mhi = "B19013_001", 
                   # labor force and unemployment: NOT AVAILABLE IN 2010 
                   # laborforce = "B23025_003",  # universe: pop > 16
                   # laborforce_unemployed = "B23025_005", # universe: pop > 16
                   # households below poverty level: 
                   hh_total = "B17017_001",
                   hh_below_pov = "B17017_002", 
                   # population at or above 200% poverty level (to calculate 
                   # those below this threshold: reference doc- # https://www.oregon.gov/odot/RPTD/RPTD%20Committee%20Meeting%20Documents/STIF-Low-Income-Methods-Guidance.pdf
                   pop_above_200_pov = "C17002_008", # universe: pop that has income? unclear 
                   # age cats: 
                   ageunder_5  = "B06001_002",
                   age5_17 = "B06001_003", 
                   age18_24 = "B06001_004",
                   age25_34 = "B06001_005", 
                   age35_44 = "B06001_006", 
                   age45_54 = "B06001_007", 
                   age55_59 = "B06001_008", 
                   age60_61 = "B06001_009",
                   # language spoken at home: 
                   only_english = "B99162_002",  # universe: pop >5
                   other_lang = "B99162_003", # universe: pop >5
                   # education categories: NOT AVAILABLE IN 2010 
                   # no_school = "B15003_002", 
                   # high_school = "B15003_017", 
                   # bachelors = "B15003_022",
                   # prof_degree = "B15003_024", 
                   # nationality: 
                   foreign = "B99051_005")
  
  census <- tidycensus::get_acs(
    geography = "tract", 
    variables = census_vars, 
    state = state, 
    year = year,
    geometry = TRUE
  )
  
  # pivoting census to wide format: 
  census_wide <- pivot_wider(census, 
                             names_from = c("variable"), 
                             values_from = c("estimate", "moe"))
  
  # making sure the names line up (California's do not): 
  # some may be missing based on acs 5yr pull: 
  # census_col_order_fixed <- census_col_order %>% 
  #   select(names(census_wide))
  # census_wide <- census_wide[, census_col_order_fixed]
  census_wide <- census_wide %>%
    select(GEOID, estimate_total_pop:estimate_pop_above_200_pov) %>%
    st_transform(., crs = crs) 
  
  # calculating the percentage of the entire universe for interpolation: 
  census_wide <- census_wide %>%
    mutate(mhi_percent_uni = estimate_mhi / sum(census_wide$estimate_mhi, na.rm = T))
  
  # loading block weights for pw_interpolation within the tier 1 crosswalk: 
  message("Grabbing Census Block Weights")
  state_blocks <- block_weights %>%
    # standardizing col names based on weight decade: 
    rename_with(~ str_remove(., "[1-9].*"), everything())

  # tract crosswalk for tier 2 crosswalk:  
  message("Grabbing Tract Parcel Crosswalk")
  # TODO - this is from 2020 CENSUS TRACTS !!! crosswalk back to 2010???
  tract_crosswalk <- aws.s3::s3read_using(read.csv, 
                                          object = "s3://tech-team-data/pws_crosswalk/EPA_SABS_parcel_weighted_pwsid_census_tracts.csv") %>%
    select(-X) %>%
    filter(pwsid %in% pwsids) %>%
    # if the geoid is missing prefix 0, add it back in
    mutate(tract_geoid = as.character(tract_geoid),
           tract_geoid = case_when(
             str_length(tract_geoid) == 10 ~ paste0(0, tract_geoid),
             TRUE ~ tract_geoid
           )) %>%
    # there are some tract geoids in the crosswalk are NAs - assuming these are errors?
    filter(!(is.na(tract_geoid)))
  
  
  ################################################################################
  ## Tier 1: Population-weighted interpolation: 
  ################################################################################
  message("Starting Tier 1 Crosswalk: population weighted interpolation")
  ## pw interpolation on count vars where the universe is total population: 
  state_wide_pop <- census_wide %>% 
    select(GEOID, estimate_total_pop:mhi_percent_uni) %>%
    # leaving these columns out because their universe is different
    select(-c(estimate_hh_total, estimate_hh_below_pov, 
              estimate_mhi, mhi_percent_uni))
  
  message("Tier 1: population weighted interoplation on pop vars")
  pw_inter_pop <- interpolate_pw(
    from = state_wide_pop,
    to = sab,
    to_id = "pwsid",
    extensive = TRUE, 
    weights = state_blocks,
    weight_column = "POP", 
    crs = crs)
  
  ## Households below poverty: 
  state_wide_hh <- census_wide %>% 
    select(estimate_hh_total, estimate_hh_below_pov)
  
  # since we're using household data, should use housing20 as weights for
  # pw interpolation: 
  message("Tier 1: population weighted interoplation on household vars")
  pw_inter_hh <- interpolate_pw(
    from = state_wide_hh,
    to = sab,
    to_id = "pwsid",
    extensive = TRUE, 
    weights = state_blocks,
    weight_column = "HOUSING", 
    crs = crs)
  
  pwsid_hh <- pw_inter_hh %>%
    mutate(estimate_hh_below_pov_per = 100*(estimate_hh_below_pov/estimate_hh_total)) 
  
  ## Median household income: 
  state_wide_mhi <- census_wide %>% 
    select(mhi_percent_uni)
  
  # pop weighted interpolation on median household income using spatially 
  # intensive because we essentially want a weighted mean:
  message("Tier 1: population weighted interoplation on mhi")
  pw_interp_mhi <- interpolate_pw(
    from = state_wide_mhi,
    to = sab,
    to_id = "pwsid",
    extensive = FALSE, 
    weights = state_blocks,
    weight_column = "POP",
    crs = crs)
  
  # transforming percentage of universe to actual mhi: 
  pwsid_mhi <- pw_interp_mhi %>%
    mutate(estimate_mhi = mhi_percent_uni*sum(census_wide$estimate_mhi,
                                              na.rm = T)) 
  
  ## Bringing everything back together: 
  pwsid_mhi_df <- pwsid_mhi %>%
    as.data.frame() %>%
    select(-starts_with("geom"))
  
  pwsid_hh_df <- pwsid_hh %>%
    as.data.frame() %>%
    select(-starts_with("geom"))
  
  # all census data: 
  all_census <- pw_inter_pop %>%
    left_join(pwsid_mhi_df) %>%
    left_join(pwsid_hh_df)
  
  # filtering NAs for tier 1: 
  pw <- all_census %>%
    filter(!(is.na(estimate_total_pop) | estimate_total_pop == 0)) %>%
    mutate(tier_crosswalk = "tier_1")
  
  # calculating percentages: 
  pw_per <- pw %>%
    as.data.frame() %>%
    select(-starts_with("geom")) %>%
    select(starts_with(c("estimate", "pwsid"))) %>%
    mutate_at(vars(!c("estimate_total_pop", "estimate_mhi", "pwsid", 
                      "estimate_hh_total", "estimate_hh_below_pov", 
                      # "estimate_laborforce_unemployed", 
                      "estimate_hh_below_pov_per")), 
              ~((./estimate_total_pop)*100)) %>%
    relocate("pwsid") %>%
    select(-c(estimate_hh_below_pov_per, estimate_hh_below_pov, 
              estimate_mhi, estimate_total_pop, 
              # estimate_laborforce_unemployed, 
              estimate_hh_total))
  
  colnames(pw_per) <- paste0(colnames(pw_per), "_per")
  
  # merging percentages back with original counts and organizing columns: 
  pw_final <- merge(pw, pw_per,
                    by.x = "pwsid", by.y = "pwsid_per") %>%
    # recalculating % households in poverty and % labor force unemployed
    mutate(estimate_hh_below_pov_per = (estimate_hh_below_pov/estimate_hh_total)*100) %>%
           # estimate_laborforce_unemployed_per = (estimate_laborforce_unemployed/estimate_laborforce)*100) %>%
    # reorganizing for clarity:
    relocate(tier_crosswalk, .after = pwsid) %>%
    # relocate(estimate_laborforce_unemployed_per, .after = estimate_other_lang_per) %>%
    relocate(estimate_hh_below_pov_per, .after = estimate_other_lang_per) 
    # relocate(estimate_laborforce:estimate_laborforce_unemployed, .after = estimate_other_lang)
  
  
  ##############################################################################
  ## Tier 2: crosswalking using the blue conduit tract parcel crosswalk 
  ##############################################################################
  message("Starting Tier 2 Crosswalk: Tract Parcel Crosswalks")
  
  # locating the pwsids that fell out of pw interpolation: 
  xwalk <- all_census %>%
    filter(is.na(estimate_total_pop) | estimate_total_pop == 0) 
  
  # finding pwsids that exist in the tract parcel crosswalk. The crosswalk 
  # is missing tribal boundaries due to a parcel issue
  pwsid_cross <- tract_crosswalk %>%
    filter(pwsid %in% xwalk$pwsid) 
  
  # keeping geoid and prepping for merge with block parcel crosswalk: 
  # it's okay that mhi_percent_uni gets dropped, since we use a weighted mean 
  # instead
  state_wide_cross <- census_wide %>%
    select(GEOID, estimate_total_pop:estimate_pop_above_200_pov)
  
  # merging census stats with crosswalk - pwsids that exist outside of the 
  # state but have state == "state abbr" will be dropped here 
  pwsid_xwalk <- merge(state_wide_cross, pwsid_cross, 
                       by.x = "GEOID", by.y = "tract_geoid", all.y = T)
  
  # multiplying census stats by tract weights:
  pwsid_weight_xwalk <- pwsid_xwalk %>% 
    mutate(across(estimate_total_pop:estimate_hh_below_pov, ~.*tract_parcel_weight)) %>%
    mutate(across(estimate_foreign:estimate_pop_above_200_pov, ~.*tract_parcel_weight))
  
  # sum across pwsid and estimate mhi using a weighted mean: 
  pwsid_weighted_xwalk <- pwsid_weight_xwalk %>%
    group_by(pwsid) %>% 
    summarize(across(estimate_total_pop:estimate_hh_below_pov, ~round(sum(.), 2)), 
              estimate_mhi = weighted.mean(estimate_mhi, tract_parcel_weight, na.rm = TRUE), 
              across(estimate_foreign:estimate_pop_above_200_pov, ~round(sum(.), 2))) 
  
  # calculating percentage: 
  pwsid_weighted_per <- pwsid_weighted_xwalk %>%
    as.data.frame() %>%
    select(-starts_with("geom")) %>%
    select(starts_with(c("estimate", "pwsid"))) %>%
    mutate_at(vars(!c("estimate_total_pop", "estimate_mhi", "pwsid")), 
              ~((./estimate_total_pop)*100)) %>%
    relocate("pwsid") 
  
  colnames(pwsid_weighted_per) <- paste0(colnames(pwsid_weighted_per), "_per")
  
  # merging percentages back with original counts: 
  pwsid_xwalked <- merge(pwsid_weighted_xwalk, pwsid_weighted_per,
                         by.x = "pwsid", by.y = "pwsid_per") %>%
    # these columns don't mean anything, but made the calculations above easier: 
    select(-c("estimate_total_pop_per", "estimate_mhi_per", "estimate_hh_total_per")) %>%
    # recalculating % households in poverty and % labor force unemployed
    mutate(estimate_hh_below_pov_per = (estimate_hh_below_pov/estimate_hh_total*100))
           # estimate_laborforce_unemployed_per = (estimate_laborforce_unemployed/estimate_laborforce)*100)
  
  
  message("Tier 2 Crosswalk: Capping Population at SDWIS")
  # want to cap max total pop to those reported by SDWIS - find those whose 
  # max total pop was more than what was reported by SDWIS and scale 
  # appropriately 
  sdwis_pop <- sab %>%
    as.data.frame() %>%
    select(-starts_with("geom")) %>%
    select(pwsid, population_served_count)
  
  sdwis_xwalk <- pwsid_xwalked %>%
    left_join(sdwis_pop) %>%
    relocate(population_served_count, .before = estimate_total_pop) 
  
  # capping max pop at SDWIS values: 
  sdwis_pop <- sdwis_xwalk %>%
    filter(estimate_total_pop > population_served_count) 
  
  # recalculating raw counts 
  sdwis_counts <- sdwis_pop %>%
    as.data.frame() %>%
    select(-geometry) %>%
    pivot_longer(., estimate_white_alone_per:estimate_pop_above_200_pov_per) %>%
    select(pwsid, population_served_count, estimate_total_pop, name, value) %>%
    mutate(raw_count = (value/100) * population_served_count, 
           col_name = gsub("_per", "", name)) %>%
    select(pwsid, population_served_count, estimate_total_pop, 
           raw_count, col_name) %>%
    # pivoting back to wide format to prep for merging: 
    pivot_wider(., names_from = col_name, values_from = raw_count) %>%
    mutate(estimate_total_pop = population_served_count,
           # we can no longer estimate raw number of households and households
           # below poverty using this method since we don't have a reference 
           # val from SDWIS. We can keep the percentage since that would be 
           # constant across tier 2 methods. 
           estimate_hh_total = NA,
           estimate_hh_below_pov = NA, 
           tier_crosswalk = "tier_2_sdiwspop")
  
  
  # combining this back with percentages: 
  sdwis_per <- sdwis_pop %>% 
    select(pwsid, estimate_mhi, 
           estimate_white_alone_per:estimate_pop_above_200_pov_per) 
  
  sdwis_pop_final <- merge(sdwis_counts, sdwis_per, by = "pwsid") 
  
  # IF statement if the dataframe above is empty because all of the pwsids
  # didn't match to the crosswalk - they either don't have parcel data or start
  # with "KYDOT" 
  if(nrow(sdwis_pop_final) != 0) {
    # sdwis_pop_final <- sdwis_pop_final %>%
    #   # fixing raw laborforce unemployed raw count, since the universe is not 
    #   # total population: 
    #   mutate(estimate_laborforce_unemployed = estimate_laborforce*(estimate_laborforce_unemployed_per/100))
      
    # recombining with other pwsids where xwalk < SDWIS: 
    xwalk_pop <-  sdwis_xwalk %>%
      filter(estimate_total_pop < population_served_count) %>%
      mutate(tier_crosswalk = "tier_2_xwalkpop")
    
    # rbinding with final xwalk: 
    sdwis_pop_final <- sdwis_pop_final[, names(xwalk_pop)]
    
    final_xwalk <- rbind(sdwis_pop_final, xwalk_pop) %>%
      select(-population_served_count) %>%
      mutate(mhi_percent_uni = NA) %>%
      st_as_sf() %>%
      st_transform(., st_crs(pw))
    
    # matching names for future binding: 
    final_xwalk <- final_xwalk[, names(pw_final)]
    
  } else { 
    # if this dataframe is empty, just take sdwis_pop_final and do some 
    # tidying to prep for a merge: 
    final_xwalk <- sdwis_pop_final %>%
      select(-population_served_count) %>%
      mutate(mhi_percent_uni = NA) %>%
      st_as_sf() %>%
      st_transform(., st_crs(pw))
    }

  
  
  ##############################################################################
  # Combining tier 1 and 2 methods & tier 3 designation: 
  ##############################################################################
  message("Recombining Data")
  
  final_pw_xwalk <- rbind(pw_final, final_xwalk)
  
  # just need to combine with missing pwsids, with NAs for all columns 
  missing <- sab %>%
    filter(!(pwsid %in% final_pw_xwalk$pwsid))
  
  # if statement since some states don't have any tier 3 crosswalked pwsids: 
  if(nrow(missing) != 0) {
    
    # adding these back in to the overall dataset, but as tier 3: 
    missing_df <- final_pw_xwalk[nrow(final_pw_xwalk)+1:nrow(missing),] 
    missing_df$pwsid <- missing$pwsid 
    missing_df <- missing_df %>%
      as.data.frame() %>%
      select(-geometry) %>%
      mutate(tier_crosswalk = "tier_3")
    
    # adding back geometries 
    missing_sab_geoms <- missing %>%
      select(pwsid)
    
    missing_sf <- missing_df %>%
      left_join(missing_sab_geoms) %>%
      st_as_sf() %>%
      st_transform(., crs = st_crs(final_pw_xwalk)) 
    
    # handling different geometry colnames
    if("geom" %in% names(missing_sf)){
      missing_sf <- missing_sf %>%
        rename(geometry = geom)
    }
    
    # completing crosswalk through rbinding: 
    complete_crosswalk <- rbind(final_pw_xwalk, missing_sf)
    complete_crosswalk_df <- complete_crosswalk %>% 
      # calculating % POC and relocating certain columns: 
      mutate(estimate_poc_alone_per = 100 - estimate_white_alone_per, 
             estimate_pop_below_200_pov_per = 100 - estimate_pop_above_200_pov_per) %>%
      relocate(estimate_poc_alone_per, .after = estimate_mixed_alone_per) %>%
      as.data.frame() %>%
      # we don't need these columns anymore
      select(-c(geometry, mhi_percent_uni))
    
    # merge back with sab information: 
    final_sab <- merge(sab, complete_crosswalk_df, 
                       by = "pwsid") %>%
      mutate(crosswalk_state = state) %>%
      relocate(crosswalk_state)
    
  } else {
    # if there are no tier 3 records, merge the tier 1 and 2 crosswalks back 
    # to the original sab dataset: 
    final_pw_xwalk_df <- final_pw_xwalk %>%
      as.data.frame() %>%
      # calculating % POC and relocating certain columns: 
      mutate(estimate_poc_alone_per = 100 - estimate_white_alone_per, 
             estimate_pop_below_200_pov_per = 100 - estimate_pop_above_200_pov_per) %>%
      relocate(estimate_poc_alone_per, .after = estimate_mixed_alone_per) %>%
      as.data.frame() %>%     
      select(-starts_with("geom"))%>% 
      # we don't need this columns anymore
      select(-c(mhi_percent_uni))
    
    final_sab <- merge(sab, final_pw_xwalk_df, 
                       by = "pwsid") %>%
      mutate(crosswalk_state = state) %>%
      relocate(crosswalk_state)
  }
  
  ##############################################################################
  # Handling geometycollections or linestrings: 
  ##############################################################################
  if(nrow(final_sab) != nrow(sab_f)){
    # finding missing sabs, and adding them back in as tier 3 geom issue: 
    odd_geoms <- sab_f %>%
      filter(!(pwsid %in% final_sab$pwsid)) %>%
      st_transform(crs = st_crs(final_sab)) %>%
      rename(geometry = geom) %>%
      mutate(tier_crosswalk = "tier_3_geom_issue", 
             crosswalk_state = state) 
    
    final_sab <- bind_rows(final_sab, odd_geoms)
    
    }
  
  
  # returning: 
  return(final_sab)
  
}
