# figuring out differences in population estimates: 
census <- demo$census
crosswalk <- census$estimate_total_pop
sdwis <- census$population_served_count

pop_comp <- census %>%
  select(pwsid, population_served_count, estimate_total_pop)

# trying areal interpolation: 
# grabbing census tracts: 
tract_geo <- tidycensus::get_acs(
  geography = "tract", 
  variables = "B01003_001", 
  state = c("TX"),
  year = 2021,
  geometry = TRUE
)

tract_geo <- tract_geo %>%
  st_transform(., crs = "ESRI:102296") %>%
  select(GEOID, estimate)

 census_sf <- census %>%
   st_transform(., crs = "ESRI:102296") %>%  
   filter(!st_is_empty(.))%>%
   select(pwsid, geometry)

interpolate <- areal::aw_interpolate(census_sf, tid="pwsid", 
                              source=tract_geo, 
                              sid="GEOID", 
                              weight = "sum",
                              output="sf", 
                              extensive="estimate")

# all estimates - a comparison: 
pop_comp <- census %>%
  as.data.frame() %>%
  select(pwsid, population_served_count, estimate_total_pop) %>%
  left_join(interpolate) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  rename(SDWIS = population_served_count, 
         crosswalk = estimate_total_pop, 
         areal_interp = estimate)

pop_comp_long <- pivot_longer(pop_comp, crosswalk:areal_interp)
ggplot(pop_comp_long, aes(x = SDWIS, y = value, color = name)) + 
  geom_point(alpha = 0.5) + 
  geom_line(aes(x = SDWIS, y = SDWIS), color = "black", lty = "dashed") +
  xlim(0,500000) + 
  ylim(0,500000) + 
  theme_minimal()

# raw numbers: 
crosswalk <- census$estimate_total_pop
sdwis <- census$population_served_count
sum(crosswalk, na.rm = T) # 44,291,253
sum(sdwis, na.rm = T) # 29,080,650
sum(interpolate$estimate, na.rm = T) # 28,855,410

# Texas population: 28,862,581
tidycensus::get_acs(
  geography = "state", 
  variables = "B01003_001", 
  state = c("TX"),
  year = 2021,
  geometry = TRUE
)

###########################
# focusing more on East TX: 
###########################
east_tx_cws <- census %>%
  filter(east_tx_flag == "yes")
sum(east_tx_cws$estimate_total_pop, na.rm = T) # crosswalk: 4,332,072
sum(east_tx_cws$population_served_count, na.rm = T) # sdwis: 2,249,752

# grabbing East TX stats: 
etx_census <- tidycensus::get_acs(
  geography = "county", 
  variables = "B01003_001", 
  state = c("TX"), 
  year = 2021,
  geometry = TRUE
)

counties <- unlist(strsplit(etx_census$NAME, split = ","))
etx_census$counties <- trimws(counties[grepl("County", counties)])

east_tx <- read.csv("./data/raw/east_TX_counties.csv") %>%
  select(-X) %>%
  mutate(county_tidy = paste0(county_name, " County"))
# running an intersection with east TX geography
east_tx_geo <- etx_census %>%
  filter(counties %in% east_tx$county_tidy) 

sum(east_tx_geo$estimate) # 1,916,924

# what about areal interp?
east_pwsid <- keys$analysis_keys 
east_pwsid <- east_pwsid %>%
  filter(east_tx_flag == "yes")

east_interp <- interpolate %>%
  filter(pwsid %in% east_pwsid$pwsid)
sum(east_interp$estimate)  # interpolating: 2,022,340



# TX-wide summary: 
# internet pop: 29,530,000
# census pop: 28,862,581
# crosswalk pop: 44,291,253
# SDWIS pop: 29,080,650
# areal pop: 28,855,410
#
# East TX: 
# internet pop: 1,918,718 (https://www.east-texas.com/east-texas-maps.htm - where we got our east TX counties)
# census pop: 1,916,924
# crosswalk pop: 4,332,072
# SDWIS pop: 2,249,752
# areal pop: 2,022,340


tx_test <- pop_comp %>%
  mutate(sdwis_crosswalk_per_off = ((SDWIS - crosswalk)/SDWIS)*100, 
         sdiws_div_croswalk = SDWIS/crosswalk)

ggplot(tx_test, aes(x = SDWIS, y = sdiws_div_croswalk)) + 
  geom_point()
