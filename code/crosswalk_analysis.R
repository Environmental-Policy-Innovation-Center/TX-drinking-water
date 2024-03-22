# loading packages: 
library(aws.s3)
library(tidyverse)
library(leaflet)
library(sf)

# loading lists:
demo <- aws.s3::s3read_using(readRDS, 
                             object = "s3://tech-team-data/state-drinking-water/TX/clean/TX_demographic_list.RData")


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

# graph for viz: 
tx_test <- pop_comp %>%
  mutate(sdwis_crosswalk_per_off = ((SDWIS - crosswalk)/SDWIS)*100, 
         sdiws_div_croswalk = SDWIS/crosswalk)

ggplot(tx_test, aes(x = SDWIS, y = sdiws_div_croswalk)) + 
  geom_point()

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


######################
# checkin out that well data from EPA ORD 
######################
wells <- aws.s3::s3read_using(read.csv, 
                     object = "s3://tech-team-data/state-drinking-water/TX/raw/TX_bg_wellpop_EPAORD.csv") %>%
  janitor::clean_names()

well_pop <- wells %>%
  select(geoid, x2020_population, population_served_by_wells_2020) %>%
  mutate(x2020_population = readr::parse_number(x2020_population), 
         population_served_by_wells_2020 = readr::parse_number(population_served_by_wells_2020)) %>%
  mutate(population_not_wells = x2020_population - population_served_by_wells_2020, 
         percent_cws = (population_not_wells/x2020_population)*100)

# grabbing census geographies to plot this - using decennial census because 
# that's what EPA's well data is based off of: 
vars <- load_variables(2020, "pl")
bg_census <- get_decennial(
  geography = "block group",
  variables = "P1_001N",
  state = "TX",
  year = 2020, 
  geometry = TRUE
)

well_census <- merge(well_pop, bg_census, by.x = "geoid", by.y = "GEOID", 
                     all.x = TRUE) %>%
  st_as_sf()

well_pal <- colorNumeric(
  palette = viridis::mako(9),
  domain = well_census$percent_cws)

leaflet() %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels, group = "Toner Lite") %>%
  addPolygons(data = well_census,
              opacity = 0.9,
              # stroke = TRUE,
              # color = "black",
              color = ~well_pal(percent_cws),
              weight = 1,
              label = paste0("% CWS: ", round(well_census$percent_cws, 2))) %>%
  addLegend("bottomright",
            pal = well_pal,
            values = well_census$percent_cws,
            title = "% CWS",
            opacity = 1)

sum(well_census$x2020_population)
# population in TX (which excludes major cities, which are likely on CWS: 7,401,970)
# population on wells (which excludes major cities): 2,493,742

# so population on CWS (assuming population of 29,530,000): 27,036,258, or 
# 91.56% of the population in TX

# % population on wells (assuming population of 29,530,000): 8.11%


###############
# investigating overlapping sabs: 
##############
# hmm - what if we remove intersecting polygons: 
test <- st_intersection(demo$census, model = "closed")

test_summary <- test %>%
  select(pwsid, n.overlaps) %>%
  as.data.frame() %>%
  select(-geometry)

intersections <- merge(census, test_summary, by = "pwsid", all.y = T)

# finding total number of overlaps: 
intersections_mini <- intersections %>%
  group_by(pwsid) %>%
  summarize(n.overlaps.total = sum(n.overlaps))

# seeing where these are: 
inter_pal <- colorNumeric(
  palette = viridis::viridis(7),
  domain = intersections_mini$n.overlaps.total, 
  na.color = "grey")
leaflet() %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels, group = "Toner Lite") %>%
  addPolygons(data = intersections_mini,
              opacity = 0.7,
              color = ~inter_pal(n.overlaps.total),
              weight = 1, 
              label = paste0("Overlaps: ", intersections_mini$n.overlaps.total)) %>%
  addLegend("bottomright",
            pal = inter_pal,
            values = intersections_mini$n.overlaps.total,
            title = "num overlaps",
            opacity = 1)
# so really the polygon that gets flagged as the most intersections 
# are just the ones that surround cities, where you have a bunch of close sabs. 
# unfortunately the ones that I want to remove (i.e., utilities in Houston 
# that are circles and overlap almost completely with the utility that serves 
# the city), only have an overlap = 1, where houston has overlap = 261. 
# ideally, I'd want to keep houston but remove the smaller circles 

# slicing data based on number of intersections: 
ggplot(intersections_mini, aes(x = n.overlaps.total)) + 
  geom_histogram()

intersection_cap <- intersections_mini %>%
  filter(n.overlaps.total < 60)
demo$census %>%
  as.data.frame() %>%
  select(-geometry) %>%
  # filter(tier == 1) %>%
  # filter(primacy_agency_code == "TX") %>%
  filter(pwsid %in% intersection_cap$pwsid) %>%
  summarize(total_pop = sum(estimate_total_pop, na.rm = T))

# just grabbing tier 1: 44,096,938
# just grabbing tier 1 and primacy agency code == "TX": 44,055,111
# if you just grab sabs where number of intersections is < 60: 37,670,246


crop <- st_crop(demo$census, demo$census)
leaflet() %>%
  addProviderTiles(providers$CartoDB.VoyagerNoLabels, group = "Toner Lite") %>%
  addPolygons(data = crop,
              opacity = 0.7,
              color = "red",
              weight = 1)
