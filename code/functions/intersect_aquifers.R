# Intersect aquifers - function to split aquifer boudaries and run 
# well intersections. 
# 
# wells: point layer from twdb showing well locations 
# aquifers: boundary of major or minor aquifer boundaries 
# aquifer_name_col: name of the column for splitting aquifer boundaries 
#     (i.e., name)
# 
# returns an sf of wells that intersect with each aquifer supplied. 
# 
# example use case: 
# intersect_aquifers(wells = well_twdb, aquifers = major_aquifers, aquifer_name_col = "aq_name")
intersect_aquifers <- function(wells, aquifers, aquifer_name_col) {
  
  # grabbing unique aquifer names: 
  aquifer_names <- aquifers %>%
    as.data.frame(.) %>%
    select(!!sym(aquifer_name_col)) %>%
    unique(.)
  
  # dataframe to store intersections
  well_aquifers <- data.frame()
  
  # looping through aquifer names: 
  for (i in 1:nrow(aquifer_names)) {
    message(paste0("Workin on: ", aquifer_names[i, 1]))
    
    # filtering and intersecting on aquifer: 
    aquifer_i <- aquifers %>%
      filter(!!sym(aquifer_name_col) == aquifer_names[i, 1])
    
    intersect_i <- st_intersection(wells, 
                                   aquifer_i)
    
    # rbinding to data frame: 
    well_aquifers <- rbind(well_aquifers, intersect_i)
    
  }
  return(well_aquifers)
}
