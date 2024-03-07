get_SDWIS_enforcement_summary <- function(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
                                  state = c("TX")){
  
  # Options here to handle scientific notation errors in web service requests: 
  options("scipen"=10, "digits"=4)
  library(dplyr)
  
  # First, determine how many total violation records there are: 
  enf_url <- paste0("https://data.epa.gov/efservice/SDW_VIOL_ENFORCEMENT/state/", state, "/COUNT")
  n_records_vi_xml <- xml2::read_xml(enf_url)
  n_records_vi_ls <- xml2::as_list(n_records_vi_xml)
  n_records_vi <- as.numeric(n_records_vi_ls$sdw_viol_enforcementList$sdw_viol_enforcement$REQUESTRECORDCOUNT)
  
  # Grabbing the first 100,000 records: 
  enf_raw <- data.table::fread(paste0("https://data.epa.gov/efservice/SDW_VIOL_ENFORCEMENT/state/", state, "/0:99999/csv"),
                                      colClasses = "character")
  # For loop requests records 100,000 to end and appends them to the first 
  # 100,000 records: 
  for (i in 1:(ceiling(n_records_vi/1e5)-1)) {
    print(i)
    enf_url <- paste0("https://data.epa.gov/efservice/SDW_VIOL_ENFORCEMENT/state/", state, "/")
    temp_url <- paste0(enf_url,
                       as.character(i*100000),
                       ":",
                       i*100000 + 99999,
                       "/CSV")
    dat <- data.table::fread(temp_url, colClasses = "character")
    enf_raw <- rbind(enf_raw, dat)
  }
  
  print("Final filtering")
  
  # tidying dates and adding year, decade, and length of violation: 
  sdwis_filt_enf <- enf_raw %>%
    filter(pwsid %in% pwsids) %>%
    mutate(enfdate = as.Date(enfdate, tryFormats = c("%Y-%m-%d")), 
           enf_year = year(enfdate), 
           enf_decade = enf_year - enf_year %% 10)
  
  # return: 
  return(sdwis_filt_enf)
}
