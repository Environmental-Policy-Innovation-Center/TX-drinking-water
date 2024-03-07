get_SDWIS_enforcement <- function(pwsids = c("TX0740039", "TX0750003", "TX0650002")){
  
  # Options here to handle scientific notation errors in web service requests: 
  options("scipen"=10, "digits"=4)
  library(dplyr)

    # First, determine how many total violation records there are: 
  enf_url <- paste0("https://data.epa.gov/efservice/ENFORCEMENT_ACTION/COUNT")
  n_records_vi_xml <- xml2::read_xml(enf_url)
  n_records_vi_ls <- xml2::as_list(n_records_vi_xml)
  n_records_vi <- as.numeric(n_records_vi_ls$enforcement_actionList$enforcement_action$REQUESTRECORDCOUNT)
  
  # Grabbing the first 100,000 records: 
  enf_raw <- data.table::fread(paste0("https://data.epa.gov/efservice/ENFORCEMENT_ACTION/ROWS/0:99999/csv"),
                               colClasses = "character")
  # For loop requests records 100,000 to end and appends them to the first 
  # 100,000 records: 
  for (i in 1:(ceiling(n_records_vi/1e5)-1)) {
    print(i)
    enf_url <- paste0("https://data.epa.gov/efservice/ENFORCEMENT_ACTION/ROWS/")
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
    mutate(enforcement_date = as.Date(enforcement_date, tryFormats = c("%Y-%m-%d")), 
           enf_year = year(enforcement_date), 
           enf_decade = enf_year - enf_year %% 10)
  
  # return: 
  return(sdwis_filt_enf)
}
