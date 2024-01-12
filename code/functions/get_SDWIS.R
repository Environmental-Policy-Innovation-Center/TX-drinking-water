################################################################################
# Downloading SDWIS data 
# Author: EmmaLi Tsai 
# Date: 12/7/2023
################################################################################
# It's  more efficient to pull all data from the api and then filter down - 
# Corrected from: https://github.com/Environmental-Policy-Innovation-Center/water_team/blob/main/data/raw_data/sdwis/sdwis_violation/downloader_sdwis_violation.Rmd
################################################################################
# get_SDWIS(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
#                cont_code = c())
# 
# Function to download SDWIS violation data for certain pwsids and contaminants. 
#
# Dependencies: 
#   dplyr filter 
#   xml2 read_xml, as_list
#   data.table fread
# 
# Inputs: 
#   pwsids:  character vector containing the pwsids of interest. Default is 
#            set to pwsids = c("TX0610218", "TX0610220", "TX0610223") as an 
#            example. 
#   cont_code: Character vector containing codes of contaminants of interest. 
#              Must be associated with a valid contaminant code recognized by 
#              the EPA. Default is set to include all violations. 
# 
# Output: data frame containing all contaminant records for the pwsid and 
#         contaminants of interest, 
# 
# Example of use: get_SDWIS(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
#                cont_code = c("1025", "0999"))
################################################################################
get_SDWIS <- function(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
                      cont_code = c()){
  
  # Options here to handle scientific notation errors in web service requests: 
  options("scipen"=10, "digits"=4)
  library(dplyr)
  
  # First, determine how many total violation records there are: 
  violation_url <- "https://data.epa.gov/efservice/VIOLATION/COUNT"
  n_records_vi_xml <- xml2::read_xml(violation_url)
  n_records_vi_ls <- xml2::as_list(n_records_vi_xml)
  n_records_vi <- as.numeric(n_records_vi_ls$violationList$violation$REQUESTRECORDCOUNT)
  # For loop requests records 100,000 to end and appends them to the first 
  # 100,000 records: 
  violations_raw <- data.frame()
  for (i in 1:(ceiling(n_records_vi/1e5)-1)) {
    print(i)
    violation_url <- "https://data.epa.gov/efservice/violation/ROWS/"
    temp_url <- paste0(violation_url,
                       as.character(i*100000),
                       ":",
                       i*100000 + 99999,
                       "/CSV")
    dat <- data.table::fread(temp_url, colClasses = "character")
    violations_raw <- rbind(violations_raw, dat)
  }
  
  print("Final filtering")
  
  # tidying dates and adding year, decade, and length of violation: 
  sdwis_filt <- violations_raw %>%
    filter(pwsid %in% pwsids) %>%
    mutate(across(compl_per_begin_date:compl_per_end_date, 
                  ~ as.Date(.x, tryFormats = c("%Y-%m-%d"))), 
           viol_year = year(compl_per_begin_date), 
           viol_decade = viol_year - viol_year %% 10,
           viol_dur = compl_per_end_date - compl_per_begin_date)
  
  if(length(cont_code != 0)){
    # filtering data based on provided arguments: 
    sdwis_filt <- sdwis_filt %>%
      filter(contaminant_code %in% cont_code)
  }
  
  # return: 
  return(sdwis_filt)
}


