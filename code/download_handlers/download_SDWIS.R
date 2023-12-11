################################################################################
# Downloading SDWIS data 
# Author: EmmaLi Tsai 
# Date: 12/7/2023
################################################################################
# Function downloads data from SDWIS: 
################################################################################
# It's  more efficient to pull all data from the api and then filter down - 
# here's a possible second function: 
# Corrected from: https://github.com/Environmental-Policy-Innovation-Center/water_team/blob/main/data/raw_data/sdwis/sdwis_violation/downloader_sdwis_violation.Rmd
download_SDWIS <- function(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
                              cont_code = c("1025", "0999")){
  
  # Options here to handle scientific notation errors in web service requests: 
  options("scipen"=10, "digits"=4)
  library(dplyr)
  
  # First, determine how many total violation records there are: 
  violation_url <- "https://data.epa.gov/efservice/VIOLATION/COUNT"
  n_records_vi_xml <- xml2::read_xml(violation_url)
  n_records_vi_ls <- xml2::as_list(n_records_vi_xml)
  n_records_vi <- as.numeric(n_records_vi_ls$violationList$violation$TOTALQUERYRESULTS)
  
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
  # filtering data based on provided arguments: 
  sdwis_filt <- violations_raw %>%
    filter(pwsid %in% pwsids) %>%
    filter(contaminant_code %in% cont_code)
  
  # return: 
  return(sdwis_filt)
}

# on older verion of the functiont that looped through each pwsid which was 
# GRUELING for a state as large as TX
# download_SDWIS_v2 <- function(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
#                            cont_code = c("1025")){
#   # grabbing starting dataframe to append to: 
#   violations_df <- data.frame()
#   for(i in 1:length(pwsids)){
#     print(paste0(pwsids[i], ": ", i, " out of ", length(pwsids)))
#     data_link_i <- paste0("https://data.epa.gov/efservice/violation/CONTAMINANT_CODE/", 
#                           cont_code, 
#                           "/pwsid/", 
#                           pwsids[i], "/ROWS/0:99999/CSV")
#     
#     # NOTE: a check is needed regarding whether there are pwsids with more than 
#     # 100,000 violations
#     # NOTE: some pwsids don't have any violations (ex. TX1010902), and will
#     # throw an error - a trycatch function is needed here to ensure that the 
#     # loop continues to run 
#     # source: https://stackoverflow.com/questions/8093914/use-trycatch-skip-to-next-value-of-loop-upon-error
#     possibleError <- tryCatch(
#       possibleError <- data.table::fread(data_link_i,
#                                         colClasses = "character"),
#       error=function(e) e
#     )
#     if(inherits(possibleError, "error")) next
#     
#     # if the pwsid has violations, then: 
#     violations_i <- data.table::fread(data_link_i,
#                                       colClasses = "character")
#     # appending to original df: 
#     violations_df <- rbind(violations_df, violations_i)
#   }
#   # lil sound when the function is complete: 
#   beepr::beep(2)
#   return(violations_df)
# }



