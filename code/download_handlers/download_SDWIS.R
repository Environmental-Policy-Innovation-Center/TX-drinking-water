################################################################################
# Downloading SDWIS data 
# Author: EmmaLi Tsai 
# Date: 12/7/2023
################################################################################
# Function downloads data from SDWIS
################################################################################
download_SDWIS <- function(pwsids = c("TX0740039", "TX0750003", "TX0650002"), 
                           cont_code = c("1025")){
  # grabbing starting dataframe to append to: 
  violations_df <- data.frame()
  for(i in 1:length(pwsids)){
    print(paste0(pwsids[i], ": ", i, " out of ", length(pwsids)))
    data_link_i <- paste0("https://data.epa.gov/efservice/violation/CONTAMINANT_CODE/", 
                          cont_code, 
                          "/pwsid/", 
                          pwsids[i], "/ROWS/0:99999/CSV")
    
    # NOTE: a check is needed regarding whether there are pwsids with more than 
    # 100,000 violations
    # NOTE: some pwsids don't have any violations (ex. TX1010902), and will
    # throw an error - a trycatch function is needed here to ensure that the 
    # loop continues to run 
    # source: https://stackoverflow.com/questions/8093914/use-trycatch-skip-to-next-value-of-loop-upon-error
    possibleError <- tryCatch(
      possibleError <- data.table::fread(data_link_i,
                                        colClasses = "character"),
      error=function(e) e
    )
    if(inherits(possibleError, "error")) next
    
    # if the pwsid has violations, then: 
    violations_i <- data.table::fread(data_link_i,
                                      colClasses = "character")
    # appending to original df: 
    violations_df <- rbind(violations_df, violations_i)
  }
  
  return(violations_df)
}
