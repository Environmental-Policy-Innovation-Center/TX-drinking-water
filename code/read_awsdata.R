# reading data fromt he aws data lake: 
# source: https://medium.com/@som028/how-to-read-and-write-data-from-and-to-s3-bucket-using-r-3fed7e686844
readFromS3 = function(filename, bucket, sep = ','){
  return(s3read_using(FUN=read.csv,
                      bucket = bucket,
                      object=filename,
                      sep = sep, header=T))
}

OR_crosswalk <- readFromS3('state-drinking-water/OR/raw/OR-sabcrosswalk-raw.rds', 'tech-team-data')
# hmmmmmm some weird things happening here from the rds -> aws -> back into R 
# okay if I download it from aws it looks great - so the issue is with the 
# readFromS3 function: 
# test <- readRDS("./data/raw/OR-sabcrosswalk-raw-aws.rds")
