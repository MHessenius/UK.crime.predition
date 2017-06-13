### Cleaning ####
crime <- readRDS("H:/RDS_files/crime.w_o.census.rds")

# get an overview about missing values
cols.w.na <- colnames(crime)[colSums(is.na(crime)) > 0]
sapply(crime[,c(cols.w.na)], function(x){sum(is.na(x))})

## First attempt:  Births
check <- crime[is.na(crime$Births),]
if(sum(check$Boroughs=="City of London")==nrow(check)){print("No data for City of London")}
crime$Births[is.na(crime$Births)] <- median(unique(crime$Births[!is.na(crime$Births)]))
# just a first suggestion....

##maybe write a loop to detect all columns where NAs are only due to "City of London"
check <- data.frame("dummy")

for(i in 1:length(cols.w.na)){
  check <- crime[is.na(crime[,cols.w.na[i]]),]
  
  if(sum(check$Boroughs=="City of London")==nrow(check)){
    print(paste(cols.w.na[i],"--", "No data for City of London"))
  }else{
  print(paste(cols.w.na[i],"--", "other reasons"))
    }
  
  rm(check)
}

