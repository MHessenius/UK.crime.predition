### Cleaning ####
crime <- readRDS("H:/RDS_files/crime.w_o.census.rds")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

## get an overview about missing values
cols.w.na <- colnames(crime)[colSums(is.na(crime)) > 0]
sapply(crime[,c(cols.w.na)], function(x){sum(is.na(x))})

## First attempt:  Births
check <- crime[is.na(crime$Births),] # b/c in births only data for london are lacking

if(sum(check$Boroughs=="City of London")==nrow(check)){print("No data for City of London")}
crime$Births[is.na(crime$Births)] <- median(unique(crime$Births[!is.na(crime$Births)]))
# just a first suggestion....

##write a loop to detect all columns where NAs are only due to "City of London"

check <- data.frame("dummy")
list <- data.frame(cols.w.na) 
list[,"outcome"] <- "dummy" #store the results in there

for(i in 1:length(cols.w.na)){
  check <- crime[is.na(crime[,cols.w.na[i]]),]
  
  if(sum(check$Boroughs=="City of London")==nrow(check)){
    list$outcome[i] <- "No data for City of London"
  }else{
    list$outcome[i] <- "other reasons"
  }
  
  rm(check)
}

#add a dummy (1 = "No data for City of London)
list$dummy <- ifelse(list$outcome=="No data for City of London", 1,0)

#add the class of each column to the dataset

list[,"class"] <- "dummy" #store the results in here
list$class <-  lapply(crime[,c(cols.w.na)], function(x){class(x)})

# next step: replace all NAs by median
# before, we need to change all into numeric/integer classes

list$convert <- ifelse(list$class=="factor" & list$dummy==1,1,0)

cols <- list$cols.w.na[which(list$convert==1)]  #store all columns that need to be converted 

crime$Perc.resd.pop.born.abroad <- as.integer(crime$Perc.resd.pop.born.abroad)
crime$Employment_Rate <- as.integer(crime$Employment_Rate)
crime$Unemployment_Rate <- as.integer(crime$Unemployment_Rate)
crime$General.Fertility.Rates <- as.integer(crime$General.Fertility.Rates)
crime$Perc.2nd.school.lang.other.than.english <- as.integer(crime$Perc.2nd.school.lang.other.than.english)
crime$X..of.economically.active.with.NVQ4....working.age <- as.integer(crime$X..of.economically.active.with.NVQ4....working.age)
crime$Crime.Rates.per.thousand.population <- as.integer(crime$Crime.Rates.per.thousand.population)
crime$People.aged.17..with.diabetes..perc. <- as.integer(crime$People.aged.17..with.diabetes..perc.)

# check outcome
list[,"class_v2"] <- "dummy" #store the results in here
list$class_v2 <-  lapply(crime[,c(cols.w.na)], function(x){class(x)})

# replace by median --> all dummies == 1
cols <- list$cols.w.na[which(list$dummy==1)]

cols.id <- list() #get the column index
for(i in 1:length(cols)){
  cols.id[i] <- which(colnames(crime)==cols[i])
}

for(i in 1:length(cols)){
  crime[is.na(crime[,cols[i]]), unlist(cols.id[i])] 
  cols.id[i] <- which(colnames(crime)==cols[i])
}




for(i in 1:length(cols)){
  crime[,cols[i]] <- median(unique(crime[,cols[i]]))
  
}
sapply(crime[,c(cols.w.na)], function(x){sum(is.na(x))})



for(i in 1:length(cols)){
  crime[is.na(crime[,cols[i]]), cols[i]] <- mean(crime[,cols[i]], na.rm = TRUE)
}

#copy&paste:
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
#adapt it
for(i in 1:unlist(cols.id)){
  crime[is.na(crime[,i]), i] <- median(unique(crime[,i], na.rm = TRUE))
}

cols.id.v2 <- unlist(cols.id)
cols.id.v2 <- data.frame(cols.id.v2)

sumofNAs <- list()
for(i in unlist(cols.id)){
  sumofNAs[i] <- sum(is.na(crime[,i]))
  
}