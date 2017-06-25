### Cleaning ####

## General Setup
crime <- readRDS("H:/RDS_files/crime.w_o.census.rds")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

## get an overview about missing values
cols.w.na <- colnames(crime)[colSums(is.na(crime)) > 0] #save all columns that contain NAs
sapply(crime[,c(cols.w.na)], function(x){sum(is.na(x))})

## First approach: in some cases data is only lacking for 'city of Lonodn'
check <- crime[is.na(crime$Births),] # b/c in births only data for london are lacking
if(sum(check$Boroughs=="City of London")==nrow(check)){print("No data for City of London")}

##write a loop to detect all columns where NAs are only due to "City of London"

check <- data.frame("dummy")
list <- data.frame(cols.w.na) 
list[,"outcome"] <- "dummy" #store the results in here

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

# to calculate the median, we need to change all into numeric/integer classes

list$convert <- ifelse(list$class=="factor" & list$dummy==1,1,0) #those need to be converted

cols <- list$cols.w.na[which(list$convert==1)]  #store all columns that need to be converted 

# convert all manually
crime$Perc.resd.pop.born.abroad <- as.integer(crime$Perc.resd.pop.born.abroad)
crime$Employment_Rate <- as.integer(crime$Employment_Rate)
crime$Unemployment_Rate <- as.integer(crime$Unemployment_Rate)
crime$General.Fertility.Rates <- as.integer(crime$General.Fertility.Rates)
crime$Perc.2nd.school.lang.other.than.english <- as.integer(crime$Perc.2nd.school.lang.other.than.english)
crime$X..of.economically.active.with.NVQ4....working.age <- as.integer(crime$X..of.economically.active.with.NVQ4....working.age)
crime$Crime.Rates.per.thousand.population <- as.integer(crime$Crime.Rates.per.thousand.population)
crime$People.aged.17..with.diabetes..perc. <- as.integer(crime$People.aged.17..with.diabetes..perc.)

# check classes after convertion
list[,"class_v2"] <- "dummy" #store the results in here
list$class_v2 <-  lapply(crime[,c(cols.w.na)], function(x){class(x)})

# replace by median --> all dummies == 1

cols <- list$cols.w.na[which(list$dummy==1)]
cols.id <- list() #get the column index

for(i in 1:length(cols)){
  cols.id[i] <- which(colnames(crime)==cols[i])
}

# For-loop to replace NAs by the median
for(i in unlist(cols.id)){
  crime[is.na(crime[,i]), i] <- median(unique(crime[,i]), na.rm = TRUE)
  
  #track the progress of the loop:
  message('Processing image ', i, ' of ', length(cols.id))
}

# for a better overview - store the col.id in the list-table
list$cols.id <- lapply(list$cols.w.na, function(x){which(colnames(crime)==x)})
list$cols.id <- as.numeric(list$cols.id)

# check the NAs again
list$sum_nas <- lapply(list$cols.id, function(x){sum(is.na(crime[,x]))})
list$sum_nas <- as.numeric(list$sum_nas)

# replace some columns manually
crime[is.na(crime$Crime.ID),"Crime.ID"] <- "NA"
crime[crime$Crime.ID=="","Crime.ID"] <- "NA"
crime[is.na(crime$Last.outcome.category),"Last.outcome.category"] <- "NA"
crime[crime$Last.outcome.category=="","Last.outcome.category"] <- "NA"
crime[is.na(crime$Context),"Context"]<- "NA"

# convert the remaining columns
list$convert_v2 <- ifelse(list$class_v2 == "factor", 1,0)

for(i in list$cols.id[list$convert_v2==1]){
    crime[,i] <- as.integer(crime[,i])
}

#check outcome again
list$class_v3 <- lapply(crime[,c(cols.w.na)], function(x){class(x)})
list$class_v3 <- as.character(list$class_v3)

#replace certain columns by median
for(i in list$cols.id[list$convert_v2==1]){
  crime[is.na(crime[,i]), i] <- median(unique(crime[,i]), na.rm = TRUE)
  
  #track the progress of the loop:
  message('Processing image ', i, ' of ', length(cols.id))
}

#replace all remaining NAs by median
for(i in list$cols.id[list$sum_nas>0]){
  crime[is.na(crime[,i]), i] <- median(unique(crime[,i]), na.rm = TRUE)
  
  #track the progress of the loop:
  message('Processing image ', i, ' of ', length(cols.id))
}

#check NAs again
list$sum_nas_v2 <- lapply(list$cols.id, function(x){sum(is.na(crime[,x]))})
list$sum_nas_v2 <- as.numeric(list$sum_nas_v2)



