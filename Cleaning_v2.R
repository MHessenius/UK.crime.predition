## General Setup
crime <- readRDS("H:/RDS_files/crime.whole.rds")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

## Overview about missing values

#get the column names
cols.w.na <- colnames(crime)[colSums(is.na(crime)) > 0]
list <- data.frame(cols.w.na) 

#get the respective column id
list$cols.id <- lapply(list$cols.w.na, function(x){which(colnames(crime)==x)})
list$cols.id <- as.numeric(list$cols.id)

#check wheter NAs are only due to a lack of data of "City of London"
check <- data.frame("dummy") #dummy for the loop

for(i in 1:length(cols.w.na)){
  check <- crime[is.na(crime[,cols.w.na[i]]),]
  
  if(sum(check$Boroughs=="City of London")==nrow(check)){
    list$outcome[i] <- "No data for City of London"
  }else{
    list$outcome[i] <- "other reasons"
  }
  rm(check)
}

#add the class of each column
list$class <-  lapply(crime[,c(cols.w.na)], function(x){class(x)})
list$class <- as.character(list$class)

#convert all factors into integer
list$factor <- ifelse(list$class=="factor",1,0) #dummy for the for-loop

for(i in list$cols.id[list$factor==1]){
  crime[,i] <- as.integer(crime[,i])
}

#check outcome of conversion
list$class_v2 <-  lapply(crime[,c(cols.w.na)], function(x){class(x)})
list$class_v2 <- as.character(list$class_v2)

#for better overview: store the NAs in the list
list$sum_nas <- lapply(list$cols.id, function(x){sum(is.na(crime[,x]))})
list$sum_nas <- as.numeric(list$sum_nas)

#replace all NAs for integer or numeric columns by the median

for(i in list$cols.id[list$class_v2=="numeric" | list$class_v2=="integer"]){
  
  crime[is.na(crime[,i]), i] <- median(unique(crime[,i]), na.rm = TRUE)
  
  #track the progress of the loop:
  message(which(i == list$cols.id[list$class_v2=="numeric" | list$class_v2=="integer"]), '/',
           length(list$cols.id[list$class_v2=="numeric" | list$class_v2=="integer"]))
}

#check sum of NAs again
list$sum_nas_v2 <- lapply(list$cols.id, function(x){sum(is.na(crime[,x]))})
list$sum_nas_v2 <- as.numeric(list$sum_nas_v2)

#replace remaining NAs manually
crime[is.na(crime$Crime.ID),"Crime.ID"] <- "NA"
crime[crime$Crime.ID=="","Crime.ID"] <- "NA"
crime[is.na(crime$Last.outcome.category),"Last.outcome.category"] <- "NA"
crime[crime$Last.outcome.category=="","Last.outcome.category"] <- "NA"
crime[is.na(crime$Context),"Context"]<- "NA"


#### check the classes of all columns

classes <- data.frame(colnames(crime))
names(classes)[names(classes) == "colnames.crime."] = "Name" 

classes$cols.id <- lapply(classes$Name, function(x){which(colnames(crime)==x)})
classes$cols.id <-as.numeric(classes$cols.id)

classes$class <- lapply(crime, function(x){class(x)})
classes$class <- as.character(classes$class)

classes$factor <- ifelse(classes$class=="factor",1,0)

#specify which factors to convert
classes$convert <- classes$factor
classes$convert[1:15] <- 0

#convert all with onvert==1 into integer

for(i in classes$cols.id[classes$convert==1]){
  crime[,i] <- as.integer(crime[,i])
}

#do remaining changes
crime[,c("Year","Month","Quarter")] <- lapply(crime[,c("Year","Month","Quarter")],factor)

#check outcome
classes$class_v2 <- lapply(crime, function(x){class(x)})
classes$class_v2 <-  as.character(classes$class_v2)


###Save the final file
# saveRDS(crime, file="crime.whole.cleaned.rds")
