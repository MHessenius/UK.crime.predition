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

##convert "n/a" into NAs
# sapply(crime, function(x){sum(x=="n/a", na.rm = TRUE)})
# sum(crime[,"New.homes..net.additional.dwellings."]=="n/a",na.rm = TRUE) #72138
# sum(crime[,"People.aged.17..with.diabetes..perc."]=="n/a",na.rm = TRUE) #6466
# sum(is.na(crime[,"New.homes..net.additional.dwellings."])) #0
# sum(is.na(crime[,"People.aged.17..with.diabetes..perc."])) #7493
crime$New.homes..net.additional.dwellings.[crime$New.homes..net.additional.dwellings.=="n/a"] <- NA
crime$People.aged.17..with.diabetes..perc.[crime$People.aged.17..with.diabetes..perc.=="n/a"] <- NA


#backup before conversion
backup <- crime[!duplicated(crime$Boroughs),]
# backup2 <- data.frame(seq(1,33,1))
# backup3 <- data.frame(seq(1,33,1))

#loop: factor --> numeric
for(i in list$cols.id[list$factor==1]){
  crime[,i] <- as.numeric(gsub(",",".",crime[,i]))
}
# 
# # compare w/ backup
# for(i in list$cols.id[list$factor==1]){
#   backup2[,1+which(i == list$cols.id[list$factor==1])] <- as.numeric(gsub(",",".",crime[!duplicated(crime$Boroughs),i]))
# }
# #to compare w/ backup2
# 
# for(i in list$cols.id[list$factor==1]){
#   backup3[,1+which(i == list$cols.id[list$factor==1])] <- crime[!duplicated(crime$Boroughs),i]
# }

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

## -- SOME MANUAL CLEANING REQUIRED -- ##

## Clean "X..of.Children.in.Poverty................" before
crime$X..of.Children.in.Poverty................ <- gsub(",",".",substr(crime$X..of.Children.in.Poverty................, 1, 4))
# unique(crime$X..of.Children.in.Poverty................) #two outliers
crime$X..of.Children.in.Poverty................[crime$X..of.Children.in.Poverty................=="8.8."] <- "8.8"
crime$X..of.Children.in.Poverty................[crime$X..of.Children.in.Poverty................=="8.3."] <- "8.3"
crime$X..of.Children.in.Poverty................ <- as.numeric(crime$X..of.Children.in.Poverty................)
classes[classes$Name=="X..of.Children.in.Poverty................","convert"] <- 0

## Clean "New.homes..net.additional.dwellings."
# sum(is.na(crime$New.homes..net.additional.dwellings.)) #72138
# crime.backup <- crime
# unique(crime$New.homes..net.additional.dwellings.)
crime$New.homes..net.additional.dwellings.<-  as.numeric(as.character(crime$New.homes..net.additional.dwellings.))
crime$New.homes..net.additional.dwellings.[is.na(crime$New.homes..net.additional.dwellings.)] <- median(unique(crime[,"New.homes..net.additional.dwellings."]), na.rm = TRUE)
classes[classes$Name=="New.homes..net.additional.dwellings.","convert"] <- 0

## -- // MANUAL CLEANING DONE -- ##

#convert all with convert==1 into integer
#gsub before

for(i in classes$cols.id[classes$convert==1]){
  crime[,i] <- as.numeric(gsub(",",".",crime[,i]))
  #track the progress of the loop:
  message(which(i == classes$cols.id[classes$convert==1]), '/',
          length(classes$cols.id[classes$convert==1]))
}

#do remaining changes
crime[,c("Year","Month","Quarter")] <- lapply(crime[,c("Year","Month","Quarter")],factor)

#check outcome
classes$class_v2 <- lapply(crime, function(x){class(x)})
classes$class_v2 <-  as.character(classes$class_v2)


###Save the final file
saveRDS(crime, file="crime.whole.cleaned.rds")
