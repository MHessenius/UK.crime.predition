### Preparatio Random Forest ####

## SETUP
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
if(!require("plyr")) install.packages("plyr"); library("plyr")
crime <- readRDS("H:/RDS_files/crime.whole.cleaned.2011.exploded.rds") 
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory
setwd(ownwd)
source("./Mapping/load_packages.R")
spatial.packages() #load all packages
## // SETUP DONE

## check wheter variable > 53 levels
count <- data.frame( name= c(colnames(crime)))
count$categories <- lapply(crime, function(x)length(unique(x)))
View(count[count$categories>=52,])

    ## the census family.type can be converted into percentiles
    
    # --------------------------------- #
    # -----    Family Type    --------- #
    # --------------------------------- #
    
    family.type <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,100:111)]
    colnames(family.type) #columns 11 - 22
    
    # create a subset to merge it by LSOA.code later
    perc.family.type <- data.frame(LSOA.code = crime[!duplicated(crime$LSOA.code),"LSOA.code"])
    
    #clean up column names
    col.names <- gsub("..measures..Value","",colnames(family.type))
    
    # replace it by perncetiles
    for(i in which(grepl( "Family.Type" , names(family.type) )==TRUE)){
      
      # assign the according percentile to every LSOA-tract
      percentile <- ecdf(family.type[,i])
      perc.family.type[,col.names[i]] <- data.frame(round_any(percentile(family.type[,i]),0.02))
      
      #track the for-loop
      print(i)
    }
    
    # merge it to crime data
    crime<- merge(crime, perc.family.type, by ="LSOA.code")
    
    # delete columns with the old names and old values
    crime[,100:111] <- NULL
    
    # --------------------------------- #
    # -----    OTHERS         --------- #
    # --------------------------------- #
    
## check remaining columns again [ variable > 53 levels]
    count <- data.frame( name= c(colnames(crime)))
    count$categories <- lapply(crime, function(x)length(unique(x)))
    View(count[count$categories>=52,])

## do the same for remaining columns
    
    others <- crime[!duplicated(crime$LSOA.code),which(data.frame(count$categories>=52)==TRUE)]
    perc.others <- data.frame(LSOA.code = crime[!duplicated(crime$LSOA.code),"LSOA.code"])
    
    # pick respective columns
    colnames(others) #everything but without LSOA / crime.id etc -> only the features
    pick.columns <- c(7:15)
    
    # run the for-loop again
    for(i in pick.columns){
      
      # assign the according percentile to every LSOA-tract
      percentile <- ecdf(others[,i])
      perc.others[,names(others)[i]] <- data.frame(round_any(percentile(others[,i]),0.02))
      
      #track the for-loop
      print(i)
    }
    
    lapply(perc.others, function(x){length(unique(x))}) #check results
    
    # merge new values
    crime <- merge(crime, perc.others, by="LSOA.code")
    # delete columns with old values
    # which(data.frame(count$categories>=52)==TRUE)
    delete.columns <- c(113, 161, 162, 163, 164, 165, 166, 167, 168)
    crime[,delete.columns] <- NULL
    
    # --------------------------------- #
    # -----    REMAINING        --------- #
    # --------------------------------- #
    
    ## check remaining columns again [ variable > 53 levels]
    count <- data.frame( name= c(colnames(crime)))
    count$categories <- lapply(crime, function(x)length(unique(x)))
    View(count[count$categories>=52,])
    
    # Longitude / Latitude could be separated into a grid
    crime.backup <- crime #do backup before
    crime$Longitude <- as.numeric(levels(crime$Longitude))[crime$Longitude]
    crime$Latitude <- as.numeric(levels(crime$Latitude))[crime$Latitude]
      # define the "by" for seq-function
      a <- (range(crime$Longitude)[2] - range(crime$Longitude)[1])/8
      b <- (range(crime$Latitude)[2] - range(crime$Latitude)[1])/7
    crime$gridGroup <- interaction(cut(crime$Longitude, breaks=seq(range(crime$Longitude)[1], range(crime$Longitude)[2], by=a)),
                               cut(crime$Latitude, breaks=seq(range(crime$Latitude)[1],range(crime$Latitude)[2], by=b)), sep="X")
    
    length(unique(crime$gridGroup)) #49
    class(crime$gridGroup) #factor
    
    # --------------------------------- #
    # -----    SAVE RESULTS   --------- #
    # --------------------------------- #
    
    # save(crime, file="./Data/crime.up.to.date.rds")
    