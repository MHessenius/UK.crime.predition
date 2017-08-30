# PreProcessing
# -> think about some intelligent features to reduce dataset

# As a first step, we could convert the census-data into percentiles
# to reduce the amount of different values (some columns have >500 different ones). 
# This script is all about that.

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory
# // NEED FOR CHANGE
#---------------------------
### SETUP ###
source("./Mapping/get.dataset.R")
crime <- get.crime.data()
train <- get.train.data()
test <- get.test.data()
source("./Mapping/load_packages.R")
spatial.packages() #get all packages
#---------------------------

## CENSUS DATA - starts with col 102 ##

  # get census-data on the LSOA level
  
  hh.lifestage <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,100:116)]
  ethnicity <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,117:122)]
  hh.composition <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,123:144)]
  family.type <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,145:156)]
  
  # --------------------------------- #
  # -----    HH Lifestage ----------- #
  # --------------------------------- #
  
  # https://www.nomisweb.co.uk/census/2011/ks102ew -- units: persons
  # HRP = household reference person
  View(hh.lifestage)
  arrange(data.frame(table(hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value)), desc(Freq))
  
  #get all different values
  list <- data.frame(names(hh.lifestage[,11:27]))
  list$unique_values <- sapply(hh.lifestage[,11:27], function(x){length(unique(x))})
  View(list)

  # to reduce the amount of different values, we could think about assigning
  # percentiles instead 

  perc.hh.lfstg <- data.frame(LSOA.code = crime[!duplicated(crime$LSOA.code),"LSOA.code"])
  
  # Clean up the column names
  col.names <- gsub(".Age.of.HRP","",colnames(hh.lifestage)) 
  col.names <- gsub("Household.Lifestage","hh.lfstg",col.names)
  col.names <- gsub("..measures..Value","",col.names)
  
  
  for(i in which(grepl( "Lifestage" , names(hh.lifestage) )==TRUE)){
    
    # assign the according percentile to every LSOA-tract
    percentile <- ecdf(hh.lifestage[,i])
    perc.hh.lfstg[,col.names[i]] <- data.frame(round_any(percentile(hh.lifestage[,i]),0.02))
    
    #track the for-loop
    print(i)
  }
  
  # --------------------------------- #
  # -----    HH Ethnicity ----------- #
  # --------------------------------- #
  
  # ethnicity
  colnames(ethnicity) #columns 11 - 16
  
  list <- data.frame(names(ethnicity[,11:16]))
  list$unique_values <- sapply(ethnicity[,11:16], function(x){length(unique(x))})
  View(list)
  # reduction would make sense as well
  
  # Clean up the column names
  col.names <- gsub("..measures..Value","",colnames(ethnicity))
  
  # for loop
  perc.ethnicity <- data.frame(LSOA.code = crime[!duplicated(crime$LSOA.code),"LSOA.code"])
  
  for(i in which(grepl( "Ethnic" , names(ethnicity) )==TRUE)){
    
    # assign the according percentile to every LSOA-tract
    percentile <- ecdf(ethnicity[,i])
    perc.ethnicity[,col.names[i]] <- data.frame(round_any(percentile(ethnicity[,i]),0.02))
    
    #track the for-loop
    print(i)
  }
  
  # --------------------------------- #
  # -----    HH Composition --------- #
  # --------------------------------- #

  colnames(hh.composition) #columns 11 - 32
  
  list <- data.frame(names(hh.composition[,11:32]))
  list$unique_values <- sapply(hh.composition[,11:32], function(x){length(unique(x))})
  range(list$unique_values) # 13- 660
  # reduction would make sense as well
  
  # Clean up the column names
  col.names <- gsub("..measures..Value","",colnames(hh.composition))
  col.names <- gsub("Household.Composition", "hh.composition", col.names)
  col.names <- gsub("household", "hh", col.names)

  
  # for loop - create data-frame
  perc.hh.composition <- data.frame(LSOA.code = crime[!duplicated(crime$LSOA.code),"LSOA.code"])
  
  for(i in which(grepl( "Composition" , names(hh.composition) )==TRUE)){
    
    # assign the according percentile to every LSOA-tract
    percentile <- ecdf(hh.composition[,i])
    perc.hh.composition[,col.names[i]] <- data.frame(round_any(percentile(hh.composition[,i]),0.02))
    
    #track the for-loop
    print(i)
  }
  
  # --------------------------------- #
  # -----    Family Type    --------- #
  # --------------------------------- #
  
  colnames(family.type) #columns 11 - 22
  
  list <- data.frame(names(family.type[,11:22]))
  list$unique_values <- sapply(family.type[,11:22], function(x){length(unique(x))})
  range(list$unique_values) # 14- 186
  View(list)
  
  # Reduction not really necessary
  # --> Skip it!
  
  ### SAVE THE RESULTS ###
  
  # remove the changed census-data -> hh.compostion / hh.lifestage / ethnicity
  # columns 100 - 144
  crime[,100:144] <- NULL
  crime<- merge(crime, perc.hh.lfstg, by ="LSOA.code")
  crime<- merge(crime, perc.ethnicity, by ="LSOA.code")
  crime<- merge(crime, perc.hh.composition, by ="LSOA.code")
  
#   saveRDS(perc.hh.composition, file="perc.hh.composition.lsoa.rds")
#   saveRDS(perc.ethnicity, file="perc.ethnicity.lsoa.rds")
#   saveRDS(perc.hh.lfstg, file="perc.hh.lfstg.lsoa.rds")
#   saveRDS(crime,file="crime.whole.cleaned.2011.rds")
  

## BACKUP - conversion of familty.type
  
#   # Clean up the column names
#   col.names <- gsub("..measures..Value","",colnames(family.type))
# 
#   
#   # for loop - create data-frame
#   perc.family.type <- data.frame(LSOA.code = crime[!duplicated(crime$LSOA.code),"LSOA.code"])
#   
#   for(i in which(grepl( "Family.Type" , names(family.type) )==TRUE)){
#     
#     # assign the according percentile to every LSOA-tract
#     percentile <- ecdf(family.type[,i])
#     perc.family.type[,col.names[i]] <- data.frame(round_any(percentile(family.type[,i]),0.02))
#     
#     #track the for-loop
#     print(i)
#   }

