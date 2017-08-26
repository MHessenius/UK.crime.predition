# PreProcessing
# -> think about some intelligent features to reduce dataset

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
#---------------------------

## CENSUS DATA - starts with col 102 ##

  # get census-data on the LSOA level
  
  hh.lifestage <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,100:116)]
  ethnicity <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,117:122)]
  hh.composition <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,123:144)]
  family.type <- crime[!duplicated(crime$LSOA.code),c(1:5,9:13,145:156)]

  # hh lifestage
  # https://www.nomisweb.co.uk/census/2011/ks102ew -- units: persons
  # HRP = household reference person
  View(hh.lifestage)
  arrange(data.frame(table(hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value)), desc(Freq))
  length(unique(hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value)) #660 diff values
  
  summary(hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value)
  hh.lifestage$quants <- quantile(hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value, probs = seq(0,1,0.01))
  quants <- quantile(hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value, probs = seq(0,1,0.01))
  quants
  
  column <- hh.lifestage$Household.Lifestage..All.categories..Household.lifestage..measures..Value
  percentile <- ecdf(column)
  percentile(500) #that's what I need
  
  
  
  # ethnicity
  View(ethnicity)
  length(unique(ethnicity$Multiple.Ethnic.Group..All.categories..Multiple.ethnic.groups..measures..Value)) #660





