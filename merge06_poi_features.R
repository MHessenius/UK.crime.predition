#-------------------------------
# Overview of this file
  # 1) Prework and Investigation
  # 2) Features on level of each borough (# 33)
  # 3) Features on level of each lsoa-tract (# 4835)

#-------------------------------
# PURPOSE: Create features for the POI-data on a borough- and lsoa-level

# recall:
# ~ 10 000 rows were ok
# ~ 70 000 rows were a mess

#-------------------------------
# Get all packages that might be necessary
source("./Mapping/load_packages.R")
spatial.packages()
#--------------------------------

### 1) PREWORK AND INVESTIGATION ###

    poi <- readRDS("H:/RDS_files/london-poi-cleaned-w-code.rds")
    crime <- readRDS("H:/RDS_files/crime.whole.cleaned.2011.rds")
    poi.perfect <- poi[poi$Perfect==1,] #the data that looked ok
    
    ### Investigate the poi - data
    
    length(unique(poi.perfect$lsoa_code)) #3337 -> there may be a risk of sparse data
    length(unique(poi.perfect$area_code)) #33
    arrange(data.frame(table(poi.perfect$Subcategory)),desc(Freq)) #subcategories in desc order
    
    #   View(poi.perfect[poi.perfect$Subcategory=="Road",]) #seems to be useless, 2212 occurences
    #   View(poi.perfect[poi.perfect$Subcategory=="Bus Line",])
    #   View(poi.perfect[poi.perfect$Subcategory=="General Travel",])
    #   View(poi.perfect[poi.perfect$Subcategory=="Boat or Ferry",])
    #   View(poi.perfect[poi.perfect$Subcategory=="Light Rail",])

### 2) FEATURES ON BORO-LEVEL ###
    
    # -> How many XY in each borough/ lsoa-tract?
  
    boro.results <- read.csv("./Data/london-borough-profiles.csv") #get all borough-codes
    boro.results <- data.frame(boro.results[1:33,"Code"])
    colnames(boro.results) <- "Code"
    boro.results$Code <- as.character(boro.results$Code)

    ###loop to sum up subcategories

    # define a vector wof interesting subcategories
    # -> take all travel-related things and police stations
    
    subc <- data.frame(names = c("Bus Station", "Bus Line", "Train Station", "Platform", "General Travel",
                 "Boat or Ferry", "Tourist Information Centre", "Airport", "Light Rail",
                 "Taxi","Rental Car Location", "Police Station"))
    subc$names <- as.character(subc$names)
    
    # start the loop
    
    for(j in 1:length(subc$names)){
      for(i in 1:length(boro.results$Code)){
        # sum it up
        boro.results[i,subc$names[j]] <- sum(poi.perfect$Subcategory==subc$names[j] & poi.perfect$area_code==boro.results$Code[i])
      }
      #track the for loop
      print(paste(j,"/",length(subc$names)))
    }
    
    # rename columns (add underscore)
    colnames(boro.results) <- gsub(" ","_",colnames(boro.results))
    
    # create a column "Transport_sum" -> simply sum up all columns related to transportation
    boro.results[,"Transport_sum"] <- apply(boro.results[,c(2,3,4,5,6,9,10,11)], 1, function(x){sum(x)})
    
    # verify results
    sum(boro.results$`Bus_Station`)==sum(poi.perfect$Subcategory=="Bus Station") #TRUE
    
    #merge this to crime-data
    
    crime$Area_id <- as.character(crime$Area_id)
    names(boro.results)[names(boro.results) == "Code"] = "Area_id" 
    crime <- merge(crime, boro.results, by="Area_id")
    colnames(crime)[160:172] <- c(paste("Sum_Boro", colnames(crime[,160:172]), sep = "_"))
    
### 3) FEATURES ON LEVEL OF EACH LSOA-TRACT ###
    # concern: spare data ...
    
    # run the lsoa-mapping script to get all lsoa_codes
    lsoa.results <- data.frame(lsoa@data$LSOA11CD)
    colnames(lsoa.results) <- "LSOA.code"
    
    # count POI-occurences of each lsoa-tract
    # make us eof dplyr-package?
    if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
    
    poi <- group_by(poi,lsoa_code) #groupy data by lsoa_code
    poi.counts <- as.data.frame(arrange(summarize(poi, count=n()), desc(count)))  #simply count it
    colnames(poi.counts) <- c("LSOA.code","Count_lsoa_poi")
    lsoa.results <- merge(lsoa.results, poi.counts, all = TRUE ) # all = True because 27 lsoa tracts in poi are missing
    lsoa.results[is.na(lsoa.results$Count_lsoa_poi),"Count_lsoa_poi"] <- 0  #replace those 27 NAs by 0
    
    # merge this column to crime data
    crime <- merge(crime, lsoa.results, by="LSOA.code")
    
#saveRDS
    #saveRDS(crime,"H:/RDS_files/crime.whole.cleaned.2011.v2.rds")
