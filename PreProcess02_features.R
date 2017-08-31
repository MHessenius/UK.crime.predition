#### Pre Processing 02 ###

# Following features were added within this script
# 1) Area: size in square km and density
# 2) LSOA tracts: size in hectares and density
# 3) summing up of crime occurences on different levels (e.g. crimes per lsoa per year)

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory
setwd(ownwd)
# // NEED FOR CHANGE
#---------------------------
### SETUP ###
source("./Mapping/get.dataset.R")
crime <- get.crime.data()
source("./Mapping/load_packages.R")
spatial.packages() #get all packages
#---------------------------

  ### ---------------------------------  ###
  ### Size of each borough & pop.density ###
  ### ---------------------------------  ###
  
  # -> run "london_crime_mapping" to get the spatial data of london

  borough <- london@data[,c("GSS_CODE","HECTARES")]
  colnames(borough) <- c("Area_id","Area_hectares")
  # 1ha = 0.01 km²
  borough$Area_square_km <- round(borough$Area_hectares/100, digits = 2)
  borough$population <- crime[!duplicated(crime$Area_id),"Population"]
  borough$Area_pop_density <- round(borough$population/borough$Area_square_km, digits = 1)
  borough[,c("population","Area_hectares")] <- NULL

crime <- merge(crime, borough, by="Area_id") # merge it by "Area_id" to crime dataset

  ### ------------------------------------- ###
  ### Population density of each LSOA-tract ###
  ### ------------------------------------- ###

  pop.density <- read.csv("./Data/pop.density.census.lsoa.tracts.csv") #get the official census-data
  names(pop.density)[names(pop.density) == "geography.code"] = "LSOA.code"
  pop.density[,c("date","geography")] <- NULL
  
  # only get the london greater london lsoa tracts
  pop.density <- pop.density[pop.density$LSOA.code %in% crime$LSOA.code[!duplicated(crime$LSOA.code)]==TRUE,]
  
  # clean up the column names
  colnames(pop.density) <- gsub("..measures..Value","",colnames(pop.density))
  colnames(pop.density) <- gsub("Area","LSOA",colnames(pop.density))
  colnames(pop.density) <- c("LSOA.code", "LSOA.tract.population","LSOA.tract.hectares","LSOA.tract.density.persons.p.hect")

# merge it to crime
crime <- merge(crime, pop.density, by="LSOA.code")

    ### ---------------------------- ###
    ### SUM UP SOME CRIME OCCURENCES ###
    ### ---------------------------- ###


    if(!require("dplyr")) install.packages("dplyr"); library("dplyr") 
    
    # create subset first for easier calculations
    crime.sub <- crime[,c("LSOA.code","Area_id","Month","Crime.type","Population","Area_square_km","LSOA.tract.population","LSOA.tract.hectares")]
    
    # number of crimes per LSOA tract per month
    crimes.p.lsoa.p.month <- data.frame(crime.sub %>% group_by(Month, LSOA.code) %>%  summarise(number = n()))
    colnames(crimes.p.lsoa.p.month) <- c("Month","LSOA.code","LSOA.crimes.p.tract.p.month")
    crime <- merge(crime, crimes.p.lsoa.p.month, by=c("Month","LSOA.code"))
    
    # number of crimes per LSOA tract per year
    crimes.p.lsoa.p.year <- data.frame(crime.sub %>% group_by(LSOA.code) %>%  summarise(number = n()))
    colnames(crimes.p.lsoa.p.year) <- c("LSOA.code", "LSOA.crimes.p.tract.p.year")
    crime <- merge(crime, crimes.p.lsoa.p.year, by=c("LSOA.code"))
    
    # number of crimes per LSOA tract per year per hectar
    crime$LSOA.crimes.p.year.p.hectar <- round(crime$LSOA.crimes.p.tract.p.year / crime$LSOA.tract.hectares, digits = 1)
    
    # number of crimes per borough per month
    crimes.p.area.p.month <- data.frame(crime.sub %>% group_by(Month, Area_id) %>%  summarise(number = n()))
    colnames(crimes.p.area.p.month) <- c("Month","Area_id","Area.crimes.p.month")
    crime <- merge(crime, crimes.p.area.p.month, by=c("Month","Area_id"))
    
    # number of crimes per borough per month per square km
    crime$Area.crimes.p.month.p.square.km <- round(crime$Area.crimes.p.month / crime$Area_square_km, digits = 1)

# save results
saveRDS(crime, file="./Data/crime.whole.cleaned.2011.rds")

