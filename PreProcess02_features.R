#### Pre Processing 02 ###
# To describe our dataset, some cool plots would be fine.
# Create those features....
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

### Size of each borough & pop.density

borough <- london@data[,c("GSS_CODE","HECTARES")]
colnames(borough) <- c("Area_id","Area_hectares")
# 1ha = 0.01 km²
borough$Area_square_km <- borough$Area_hectares/100
borough$population <- crime[!duplicated(crime$Area_id),"Population"]
borough$Area_pop_density <- borough$population/borough$Area_square_km
View(borough)
# merge it by "Area_id" to crime dataset

### Population density of each LSOA-tract??

# first goal: get the spatial size of each tract...

View(lsoa)
