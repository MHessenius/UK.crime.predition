##### PURPOSE
# - merge POI data to the dataset
#------

# The original data seems incredible messy. There are no longitude / latitude
# values that make sense for almost 70k rows. The columns are totally messed up.
# Approx. 10k rows seem to be ok. As the cleaning in R was just a mess - especially
# the conversion from factor to numerics, I cleaned it in excel.

# Basically I found the right values for lat in the column location" and
# the right values for longitude in the column "name".

# --------
# EXCEL-CLEANED FILE

poi.cleaned <- read.csv("H:/RDS_files/london-poi-cleaned.csv", sep=",")

# convertion: from factor to numeric
poi.cleaned$lat <- as.numeric(levels(poi.cleaned$lat))[poi.cleaned$lat]
poi.cleaned$lng <- as.numeric(levels(poi.cleaned$lng))[poi.cleaned$lng]

# cleaning of 1 row with NAs in long/lat
    sum(is.na(poi.cleaned$lat)) #1
    sum(is.na(poi.cleaned$lng)) #1 
    poi.cleaned[is.na(poi.cleaned$lat),] # 72540
    poi.cleaned <- poi.cleaned[-72540,]
    sum(is.na(poi.cleaned$lat)) #0
    sum(is.na(poi.cleaned$lng)) #0 
    
# next step: find out whether a POI is within a certain borough...

#what we need: 
  # g.ld4 (greater london map with boroughs) as a SpatialPolygonsDataFrame // and 
  # coordinates (longitude / lattitude) as a SpatialPointsDataframe

class(g.ld4) #SpatialPolygonsDataFrame -> rdy
class(poi.cleaned) #data.frame -> transformation necessary
  coordinates(poi.cleaned) = ~lng + lat
  class(poi.cleaned) # "SpatialPointsDataFrame" --> rdy
  
  # they need to have the same CRS
    proj4string(g.ld4) # "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    CRS.new <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    proj4string(poi.cleaned) <- CRS.new
    identicalCRS(g.ld4, poi.cleaned) #TRUE
    
over(poi.cleaned, g.ld4[,"CODE"] ) #works

# merge that to the whole POI-dataset

poi.cleaned$area_code <- over(poi.cleaned, g.ld4[,"CODE"] )

# Verify the result
unique(poi.cleaned@data$area_code)
  poi.cleaned2 = data.frame(poi.cleaned) #change it to a normal data.frame
  poi.subset <- subset(poi.cleaned2,CODE=="E09000002" | CODE=="E09000004" | CODE=="E09000027" ) #why did he rename the colum?
plot(g.ld4)
points(poi.subset$lng, poi.subset$lat, pch=20) # seems to work :)
rm(poi.cleaned2)

#save it as RDS
poi.cleaned <- data.frame(poi.cleaned)
names(poi.cleaned)[names(poi.cleaned) == "CODE"] = "area_code"
    #delete NAs
    length(unique(poi.cleaned$area_code)) #34
    sum(is.na(poi.cleaned$area_code)) #99
    poi.cleaned <- poi.cleaned[!is.na(poi.cleaned$area_code),]
# saveRDS(poi.cleaned, "H:/RDS_files/london-poi-cleaned-w-code.rds")
    
# NEXT STEP
  # -> creation of features!


##-- ARCHIVE: Investigation of the original poi-file

# read the data

# poi <- read.csv("H:/RDS_files/london-poi.csv", sep=";")
# 
# View(poi[,c("lng","lat")])
# sum(poi$lng=="London") #69465
# sum(poi$lat=="poi") #69515
# 
# poi.subset <- poi[poi$lat=="poi" & poi$lng=="London",]
# nrow(poi.subset) #69465
# 
# nrow(poi[poi$lat=="poi" & poi$lng!="London",]) #50
# View(poi[poi$lat=="poi" & poi$lng!="London",]) #probably only a conversion problem
# rm(poi)