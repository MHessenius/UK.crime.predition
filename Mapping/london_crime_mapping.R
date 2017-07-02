# Plot maps for london - based on the "mapping_how_to.R"

# get all packages
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("Rtools")) install.packages("Rtools"); library("Rtools")
if(!require("ggmap")) install.packages("ggmap"); library("ggmap")
if(!require("RCurl")) install.packages("RCurl"); library("RCurl")
if(!require("xlsx")) install.packages("xlsx"); library("xlsx")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("rgdal")) install.packages("rgdal"); library("rgdal")
if(!require("sp")) install.packages("sp"); library("sp")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("scales")) install.packages("scales"); library("scales")
if(!require("Cairo")) install.packages("Cairo"); library("Cairo")
if(!require("gpclb")) install.packages("gpclb"); library("gpclb")
if(!require("rgeos")) install.packages("rgeos"); library("rgeos")
if(!require("raster")) install.packages("raster"); library("raster")


# simply adapt the approacj to Greater London
# 1st step: data of the boundaries

setwd("H:/Mapping/statistical-gis-boundaries-london/ESRI_London_Borough/")
london <- readOGR(dsn = ".", layer = "London_Borough_Excluding_MHW") #exact 33 entries = greater london :)
london <- fortify(london, region="GSS_CODE") #fortify makes it readable for ggplot

## SIDESTEP: add the borough-codes to the crime-dataset
# crime <- readRDS("H:/Mapping/statistical-gis-boundaries-london/crime.rds") #w/o features
# london.profiles <- read.csv("H:/Mapping/statistical-gis-boundaries-london/london-borough-profiles.csv")
# london.profiles <- london.profiles[1:33,]
# names(london.profiles)[names(london.profiles) == "Code"] = "id"
# london.profiles <- london.profiles[!duplicated(london.profiles$Area_name),]
# london.profiles <- london.profiles[,c("id","Area_name")]
# london.profiles$Area_name <- as.character(london.profiles$Area_name)
# names(crime)[names(crime) == "Boroughs"] = "Area_name"
# sum(crime$Area_name %in% london.profiles$Area_name==TRUE) #all names correct --> ready to merge
# crime <- left_join(crime, london.profiles, by="Area_name")
# #for better descriptions in a plot, it's recommended to shorten long area_names
# crime$Area_name_red <- crime$Area_name 
# crime$Area_name_red <- as.character(crime$Area_name_red)
# crime$Area_name_red[crime$Area_name_red=="Barking and Dagenham"] <- as.character("Bark. and D.")
# crime$Area_name_red[crime$Area_name_red=="Kingston upon Thames"] <- as.character("Kingst. u.T.")
# crime$Area_name_red[crime$Area_name_red=="Hammersmith and Fulham"] <- as.character("Hamm. and F.")
# crime$Area_name_red[crime$Area_name_red=="Richmond upon Thames"] <- as.character("Richm. u.T.")
# crime$Area_name_red[crime$Area_name_red=="Kensington and Chelsea"] <- as.character("K. and Chelsea")
# saveRDS(crime, "H:/RDS_files/crime.w_o_features.rds")
# --> now it contains the code of each borough

# 2nd step: get crime-data
# only take the 2011-occurences and only Robberies

crime <- readRDS("H:/RDS_files/crime.w_o_features.rds")
crime <- crime[crime$Year=="2011",]
# crime <- crime[crime$Month=="01",] #only January to reduce the dataset

# just create artificial data
test <- crime
test <- test[!duplicated(test$id),]
test <- data.frame(test$id)
test$artificial <- sample(1:100, size=nrow(test), replace=TRUE)/100
names(test)[names(test) == "test.id"] = "id"

#merge test with the map-data
plotData <- left_join(london, test, by="id") #done

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = artificial), color = "black", size = 0.25) +
  coord_fixed() +
  theme_nothing()
ggsave(p, file = "H:/Mapping/Tutorial/cartographic/Own Plots/TEST1234.png", width = 6, height = 4.5, type = "cairo-png")

#result:
# --> works perfectly, now we can add more content on the level of each Borough
# examples:
# number of crimes / population density / unemployment rate etc.

# few more adjustment: 
# for the plot of the labels we need the median of long/lat

##get the center of the Boroughs
median.borough <- aggregate(cbind(long,lat)~id, data=plotData, FUN = function(x)mean(range(x)))
colnames(median.borough) <- c("id", "mean_long", "mean_lat")
plotData <- left_join(plotData, median.borough, by="id")

##add the borough names to plotData
borough.names <- crime[!duplicated(crime$Area_name_red),]
borough.names <- borough.names[,c("id","Area_name_red")]
plotData <- left_join(plotData, borough.names, by="id")
rm(borough.names)

## now simply feed plotData with other content
## --> see "descriptive_analytics_by_borough.R"

#saveRDS(plotData, file="/plot.data.london.rds")

#--------------------------------------#
#### -- ARCHIVE -- #######

# #now merge this file to crime to get the code of the borough
# names(london.profiles)[names(london.profiles) == "Area_name"] = "Boroughs"
# crime <- join(crime, london.profiles, by = "Boroughs") #done :)
# crime <- crime[,c("Longitude","Latitude","id")]
# london.mapdata.backup <- london
# 
# #rearrange columns
# london <- london[,c(6,1,2,3,4,5,7)]
# crime <- crime[,c(3,1,2)]

#now feed the test-dataset with more content
# plotData <- left_join(total.crime.2011, plotData, by="id")
# names(plotData)[names(plotData) == "sumFreq"] = "total_crimes" 

# p <- ggplot() +
#   geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
#                                     fill = total_crimes), color = "black", size = 0.25) +
#   scale_fill_distiller(palette = "Reds", breaks = pretty_breaks(n = 8)) +
#   theme_nothing(legend=TRUE)+
#   labs(title="Total crimes in 2011", subtitle="Distinguished by borough", caption="Source:...", fill="Total Crimes")+
#   geom_text(data=plotData, aes(x=mean_long,y=mean_lat, label=Area_name), size=1) +
#   guides(fill = guide_legend(reverse = TRUE))
# ggsave(p, file = "Own Plots/total_crimes_2011_range.png", width = 6, height = 4.5, type = "cairo-png")
# 
# 
# 
# ## get the map of London
# map<-get_map(location='London', zoom=12, maptype = "terrain",
#              source='google',color='color')
# ggmap(map) #should be fine
# 
# ##
# uk.adm2 <- readRDS("H:/Seminar/GBR_adm2.rds")
# 
# uk.adm2.df <- fortify(uk.adm2, region = "NAME_2")
# 
# 
# load("H:/Seminar/GBR_adm2.rds")
# 
# 
# ### GSI Files
# # Read SHAPEFILE.shp from the current working directory (".")
# shape <- readOGR(dsn = ".", layer = "H:/Mapping/statistical-gis-boundaries-london/ESRI_London_Borough/London_Borough_Excluding_MHW.shp")
# 
# #use maptools-package
# readShapeSpatial("H:/Mapping/statistical-gis-boundaries-london/ESRI_London_Borough/London_Borough_Excluding_MHW.shp")
# 
# 
# 
# ## get London data for whole 2016
# file_names <- dir("London_2016/")
# setwd("C:/Users/hessenim.hub/Downloads/UK/London_2016/")
# l.crime.2016 <- do.call(rbind,lapply(file_names,read.csv))
# setwd("C:/Users/hessenim.hub/Downloads/UK/")
# l.crime.2016 <- l.crime.2016[!is.na(l.crime.2016$Longitude),]
# 
# 
# ## 
# ggmap(map) + geom_point(
#   aes(x=Longitude, y=Latitude), 
#   data=l.crime.2016, alpha=.2, na.rm = T) 
# 
# 
# ########### MAP ALL ########
# UK_map<-get_map(location='London', zoom=6, maptype = "terrain",
#                 source='google',color='color')
# ggmap(UK_map)
# 
# ggmap(UK_map) + geom_point(
#   aes(x=Longitude, y=Latitude), 
#   data=all, alpha=.2, na.rm = T) 
# 
# ########### MAP Berkshire ########
# file_names <- dir("Bedfordshire_2016/")
# bedfordshire.crime.2016 <- do.call(rbind,lapply(paste("Bedfordshire_2016/",file_names,sep=""),read.csv))
# 
# bedfordshire_map <- get_map(location= c(lon= -0.416666666667, lat=52.0833333333), zoom=10, maptype = "terrain",
#                             source='google',color='color')
# ggmap(bedfordshire_map)
# 
# ggmap(bedfordshire_map) + geom_point(
#   aes(x=Longitude, y=Latitude), 
#   data=bedfordshire.crime.2016, alpha=.2, na.rm = T) 
# 
# ##looks ok, so what's the clue with london?
# 
# ### london ranges coordinates
# lon <- range(l.crime.2016$Longitude)
# lat <- range(l.crime.2016$Latitude)
# center <- c(mean(lon),mean(lat))
# 
# london_map_coordinates <- get_map( location = center, zoom=13, maptype = "terrain",
#                                    source='google',color='color')
# ggmap(london_map_coordinates)
# 
# london_map <- get_map( location='London', zoom=11, maptype = "terrain",
#                        source='google',color='color')
# ggmap(london_map)
# 
# #only plot london crimes
# ggmap(london_map_coordinates) + geom_point(
#   aes(x=Longitude, y=Latitude), 
#   data=l.crime.2016, alpha=.2, na.rm = T) 
# 
# #plot all crimes on london
# ggmap(london_map_coordinates) + geom_point(
#   aes(x=Longitude, y=Latitude), 
#   data=london.crime, alpha=.2, na.rm = T) 

