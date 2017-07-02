### MAPPING - HOW TO?

# What's a shapefile?
# -> holds a bunch of information to draw borders 
# -> rule of thumb: it's easier if you stick to lower zoom-levels

# Useful packages 
# 
# ggplot2
# rgdal: reads in shape files
# scales: tells ggplot what the proper scale should be
# ggmap: comes with a nice theme_nothing() function
# dplyr: I only use the left_join() function but this is a very useful package to add to your toolbox if you haven't already
# Cairo: creates high quality vector and bitmap images


# Simply work through the tutorial  --> http://www.kevjohnson.org/making-maps-in-r/

setwd("H:/Mapping/Tutorial/cartographic/")

# read OGR function
# -> dsn: the directory
# -> layer: shapefile-name without shp

tract <- readOGR(dsn = ".", layer = "cb_2016_13_tract_500k") #layer without .shp
names(tract) # "GEOID"
tract <- fortify(tract, region="GEOID") #fortify is a function from ggplot to transform it in a way ggplot can understand

#read the data
data <- read.csv("H:/Mapping/Tutorial/data/ACS_12_5YR_S2701_with_ann.csv", stringsAsFactors = FALSE)
# interesting columns: percent of people with no insurance
data <- data[,c("GEO.id2", "HC03_EST_VC01")]
colnames(data) <- c("id", "percent")
data$id <- as.character(data$id)
df <- data
df$percent <- as.numeric(df$percent)
df$percent[is.na(df$percent)] <- median(df$percent, na.rm=TRUE)
df$percent <- df$percent/100

# no need to prepare data for the merging since ID's are identical
plotData <- left_join(tract, df)

#first plot
p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent), color = "black", size = 0.25)
ggsave(p, file = "map1.png", width = 6, height = 4.5, type = "cairo-png")

#what happened here?
#left_join simply added the percentages to the spatial data

#add coord_map to keep map in shape 

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent), color = "black", size = 0.25) +
  coord_map()
ggsave(p, file = "map2.png", width = 5, height = 4.5, type = "cairo-png")

# census tract borders make it difficult to see data in heavily populated areas
# use the county borders
setwd("H:/Mapping/Tutorial/cartographic/")
county <- readOGR(dsn = ".", layer = "cb_2016_13_tract_500k")
names(county)
county <- fortify(county, region="COUNTYFP")

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map()
ggsave(p, file = "map3.png", width = 5, height = 4.5, type = "cairo-png")

# important: order of geom_polygon does matter!
# change colors by using: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens")
ggsave(p, file = "map4.png", width = 5, height = 4.5, type = "cairo-png")

# more adjustment: higher percentages at the top etc

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10)) + #add breaks easily
  guides(fill = guide_legend(reverse = TRUE))
ggsave(p, file = "map5.png", width = 5, height = 4.5, type = "cairo-png")

# more adjustment: add a title, get rid of grey background, remove "percentages"

p <- ggplot() +
  geom_polygon(data = plotData, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens", labels = percent,
                       breaks = pretty_breaks(n = 10), values = c(1,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_nothing(legend = TRUE) + #removes the background
  labs(title = "Percentage of Population Without\nHealth Insurance",
       fill = "")
ggsave(p, file = "map6.png", width = 5, height = 4.5, type = "cairo-png")