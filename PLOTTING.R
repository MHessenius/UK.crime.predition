## FINAL PLOTTING
# get the shapefile
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip",destfile="ne_10m_admin_1_states_provinces.zip")
unzip("ne_10m_admin_1_states_provinces.zip",exdir="NaturalEarth")
border <- shapefile("NaturalEarth/ne_10m_admin_1_states_provinces.shp")
GreaterLondon <- border[paste(border$region)=="Greater London",]

# get plotting data
smp_size <- floor(0.7 * nrow(crime))
set.seed(123)
train_ind <- sample(seq_len(nrow(crime)), size = smp_size) # sample it
train <- crime[train_ind,]
plotData <- train[1:1000,c("Crime.type","Longitude","Latitude", "Area_id")]

# do the plotting
GreaterLondon_fortify <- fortify(GreaterLondon, region="OBJECTID_1")
gg <- ggplot()
gg <- gg + geom_polygon(data=GreaterLondon_fortify, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5) 
gg <- gg + 
  geom_point(data=plotData, aes(x=Longitude, y=Latitude, color=Crime.type))+
  coord_fixed(1.3)+
  theme_bw() +
  theme(panel.border = element_blank(), #to get transparent background
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))
        
plot(gg)
