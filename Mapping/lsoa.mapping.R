### LSOA mapping

# london <- readOGR(dsn = ".", layer = "London_Borough_Excluding_MHW") #exact 33 entries = greater london :)
# names(london)
# london <- fortify(london, region="GSS_CODE")

setwd("H:/Mapping/Tutorial/cartographic/LSOA/2011_london_boundaries/LSOA_2011_BFE_London/")

#get all filenames
shps <- dir(getwd(), "*.shp")
shps <- gsub(".shp","",shps)

#assign all files to the R-Console
for (shp in shps) assign(shp, readOGR('.',layer=shp))

#save first dataframe for the loop
lsoa <- LSOA_2011_BFE_Barking_and_Dagenham

#merge them all together

for(i in 2:length(shps)){
  temp <- get(shps[i])
  lsoa <- rbind(lsoa, temp)
  rm(temp)
  print(paste(i,"/",length(shps)))
}

#remove all .shp-files from r-console
rm(list = c(shps))

# do the fortify call (make it readable for ggplot)
# first we need to find out whcih region should be adressed
names(lsoa)
length(unique(lsoa$LSOA11CD))

lsoa <- fortify(lsoa, region="LSOA11CD")

# try first plots
p <- ggplot()+
  geom_polygon(data = lsoa, aes(x=long, y = lat, group = group),
               color="black", fill="palegreen", size=0.25) +
  coord_fixed(1.3)+
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  xlab("")+
  ylab("")
ggsave(p,file="H:/Mapping/Tutorial/cartographic/LSOA/PLOTS/all_lsoa.png", bg = "transparent")

## PLOT SINGLE BOROUGHS TO COMPARE IT

# City of London
citylondon <-  readOGR(dsn=".", layer="LSOA_2011_BFE_City_of_London")
citylondon <- fortify(citylondon, region="LSOA11CD")
ggplot()+
  geom_polygon(data= citylondon, aes(x=long, y=lat, group = group), 
               color="black", fill="palegreen", size=0.25)

# Westminster
Westminster <-  readOGR(dsn=".", layer="LSOA_2011_BFE_Westminster")
Westminster <- fortify(Westminster, region="LSOA11CD")
ggplot()+
  geom_polygon(data= Westminster, aes(x=long, y=lat, group = group), 
               color="black", fill="palegreen", size=0.25)

# Islington
Islington <-  readOGR(dsn=".", layer="LSOA_2011_BFE_Islington")
Islington <- fortify(Islington, region="LSOA11CD")
ggplot()+
  geom_polygon(data= Islington, aes(x=long, y=lat, group = group), 
               color="black", fill="palegreen", size=0.25)