#-------------------------------
# PURPOSE: Create features for the POI-data on a borough- and lsoa-level

# recall:
# ~ 10 000 rows were ok
# ~ 70 000 rows were a mess

#-------------------------------

poi <- readRDS("H:/RDS_files/london-poi-cleaned-w-code.rds")
crime <- readRDS("H:/RDS_files/crime.whole.cleaned.2011.rds")

poi.perfect <- poi[poi$Perfect==1,]
length(unique(poi.perfect$lsoa_code)) #3337 -> there may be a risk of sparse data
length(unique(poi.perfect$area_code)) #33
sum(poi.perfect$Subcategory!="") #-> all contain sth.
table(poi.perfect$Subcategory)

### FEATURES
# -> How many XY in each borough/ lsoa-tract?
boro.results <- data.frame(poi.perfect$area_code[!duplicated(poi.perfect$area_code)])
colnames(boro.results) <- "Area_code"

# first try for bus stations (# 885) in each borough
# tbd.
