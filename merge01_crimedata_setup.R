## Setup

# since the files of the crime-dataset, are all per borough & per month, 
# they need to be merged

file_names <- dir("C:/Users/hessenim.hub/Downloads/01_Final/Greater London/")
setwd("C:/Users/hessenim.hub/Downloads/01_Final/Greater London//") #exact working director necessary for the do.call command
crime <- do.call(rbind,lapply(file_names,read.csv))
setwd("H:/GitHub/UK.crime.predition")

## CLEANING
crime <- crime[!is.na(crime$Longitude),] #no location = no LSOA.code
crime <- crime[!crime$LSOA.code=="",]

# sapply(crime, function(x){sum(is.na(x))})

## Gsub to get the names of the 348 local authorities
crime$Boroughs <- gsub(" [0-9]..*", "", crime$LSOA.name)

## Create year-column & month-column & quarter-column
crime$Year <- substr(crime$Month, 1, 4)
crime$Month <- substr(crime$Month, 6,7)

crime$Quarter <- crime$Month
crime$Quarter[crime$Quarter=="01"] <- 1
crime$Quarter[crime$Quarter=="02"] <- 1
crime$Quarter[crime$Quarter=="03"] <- 1
crime$Quarter[crime$Quarter=="04"] <- 2
crime$Quarter[crime$Quarter=="05"] <- 2
crime$Quarter[crime$Quarter=="06"] <- 2
crime$Quarter[crime$Quarter=="07"] <- 3
crime$Quarter[crime$Quarter=="08"] <- 3
crime$Quarter[crime$Quarter=="09"] <- 3
crime$Quarter[crime$Quarter=="10"] <- 4
crime$Quarter[crime$Quarter=="11"] <- 4
crime$Quarter[crime$Quarter=="12"] <- 4

## deal with Boroughs (there are more boroughs than necessary)
london.boroughs <- c("City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing","Enfield", "Greenwich", "Hackney","Hammersmith and Fulham","Haringey", "Harrow","Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest","Wandsworth", "Westminster")
london.boroughs <- data.frame(london.boroughs)
london.boroughs$london.boroughs <- as.character(london.boroughs$london.boroughs)

crime <- crime[crime$Boroughs %in% london.boroughs$london.boroughs == TRUE,] #only take the relevant ones

#saveRDS(crime, file="H:/Mapping/statistical-gis-boundaries-london/crime.rds")


##---- ARCHIVE ----- ####
# --> code that is probably not necessary anymore

# --> deal with the boroughs
# list.boroughs <- data.frame(unique(crime$Boroughs))
# names(list.boroughs)[names(list.boroughs) == "unique.crime.Boroughs."] = "Unq.name"
# list.boroughs$check <- ifelse(list.boroughs$Unq.name %in% london.boroughs$london.boroughs == TRUE,1,0)
# length(crime$Boroughs[crime$Boroughs %in% list.boroughs$Unq.name])


# # --> create a second frame with slightly different names
# # for a possible easier matching
# crime$authority_v2 <- crime$authority  
# 
# # renaming
# crime$authority_v2[crime$authority_v2=="Bristol"] <- "Bristol, City of"
# crime$authority_v2[crime$authority_v2=="St Alabans"] <- "St. Albans"
# crime$authority_v2[crime$authority_v2=="Herefordshire"] <- "Herefordshirem, County of"
# crime$authority_v2[crime$authority_v2=="Isle of Anglesey"] <- "Anglesey, Isle of"
# crime$authority_v2[crime$authority_v2=="King's Lynn and West Norfolk"] <- "Kings Lynn and West Norfolk"
# crime$authority_v2[crime$authority_v2=="Kingston upon Hull"] <- "Kingston upon Hull, City of"
# crime$authority_v2[crime$authority_v2=="The Vale of Glamorgan"] <- "Vale of Glamorgan, The"
# crime$authority_v2[crime$authority_v2=="St Edmundsbury"] <- "St. Edmundsbury"
# 
