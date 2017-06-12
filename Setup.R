## Setup
file_names <- dir("C:/Users/hessenim.hub/Downloads/01_Final/Greater London/")
setwd("C:/Users/hessenim.hub/Downloads/01_Final/Greater London//") #exact working director necessary for the do.call command
crime <- do.call(rbind,lapply(file_names,read.csv))
setwd("H:/GitHub/UK.crime.predition")

## CLEANING
crime <- crime[!is.na(crime$Longitude),] #no location = no LSOA.code
crime <- crime[!crime$LSOA.code=="",]

# sapply(crime, function(x){sum(is.na(x))})

## Gsub to get the names of the 348 local authorities
crime$authority <- gsub(" [0-9]..*", "", crime$LSOA.name)

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
