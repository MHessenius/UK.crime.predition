##merge data

## merge data from Datasheet 1

#births
births <- read.csv("Datasheet1/births.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"))
births[,4:7] <- NULL
crime <- merge(crime, births, by=c("Boroughs","Year"))
rm(births)

#born abroad
born_abroad <- read.csv("Datasheet1/born.abroad.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, born_abroad, by=c("Boroughs","Year"))
rm(born_abroad)

#employment
employment <- read.csv("Datasheet1/employment_rates.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, employment, by=c("Boroughs","Year"))
rm(employment)

#unemployment
unemployment <- read.csv("Datasheet1/unemployment_rates.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, unemployment, by=c("Boroughs","Year"))
rm(unemployment)

#fertility_rates
fertility<- read.csv("Datasheet1/fertility_rates.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, fertility, by=c("Boroughs","Year"))
rm(fertility)

#population.estimates
population <- read.csv("Datasheet1/population.estimates.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, population, by=c("Boroughs","Year"))
rm(population)

#secondary.school.languageg.other.than.english
second.school.lang <- read.csv("Datasheet1/secondary.school.languageg.other.than.english.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, second.school.lang, by=c("Boroughs","Year"))
rm(second.school.lang)

##XLSX-files

#deaths
deaths <- read.csv("Datasheet1/XLSX/deaths.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, deaths, by=c("Boroughs","Year"))
rm(deaths)

# NEETS_numbers
neets_numbers <- read.csv("Datasheet1/XLSX/NEETS_numbers.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, neets_numbers, by=c("Boroughs","Year"))
rm(neets_numbers)

#NEETS_rates
NEETS_rates <- read.csv("Datasheet1/XLSX/NEETS_rates.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, NEETS_rates, by=c("Boroughs","Year"))
rm(NEETS_rates)

#new_nino_rate
new_nino_rate <- read.csv("Datasheet1/XLSX/new_nino_rate.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, new_nino_rate, by=c("Boroughs","Year"))
rm(new_nino_rate)

#nino_registration
nino_registration <- read.csv("Datasheet1/XLSX/nino_registration.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, nino_registration, by=c("Boroughs","Year"))
rm(nino_registration)

#SMR
SMR <- read.csv("Datasheet1/XLSX/SMR.csv", na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, SMR, by=c("Boroughs","Year"))
rm(SMR)

#### DATASHEET 2###
datasheet2 <- read.csv("Data/datasheet2.csv",na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, datasheet2, by=c("Boroughs","Year"))

#### DATASHEET 3###
datasheet3 <- read.csv("Data/datasheet3.csv",na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
crime <- merge(crime, datasheet3, by=c("Boroughs","Year"))

#### DATASHEET 4###
datasheet4 <- read.csv("Data/datasheet4.csv",na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
datasheet4 <- datasheet4[-c(100:114),]
crime <- merge(crime, datasheet4, by=c("Boroughs","Year"))

#### DATASHEET 5###
datasheet5 <- read.csv("Data/datasheet5.csv",na.strings=c("NA","NaN",""," ","x","X","!","-"))
crime <- merge(crime, datasheet5, by=c("Boroughs","Year"))

### WEATHER ###
weather <- read.csv("Data/weather_input_update.csv",na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
weather <- weather[-c(37:72),] #delete NA columns
weather$Month <- paste0("0",weather$Month) #prepare the merging
weather$Month <- ifelse(weather$Month=="010","10",
                        ifelse(weather$Month=="011","11",
                               ifelse(weather$Month=="012","12",weather$Month)))

crime <-  merge(crime, weather, by=c("Year","Month"))
rm(weather)

### Youth unemployment rate, quarterly ###

youth.unemployment <- read.csv("Data/youth_unemployment_rates_london_uk_quarterly.csv", sep=";",header = TRUE, row.names = NULL)
crime <-  merge(crime, youth.unemployment, by=c("Year","Quarter"))
rm(youth.unemployment)

### Unemployment rates, uk, london, monthly ###
unemployment.monthly <- read.csv("Data/Unemployment_Rates_London_UK_monthly.csv", sep=";", header=TRUE)
unemployment.monthly$Month <- paste0("0",unemployment.monthly$Month) #prepare the merging
unemployment.monthly$Month <- ifelse(unemployment.monthly$Month=="010","10",
                        ifelse(unemployment.monthly$Month=="011","11",
                               ifelse(unemployment.monthly$Month=="012","12",unemployment.monthly$Month)))
crime <-  merge(crime, unemployment.monthly, by=c("Year","Month"))
rm(unemployment.monthly)
