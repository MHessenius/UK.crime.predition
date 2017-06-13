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

### WEATHER ### -- not working yet
weather <- read.csv("Data/weather_input_update.csv",na.strings=c("NA","NaN",""," ","x","X","!","-"), sep=";")
weather <- weather[-c(37:72),]
weather$year_month <- paste0(weather$Year,"_",weather$Month)
weather$Year <- NULL
weather$Month <- NULL
crime$year_month <- paste0(crime$Year,"_",crime$Month)
  
crime <-  merge(crime, weather, by="year_month")
