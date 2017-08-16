### Merge census data
# following files were merged
# - age_strucutre
# - ethnic_group
# - hh_composition
# - lone_parents_hh_dependent_children

### create a for loop to put all .csv-files into different dataframes

setwd("C:/Users/hessenim.hub/Downloads/00_census_data/")

temp = list.files(pattern="*.csv") #all .csv - files

for(i in 1:length(temp)){
  assign(gsub(".csv","",temp[i]), read.csv(temp[i],sep=","))
} #assign all of them into the environment

### change column_name "geography-code" into "LSOA.code"

temp2 <- gsub(".csv","",temp) #list of all data.frames in the environment

df <- data.frame() #dummy data.frame for the loop

for(i in 1:length(temp2)){
  df <- data.frame(get(temp2[i]))
  names(df)[names(df) == "geography.code"] = "LSOA.code"
  assign(temp2[i], df)
}

### delete all columns that aren't necessary "date"/"geography"/"Rural.Urban"

for(i in 1:length(temp2)){
  df <- data.frame(get(temp2[i]))
  df[,c("date","geography","Rural.Urban")] <- NULL
  assign(temp2[i], df)
}

### merge them all together

for(i in 1:length(temp2)){
  crime <- merge(crime, get(temp2[i]), by ="LSOA.code")
}

