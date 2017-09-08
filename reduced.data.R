### Get the XY most important variables of the dataset

# get the crime dataset
ownwd <- "H:/GitHub/UK.crime.predition/" #set your GitHub directory
setwd(ownwd)
source("./Mapping/get.dataset.R")
crime <- get.crime.data() #get the up-to-date file from GitHub

# get the Variable Importance output on 800k rows by RF

var.imp <- read.csv("./Data/RFoutput.csv")
colnames(var.imp) <- c("Variable","Value")
var.imp$Value <- NULL
missing.columns <- data.frame(Variable = c("Crime.type","Longitude","Latitude"))
var.imp <- rbind(var.imp, missing.columns)
my.vars <- as.character(droplevels(var.imp$Variable[c(1:50,175:177)]))

crime.red <- crime[,my.vars] #change the numbers if necessary 
