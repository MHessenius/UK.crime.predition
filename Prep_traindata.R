### Get the traindata ready ###

crime.cleaned <- readRDS("H:/RDS_files/crime.whole.cleaned.rds")
train <- crime.cleaned[crime.cleaned$Year==2011,]
train <- train[train$Quarter==1 | train$Quarter==2,]
train[,c("Last.outcome.category","Context","Year")] <- NULL
rm(crime.cleaned)

##Convertions
train$Longitude <- as.factor(train$Longitude)
train$Latitude <- as.factor(train$Latitude)
train$Boroughs <- as.factor(train$Boroughs)
train$Crime.ID <- as.factor(train$Crime.ID)

# saveRDS(train, file="H:/RDS_files/traindata.malte.rds")

## For better overview
# classes.train <- data.frame(colnames(train))
# colnames(classes.train) <- "Name"
# classes.train$class <- lapply(train, function(x){class(x)})
# classes.train$class_v2 <- lapply(train, function(x){class(x)})
# classes.train$class_v3 <- lapply(train, function(x){class(x)})
