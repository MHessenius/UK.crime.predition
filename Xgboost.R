crime <- readRDS("./crime.red.both.top32.rds")
crime$Crime.type <- as.character(crime$Crime.type)
crime$Crime.type[crime$Crime.type == 'Anti-social behaviour'] <- 'Anti.social.behaviour'
crime$Crime.type[crime$Crime.type == 'Other crime'] <- 'Other.crime'
crime$Crime.type[crime$Crime.type == 'Vehicle crime'] <- 'Vehicle.crime'
crime$Crime.type[crime$Crime.type == 'Criminal damage and arson'] <- 'Criminal.damage.and.arson'
crime$Crime.type[crime$Crime.type == 'Other theft'] <- 'Other.theft'
crime$Crime.type[crime$Crime.type == 'Violent crime'] <- 'Violent.crime'
crime$Crime.type[crime$Crime.type == 'Public disorder and weapons'] <- 'Public.disorder.and.weapons'
crime$Crime.type <- factor(crime$Crime.type)
list <- data.frame(colnames(crime)) 
colnames(list) <- "df"
list$cols.id <- lapply(list$df, function(x){which(colnames(crime)==x)})
list$cols.id <- as.numeric(list$cols.id)

###add the class of each column
list$class <-  lapply(crime, function(x){class(x)})
list$class <- as.character(list$class)

### convert all factors into integer

list$integer <- ifelse(list$class=="integer",1,0) #dummy for the for-loop

#loop: integer --> numeric
for(i in list$cols.id[list$integer==1]){
  crime[,i] <- as.numeric(gsub(",",".",crime[,i]))
}

crime$Latitude <- as.numeric(as.character(crime$Latitude))
crime$Longitude <- as.numeric(as.character(crime$Longitude))

train.xgb <- get.train.data()
test.xgb <- get.test.data()

train.xgb.red <- train.xgb[1:420000,]
test.xgb.red <- test.xgb[1:180000,]
test.val <- test.xgb.red$Crime.type
test.xgb.red$Crime.type <- NULL

levels(train.xgb.red$Crime.type) <- make.names(levels(factor(train.xgb.red$Crime.type)))

model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE 
)

xgb.parms <- expand.grid(nrounds = 500, 
                         max_depth = 10, 
                         eta = 0.05, 
                         gamma = 5,
                         colsample_bytree = 0.7,
                         min_child_weight = 1,
                         subsample = 0.7)
xgb <- train(factor(Crime.type)~., data = train.xgb.red,  
             method = "xgbTree",
             tuneGrid = xgb.parms, 
             metric = "Accuracy", trControl = model.control)
xgb.pred <- predict(xgb, newdata = test.xgb.red, type = "raw")

results <- as.factor(colnames(xgb.pred)[apply(xgb.pred,1,which.max)])
test.val <- test.xgb.red$Crime.type

confusionMatrix(results,test.val)
