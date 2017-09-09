crime <- readRDS("./crime.red.both.top32.rds")

## transform integers to numeric
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

crime$Latitude <- as.numeric(as.character(train$Latitude)) ### not working anymore
crime$Longitude <- as.numeric(as.character(train$Longitude)) ### not working anymore

train.test.split <- sample(2, nrow(crime), replace = TRUE, prob = c(0.7, 0.3))
train = crime[train.test.split == 1,]
test <- get.test.data()
train <- get.train.data()
test <- sparse.model.matrix(~.-1, data = test)

# construct matrices for training
X <- sparse.model.matrix(Crime.type ~.-1, data = train)
Y <- as.numeric(train[, "Crime.type"]) - 1
numclass <- range(Y)[2] + 1

# set the parameter
params <- list("objective" = "multi:softprob",
               "eta" = .5,
               "max_depth" = 5,
               "eval_metric" = "mlogloss",
               "num_class" = numclass)

# cross-validation
bst.cv <-  xgb.cv(params = params, data = X, label = Y, nfold = 3, nround = 50, verbose = T)

# training with gradient boost
bst <- xgboost(data = X, label = Y, params = params, nrounds = 50)

# save the model
xgb.dump(bst, with.stats = TRUE)

# apply prediction
pred <- predict(bst, newdata = test)
