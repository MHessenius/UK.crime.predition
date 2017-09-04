#---------------------------
## First steps to start the modeling

# Overview (up-to-date)
# 1) Prework and Splitting
# 2) 1st attempt: Naive Bayes -- so far poor accuracy, but works
# 3) Some first sketches (SVM, Decision Tree)

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory

test.pct <- 0.002 # only use X per-cent of the train-data to get first & fast preliminary results
                 # set it to 1 if whole train-data should be used

# // NEED FOR CHANGE
#---------------------------

### 1) PREWORK AND SPLITTING ###
    setwd(ownwd)
    crime <- readRDS("DROPBOX-PATH")
    
    source("./Mapping/get.dataset.R")
    train <- get.train.data()
    test <- get.test.data()

    #cleaning & conversion
    crime[,c("Last.outcome.category","Context","Year")] <- NULL
    crime[,c("Area_id","Area_name","Crime.ID","Longitude","Latitude")] <- lapply(crime[,c("Area_id","Area_name","Crime.ID","Longitude","Latitude")], factor)
    crime$Crime.type <- droplevels(crime$Crime.type) #drop unused levels
        # since some levels don't occur
        # see ->  arrange(data.frame(table(crime$Crime.type)), desc(Freq))

    #define sample size
    smp_size <- floor(0.8 * nrow(crime))
    
    #split data
    set.seed(123)
    train_ind <- sample(seq_len(nrow(crime)), size = smp_size) #index
    train <- crime[train_ind,]
    test <- crime[-train_ind,]
    
    #reduce train & test for first & fast results
    if(test.pct==1){print("nothing changes - train")}else{
      train <- train[sample(seq_len(nrow(train)), size = floor(test.pct * nrow(train))),]
    }
    if(test.pct==1){print("nothing changes - test")}else{
      test <- test[sample(seq_len(nrow(test)), size = floor(test.pct * nrow(test))),]
    }
    
    #check classes
#     list <- data.frame(colnames(train))
#     list$class <- lapply(train, function(x){class(x)})
#     View(list)
    
#---------------------------

### 2) 1st ATTEMPT ON CRIME TYPE ###
    # Naive Bayes or Decision Tree as suggested in one paper (#4 in excel-file)
    
    # NAIVE BAYES 
    # some hints in BADS Exercise 4.3 
    if(!require("caret")) install.packages("caret"); library("caret")
    if(!require("e1071")) install.packages("e1071"); library("e1071")
    
    nb <- naiveBayes(Crime.type~., data = train.red) #train nb model on all variables
    
    
    x_test <- test[,-13] #all except crime.type
    y_test <- test[,13] #all except crime.type
    pred.nb <- predict(nb, x_test)
    confusionMatrix(pred.nb, y_test) # accuracy of 13 per cent :D

#---------------------------    
    
### 3) Some first attempts
    
    ##--- SVM ---##
    
    svm_model <- svm(Crime.type ~ ., data=train.red)
    summary(svm_model)
    x <- train[,-13]
    y <- train[,13]
    
    pred.svm <- predict(svm_model,x)
    system.time(pred <- predict(svm_model,x))
    
    ## --> TAKES AGES!, even fore a ridiculously small dataset
    
    
    ##--- DECISION TREE ---## from: http://dataaspirant.com/2017/02/03/decision-tree-classifier-implementation-in-r/
    
    if(!require("rpart")) install.packages("rpart"); library("rpart")
    if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")
    if(!require("caret")) install.packages("caret"); library("caret")
    
    ## 1st attempt
    train.red <- train[1:5000,]
    
    ##
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    set.seed(123)
    dtree_fit <- caret::train(Crime.type ~., data = train.red, method = "rpart",
                       parms = list(split = "information"),
                       trControl=trctrl,
                       tuneLength = 10)
    
    dt <- rpart(Crime.type ~ ., data = train) # create decision tree classifier
    pred.dt <- predict(dt, newdata = loans, type = "prob")[, 2] # calculate predictions (in-sample)
    
    # Calculate performance according to brier score
    y <- as.numeric(loans$BAD) - 1 # This is a good example of why you need to be careful when transforming factor variables to numeric
    brier.dt <- sum((y - pred.dt)^2) / length(y) # compute tree's Brier Score
    
    ### Producing a nice chart of the tree 
    # (for further improvements see http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html)
    # In order to visualize your decision trees, "rpart.plot" is a handy package. Initially, install the package and load it with the library() function
    if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")
    
    # Visualize the results from "dtree" using the prp() function. 
    prp(dt)
    prp(dt, extra = 104, border.col = 0) # Print the percentage of observations and class probabilities in each node