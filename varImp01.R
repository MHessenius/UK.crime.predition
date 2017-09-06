#---------------------------
# Variable Importance

#Overview
# 1) First sketch with MLR-package

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory
setwd(ownwd)
test.pct <- 0.3 #percentage of the train-set to get faster results while coding, set to 1 for final tuning

# // NEED FOR CHANGE
#---------------------------

### SETUP ###
source("./Mapping/get.dataset.R")
crime <- get.crime.data()
train <- get.train.data()
test <- get.test.data()

### 1) MLR Package ###
    # https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
    # unfortunately, it's not able to access the pure data - 
    # so far I could only plit the rankings... :/

    if(!require("mlr")) install.packages("mlr"); library("mlr")
    
    #reduce it to get quick results
    # train <- train[sample(seq_len(nrow(train)), size = floor(0.1 * nrow(train))),]
    # test <- test[sample(seq_len(nrow(test)), size = floor(0.1 * nrow(test))),]
    
    listLearners("classif")[c("class","package")]
    
    #tasK: a task is nothing but the data set on which a learner learns. 
    trainTask <- makeClassifTask(data = train,target = "Crime.type")
    testTask <- makeClassifTask(data = test, target = "Crime.type")
    
    #deeper look
    str(getTaskData(trainTask))
    
    if(!require("FSelector")) install.packages("FSelector"); library("FSelector")
    if(!require("rJava")) install.packages("rJava"); library("rJava")
    
    #Chi-Squared #Information gain
    start.time <- Sys.time()
    v.importance.chi <- generateFilterValuesData(trainTask, method = "chi.squared")
    #v.importance.infogain <- generateFilterValuesData(trainTask, method = "information.gain")
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    
    #plot it
    plotFilterValues(v.importance.chi,n.show = 80)
    plotFilterValues(v.importance.infogain,n.show = 41)
    
    # cool package to play around with:
    if(!require("ggvis")) install.packages("ggvis"); library("ggvis")
    plotFilterValuesGGVIS(v.importance)

### 2) RANDOM FOREST ###
    
    if(!require("caret")) install.packages("caret"); library("caret")
    if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
    
    source("./Mapping/get.dataset.R")
    test.pct <- 1
    crime <- get.crime.data()
    train <- get.train.data()
    test <- get.test.data()
    
    # deal with the >53 categories problem
    count <- data.frame( name= c(colnames(train)))
    count$categories <- lapply(train, function(x)length(unique(x)))
    
    cat(as.character(count[count$categories>=52,"name"]), sep=",") # copy& paste output in formula below
    drops <- c("LSOA.code","Crime.ID","Longitude","Latitude","Location","LSOA.name")
    train.red <- train[,!(names(train) %in% drops)]
    
    # checl before
    count <- data.frame( name= c(colnames(train.red)))
    count$categories <- lapply(train.red, function(x)length(unique(x)))
    View(count[count$categories>=52,])
    
    
    # sum(apply(train.red,2,function(x)length(unique(x)))>52) #0
    train.red2 <- train.red[1:10000,]
    train.red2 <- droplevels(train.red2)
    start.time <- Sys.time()
    fit=randomForest(factor(Crime.type)~., data=train.red2, ntree = 700, mtry = 5,importance = TRUE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    varImp(fit)
    importance_by_rf <-    importance(fit, scale = FALSE)
    importance_by_rf <- sort(importance_by_rf[,"MeanDecreaseAccuracy"], decreasing = TRUE)
    plot(importance_by_rf[1:100])
    
### 3) XGB ###
    #variable selection based on xgb
    if(!require("xgboost")) install.packages("xgboost"); library("xgboost") # load the package
    
    #tuning values takem from BADS
    xgb_best_nrounds <-  80 #xgb$bestTune$nrounds
    xgb_best_max_depth <- 5 # xgb$bestTune$max_depth 
    xgb_best_eta <- 0.2 #  xgb$bestTune$eta
    xgb_best_gamma <- 0 # xgb$bestTune$gamma
    xgb_best_colsample_bytree <-  0.8 #xgb$bestTune$colsample_bytree
    xgb_best_min_child_weight <-  1 #xgb$bestTune$min_child_weight
    xgb_best_subsample <- 0.8 # xgb$bestTune$subsample
    
    # try to handle vector size
    train.red <- train[1:5000,]
    train.red$ASB <- ifelse(train.red$Crime.type=="Anti-social behaviour",1,0)
    sum(train.red$ASB) #1750
    
    # Train xgb model and use already known parameters
    xgb <- caret::train(ASB~. -Crime.ID, data = train.red,
                        method = 'xgbTree', trControl = trainControl(method = 'none'), importance = TRUE,
                        tuneGrid = expand.grid(nrounds = xgb_best_nrounds, max_depth = xgb_best_max_depth, eta = xgb_best_eta, gamma = xgb_best_gamma, colsample_bytree = xgb_best_colsample_bytree,
                                               min_child_weight = xgb_best_min_child_weight, subsample = xgb_best_subsample))
    
    # the xgboost package includes the function xgb.importance()
    importance_by_xgb <- xgb.importance(model = xgb$finalModel, feature_names = xgb$finalModel$xNames)
    #choose important variables based on xgb
    # we base the decision on Gain in Gini purity
    plot_importance_by_xgb <- plot(importance_by_xgb$Gain ) 
    top_by_xgb<- importance_by_xgb$Feature[1:25]
    top_by_xgb
    
    varImp_list <- list("top_by_rf" = top_by_rf, "plot_importance_by_rf" = plot_importance_by_rf, "top_by_xgb" = top_by_xgb, "plot_importance_by_xgb" = plot_importance_by_xgb)
    
    return(varImp_list)
    
### 4) FilterVarImp -- caret ###
    
    filterVarImp(train.red, train)
    