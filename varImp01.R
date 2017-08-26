#---------------------------
# Variable Importance

#Overview
# 1) First sketch with MLR-package

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory
test.pct <- 0.1 #percentage of the train-set to get faster results while coding, set to 1 for final tuning

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
    v.importance.infogain <- generateFilterValuesData(trainTask, method = "information.gain")
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    
    #plot it
    plotFilterValues(v.importance.chi,n.show = 41)
    plotFilterValues(v.importance.infogain,n.show = 41)
    
    # cool package to play around with:
    if(!require("ggvis")) install.packages("ggvis"); library("ggvis")
    plotFilterValuesGGVIS(v.importance)

