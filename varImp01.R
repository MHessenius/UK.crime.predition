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
    
    #  cat(as.character(count[count$categories>=52,"name"]), sep=",")
    drops <- c("LSOA.code","Crime.ID","Longitude","Latitude","Location","LSOA.name","Family.Type..All.lone.parent.housholds.with.dependent.children..measures..Value","Family.Type..Lone.parent.in.part.time.employment..Total..measures..Value","Family.Type..Lone.parent.in.full.time.employment..Total..measures..Value","Family.Type..Lone.parent.not.in.employment..Total..measures..Value","Family.Type..Female.lone.parent..Total..measures..Value","Family.Type..Female.lone.parent..In.part.time.employment..measures..Value","Family.Type..Female.lone.parent..Not.in.employment..measures..Value","POI.Count_lsoa_poi","LSOA.tract.population","LSOA.tract.hectares","LSOA.tract.density.persons.p.hect","LSOA.crimes.p.tract.p.month","LSOA.crimes.p.tract.p.year","LSOA.crimes.p.year.p.hectar","Area.crimes.p.month","Area.crimes.p.month.p.square.km")
    train.red <- train[,!(names(train) %in% drops)]
    
    
    sum(apply(train.red,2,function(x)length(unique(x)))>52) #0
    
    start.time <- Sys.time()
    fit=randomForest(factor(Crime.type)~., data=train.red)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
    varImp(fit)
    
### 3) WEIGHT OF EVIDENCE ###
    
    