#---------------------------
# Variable Importance

#Overview
# 1) First sketch with MLR-package

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory
test.pct <- 0.3

# // NEED FOR CHANGE
#---------------------------

### SETUP ###
source("./Mapping/get.dataset.R")
crime <- get.crime.data()
train <- get.train.data()
test <- get.test.data()

### 1) MLR Package ###
# https://www.analyticsvidhya.com/blog/2016/08/practicing-machine-learning-techniques-in-r-with-mlr-package/
if(!require("mlr")) install.packages("mlr"); library("mlr")

#reduce it to get quick results
# train <- train[sample(seq_len(nrow(train)), size = floor(0.1 * nrow(train))),]
# test <- test[sample(seq_len(nrow(test)), size = floor(0.1 * nrow(test))),]

listLearners("classif")[c("class","package")]

#tasK: a task is nothing but the data set on which a learner learns. 
trainTask <- makeClassifTask(data = train,target = "Crime.type")
testTask <- makeClassifTask(data = test, target = "Crime.type")


#check the task
#trainTask #positive class --> "No" --> should be changed to "yes"
#trainTask <- makeClassifTask(data = train,target = "return_customer", positive = "yes")

#deeper look
str(getTaskData(trainTask))

if(!require("FSelector")) install.packages("FSelector"); library("FSelector")
if(!require("rJava")) install.packages("rJava"); library("rJava")

#Chi-Squared #Information gain
v.importance.chi <- generateFilterValuesData(trainTask, method = "chi.squared")
v.importance.infogain <- generateFilterValuesData(trainTask, method = "information.gain")
plotFilterValues(v.importance.chi,n.show = 41)



plotFilterValues(v.importance.infogain,n.show = 41)

generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
generateFilterValuesData(trainTask, method = "chi.squared"), desc(chi.squared)

if(!require("ggvis")) install.packages("ggvis"); library("ggvis")
plotFilterValuesGGVIS(v.importance)

getFilterValues(v.importance, method="chi.squared")
getFilteredFeatures()
filterFeatures(trainTask ,fval = v.importance, perc = 0.5)

data.frame(getFilterValues(trainTask, method="chi.squared"))
