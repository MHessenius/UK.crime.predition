#---------------------------
## First steps to start the modeling

# Overview
# 1) Prework and Splitting
# 2) ...

#---------------------------
# NEED FOR CHANGE
ownwd <- "H:/GitHub/UK.crime.predition/" #set your own directory

test.pct <- 0.002 # only use X per-cent of the train-data to get first & fast preliminary results
                 # set it to 1 if whole train-data should be used

# // NEED FOR CHANGE
#---------------------------

### 1) PREWORK AND SPLITTING ###
    setwd(ownwd)
    crime <- readRDS("./Data/crime.whole.cleaned.2011.rds")

    #define sample size
    smp_size <- floor(0.8 * nrow(crime))
    
    #split data
    set.seed(123)
    train_ind <- sample(seq_len(nrow(crime)), size = smp_size) #index
    train <- crime[train_ind,]
    test <- crime[-train_ind,]
    
    #reduce data for first & fast results
    if(test.pct==1){print("nothing changes")}else{
      train <- train[sample(seq_len(nrow(train)), size = floor(test.pct * nrow(train))),]
    }
    
    #check classes first
    list <- data.frame(colnames(train))
    list$class <- lapply(train, function(x){class(x)})
    View(list)
    
    #cleaning & conversion
    train[,c("Last.outcome.category","Context","Year")] <- NULL
    train[,c("Area_id","Area_name","Crime.ID","Longitude","Latitude")] <- lapply(train[,c("Area_id","Area_name","Crime.ID","Longitude","Latitude")], factor)
    
#---------------------------

### 2) 1st ATTEMPT ON CRIME TYPE ###
    # Naive Bayes or Decision Tree as suggested in the paper (#4 in excel-file)
    
    # NAIVE BAYES --> SO FAR ONLY NAS
    # some hints in BADS Exercise 4.3 
    
    if(!require("e1071")) install.packages("e1071"); library("e1071")
    
    nb <- naiveBayes(Crime.type~., data = train) #train nb model on all variables
    
    summary(nb)
    
    pred.nb <- predict(nb, newdata = train, type = "raw") #pred a-posteriory condition probablts for the NB-Classifier
    # Note that the predict function returns probabilities for both classes in a matrix when applied to a naive bayes model
    head(pred.nb)
    pred.nb <- pred.nb[, 2] # Extract the probabilities for the "1" event, i.e. a bad loan, saved in the second column
    brier.nb <- sum((y- pred.nb)^2) / length(y) #calculate the Brier score for the classifier
    
#---------------------------    
    
### 3) PLAY AROUND WITH RANDOM FOREST
    
    # [....]