### GET - DATASET ###

get.crime.data <- function(){
      
      crime <- readRDS("./Data/crime.whole.cleaned.2011.rds")
      
      #cleaning & conversion
      crime[,c("Last.outcome.category","Context","Year")] <- NULL
      crime[,c("Area_id","Area_name","Crime.ID","Longitude","Latitude")] <- lapply(crime[,c("Area_id","Area_name","Crime.ID","Longitude","Latitude")], factor)
      crime$Crime.type <- droplevels(crime$Crime.type) #drop unused levels
      
      
      return(crime)
      return(train)
      return(test)
}

get.train.data <- function(){
  
  #define sample size
  smp_size <- floor(0.8 * nrow(crime))
  
  #split data
  set.seed(123)
  train_ind <- sample(seq_len(nrow(crime)), size = smp_size) #index
  train <- crime[train_ind,]

  #reduce train & test for first & fast results
  if(test.pct==1){print("nothing changes - train")}else{
    train <- train[sample(seq_len(nrow(train)), size = floor(test.pct * nrow(train))),]
  }

  return(train)
}

get.test.data <- function(){
  
  #define sample size
  smp_size <- floor(0.8 * nrow(crime))
  
  
  #split data
  set.seed(123)
  train_ind <- sample(seq_len(nrow(crime)), size = smp_size) #index
  test <- crime[-train_ind,]
  
  #reduce train & test for first & fast results
    if(test.pct==1){print("nothing changes - test")}else{
    test <- test[sample(seq_len(nrow(test)), size = floor(test.pct * nrow(test))),]
    }
  return(test)
}