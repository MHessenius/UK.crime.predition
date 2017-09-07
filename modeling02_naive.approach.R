## SETUP
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("stringr")) install.packages("stringr"); library("stringr")

##naive approach: assign classes by probabilites

train <- data.table(crime)
probs <- data.frame(train[, .N, by = Crime.type][order(N, decreasing = T)][1:11])
probs$probs <- probs$N/nrow(crime)
sum(probs$probs) #1

naive.prediction <- sample(probs$Crime.type, size=nrow(crime), rep=TRUE, prob= probs$probs)

## evaluation??

confusionMatrix(naive.prediction, crime$Crime.type)
