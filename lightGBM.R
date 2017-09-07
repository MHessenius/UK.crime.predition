crime <- readRDS("./crime.whole.cleaned.2011.exploded.rds")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
train <- data.table(crime)
if(!require("stringr")) install.packages("stringr"); library("stringr")
train$LSOA.name <- as.character(train$LSOA.name)
train$LSOA.name <- str_sub(train$LSOA.name, 1, str_length(train$LSOA.name)-4)
train$LSOA.name <- as.factor(train$LSOA.name)
levels(train$LSOA.name)
train$Latitude <- as.numeric(as.character(train$Latitude))
train$Longitude <- as.numeric(as.character(train$Longitude))
### Safe bet 

--
  title: "Predicting And Mapping Arrest Types in London with LightGBM, R, ggplot2"

# Setup
#in order to install lightbgm three things have to be installed: git , cmake and Rtools (visit their websites to download)

Setup the R packages.
if(!require("devtools")) install.packages("devtools"); library("devtools")
install_github("Microsoft/LightGBM", subdir = "R-package")

if(!require("lightgbm")) install.packages("lightgbm"); library("lightgbm")
if(!require("Matrix")) install.packages("Matrix"); library("Matrix")
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("viridis")) install.packages("viridis"); library("viridis")
if(!require("ggmap")) install.packages("ggmap"); library("ggmap")
if(!require("randomcoloR")) install.packages("randomcoloR"); library("randomcoloR")

#### light gbm
## crime type index
train <- train %>% mutate(category_index = as.numeric(factor(Crime.type)) - 1)

train %>% select(category_index, Crime.type) %>% head()

# lightgbm Training

# declare categorical feature names, if any
categoricals <- NULL
# proportion of data to train on
split <- 0.6

set.seed(123)
trainIndex <- createDataPartition(train$category_index, p = split, list = FALSE, times = 1)




dtrain <- lgb.Dataset((train %>% data.matrix())[trainIndex,],
                      categorical_feature = categoricals,
                      label = train$category_index[trainIndex], free_raw_data=T)

dtest <- lgb.Dataset.create.valid(dtrain,
                                  (train %>% data.matrix())[-trainIndex,],
                                  label = train$category_index[-trainIndex])

params <- list(objective = "multiclass", metric = "multi_logloss")
valids <- list(test=dtest)

num_classes <- length(unique(train$category_index))

# preformat sizes for use in data visualizations later
train_size_format <- length(trainIndex) %>% format(big.mark=",")
test_size_format <- (train %>% nrow() - length(trainIndex)) %>% format(big.mark=",")

''''
The size of the training set is **`r train_size_format`** and the size of the test set is **`r test_size_format`**.

```{r}

# determine elapsed runtime 
system.time(
  
  # training output not printed to notebook since spammy. (verbose = 0 + record = T)
  bst <- lgb.train(params,
                   dtrain,
                   nrounds = 500,
                   valids,
                   num_threads = 4,
                   num_class = num_classes,
                   verbose = 0,
                   record = T,
                   early_stopping_rounds = 5,
                   categorical_feature = categoricals
  )
  
)[3]

# multilogloss of final iteration on test set
paste("# Rounds:", bst$current_iter())
paste("Multilogloss of best model:", bst$record_evals$test$multi_logloss$eval %>% unlist() %>% tail(1))
```

Calculate variable importance. (note: takes awhile since single-threaded)

```{r}
df_imp <- tbl_df(lgb.importance(bst, percentage = TRUE))
df_imp
```

#### Prediction

`preds` is a 1D vector of probabilities for each vector, of nrows x nclasses. Reshape accordingly and iterate through for the predicted label (label with the largest probability) and the corresponding probability.

```{r}
test <- (train %>% select(Longitude, Latitude, Unemployment_Rate_UK, Crime.ID, Unemployment_Rate_London, Quarter,Youth_Unemployment_Rates_UK_16_24,Youth_Unemployment_Rates_London_16_24,LSOA.code,LSOA.name,Sun.Hours, Max.degree.C, Gap_London_UK, Rainfall.mm, Area.crimes.month.p.square.km, Area.crimes.p.month, Min.degree.C, Month, LSOA.tract.density.persons.p.hect, POI.Count_lsoa_poi) %>% data.matrix())[-trainIndex,]

preds_matrix <- predict(bst, test, reshape=T)

preds_cor <- cor(preds_matrix)

preds_matrix[1:2,]

# likely not most efficient method
results <- t(apply(preds_matrix, 1, function (x) {
  max_index = which(x==max(x))
  return (c(max_index-1, x[max_index]))
}))
```

```{r}
df_results <- data.frame(results, label_act = train$category_index[-trainIndex]) %>%
  tbl_df() %>%
  transmute(label_pred = X1, prod_pred = X2, label_act)

df_results %>% arrange(desc(prod_pred)) %>% head(20)

rm(preds_matrix)
```


Confusion matrix:
  
  ```{r}
cm <- confusionMatrix(df_results$label_pred, df_results$label_act)

data.frame(cm$overall)
```
###
