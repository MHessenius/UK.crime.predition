#### light GBM R start required !!!####

crime <- readRDS("./crime.red.RF.rds")

crime$Crime.type <- as.numeric(as.factor(crime$Crime.type)) - 1

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

crime$Latitude <- as.numeric(as.character(crime$Latitude))
crime$Longitude <- as.numeric(as.character(crime$Longitude))

#### splitting function required!!!! #####

train.gbm <- get.train.data()
test.gbm <- get.test.data()
dtrain <- lgb.Dataset((train.gbm[,-47] %>% data.matrix()),
                       categorical_feature = categoricals,
                       label = train.gbm[ ,47], free_raw_data=T)
 
dtest <- lgb.Dataset.create.valid(dtrain,
                                   (test.gbm[ ,-47] %>% data.matrix()),
                                   label = test.gbm[,47])
 
params <- list(objective = "multiclass", metric = "multi_logloss")
valids <- list(test=dtest)
categoricals <- NULL
 
num_classes <- length(unique(train.gbm$Crime.type))
 bst <- lgb.train(params,
                  dtrain,
                  nrounds = 1000,
                  valids,
                  num_threads = 15,
                  num_class = num_classes,
                  verbose = 0,
                  max_depth= 10,
                  num_leaves = 150,
                  learning_rate = 0.05,
                  max_bin = 200,
                  record = T,
                  early_stopping_rounds = 5,
                  categorical_feature = categoricals
 )
test <- test.gbm[ ,-47] %>% data.matrix()
preds_matrix <- predict(bst, test, reshape=T)
# likely not most efficient method
results <- t(apply(preds_matrix, 1, function (x) {
   max_index = which(x==max(x))
   return (c(max_index-1, x[max_index]))
 }))

df_results <- data.frame(results, label_act = test.gbm[,47]) %>%
   tbl_df() %>%
   transmute(label_pred = X1, prod_pred = X2, label_act)

df_results %>% arrange(desc(prod_pred)) %>% head(50)

cm <- confusionMatrix(df_results$label_pred, df_results$label_act)
data.frame(cm$overall)
