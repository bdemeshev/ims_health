library("ggplot2")
library("dplyr")
library("xlread")
library("readxl")
library("vcd")
library("caret")
library("randomForest")
library("psych")
# https://github.com/dmlc/xgboost
devtools::install_github("dmlc/xgboost", subdir='R-package')

library("xgboost")

df <- read_excel("../data/case_1_pharmacy_segmentation.xlsx")
glimpse(df)
names(df)[1:4] <- c("id","type","population","income")
names(df)[5:17] <- paste("factor",1:13,sep = "")
  
df <- df[-1,]

describe(df)


df <- mutate(df, population=as.numeric(population),
             income=as.numeric(income),
             factor3=as.numeric(factor3)
             )
df <- mutate_each(df, "as.factor", type, factor1,factor2,factor4:factor13)

df <- dplyr::select(df, -id)

summary(df$type)
summary(df$factor9)
table(df$factor9,df$type)
mosaic(data=df, ~factor9+type, shade=TRUE)

in_train <- createDataPartition(df$type, p=0.75, list=FALSE)
in_train

# реально предсказываем NA
in_train <- !is.na(df$type)
train_sample <- df[in_train,]

train_sample <- na.omit(train_sample)

test_sample <- df[-in_train,]
nrow(train_sample)
nrow(test_sample)

#par_grid <- expand.grid(mtry=5)
#model <- train(data=train_sample, type~., method="rf",verbose=TRUE,
#               tuneGrid=par_grid)
# очень долго думает



model <- randomForest(data=train_sample, 
                      type~.)
preds <- predict(model, test_sample, type="prob")
head(preds)

# evaluate quality
in_subtrain <- createDataPartition(train_sample$type, p=0.75, list=FALSE)
subtrain_sample <- train_sample[in_subtrain,]
subtest_sample <- train_sample[-in_subtrain,]
nrow(subtest_sample)
nrow(subtrain_sample)
model <- randomForest(data=subtrain_sample, 
                      type~.)
subtest_sample$preds <- predict(model, subtest_sample)
table(subtest_sample$type, subtest_sample$preds)
mosaic(data=subtest_sample, ~preds+type, shade=TRUE)
#

# xgboost - больше мороки, а качество похоже на randomForest
x_train <- model.matrix(data = subtrain_sample, type~.)
x_test <- model.matrix(data = subtest_sample, type~.)

label_train <- as.numeric(subtrain_sample$type)-1
label_train

bst <- xgboost(data=x_train, label=label_train, 
               max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "multi:softmax", num_class=4)
subtest_sample$preds <- predict(bst, x_test)
table(subtest_sample$type, subtest_sample$preds)
mosaic(data=subtest_sample, ~preds+type, shade=TRUE)


bst <- xgboost(data=x_train, label=label_train, 
               max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "multi:softprob", num_class=4)
preds <- matrix(predict(bst, x_test),ncol=4,byrow=TRUE)
head(preds)
# совсем не похоже на randomForest


