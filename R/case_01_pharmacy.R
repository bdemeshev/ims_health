library("ggplot2")
library("dplyr")
library("xlread")
library("readxl")
library("vcd")
library("caret")
library("randomForest")
library("psych")

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

par_grid <- expand.grid(mtry=5)
model <- train(data=train_sample, type~., method="rf")

model <- randomForest(data=train_sample, 
                      type~.)




