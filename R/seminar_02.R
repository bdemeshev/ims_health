# devtools
# library(devtools)
# install_github("bdemeshev/rims")

# загружаем
library(dplyr)
library(randomForest)
library(vcd)
library(readxl)
library(psych)

# устанавливаем рабочую папку
setwd("~/Documents/ims_health/data")
# загружаем файл
ph <- read_excel("case_1_pharmacy_segmentation.xlsx")
tail(ph) # смотрим на хвостик

# переименуем названия столбцов 
colnames(ph)[1:4] <- c("id", "type", "population","income")
colnames(ph)[5:17] <- paste("factor", 1:13, sep="_")

ph2 <- mutate(ph, type=factor(type), l_income=log(income))
ph3 <- mutate_each(ph2, "factor", factor_1:factor_13, id)

glimpse(ph)

ph2 <- mutate(ph, type=factor(type), l_income=log(income))
glimpse(ph2)
ph$type[3] <- "Привет"
ph2$type[3] <- "Привет"
levels(ph2$type) # <- c("A","B","C","D","E")
glimpse(ph2)
ph3 <- mutate_each(ph2, "factor", factor_1:factor_13, id)
glimpse(ph3)

# ph4 <- filter(ph3, factor_4=="НЕТ", income>30000)
ph_test <- filter(ph3, is.na(type))
ph_train <- filter(ph3, !is.na(type))
nrow(ph_train)
nrow(ph_test)

describe(ph_train) # описание данных из пакета pscyh

# пропущенных наблюдений (в factor_3 и factor_9) очень мало
# их не жалко
ph_train2 <- na.omit(ph_train)
nrow(ph_train2)
nrow(ph)
nrow(ph2)
nrow(ph3)
nrow(ph_test)
nrow(ph_train)

best_formula <- type~income+factor_1
all_formula <- type~.-id

model_rf1 <- randomForest(data=ph_train2, best_formula)
ph_test$for_rf <- predict(model_rf1, newdata=ph_test)

nrow(ph_test)
head(ph_test)

summary(ph_test$for_rf)
help(predict)

set.seed(13)
model_rf2 <- randomForest(data=ph_train2, all_formula)
ph_test$for_rf <- predict(model_rf2, newdata=ph_test)
summary(ph_test$for_rf)

predictions <- predict(model_rf2, 
        newdata=ph_test, type = "prob")
head(predictions, n=10)
runif(1) # случайное число от 0 до 1
set.seed(13) # устанавливаем значение зерна генератора случайных чисел


help(runif)
nrow(ph_train2) # число строк в обучающей выборки
# ph_subtrain <- ph_train2[,]

runif(5, min=1, max=20)
sample(1:20, size=5)

row_nums <- sample(1:nrow(ph_train2), size = 800)
row_nums # номера строк для тестовой выборки

ph_subtest <- ph_train2[row_nums,]
ph_subtrain <- ph_train2[-row_nums,]
set.seed(13)
rf2_evaluation <- randomForest(data=ph_subtrain, all_formula)
ph_subtest$for_rf <- predict(rf2_evaluation, newdata=ph_subtest)

table(ph_subtest$type, ph_subtest$for_rf)
mosaic(data=ph_subtest, ~for_rf+type+factor_4, shade=TRUE)
glimpse(ph_subtest)


