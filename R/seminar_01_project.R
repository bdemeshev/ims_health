# План для первого занятия
# Разная предварительная обработка данных

#1. установить какие-нибудь пакеты
install.packages("readxl")
library("readxl")
library("foreign")

#2. прочитать данные 
getwd()
setwd("C:/Users/Mu/Google Диск/IMS Health")

df1 <- read_excel("data/case_1.xlsx", sheet=1)  
df2  <- read.csv("data/case_1.1.csv", sep = ";", dec = ",")

#3. взглянуть на данные
# если написать head в русской раскладке, то получится руфв, как крыша почти :)

library(dplyr)

dim(df1)

glimpse(df1)
head(df1)

#4. поменять имена переменных
names(df1)
names(df1)[1:4] <- c("id","type","population","income")
names(df1)[5:17] <- paste("factor", 1:13, sep = "")

# столбик в табличке, клеточка в табличке
df1[,1]
df1$id[1:10]

df1[5,1]

# убрать первую строку таблицы, в которую попали названия
df1 <- df1[-1,]

#5. разделить переменные на качественные и количественные 

df1 <- mutate_each(df1, "as.factor", type, factor1,factor2,factor4:factor13)
df1 <- mutate_each(df1, "as.numeric", population, income, factor3)

# в переменной factor3 создаются 199 NA на месте ответа "отсутствует"
sum(is.na(df1$factor3))

# 6. выделять кусочки
# выбрать некоторые столбцы из таблички
df1.1 <- dplyr::select(df1, id, population, factor1)
df1.2 <- dplyr::select(df1, id:income)
df1.3 <- dplyr::select(df1, starts_with("factor"))
df1.4 <- dplyr::select(df1, -starts_with("factor"))

# выбрать некоторые наблюдения 
# выбрать аптеки и городов с населением больше 500000 и доходом меньше 20000
df1.6 <- filter(df1, population>500000, income<20000)

# выбрать аптеки из группы А
df1.7 <- filter(df1, type=="A")

# выбрать аптеки из делового района или из центра
df1.8 <- filter(df1, factor12 %in% c("ЦЕНТР", "ДЕЛОВОЙ"))

#7. добавить новый признак
# аптека находится в жилом доме в центре

df1 <- mutate(df1, livecenter=(factor9=="ЖИЛОЙ")*(factor12=="ЦЕНТР"))
df1$livecenter[1:10]

#8. разделить выборку на учебную и тестовую

in_train <- !is.na(df1$type)
train_sample <- df1[in_train,]
train_sample <- na.omit(train_sample)

test_sample <- df[-in_train,]

nrow(train_sample)
nrow(test_sample)

#9. описательные статистики по учебной выборке

summary(train_sample)

#10. описательные статистики отдельно по классам аптек

classes <- group_by(train_sample, type)
summary <- summarise(classes, mpop=mean(population), sdpop=sd(population), 
          minc=mean(income), sdinc=sd(income), center=sum(factor12 == "ЦЕНТР"),
          network=sum(factor8 == "СЕТЬ"))

#11. зависимость целевой переменной от фактора 9
table(df$factor9,df$type)

#12. упорядочить выборку по возрастанию дохода
train_sample2 <- arrange(train_sample, income)
head(train_sample2)

# упорядочить по убыванию количества населения
train_sample3 <- arrange(train_sample, -population)
head(train_sample3)

#13. склеить две таблички (summary, train_sample)

train_sum <- left_join(train_sample, summary, by="type")
train_sum[1:10, c(2, 20:24)]

#14. записать что-нибудь получившееся
write.csv()