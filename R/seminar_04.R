# Второе занятие -- случайный лес

# чтобы скачать пакет с github:
# library(devtools)
# install_github("bdemeshev/rims")

# загружаем нужные пакеты (предварительно установленные)
library(dplyr)
library(randomForest)
library(vcd)
library(readxl)
library(psych)
library(ggplot2)
library(reshape2)

# устанавливаем рабочую папку
setwd("~/Documents/ims_health/data")

# загружаем файл с данными из этой папки в таблицу ph
ph <- read_excel("case_1_pharmacy_segmentation.xlsx")

tail(ph) # смотрим на хвостик

# переименуем названия столбцов
# первые четыре меняем на заданные "id", "type"...
colnames(ph)[1:4] <- c("id", "type", "population","income")

# столбцам с пятого по семнадцатый присваиваем имена типа "factor_1", "factor_2" ...
colnames(ph)[5:17] <- paste("factor", 1:13, sep="_")

# краткое описание каждой переменной
# видно, что сейчас R различает несколько объектов типа (dbl) -- числа, 
# а остальные относятся к (chr) -- символьные переменные, то есть просто слова
glimpse(ph)


# чтобы не выполнять команды для каждой переменной отдельно существуют функции mutate(), mutate_each()

# mutate() одновременно вычисляет значения нескольких функций от нескольких переменных
# например, создаем новую таблицу ph2, которая повторяет таблицу ph,
# но содержит факторную переменную type и новый столбец l_income с логарифмами дохода
ph2 <- mutate(ph, type=factor(type), l_income=log(income))

glimpse(ph2)

# mutate_each() выполняет одну указанную функцию для целого списка переменных
# таблица ph3 получается из ph2 после применения функции factor()
# к остальным факторным переменным factor_1, factor_2...factor_13, id
ph3 <- mutate_each(ph2, "factor", factor_1:factor_13, id)

ph3$id

# задача: восстановим городские данные

ph3$population
unique(ph3$population)

part_I <- ph3 %>% group_by(population) %>% summarise(av_inc = mean(income))
part_II <- ph3 %>% group_by(population) %>% summarise(n_ABCD=n())
part_II

part_III <- ph3 %>% group_by(population,type) %>% summarise(kolvo=n())
part_III_pivot <- part_III %>% dcast(population~type) 


# dcast(data=..., row_var~col_var)
help(dcast)

is.na(part_III_pivot)

part_III_pivot[is.na(part_III_pivot)] <- 0

part_III_pivot
part_III_pivot[-3,]
part_III_pivot[-(3:5),]
part_III_pivot %>% filter(A>0)
part_I

city <- left_join(x=part_I, y=part_III_pivot,
                  by="population")
city

# model_ols <- lm(data=city, av_inc~population+A+B+C+D+`NA`)
colnames(city)
colnames(city)[7] <- "unknown"
colnames(city)
model_ols <- lm(data=city, av_inc~population+A+B+C+D+unknown)
# отчет по модели
summary(model_ols)

# доверительные интервалы
confint(model_ols)

city_test <- data.frame(population=10000,
              A=1,B=1,C=1,D=1,unknown=0)
city_test

predict(model_ols, newdata=city_test)

library(sjPlot)
sjp.lm(model_ols)

model_simple <- lm(data=city, av_inc~population)
summary(model_simple)

anova(model_simple, model_ols)


form <- av_inc~population+A+B+C+D+unknown-A


model_b <- lm(data=city, av_inc~A+B+A:B)
summary(model_b)

model_b <- lm(data=city, av_inc~A*B*C)
summary(model_b)

model_b <- lm(data=city, av_inc~A*B*C-A:B:C)
summary(model_b)

model_b <- lm(data=city, av_inc~(A+B+C)^3)
summary(model_b)

model_b <- lm(data=city, av_inc~I((A+B+C)^3) )
summary(model_b)

model_b <- lm(data=city, av_inc~I((A+B+C)^3) )
summary(model_b)

model_b <- lm(data=city, 
    av_inc~poly(population,3,raw=TRUE) + A + B+ C+D+unknown )
summary(model_b)


city$res <- resid(model_b)
qplot(data=city, x=population, y=res)
qplot(data=city, x=res)

plot(model_b)

