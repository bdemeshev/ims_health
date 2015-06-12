# Четвертое занятие -- линейная регрессия

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
glimpse(ph)

# добавляем значения логарифма дохода,
# преобразуем тип нужных переменных к факторам
ph2 <- mutate(ph, type=factor(type), l_income=log(income))
glimpse(ph2)
ph3 <- mutate_each(ph2, "factor", factor_1:factor_13, id)

# задача: восстановим городские данные

ph3$population
# уникальные значения в переменной количества населения
# указывают на разные города
unique(ph3$population)

# сгруппируем по количеству населения(то есть по городам)
# посчитаем для каждого города среднее значение дохода
part_I <- ph3 %>% group_by(population) %>% summarise(av_inc = mean(income))

# рассчитываем для каждого города количество аптек всех типов
part_II <- ph3 %>% group_by(population) %>% summarise(n_ABCD=n())
part_II

# группируем не только по городу, но и по типам аптек
# считаем в каждом городе количество аптек каждого типа
part_III <- ph3 %>% group_by(population,type) %>% summarise(kolvo=n())

# преобразуем из длинной тиблицы в широкую
# с помощью функции dcast(значения по строкам ~ значения по столбцам)
part_III_pivot <- part_III %>% dcast(population~type) 

# dcast(data=..., row_var~col_var)
help(dcast)

# смотрим, на каких местах в полученной таблице стоят NA
is.na(part_III_pivot)
# на эти места ставим нули 
part_III_pivot[is.na(part_III_pivot)] <- 0

# разные способы отобрать строки из таблицы
part_III_pivot
# берем все, кроме третьей
part_III_pivot[-3,]
# все кроме строк с третьей по пятую
part_III_pivot[-(3:5),]
# выбрать те города, в которых есть аптеки типа А
part_III_pivot %>% filter(A>0)

# склеиваем два полученных кусочка в одну таблицу
# средний доход и количество аптек каждого типа для каждого города
city <- left_join(x=part_I, y=part_III_pivot,
                  by="population")
city

# оценим линейную регрессию
# так нельзя записывать, так как NA означает пропущенную переменную
# model_ols <- lm(data=city, av_inc~population+A+B+C+D+`NA`)

# присвоим столбцу с типом аптек NA имя unknown
colnames(city)
colnames(city)[7] <- "unknown"
colnames(city)

# оцениваем модель линейной регрессии
# объясняем средний доход по численности населения и 
# количеству аптек каждого типа
model_ols <- lm(data=city, av_inc~population+A+B+C+D+unknown)
# отчет по модели
summary(model_ols)

# доверительные интервалы
confint(model_ols)

# создаем одно наблюдение для теста с заданными значениями
city_test <- data.frame(population=10000,
              A=1,B=1,C=1,D=1,unknown=0)
city_test

# по оценненым параметрам строим прогноз среднего дохода
predict(model_ols, newdata=city_test)

# график для оцененной регрессии
library(sjPlot)
sjp.lm(model_ols)

# оценим более простую модель
model_simple <- lm(data=city, av_inc~population)
summary(model_simple)

# сравнении двух моделей (одна вложена в другую)
anova(model_simple, model_ols)

# разные способы задать формулу
form <- av_inc~population+A+B+C+D+unknown-A

model_b <- lm(data=city, av_inc~A+B+A:B)
summary(model_b)

model_b <- lm(data=city, av_inc~A*B*C)
summary(model_b)

model_b <- lm(data=city, av_inc~A*B*C-A:B:C)
summary(model_b)

model_b <- lm(data=city, av_inc~(A+B+C)^3)
summary(model_b)

# I() -- понимать формулу так, как написано
model_b <- lm(data=city, av_inc~I((A+B+C)^3) )
summary(model_b)

model_b <- lm(data=city, av_inc~I((A+B+C)^3) )
summary(model_b)

model_b <- lm(data=city, 
    av_inc~poly(population,3,raw=TRUE) + A + B+ C+D+unknown )
summary(model_b)

# достанем полученные остатки модели
city$res <- resid(model_b)
# построим графики остатков
qplot(data=city, x=population, y=res)
qplot(data=city, x=res)

# набор встроенных графиков для линейной регрессии  
plot(model_b)

