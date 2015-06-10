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

glimpse(ph3)

# с помощью функции filter() 
# в ph4 можно отобрать строки из ph3, где значение factor_4 -- "НЕТ" и доход больше 30000
# ph4 <- filter(ph3, factor_4=="НЕТ", income>30000)

# включаем в тестовую выборку строки, в которых 
# значение переменной type равно NA с помощью функции is.na()
ph_test <- filter(ph3, is.na(type))

# в учебную, наоборот, те в которых не равно NA (! -- не)
ph_train <- filter(ph3, !is.na(type))

# количество наблюдений в учебной и тестовой выборках
nrow(ph_train)
nrow(ph_test)

# можно посмотреть, сколько значений пропущено в каждой переменной
describe(ph_train) # описание данных из пакета psych

# пропущенных наблюдений (в factor_3 и factor_9) очень мало
# их не жалко

# функция na.omit() убирает все строки, в которых хотя бы одна
# переменная равна NA
ph_train2 <- na.omit(ph_train)
nrow(ph_train2)

# можно создать отдельные объекты для формул
# объясняемая переменная ~ объясняющие
best_formula <- type~income+factor_1
all_formula <- type~.-id

# запускаем алгоритм случайного леса, результаты записываем в model_rf1
model_rf1 <- randomForest(data=ph_train2, best_formula)

# создаём дамми-переменную, равную 1 для аптек типа А и 0 иначе
ph3$dA <- ifelse(ph3$type=="A",1,0)
ph3 <- mutate(ph3, dB=ifelse(type=="B",1,0) )

glimpse(ph3)

# регрессия МНК на факторную переменную
model <- lm(data=ph_train2, income~population+type)
summary(model)


ph_temp <- model.matrix(data=ph_train2, income~population+type)
head(ph_temp)

# интерпретация алгоритма случайного леса
model_rf2 <- randomForest(data=ph_train2, all_formula)
# коэффициенты важности всех объясняющих переменных
dG <- importance(model_rf2)
dG
sum(dG)
dG/sum(dG)

# зависимость от количественной переменной
pharm_typical <- ph_train2[7,]
pharm_typical

# 
rep(1,9)
rep("Привет",77)

ph_vary_income <- ph_train2[rep(7,100),]
glimpse(ph_vary_income)

ph_vary_income$income <- seq(from=10000,to=40000,len=100)
glimpse(ph_vary_income)


prognoz_vary_income <- predict(model_rf2, 
                               newdata=ph_vary_income,
                               type="prob")
head(prognoz_vary_income)
tail(prognoz_vary_income)

ph_vary_income <- cbind(ph_vary_income, prognoz_vary_income)

bg <- qplot(data=ph_vary_income, x=income, y=A)
bg

bg + coord_flip()

bg + stat_smooth()

bg + stat_smooth(method="lm")

bg + geom_line(aes(y=B),colour="red")
bg + geom_point(aes(y=B),colour="red")

five_vars <- select(ph_vary_income, income, A, B, C, D)

# самые частые преобразования данных:

# отбор наблюдений
# new <- filter(old, income > 10^4)
# отбор переменных 
# new <- select(old, income > 10^4)
# изменение таблички
# new <- mutate(old, linc = log(income))
# группировка (нарезка на подтаблички)
# new <- group_by(old, type)
# описание нарезанных подтабличек
# new <- summarise(old, mean=mean(income))
# сортировка
# new <- arrange(old, income)

head(five_vars)

five_vars_flat <- melt(data = five_vars, id.vars = "income")
tail(five_vars_flat)

# library(reshape2)

fvf <- select(ph_vary_income, income, A, B, C, D) %>%
  melt(id.vars = "income")
  
qplot(data=five_vars_flat, 
      x=income, 
      y=value, 
      col=variable)

qplot(data=five_vars_flat, 
      x=income, 
      y=value, 
      col=variable, geom="line")

ph3_new <- group_by(ph3, type) %>% 
  mutate(delta_inc=income-mean(income))
ph3_new %>% select(delta_inc,income) %>% head()

micro_table <- group_by(ph3,type) %>% 
  summarise(kolvo=n())
micro_table

ph3_newnew <- left_join(x=ph3_new,
                        y=micro_table,
                        by="type")
ph3_newnew %>% select(income,kolvo) %>% head()
