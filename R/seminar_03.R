# Третье занятие

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

# добавляем переменную логарифма дохода
# преобразуем все факторные переменные к типу "фактор"
ph2 <- mutate(ph, type=factor(type), l_income=log(income))
ph3 <- mutate_each(ph2, "factor", factor_1:factor_13, id)

glimpse(ph3)

# создаем учебную и тестовую выборки
# включаем в тестовую выборку строки, в которых значение переменной type равно NA с помощью функции is.na()
ph_test <- filter(ph3, is.na(type))

# в учебную те, в которых не равно NA (! -- не)
ph_train <- filter(ph3, !is.na(type))

# количество наблюдений в учебной и тестовой выборках
nrow(ph_train)
nrow(ph_test)

# можно посмотреть, сколько значений пропущено в каждой переменной
describe(ph_train) # описание данных из пакета psych

# пропущенных наблюдений (в factor_3 и factor_9) очень мало, их не жалко
# функция na.omit() убирает все строки, в которых хотя бы одна переменная равна NA
ph_train2 <- na.omit(ph_train)
nrow(ph_train2)

# можно создать отдельные объекты для формул
# объясняемая переменная ~ объясняющие
best_formula <- type~income+factor_1
all_formula <- type~.-id

# пример регрессии МНК на факторную переменную
# объясняем уровень дохода с помощью количества населения и типов аптек
model <- lm(data=ph_train2, income~population+type)
summary(model)

ph_temp <- model.matrix(data=ph_train2, income~population+type)
head(ph_temp)

# два способа создать дамми-переменную, равную 1 для аптек типа А и 0 иначе
ph3$dA <- ifelse(ph3$type=="A",1,0)
ph3 <- mutate(ph3, dB=ifelse(type=="B",1,0))
glimpse(ph3)

# интерпретация алгоритма случайного леса
# запускаем алгоритм случайного леса, включая все переменные
model_rf2 <- randomForest(data=ph_train2, all_formula)

# коэффициенты важности всех объясняющих переменных:
# падение индекса Джини для каждого признака
dG <- importance(model_rf2)
dG
# суммарное падение для каждого признака
sum(dG)
# доля вклада в суммарное падение
dG/sum(dG)

# исследуем зависимость от количественной переменной
# зафиксируем значения всех переменных, например, на уровне седьмого наблюдения
pharm_typical <- ph_train2[7,]
pharm_typical

# вектор из 9 единиц
rep(1,9)
# вектор из 77 "Привет"
rep("Привет",77)

# создаем таблицу из 100 строк седьмого наблюдения
ph_vary_income <- ph_train2[rep(7,100),]
glimpse(ph_vary_income)

# изменяем доход от 10000 до 40000,
# чтобы установить его влияние на вероятность аптеки попасть в 
# определенный класс при фиксированных значениях остальных признаков
ph_vary_income$income <- seq(from=10000,to=40000,len=100)
glimpse(ph_vary_income)

# строим прогнозы для оцененной модели и полученных 100 наблюдений 
# выбираем тип "вероятность"
# получаем изменение вероятностей при изменении дохода
prognoz_vary_income <- predict(model_rf2, 
                               newdata=ph_vary_income,
                               type="prob")
head(prognoz_vary_income)
tail(prognoz_vary_income)

# склеиваем таблицы с данными и прогнозами в одну
ph_vary_income <- cbind(ph_vary_income, prognoz_vary_income)

# разная визуализация изменения вероятности при изменении дохода
# изменение вероятности аптеки попасть в группу А
bg <- qplot(data=ph_vary_income, x=income, y=A)
bg

# добавляем к основному графику разные преобразования
bg + coord_flip()

bg + stat_smooth()

bg + stat_smooth(method="lm")

# добавляем вероятность попасть в тип В
bg + geom_line(aes(y=B),colour="red")
bg + geom_point(aes(y=B),colour="red")

# выбираем столбцы со значениями дохода 
# и вероятностями попадания аптеки к каждому типу
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

# преобразование широкой таблицы в длинную по переменной доход
five_vars_flat <- melt(data = five_vars, id.vars = "income")
tail(five_vars_flat)

# другой способ записи
fvf <- select(ph_vary_income, income, A, B, C, D) %>%
  melt(id.vars = "income")
  
# строим график, на котором изображены вероятности для всех типов, которые есть в таблице
# цвет линии зависит от типа аптеки
qplot(data=five_vars_flat, 
      x=income, 
      y=value, 
      col=variable)

qplot(data=five_vars_flat, 
      x=income, 
      y=value, 
      col=variable, geom="line")

# сгруппируем все наблюдения по переменной "тип",
# посчитаем для каждого типа отклонение уровня дохода от среднего
# в этом типе
ph3_new <- group_by(ph3, type) %>% 
  mutate(delta_inc=income-mean(income))
  
# выберем только переменные дохода и отклонения от среднего
ph3_new %>% select(delta_inc,income) %>% head()

# посчитаем количество аптек каждого типа
micro_table <- group_by(ph3,type) %>% 
  summarise(kolvo=n())
micro_table

# подклеим к таблице ph3_new значения количества аптек каждого типа
# из таблицы micro_table по переменной "тип"
# то есть ко всем аптекам типа А подклеиваем их количество и так далее
ph3_newnew <- left_join(x=ph3_new,
                        y=micro_table,
                        by="type")
# выберем из полученной таблицы столбцы дохода и количества
ph3_newnew %>% select(income,kolvo) %>% head()
