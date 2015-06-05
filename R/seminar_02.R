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

# изменим тип переменной type на факторный
factor_type <- factor(ph$type)

# чтобы не выполнять команды для каждой переменной отдельно существуют функции mutate(), mutate_each()

# mutate() одновременно вычисляет значения нескольких функций от нескольких переменных
# например, создаем новую таблицу ph2, которая повторяет таблицу ph,
# но содержит факторную переменную type и новый столбец l_income с логарифмами дохода
ph2 <- mutate(ph, type=factor(type), l_income=log(income))

glimpse(ph2) # в описании тип переменной type изменился

# присваиваем третьему элементу вектора type из ph значение "Привет" 
ph$type[3] <- "Привет"

# то же самое не будет работать с ph2, так как переменная type
# факторная и не имеет такого значения среди возможных
ph2$type[3] <- "Привет"

# значения факторной переменной type
levels(ph2$type)

# можно добавить новое значение при необходимости
# levels(ph2$type) <- c("A","B","C","D","E") 

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

# получаем предсказания для наблюдений тестовой выборки
# и записываем их в новый столбец for_rf в таблицу ph_test
ph_test$for_rf <- predict(model_rf1, newdata=ph_test)

# посмотрим на полученные значения
summary(ph_test$for_rf)
help(predict)

# небольшое отступление -- генерация случайных чисел
# чтобы можно было воспроизводить результат
set.seed(13) # устанавливаем значение зерна генератора случайных чисел

runif(1) # случайное число от 0 до 1
help(runif)

runif(5, min=1, max=20) # пять случайных чисел от 1 до 20
sample(1:20, size=5) # пять целых случайных чисел от 1 до 20

# запускаем случайный лес с другой формулой, включающей все 
# объясняющие переменные, кроме id
set.seed(13)
model_rf2 <- randomForest(data=ph_train2, all_formula)
ph_test$for_rf <- predict(model_rf2, newdata=ph_test)
summary(ph_test$for_rf)

# предсказывать можно не только сам класс, но и вероятности
# попадания в каждый класс
predictions <- predict(model_rf2, 
                       newdata=ph_test, type = "prob")
head(predictions, n=10)

nrow(ph_train2) # число строк в обучающей выборки

# проверим качество полученной классификации
# для этого разделим учебную выборку ещё на два кусочка subtrain и subtest
# случайно генерируем номера строк для 800 наблюдений, которые попадут в subtest
row_nums <- sample(1:nrow(ph_train2), size = 800)

# в subtest попадают строки учебной выборки со сгенерированными номерами
ph_subtest <- ph_train2[row_nums,]
# в subtrain все остальные
ph_subtrain <- ph_train2[-row_nums,]

# запускаем алгоритм случайного леса для subtrain
set.seed(13)
rf2_evaluation <- randomForest(data=ph_subtrain, all_formula)

# строим предсказания для subtest
ph_subtest$for_rf <- predict(rf2_evaluation, newdata=ph_subtest)

# таблица, в которой по строкам указаны истинные классы,
# а по столбцам предсказанные 
table(ph_subtest$type, ph_subtest$for_rf)

# красивый мозаичный график
mosaic(data=ph_subtest, ~for_rf+type+factor_4, shade=TRUE)
