# семинар 05

library(readxl)
library(glmnet)
library(tidyr)
library(lmtest)
library(car)
library(dplyr)
library(psych)
library(reshape2)
library(lubridate)
library(zoo)

ik <- read_excel("case_3_original.xlsx")
glimpse(ik)

colnames(ik)


12:1

rep(12:1, 2)
rep(12:1, each=2)
rep(c("usd","upak"),12)

newnames <- paste0("2012-",
                   rep(12:1, each=2),"_",
                   rep(c("usd","upak"),12)  )
newnames
colnames(ik)[5:28] <- newnames

glimpse(ik)
describe(ik)


ik_melted <- melt(data=ik, id.vars = c("id","firm","name","full_name"))
glimpse(ik_melted)

ik_final <- separate(ik_melted, variable, into=c("date","sales"), sep="_")
glimpse(ik_final)
?separate

ik_final <- ik_final %>% mutate(sales=factor(sales))
glimpse(ik_final)

ik_final <- ik_final %>% mutate(date=as.yearmon(date))
glimpse(ik_final)

ymd("2015-06-18") + days(42) + months(3)

# простые операции с датами
ik_plus <- ik_final %>% mutate(year=year(date), month=month(date),
              newdate=as.POSIXct(date) + months(5) + days(10))
glimpse(ik_plus)


# мультиколлинеарность

# жесткая
m1 <- lm(data=ik_plus, value~month+I(2*month))
summary(m1)

# нестрогая 
m2 <- lm(data=cars, dist~poly(speed,4,raw = TRUE) )
summary(m2)
help(cars)
cars

m2 <- lm(data=cars, dist~speed+I(speed^2)+
           I(speed^3) + I(speed^4)+I(speed^5))
summary(m2)
vif(m2)

# по здравому смыслу упростить модель до исчезновения МК
m2 <- lm(data=cars, dist~speed)
summary(m2)

# для функции cv.glmnet нужен отдельно y и отдельно матрица X без константы
y <- cars$dist
X <- model.matrix(data=cars,
                  ~0+speed+I(speed^2)+
                    I(speed^3)+I(speed^4)+I(speed^5))

mlasso <- cv.glmnet(X, y)
mlasso$lambda.1se

coef(mlasso, s=5) # коэффициенты регрессии для lambda=5
coef(mlasso, s="lambda.1se")

plot(mlasso)

vignette(package="glmnet")
vignette("glmnet_beta", package="glmnet")

plot(mlasso$glmnet.fit, xvar = "lambda", label = TRUE)


