library(ggplot2) # графики
library(lmtest) # тестирование гипотез
library(sandwich) # se_HC
library(mfx) # логит-пробит
library(dplyr) # манипуляции с данными
library(hexbin)
library(quantreg)

d <- diamonds
glimpse(d)
help(diamonds)

qplot(data=d, carat, price)

nrow(d)

1:5
set.seed(03)
rows <- sample(1:nrow(d), size=1000)
head(rows)

d_sel <- d[rows, ]
qplot(data=d_sel, carat, price)

qplot(data=d, carat, price, geom="hex")  

d2 <- mutate(d, lprice=log(price), lcarat=log(carat))
model <- lm(data=d2, lprice~lcarat + cut)


ggplot(d2, 
  aes(x=lcarat, y=lprice)) + geom_point(aes(alpha=0.005))

qplot(data=d2, x=lcarat, y=lprice, alpha=0.1)

coeftest(model)
coeftest(model, vcov. = vcovHC )

bptest(model)

model_0 <- lm(data=d2, price~carat + cut)
qplot(data=d, carat, price)

model_q <- rq(data=d2, price~carat+cut, tau = c(0.1,0.5,0.9))
summary(model_q)

glimpse(d)

d3 <- mutate(d2, ideal=ifelse(cut=="Ideal",1,0))
glimpse(d3)

model_logit <- logitmfx(data=d3, ideal~  carat + clarity)
model_logit
summary(model_logit$fit)

levels(d3$clarity)

newdata <- data.frame(carat=c(15,16), clarity=c("IF","IF"))
predict(model_logit$fit, newdata=newdata, type="response")
predict(model_logit$fit, newdata=newdata)

# 
devtools::install_github("bdemeshev/rims")
library(rims)

a <- "Привет"
a
library(stringr)
b <- str_conv(a,"cp1251")
b

b
str_utf2cp(b)


