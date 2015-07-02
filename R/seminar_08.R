library(readxl)
library(TSA)
library(lattice)
library(forecast)

d <- read_excel("../data/case_4_.xlsx")
str(d)
d$Sales
d[,"Sales"]

dts <- ts(d, frequency = 52, start=c(2015,10) )
dts[3,2]
str(dts)
dts$Sales
dts[,"Sales"]

sales <- dts[,"Sales"]
start(dts)
end(dts)
frequency(dts)
window(dts, start=c(2015,50), end=c(2016,5))
time(dts)
1/52
cycle(dts)
factor(cycle(dts)) # факторные переменные для номера сезона/периода

diff(sales)
lag(sales, -1)

# with NA
new <- ts.union(sales, lag(sales,-1), diff(sales))
# without NA
new2 <- ts.intersect(sales, lag(sales,-1), diff(sales))

new2
xyplot(dts)

tsdisplay(sales)
model0 <- auto.arima(sales)
model0
colnames(dts)

devent <- dts[,"Doctor Event"]
model1 <- auto.arima(sales, xreg=devent)
model_1manual <- Arima(sales, xreg=devent, order=c(1,0,1))
model1
model_1manual

all <- ts.intersect(sales, d_sales=diff(sales), devent)
all

model1b <- Arima(all[,"d_sales"], 
                 xreg=all[,"devent"], 
                 order=c(0,0,1))
model1b
model1


colnames(dts)
phevent <- dts[,"Pharmacy Event"]
x <- ts.union(devent, phevent)
x
model2 <- arimax(sales, order=c(0,1,1), 
                 xtransf = x, 
                 transfer = list(c(1,0), c(1,0)) )
model2

colnames(dts)

xyplot(dts)

tfun <- rep(list(c(1,0)),4)
x <- dts[,c(3, 5, 13, 14)]

model3 <- arimax(sales, order=c(0,0,1), 
                 xtransf = data.frame(devents, devents), 
                 transfer = list(c(0,0),c(1,0)), method="ML")
model3
# кросс-корреляции ?
colnames(dts)
dd <- dts[,"Doctor Detailing"]

model3 <- arimax(sales, order=c(0,0,1), 
                 xtransf = data.frame(dd), 
                 transfer = list(c(1,0)), method="ML")
model3

dts2 <- ts.intersect(dts, lag(dts,-1))
head(dts2)

library(glmnet)
colnames(dts2)
y <- dts2[,"dts.Sales"]
X <- dts2[, c(-1,-2,-17,-18)]

model <- cv.glmnet(as.matrix(X), as.vector(y))
model$lambda.1se
model$lambda.min
coef(model, s="lambda.1se")
coef(model, s="lambda.min")

model <- glmnet(as.matrix(X), as.vector(y), lambda = 0.05)
cde <- as.vector(coef(model))[c(2,16)]
sum(devent)*sum(cde)


dts3 <- ts.intersect(dts, lag(dts,-1))
colnames(dts3)
y <- dts3[,"dts.Sales"]
X <- dts3[, 19:32]

model <- cv.glmnet(as.matrix(X), as.vector(y))
model$lambda.1se
model$lambda.min

model <- glmnet(as.matrix(X), as.vector(y), lambda=3500)
coef(model)
