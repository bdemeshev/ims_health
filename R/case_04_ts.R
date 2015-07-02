library(stringr)
library(forecast)
library(ggplot2)
library(TSA)
#library(dplyr)
library(readxl)
library(lattice)

data(airmiles)
h <- airmiles
xyplot(airmiles)
# xyplot(airmiles, )
?xyplot
str(airmiles)

a <- read_excel("../data/case_4_.xlsx")
glimpse(a)
dplyr::glimpse(a)

ats <- ts(a, frequency = 52, start=c(2010,2)) 
ats[,"Sales"]
start(ats)
end(ats)
frequency(ats)
time(ats)
factor(cycle(ats))

dls
lag_s <- lag(sales)

new <- ts.union(sales, stats::lag(sales,-1), diff(sales))
new

?lag
ts.

sales <- ats[,"Sales"]
dls <- diff(log(sales))
dls
lag.plot(dls, 9, do.lines=FALSE)

lag.plot(airmiles, 12, do.lines=FALSE)


window(ats, start=c(2010,10), end=c(2010,20))

xyplot(airmiles, type="h")
xyplot(a$Sales)
str(ats)
xyplot(ats)
xyplot(ats, type="h")
start(ats)



tsdisplay(ats[,"Sales"])

ats[,"Sales"]

# ats[, "new"] <- diff(ats[, "Sales"])

devents <- ats[,"Doctor Event"]
model1 <- auto.arima(sales, xreg=devents)
model1

model2 <- arimax(sales, order=c(1,1,1), 
                   
                   
                 xtransf=data.frame(devents=devents), 
                 transfer=list(c(1,1)), method="ML")
model2
?arimax

air.m1=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
              xreg=data.frame(Dec96=1*(seq(airmiles)==12),
                              Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)),
              method='ML')
air.m1
?arimax


t <- data.frame(I911=1*(seq(airmiles)==69),
           I911=1*(seq(airmiles)==69))
t
str(air.m1)

seq(ats)


bb <- stl(airmiles[,1], s.window = "periodic")
plot(bb)
airmiles
str(airmiles)


data.frame(devents=devents, devents=devents)


