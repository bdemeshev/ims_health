library(forecast)

# создание временного ряда из вектора
x <- 1:10
x
xt <- ts(x, start=2017.5, frequency=4)
xt
xt <- ts(x, start=2017.5, frequency=12)
xt

help(wineind)
wineind

plot(wineind)


w <- wineind

# прогноз
ses(w, h=3)
plot(ses(w, h=3))

model <- ets(w, model="AAA")
summary(model)

forecast(model, h=12)
plot(forecast(model, h=12))

# авто-подбор
model <- ets(w)
summary(model)

data()
thetaf(LakeHuron, h=4, level = 0.7)
plot(thetaf(LakeHuron, h=4))

devtools::install_github("bdemeshev/sophisthse")

# скачать
# распаковать
devtools::install("...")

library(sophisthse)
z <- sophisthse("POPNUM_Y")

plot(z)
plot(thetaf(z, h=3))

tsdisplay(wineind)
Acf(wineind, plot = FALSE)
?Acf

tsdisplay(LakeHuron)

y <- arima.sim(n=200,
  model = list(ar=c(0.1,0.2)))
tsdisplay(y)

model_ar2 <- Arima(LakeHuron, order=c(2,0,0))
summary(model_ar2)

forecast(model_ar2, h=3)
plot(forecast(model_ar2, h=3))

m_huron <- auto.arima(LakeHuron, approximation = FALSE)
summary(m_huron)
LakeHuron
forecast(m_huron, h=3)

sessionInfo()

