library("ggplot2")
library("dplyr")
library("xlread")
library("readxl")
library("vcd")
library("caret")
library("randomForest")
library("psych")
library("reshape2")
library("stringr")

df <- read_excel("../data/case_2_predict_corrected.xlsx", sheet = "Report")
df2 <- read_excel("../data/case_2_predict_corrected.xlsx", sheet = "Top-10")
df3 <- read_excel("../data/case_2_predict_corrected.xlsx", sheet = "Top-10b")

glimpse(df)
glimpse(df2)
glimpse(df3)

glimpse(df)
unique(df$`Time period`)

glimpse(df2)

df2_melted <- melt(df2, id.vars=c("Brand","what"))
glimpse(df2_melted)

df2_melted <- mutate(df2_melted, value=as.numeric(value))

glimpse(df3)

df3_melted <- melt(df3, id.vars=c("letter","what"))
glimpse(df3_melted)

# тестируем кодировки
a <- "sssfdваоылваоы в"
b <- str_conv(a, encoding = "cp1251")

str_conv(string = b, encoding = "utf8")
library(rims)
d <- str_utf2cp(b)
str(d)

colnames(df)
