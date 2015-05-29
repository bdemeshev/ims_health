library("ggplot2")
library("dplyr")
library("xlread")
library("readxl")
library("vcd")
library("caret")
library("randomForest")
library("psych")
library("reshape2")

df2 <- read_excel("../data/case_2_predict_corrected.xlsx", sheet = "Report")
df3 <- read_excel("../data/case_2_predict_corrected.xlsx", sheet = "Top-10")

glimpse(df2)
glimpse(df3)

head(df3)
df3_1 <- df3[1:40,]
df3_2 <- df3[41:81,1:18]
colnames(df3_2) <- df3_2[1,]
head(df3_1)
head(df3_2)
colnames(df3_1)[1:2] <- c("brand","variable")
colnames(df3_2)[1:2] <- c("brand","variable")

df3_1_melted <- melt(data=df3_1,id.vars = c("brand","variable"))
head(df3_1_melted)
