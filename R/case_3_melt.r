library(dplyr)
library(reshape2)
library(readxl)
library(zoo)
library(lubridate)
library(stringr)
library(tidyr)

# читаем файл
df <- read_excel("../data/case_3_melt.xlsx",sheet = 1)
glimpse(df)

# задаем верные имена столбцов
old_colnames <- colnames(df)[5:28]
dates <- rep(paste0("2012-",12:1),each=2)
colnames(df)[5:28] <- paste0(old_colnames, "_", dates )
colnames(df)

# мелтим
df_melted <- melt(df, id.vars = c("id","firm","name","full_name"))
glimpse(df_melted)

# разрезаем столбец на два
df_final <- df_melted %>% separate(variable, into=c("var","date"), sep="_")

# happy
glimpse(df_final)
