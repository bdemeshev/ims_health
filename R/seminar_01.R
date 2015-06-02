setwd("~/Documents/ims_health/data")

2 + 3

a <- 5

moe_lubimoe_chislo <- c(7.5, 6, 13)
moe_lubimoe_chislo
max(moe_lubimoe_chislo)
y <- 100:102
y
d <- seq(from=10.5,to=500,by=11)
d

# считаем среднее арифметическое вектора d
mean(d)

y
moe_lubimoe_chislo

y[3]

y[3] <- 56
y
t<-67

cos <- data.frame(y=y, moe=moe_lubimoe_chislo)
cos
cos[5]
cos[3,2]
cos[1,]
cos[,1]

Moe_lubimoe_chislo <- 7
moe_lubimoe_chislo
Moe_lubimoe_chislo

cos$moe


library(readxl)
ph <- read_excel("case_1_pharmacy_segmentation.xlsx")
head(ph,n=3)
tail(ph,n=3)

library(psych)
describe(ph)

ph[,3]
ph$`Население в городе`
ph$`Категория аптеки`
colnames(ph)

colnames(ph)[1:4] <- 
  c("id", "type", "population","income")
ph$type
colnames(ph)
1:13
paste0("privet",1:13)
paste("privet",1:13,sep="_")

colnames(ph)[5:17] <- 
  paste("privet",1:13,sep="_")

colnames(ph)

library(dplyr)

sin(cos(0))
0 %>% cos() %>% sin()

solntse <- ph %>% group_by(type) %>% 
  summarise(m_inc=mean(income), 
            max_pop=max(population)) 
  
solntse
solntse

write.csv(solntse, file="results.csv")

write.table(solntse, file="results.csv",sep = ";",dec=",",
            row.names = FALSE)

# dplyr, psych, readxl, ggplot2, randomForest, vcd


help("write.table")

# CRAN название пакета
# stats.stackexchange.com


