#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Trachurus trachurus

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/trachurus.csv"
trachurus <- read.csv2(file)
trachurus$length <- as.integer(trachurus$length)
trachurus$weight <- as.numeric(trachurus$weight)
str(trachurus)



#Calculations

trachurus$logL <- log(trachurus$length)
trachurus$logW <- log(trachurus$weight)

trachurus2 <- Subset(trachurus,logW >= -0.5)
str(trachurus2)

lm1_tr <- lm(logW~logL,data=trachurus2)
fitPlot(lm1_tr,xlab="log Total Length (mm)",ylab="log Weight (g)",main="trachurus")
summary(lm1_tr)

hoCoef(lm1_tr,2,3)
confint(lm1_tr)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_tr)

#coefficients A and CL95%
a1 <- lm1_tr
exp(a1$coefficients[1])
a <- confint(lm1_tr)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]