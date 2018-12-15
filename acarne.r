#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Pagellus acarne

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/acarne.csv"
acarne <- read.csv2(file)
acarne <- Subset(acarne, !is.na(weight) & !is.na(length))
acarne$length <- as.integer(acarne$length)
acarne$weight <- as.numeric(acarne$weight)
str(acarne)



#Calculations

acarne$logL <- log(acarne$length)
acarne$logW <- log(acarne$weight)

acarne2 <- Subset(acarne,logW >= -0.5)
str(acarne2)

lm1_acarne <- lm(logW~logL,data=acarne2)
fitPlot(lm1_acarne,xlab="log Total Length (mm)",ylab="log Weight (g)",main="acarne")
summary(lm1_acarne)

hoCoef(lm1_acarne,2,3)
confint(lm1_acarne)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_acarne)

#coefficients A and CL95%
a1 <- lm1_acarne
exp(a1$coefficients[1])
a <- confint(lm1_acarne)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
