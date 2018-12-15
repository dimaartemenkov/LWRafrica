#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Trachurus trecae

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/trecae.csv"
trecae <- read.csv2(file)
trecae$length <- as.integer(trecae$length)
trecae$weight <- as.numeric(trecae$weight)
str(trecae)



#Calculations

trecae$logL <- log(trecae$length)
trecae$logW <- log(trecae$weight)

trecae2 <- Subset(trecae,logW >= -0.5)
str(trecae2)

lm1_t <- lm(logW~logL,data=trecae2)
fitPlot(lm1_t,xlab="log Total Length (mm)",ylab="log Weight (g)",main="trecae")
summary(lm1_t)

hoCoef(lm1_t,2,3)
confint(lm1_t)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_t)

#coefficients A and CL95%
a1 <- lm1_t
exp(a1$coefficients[1])
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
