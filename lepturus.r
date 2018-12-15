#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Trichiurus lepturus

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/lepturus.csv"
lepturus <- read.csv2(file)
lepturus <- Subset(lepturus, !is.na(weight) & !is.na(length))
lepturus$length <- as.integer(lepturus$length)
lepturus$weight <- as.numeric(lepturus$weight)
str(lepturus)



#Calculations

lepturus$logL <- log(lepturus$length)
lepturus$logW <- log(lepturus$weight)

lepturus2 <- Subset(lepturus,logW >= -0.5)
str(lepturus2)

lm1_lepturus <- lm(logW~logL,data=lepturus2)
fitPlot(lm1_lepturus,xlab="log Total Length (mm)",ylab="log Weight (g)",main="lepturus")
summary(lm1_lepturus)

hoCoef(lm1_lepturus,2,3)
confint(lm1_lepturus)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_lepturus)

#coefficients A and CL95%
a1 <- lm1_lepturus
exp(a1$coefficients[1])
a <- confint(lm1_lepturus)
exp(a[1,1])
exp(a[1,2])

#coefficients B  and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
