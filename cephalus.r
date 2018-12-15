#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Mugil cephalus

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/cephalus.csv"
cephalus <- read.csv2(file)
cephalus <- Subset(cephalus, !is.na(weight) & !is.na(length))
cephalus$length <- as.integer(cephalus$length)
cephalus$weight <- as.numeric(cephalus$weight)
str(cephalus)



#Calculations

cephalus$logL <- log(cephalus$length)
cephalus$logW <- log(cephalus$weight)

cephalus2 <- Subset(cephalus,logW >= -0.5)
str(cephalus2)

lm1_cephalus <- lm(logW~logL,data=cephalus2)
fitPlot(lm1_cephalus,xlab="log Total Length (mm)",ylab="log Weight (g)",main="cephalus")
summary(lm1_cephalus)

hoCoef(lm1_cephalus,2,3)
confint(lm1_cephalus)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_cephalus)

#coefficients A and CL95%
a1 <- lm1_cephalus
exp(a1$coefficients[1])
a <- confint(lm1_cephalus)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
