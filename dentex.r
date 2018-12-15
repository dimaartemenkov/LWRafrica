#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Dentex dentex

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/dentex.csv"
dentex <- read.csv2(file)
dentex <- Subset(dentex, !is.na(weight) & !is.na(length))
dentex$length <- as.integer(dentex$length)
dentex$weight <- as.numeric(dentex$weight)
str(dentex)



#Calculations

dentex$logL <- log(dentex$length)
dentex$logW <- log(dentex$weight)

dentex2 <- Subset(dentex,logW >= -0.5)
str(dentex2)

lm1_dentex <- lm(logW~logL,data=dentex2)
fitPlot(lm1_dentex,xlab="log Total Length (mm)",ylab="log Weight (g)",main="dentex")
summary(lm1_dentex)

hoCoef(lm1_dentex,2,3)
confint(lm1_dentex)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_dentex)

#coefficients A and CL95%
a1 <- lm1_dentex
exp(a1$coefficients[1])
a <- confint(lm1_dentex)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
