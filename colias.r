#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Scomber colias

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/colias.csv"
colias <- read.csv2(file)
colias <- Subset(colias, !is.na(weight) & !is.na(length))
colias$length <- as.integer(colias$length)
colias$weight <- as.numeric(colias$weight)
str(colias)



#Calculations

colias$logL <- log(colias$length)
colias$logW <- log(colias$weight)

colias2 <- Subset(colias,logW >= -0.5)
str(colias2)

lm1_colias <- lm(logW~logL,data=colias2)
fitPlot(lm1_colias,xlab="log Total Length (mm)",ylab="log Weight (g)",main="colias")
summary(lm1_colias)

hoCoef(lm1_colias,2,3)
confint(lm1_colias)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_colias)

#coefficients A and CL95%
a1 <- lm1_colias
exp(a1$coefficients[1])
a <- confint(lm1_colias)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
