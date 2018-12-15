#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Merluccius senegalensis

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/senegalensis.csv"
senegalensis <- read.csv2(file)
senegalensis <- Subset(senegalensis, !is.na(weight) & !is.na(length))
senegalensis$length <- as.integer(senegalensis$length)
senegalensis$weight <- as.numeric(senegalensis$weight)
str(senegalensis)



#Calculations

senegalensis$logL <- log(senegalensis$length)
senegalensis$logW <- log(senegalensis$weight)

senegalensis2 <- Subset(senegalensis,logW >= -0.5)
str(senegalensis2)

lm1_senegalensis <- lm(logW~logL,data=senegalensis2)
fitPlot(lm1_senegalensis,xlab="log Total Length (mm)",ylab="log Weight (g)",main="senegalensis")
summary(lm1_senegalensis)

hoCoef(lm1_senegalensis,2,3)
confint(lm1_senegalensis)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_senegalensis)

#coefficients A and CL95%
a1 <- lm1_senegalensis
exp(a1$coefficients[1])
a <- confint(lm1_senegalensis)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
