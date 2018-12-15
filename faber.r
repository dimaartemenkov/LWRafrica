#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Zeus faber

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/faber.csv"
faber <- read.csv2(file)
faber <- Subset(faber, !is.na(weight) & !is.na(length))
faber$length <- as.integer(faber$length)
faber$weight <- as.numeric(faber$weight)
str(faber)



#Calculations

faber$logL <- log(faber$length)
faber$logW <- log(faber$weight)

faber2 <- Subset(faber,logW >= -0.5)
str(faber2)

lm1_faber <- lm(logW~logL,data=faber2)
fitPlot(lm1_faber,xlab="log Total Length (mm)",ylab="log Weight (g)",main="faber")
summary(lm1_faber)

hoCoef(lm1_faber,2,3)
confint(lm1_faber)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_faber)

#coefficients A and CL95%
a1 <- lm1_faber
exp(a1$coefficients[1])
a <- confint(lm1_faber)
exp(a[1,1])
exp(a[1,2])

#coefficients B  and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
