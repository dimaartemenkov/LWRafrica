#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Sardinella aurita

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/aurita.csv"
aurita <- read.csv2(file)
aurita$length <- as.integer(aurita$length)
aurita$weight <- as.numeric(aurita$weight)
str(aurita)



#Calculations

aurita$logL <- log(aurita$length)
aurita$logW <- log(aurita$weight)

aurita2 <- Subset(aurita,logW >= -0.5)
str(aurita2)

lm1_aurita <- lm(logW~logL,data=aurita2)
fitPlot(lm1_aurita,xlab="log Total Length (mm)",ylab="log Weight (g)",main="aurita")
summary(lm1_aurita)

hoCoef(lm1_aurita,2,3)
confint(lm1_aurita)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_aurita)

#coefficients A and CL95%
a1 <- lm1_aurita
exp(a1$coefficients[1])
a <- confint(lm1_aurita)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
