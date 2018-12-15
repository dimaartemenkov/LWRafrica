#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Brama brama

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/brama.csv"
brama <- read.csv2(file)
brama$length <- as.integer(brama$length)
brama$weight <- as.numeric(brama$weight)
str(brama)



#Calculations

brama$logL <- log(brama$length)
brama$logW <- log(brama$weight)

brama2 <- Subset(brama,logW >= -0.5)
str(brama2)

lm1_br <- lm(logW~logL,data=brama2)
fitPlot(lm1_br,xlab="log Total Length (mm)",ylab="log Weight (g)",main="brama")
summary(lm1_br)

hoCoef(lm1_br,2,3)
confint(lm1_br)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_br)

#coefficients A and CL95%
a1 <- lm1_br
exp(a1$coefficients[1])
a <- confint(lm1_br)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
