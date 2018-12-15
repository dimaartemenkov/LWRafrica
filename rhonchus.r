#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Caranx rhonchus

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/rhonchus.csv"
rhonchus <- read.csv2(file)
rhonchus <- Subset(rhonchus, !is.na(weight) & !is.na(length))
rhonchus$length <- as.integer(rhonchus$length)
rhonchus$weight <- as.numeric(rhonchus$weight)
str(rhonchus)



#Calculations

rhonchus$logL <- log(rhonchus$length)
rhonchus$logW <- log(rhonchus$weight)

rhonchus2 <- Subset(rhonchus,logW >= -0.5)
str(rhonchus2)

lm1_rhonchus <- lm(logW~logL,data=rhonchus2)
fitPlot(lm1_rhonchus,xlab="log Total Length (mm)",ylab="log Weight (g)",main="rhonchus")
summary(lm1_rhonchus)

hoCoef(lm1_rhonchus,2,3)
confint(lm1_rhonchus)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_rhonchus)

#coefficients A and CL95%
a1 <- lm1_rhonchus
exp(a1$coefficients[1])
a <- confint(lm1_rhonchus)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
