#Load FSA (fishR Vignette - Length-Weight Relationships) and other packages

library(FSA)
library(tidyr)



#Load data Beryx splendens

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/splendens.csv"
splendens <- read.csv2(file)
splendens$length <- as.integer(splendens$length)
splendens$weight <- as.numeric(splendens$weight)
str(splendens)



#Calculations

splendens$logL <- log(splendens$length)
splendens$logW <- log(splendens$weight)

splendens2 <- Subset(splendens,logW >= -0.5)
str(splendens2)

lm1_sp <- lm(logW~logL,data=splendens2)
fitPlot(lm1_sp,xlab="log Total Length (mm)",ylab="log Weight (g)",main="splendens")
summary(lm1_sp)

hoCoef(lm1_sp,2,3)
confint(lm1_sp)



#Shows coefficients r2, A and B

#coefficient r2
summary(lm1_sp)

#coefficients A and CL95%
a1 <- lm1_sp
exp(a1$coefficients[1])
a <- confint(lm1_sp)
exp(a[1,1])
exp(a[1,2])

#coefficients B and CL95%
a1$coefficients[2]
a[2,1]
a[2,2]
