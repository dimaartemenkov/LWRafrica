#fishR Vignette - Length-Weight Relationships

library(FSA)
library(tidyr)

file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/obscurus.csv"
obscurus <- read.csv2(file)
obscurus <- Subset(obscurus, !is.na(weight) & !is.na(length))
obscurus$length <- as.integer(obscurus$length)
obscurus$weight <- as.numeric(obscurus$weight)
str(obscurus)

obscurus$logL <- log(obscurus$length)
obscurus$logW <- log(obscurus$weight)

obscurus2 <- Subset(obscurus,logW >= -0.5)
str(obscurus2)

lm1_obscurus <- lm(logW~logL,data=obscurus2)
fitPlot(lm1_obscurus,xlab="log Total Length (mm)",ylab="log Weight (g)",main="obscurus")
summary(lm1_obscurus)

hoCoef(lm1_obscurus,2,3)
confint(lm1_obscurus)

summary(lm1_obscurus)

#coefficients A:
a1 <- lm1_obscurus
exp(a1$coefficients[1])
a <- confint(lm1_obscurus)
exp(a[1,1])
exp(a[1,2])

#coefficients B:
a1$coefficients[2]
a[2,1]
a[2,2]
