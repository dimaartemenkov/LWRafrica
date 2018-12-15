#Load FishBase data

library(rfishbase)



#Load LWR fishes data from FishBase literature 

beryx <- length_weight("Beryx splendens")
bbrama <- length_weight("Brama brama")
ttrecae <- length_weight("Trachurus trecae")
ttrachurus <- length_weight("Trachurus trachurus")
sardinella <- length_weight("Sardinella aurita")
merluccius <- length_weight("Merluccius senegalensis")
mugil <- length_weight("Mugil cephalus")
scomber <- length_weight("Scomber colias")
ddentex <- length_weight("Dentex dentex")
pagellus <- length_weight("Pagellus acarne")
caranx <- length_weight("Caranx rhonchus")
trichiurus <- length_weight("Trichiurus lepturus")
zeus <- length_weight("Zeus faber")

beryx2 <- cbind(beryx$Species, beryx$a, beryx$b)
bbrama2 <- cbind(bbrama$Species, bbrama$a, bbrama$b)
ttrecae2 <- cbind(ttrecae$Species, ttrecae$a, ttrecae$b)
ttrachurus2 <- cbind(ttrachurus$Species, ttrachurus$a, ttrachurus$b)
sardinella2 <- cbind(sardinella$Species, sardinella$a, sardinella$b)
merluccius2 <- cbind(merluccius$Species, merluccius$a, merluccius$b)
mugil2 <- cbind(mugil$Species, mugil$a, mugil$b)
scomber2 <- cbind(scomber$Species, scomber$a, scomber$b)
ddentex2 <- cbind(ddentex$Species, ddentex$a, ddentex$b)
pagellus2 <- cbind(pagellus$Species, pagellus$a, pagellus$b)
caranx2 <- cbind(caranx$Species, caranx$a, caranx$b)
trichiurus2 <- cbind(trichiurus$Species, trichiurus$a, trichiurus$b)
zeus2 <- cbind(zeus$Species, zeus$a, zeus$b)

total <- rbind(beryx2, bbrama2, ttrecae2, ttrachurus2, sardinella2, merluccius2, 
				mugil2, scomber2, ddentex2, pagellus2, caranx2, trichiurus2, zeus2)
colnames(total) <- c("Species", "a", "b")



#Change working directory and save LWR fishes data from FishBase literature

getwd()
setwd("C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/")
getwd()

write.csv(total, file = "Length-Weight Data of RFishBase 11-12-2018.csv")



#Load compiled the LWR fishes study with LWR fishes data from FishBase literature separated by fishes zones

#Create data plots for pelagic, epipelagic and demersal zones

# pelagic
file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/pelagic.csv"
pelagic <- read.csv2(file)
str(pelagic)

lm1 <- lm(a~b+Species,data=pelagic)
confint(lm1)
fitPlot(lm1,xlab="b",ylab="a",legend="",main="Pelagic zone")


# epipelagic
file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/epipelagic.csv"
epipelagic <- read.csv2(file)
str(epipelagic)

lm2 <- lm(a~b+Species,data=epipelagic)
confint(lm2)
fitPlot(lm2,xlab="b",ylab="a",legend="",main="Epipelagic zone")


# demersal
file <- "C:/Users/user/Google Диск/Работа/20181207 статья aiep африка/07-12-2018/demersal.csv"
demersal <- read.csv2(file)
str(demersal)

lm3 <- lm(a~b+Species,data=demersal)
confint(lm3)
fitPlot(lm3,xlab="b",ylab="a",legend="top",main="Demersal zone")