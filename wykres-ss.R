install.packages(zoo)
#importing data
library(readxl)
library(zoo)
stopyzwrotu <- read_excel("stopyzwrotu.xlsx")
View(stopyzwrotu)
str(stopyzwrotu)

stopyzwrotu <- stopyzwrotu[,-1]

#Variables for all calculations
fundusz <- c(stopyzwrotu$fundusz)
euro <- c(stopyzwrotu$euro)
ccc <- c(stopyzwrotu$ccc)
zloto <- c(stopyzwrotu$zloto)
fundusz
#importing weights from file
weights4inv <- read.table("weights4inv.txt",dec=",", header=TRUE, quote="\"",stringsAsFactors=FALSE)
w1 <- weights4inv$W1
w1 <- as.numeric(w1)
w2 <- weights4inv$W2
w2 <- as.numeric(w2)
w3 <- weights4inv$W3
w3 <- as.numeric(w3)
w4 <- weights4inv$W4
w4 <- as.numeric(w4)

#calculating SD
s1 <- sd(fundusz,na.rm=TRUE)
s2 <- sd(euro)
s3 <- sd(ccc)
s4 <- sd(zloto)
#Calculating corellation
corr12 <- cor(fundusz,euro)
corr13 <- cor(fundusz,ccc)
corr14 <- cor(fundusz, zloto)
corr23 <- cor(euro, ccc)
corr24 <- cor(euro, zloto)
corr34 <- cor(ccc, zloto)
#calculating ip
iportfolio <- mean(fundusz)*w1+mean(euro)*w2+mean(ccc)*w3+mean(zloto)*w4
#portfolio risk
sdp <- (w1^2*s1^2 + w2^2*s2^2 + w3^2*s3^2 + w4^2*s4^2 + 2*w1*w2*s1*s2*corr12 + 2*w1*w3*s1*s3*corr13 + 2*w1*w4*s1*s4*corr14 + 
          2*w2*w3*s2*s3*corr23 + 2*w2*w4*s2*s4*corr24 + 2*w3*w4*s3*s4*corr34)^0.5
#calculating effectivness
rf <- 0.0
sharp <- (iportfolio-rf)/sdp
#preparing df with results
data <- cbind(w1, w2, w3, w4, iportfolio, sdp, sharp)
data <- as.data.frame(data)
#finding interesting portfolios
min.risk <- subset(data, data$sdp==min(data$sdp))
max.effectivness <- subset(data, data$sharp==max(data$sharp))
max.ip <- subset(data, data$iportfolio==max(data$iportfolio))
max.w1 <- subset(data, data$w1==1 & data$w2==0 & data$w3==0 & data$w4==0)
max.w2 <- subset(data, data$w2==1 & data$w2==1 & data$w3==0 & data$w4==0)
max.w3 <- subset(data, data$w3==1 & data$w2==0 & data$w3==1 & data$w4==0)
max.w4 <- subset(data, data$w4==1 & data$w2==0 & data$w3==0 & data$w4==1)
des <- c("Minimal risk portfolio", "Maximum efficiency portfolio", "Maximum rate of return portfolio", "Max weight one portfolio", "Max weight two portfolio", "Max weight three portfolio", "Max weight four portfolio")
#Creating table with results 3 portfolios and showing results in console
results <- cbind(rbind(min.risk, max.effectivness, max.ip, max.w1, max.w2, max.w3, max.w4), des)
results
write.csv(x=results, file = "resultsss.csv", row.names=FALSE)
#creating and saving OS
plot(sdp, iportfolio, type= "p", col = "red", xlim=c(0,0.5))

datawithoutSS <-subset(data, data$w1>=0)
datawithoutSS <-subset(datawithoutSS, datawithoutSS$w2>=0)
datawithoutSS <-subset(datawithoutSS, datawithoutSS$w3>=0)
datawithoutSS <-subset(datawithoutSS, datawithoutSS$w4>=0)

points(datawithoutSS$sdp,datawithoutSS$iportfolio, col = "purple")

# title(main="Opportunity set for four risky assets without SS")
points(min.risk$sdp, min.risk$iportfolio, pch=19, col="green")
points(max.effectivness$sdp, max.effectivness$iportfolio, pch=19, col="blue")
points(max.ip$sdp, max.ip$iportfolio, pch=19, col="yellow")
points(max.w1$sdp, max.w1$iportfolio, pch=19, col="black")
points(max.w2$sdp, max.w2$iportfolio, pch=19, col="black")
points(max.w3$sdp, max.w3$iportfolio, pch=19, col="black")
points(max.w4$sdp, max.w4$iportfolio, pch=19, col="black")
legend(legend = c("Opportunity set without SS", "Minimum risk portfolio", "Maximum efficiency portfolio", "Maximum RoR portfolio",
                  "One-element portfolio"),
       pch = c(19, 19, 19, 19, 19),
       col = c("red", "green", "blue", "yellow", "black"),
       "right")
dev.copy(png, filename="plotss.png")
dev.off ()


