install.packages("PortfolioAnalytics")
install.packages("PerformanceAnalitycs")
install.packages("DEoptim")
install.packages("ROI")
install.packages("ROI.plugin.quadprog")
install.packages("ROI.plugin.glpk")

library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(DEoptim)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

#Miesieczne stopy logarytmiczne z inwestycji 
portfel <- Stopy_logarytmiczne_do_r

#Usuwanie wiersza z NA
portfel <- na.omit(portfel)

#Inwestycje jako szeregi czasowe, pierwsza kolumna to data
portfel$Data <- as.Date(portfel$Data, format = "%Y-%m-%d")
portfel.xts <- xts(x = portfel[,-1], order.by = as.Date(portfel$Data))

#Zmienna bez kolumny z datami
portfel <- portfel[,-1]

#Zmienne (poszczególne inwestycje) do obliczeń
AMLP <- portfel$ilAMLP
Euro <- portfel$ileuro
CCC <- portfel$ilccc
Zloto <- portfel$ilzloto

#Stopy zwrotu z inwestycji
stzwrAMLP <- log(AMLP[1]/AMLP[105])/105
stzwrEuro <- log(Euro[1]/Euro[105])/105
stzwrCCC <- log(CCC[1]/CCC[105])/105
stzwrZloto <- log(Zloto[1]/Zloto[105])/105

#Wagi inwestycji w portfelu
wAMLP <- seq(0.00,1,0.01)
wEuro <- seq(0.00,1,0.01)
wCCC <- seq(0.00,1,0.01)
wZloto <- 1-wAMLP-wEuro-wCCC

#Odchylenia standardowe
sAMLP <- sd(AMLP)
sEuro <- sd(Euro)
sCCC <- sd(CCC)
sZloto <- sd(Zloto)

#Korelacja portfela
korelacja <- cor(portfel)

#Portfolio portfela o minimalnym ryzyku
ppmr <- portfolio.spec(assets = colnames(portfel))

#Ograniczenia portfela
ppmr <- add.constraint(portfolio = ppmr, type = "weight_sum", min_sum = 1, max_sum = 1)
ppmr <- add.constraint(portfolio = ppmr, type = "box", min = 0, max = 1)

#Cel wyznaczania portfela
ppmr <- add.objective(portfolio = ppmr, type = "risk", name = "StdDev")

#Portfel o minimalnym ryzyku
pmr <- optimize.portfolio(R = portfel.xts, portfolio = ppmr, 
                          optimize_method = "ROI", trace = TRUE)

#Portfolio portfela o maksymalnej efektywności
ppme <- portfolio.spec(assets = colnames(portfel))

#Ograniczenia portfela
ppme <- add.constraint(portfolio = ppme, type = "weight_sum", min_sum = 1, max_sum = 1)
ppme <- add.constraint(portfolio = ppme, type = "box", min = 0, max = 1)

#Cel wyznaczania portfela
ppme <- add.objective(portfolio = ppme, type = "return", name = "mean")
ppme <- add.objective(portfolio = ppme, type = "risk", name = "StdDev")

#Portfel o maksymalnej efektywności
pme <- optimize.portfolio(R = portfel.xts, portfolio = ppme, optimize_method = "ROI",
                          maxSR=TRUE, trace=TRUE)

#Sprawdzanie kontrolnie wag inwestycji
sum(pmr$weights)
sum(pme$weights)
all(pmr$weights >= 0 & pmr$weights <= 1)
all(pme$weights >= 0 & pme$weights <= 1)


#Stopa zwrotu, ryzyko, efektywność oraz wagi utworzonych portfeli
pmr_stzwr <- sum(weights(pmr) * colMeans(portfel.xts))
pmr_ryz <- sqrt(t(weights(pmr)) %*% cov(portfel.xts) %*% weights(pmr))
pmr_sharpe <- pmr_stzwr / pmr_ryz
pmr_weights <- weights(pmr)

pme_stzwr <- sum(weights(pme) * colMeans(portfel.xts))
pme_ryz <- sqrt(t(weights(pme)) %*% cov(portfel.xts) %*% weights(pme))
pme_sharpe <- pme_stzwr / pme_ryz
pme_weights <- weights(pme)