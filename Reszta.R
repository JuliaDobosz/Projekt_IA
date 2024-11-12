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
library(data.table)
library(ggplot2)
library(scales)

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
stzwrAMLP <- (AMLP[1]-AMLP[105])/105
stzwrEuro <- (Euro[1]-Euro[105])/105 
stzwrCCC <- (CCC[1]-CCC[105])/105
stzwrZloto <- (Zloto[1]-Zloto[105])/105

#Wagi inwestycji w portfelu
wAMLP <- seq(0.00,1,0.01)
wEuro <- seq(0.00,1,0.01)
wCCC <- seq(0.00,1,0.01)
wZloto <- 1-wAMLP-wEuro-wCCC
wAMLP <- as.numeric(wAMLP)
wEuro <- as.numeric(wEuro)
wCCC <- as.numeric(wCCC)
wZloto <- as.numeric(wZloto)
wagi <- data.table(wAMLP, wEuro, wCCC, wZloto)

#Odchylenia standardowe
sAMLP <- sd(AMLP)
sEuro <- sd(Euro)
sCCC <- sd(CCC)
sZloto <- sd(Zloto)

wagi[, ':=' (er_p = wAMLP * stzwrAMLP + wEuro * stzwrEuro + wCCC * stzwrCCC + wZloto * stzwrZloto,
             sd_p = sqrt(pmax(0, wAMLP^2 * sAMLP^2 +
                                wEuro^2 * sEuro^2 +
                                wCCC^2 * sCCC^2 +
                                wZloto^2 * sZloto^2 +
                                2 * wAMLP * wEuro * korAMLPEuro * sAMLP * sEuro +
                                2 * wAMLP * wCCC * korAMLPCCC * sAMLP * sCCC +
                                2 * wAMLP * wZloto * korAMLPZloto * sAMLP * sZloto +
                                2 * wEuro * wCCC * korEuroCCC * sEuro * sCCC +
                                2 * wEuro * wZloto * korEuroZloto * sEuro * sZloto +
                                2 * wCCC * wZloto * korCCCZloto * sCCC * sZloto)))]


#Korelacja portfela
korelacja <- cor(portfel)
korAMLPEuro <- cor(AMLP,Euro)
korAMLPCCC <- cor(AMLP, CCC)
korAMLPZloto <- cor(AMLP, Zloto)
korEuroCCC <- cor(Euro, CCC)
korEuroZloto <- cor(Euro, Zloto)
korCCCZloto <- cor(CCC, Zloto)

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



#Zmiana wag portfela w czasie; zwraca dziwne wartości
ppmr <- add.constraint(ppmr, type = "full_investment")
ppme <- add.constraint(ppme, type = "full_investment")

pmr_zmienne <- optimize.portfolio.rebalancing(R = portfel.xts, 
                    portfolio = ppmr, opimize_method = "ROI", rebalance_on = "years", 
                    training_period = 24, rolling_window = 24)

pme_zmienne <- optimize.portfolio.rebalancing(R = portfel.xts, 
                    portfolio = ppme, opimize_method = "ROI", rebalance_on = "years", 
                    training_period = 24, rolling_window = 24)

#Zbiór możliwości 
ggplot() +
  geom_point(data = wagi, aes(x = sd_p, y = er_p, color = wAMLP - wZloto)) +
  geom_point(data = data.table(sd = c(sAMLP, sEuro, sCCC, sZloto), mean = c(stzwrAMLP, stzwrEuro, stzwrCCC, stzwrZloto)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(min(wagi$er_p) * 1.005, max(wagi$er_p * 1)))+
  scale_x_continuous(label = percent, limits = c(0, max(wagi$sd_p) * 1.005)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)


