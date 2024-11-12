install.packages("lpSolve")
install.packages("fPortfolio")

library(lpSolve)
library(fPortfolio)

#Wczytujemy dane z pliku stzwr.csv
dane <- ts(portfel, start=c(2016, 2), end=c(2024, 10), frequency=12)
data <- as.timeSeries(dane)

data <- data[, c("ilAMLP", "ileuro", "ilccc", "ilzloto")]

data <- covEstimator(data)

shortSpec <- portfolioSpec()

setSolver(shortSpec) <- "solveRshortExact"

shortFrontier <- portfolioFrontier(data, constraints="LongOnly")

weightsPlot(shortFrontier,labels = TRUE, col = NULL)
