library (PerformanceAnalytics)

stopy <- Stopy_logarytmiczne_do_r
stopy$Data <- as.Date(stopy$Data, format = "%Y-%m-%d")
stopy.xts <- xts(x = stopy[,-1], order.by = as.Date(stopy$Data))

#Współczynniki
Sharpe <- round(SharpeRatio(R= stopy.xts, Rf= 0.0), digits= 3)
Calmar <- round(CalmarRatio(R= stopy.xts), digits= 3)
Sterling <- round(SterlingRatio(R= stopy.xts), digits= 3)