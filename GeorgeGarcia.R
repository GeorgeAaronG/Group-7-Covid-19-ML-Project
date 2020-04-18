# Group 7 ML Project
# George Garcia

###########

library(BatchGetSymbols)
library(tidyverse)
library(forecast)
options(scipen = 999, digits = 3)

companies <- c('AAPL', 'AMZN', 'MSFT', 'JPM', 'JNJ')
begin <- Sys.Date() - 200
end <- Sys.Date()

stockData <- BatchGetSymbols(tickers = companies,
                             first.date = begin,
                             last.date = end,
                             do.cache = FALSE)

apple <- stockData$df.tickers %>%
  filter(ticker == "AAPL")
head(apple)

appleTrain <- apple[1:97, ]
appleValid <- apple[98:138, ]

# Time series til Feb 19, 2020
appleTS <- ts(appleTrain$price.close,
            start = c(2019, 10),
            end = c(2020, 02),
            freq = 97)

appleValidTS <- ts(appleValid$price.close,
                   start = c(2020, 02),
                   end = c(2020, 04),
                   freq = 41)

appleAR <- tslm(appleTS ~ trend + I(trend^2))
summary(appleAR)

plot(appleTS, xlab = "Time", 
     ylab = "APPL Close Price",
     bty = "l")

# Forecast after Feb 20, 2020
applePredict <- forecast(appleAR, h = 41, level = 95)

plot(applePredict, xlab = "Time",
     ylab = "AAPL Close Price",
     ylim = c(200, 400),
     bty = "l",
     xaxt = "n",
     flty = 2)
lines(applePredict$fitted, lwd = 2, col = "blue")
lines(appleValidTS, lwd = 2, col = "red", lty = 3)
