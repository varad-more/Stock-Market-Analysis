install.packages('quantmod', dependencies=TRUE)
library(quantmod)
library(xts)

#?quantmod

toDate <- function(x) as.Date(x, origin = "2007-09-17")
z <- read.csv("//home//varad//PRACS//RPL//NSEI.csv", header = TRUE, sep = ",", FUN = toDate)
NSEI <- as.xts(z)

#NSEI$Open <- as.double(NSEI$Open)

start <- as.Date("2007-09-17")
end <- as.Date("2021-04-16")
getSymbols("^NSEI",src="yahoo",from=start,to=end)


chartSeries(NSEI, TA=NULL)

data=NSEI[,4]
macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA, percent = FALSE)
chartSeries(NSEI, TA="addMACD()")


#head(NSEI)
#tail(NSEI)

plot(NSEI[, "NSEI.Close"], main = 'Everyday Closing Graph')
plot(NSEI[, "NSEI.Open"], main = 'Everyday Opening Graph')


#candlechart
candleChart(NSEI, up.col = "green", dn.col = "red", theme = "white")

#moving averages
candleChart(NSEI, up.col = "green", dn.col = "red", theme = "white")
addSMA(n = 20)

#20day,50day,200 day moving averages
candleChart(NSEI, up.col = "green", dn.col = "red", theme = "white", subset = "2020-01-04/")
#addSMA(n = c(20, 50, 200))

summary(NSEI)

chartSeries(NSEI, subset = 'last 12 months', type = 1)
addBBands()

library(tseries, quietly = T)

#nifty = NSEI

#x <- as.data.frame(NSEI.Adjusted) 
#nifty$NSEI.Adjusted[is.na(nifty$NSEI.Adjusted)]<-mean(nifty$NSEI.Adjusted,na.rm=TRUE)

NSEI$NSEI.Adjusted[is.na(NSEI$NSEI.Adjusted)] <- mean(NSEI$NSEI.Adjusted,na.rm=TRUE)

adf.test(NSEI$NSEI.Adjusted)

ret_NSEI <- 100*diff(log(NSEI$NSEI.Adjusted[2000:2638]))
      
library(forecast, quietly = T)


NSEI_ret_train <- ret_NSEI[1:(0.9*length(ret_NSEI))]
NSEI_ret_test <- ret_NSEI[(0.9*length(ret_NSEI)+1):length(ret_NSEI)]
fit <- Arima(NSEI_ret_train, order = c(2,0,2))
#fit<-auto.arima(NSEI_ret_train)

preds <- predict(fit, n.ahead = (length(ret_NSEI) - (0.9*length(ret_NSEI))))$pred
test_forecast <- forecast(fit,h = 10)
plot(test_forecast, main = "Arima forecast for NSEI")
accuracy(preds, NSEI_ret_test)


#candlestick chart using plotly
#update.packages("ggplot2")
#install.packages('plot_ly')
#library('plot_ly')
library(plotly)

df <- data.frame(Date=index(NSEI),coredata(NSEI))
df <- tail(df, 100)
fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~NSEI.Open, close = ~NSEI.Close,
                      high = ~NSEI.High, low = ~NSEI.Low) 
fig <- fig %>% layout(title = "Candlestick Chart of Nifty50")
fig






df <- data.frame(Date=index(NSEI),coredata(NSEI))
# annotation
a <- list(text = "Stock Split",
          x = '2014-06-06',
          y = 1.02,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)

# use shapes to create a line
l <- list(type = line,
          x0 = '2014-06-06',
          x1 = '2014-06-06',
          y0 = 0,
          y1 = 1,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 0.5)
)

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~NSEI.Open, close = ~NSEI.Close,
                      high = ~NSEI.High, low = ~NSEI.Low) 
fig <- fig %>% layout(title = "NIFTY 50",
                      annotations = a,
                      shapes = l)

fig





