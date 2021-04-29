#################
# Authors: Varad More, Sakshi Khose, Pratheek, Nikunj
# Written On: 20/04/2021
# R Version: 4.0.5
################

#install.packages('quantmod', dependencies=TRUE)
library(quantmod)
library(xts)
library(plotly)
#if(!require(tsfknn)) install.packages("tsfknn")

#candlestick chart using plotly
#update.packages("ggplot2")
#install.packages('plot_ly')
#library('plot_ly')

#?quantmod
#NSEI$Open <- as.double(NSEI$Open)

start <- as.Date("2007-09-17")
end <- as.Date("2021-04-16")
getSymbols("^NSEI",src="yahoo",from=start,to=end)




# toDate <- function(x) as.Date(x, origin = "2007-09-17")
# z <- read.zoo("//home//varad//PRACS//RPL//NSEI.csv", header = TRUE, sep = ",", FUN = toDate)
# NSEI <- as.xts(z)

#NSEI$Open <- as.double(NSEI$Open)


chartSeries(NSEI, TA=NULL, theme = "white")

data=NSEI[,4]
macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA, percent = FALSE)
chartSeries(NSEI, TA="addMACD()", theme="white")


#head(NSEI)
#tail(NSEI)

plot(NSEI[, "NSEI.Close"], main = 'Everyday Closing Graph')
plot(NSEI[, "NSEI.Open"], main = 'Everyday Opening Graph')
plot(NSEI[, "NSEI.Volume"], main = 'Volume Traded', subset="2020:01::")

#candlechart
candleChart(NSEI, up.col = "green", dn.col = "red", theme = "white")

#Simple moving averages
candleChart(NSEI, up.col = "green", dn.col = "red", theme = "white" , subset="2020:01::")
addSMA(n = 20)

#20day,50day,200 day moving averages
candleChart(NSEI, up.col = "green", dn.col = "red", theme = "white", subset = "2020-01-04/")
addSMA(n = c(20, 50, 200))

# Bollinger Bands
chartSeries(NSEI, subset = 'last 12 months', type = 1)
addBBands()

chartSeries(NSEI, type = c("auto", "matchsticks"), 
            subset = '2018-01::', theme = "white",
            show.grid = TRUE,
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addMACD(),addVo(),addSMA(n=200,col = 'blue'),addSMA(n=50,col = 'red'),addSMA(n=22,col = 'green'),
                 addROC(n=200,col = 'blue'),addROC(n=50,col = 'red'),addROC(n=22,col = 'green'))) # rate of change       


# Exponential Moving Averages
chartSeries(NSEI, sub="EMA NSEI", theme=chartTheme('white'),
            type = c("auto", "matchsticks"), 
            subset = '2020-01::',
            show.grid = TRUE,
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addADX(n = 14, maType = "EMA")))

NSEI = na.omit(NSEI)
NSEI.EMA.20<- EMA(NSEI$NSEI.Adjusted, n=20)
addTA(NSEI.EMA.20, on=1, col = "green")


# Multiple moving averages for decisive purpose
chartSeries(NSEI, sub="EMA NSEI", theme=chartTheme('white'),
            type = c("auto", "matchsticks"), 
            subset = '2020-01::',
            show.grid = TRUE,
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
)
NSEI.EMA.10 <- EMA(NSEI$NSEI.Close, n=10 ) 
NSEI.EMA.50 <- EMA(NSEI$NSEI.Close, n=50 ) 
NSEI.EMA.200 <- EMA(NSEI$NSEI.Close, n=200 ) 
Fast.Diff <- NSEI.EMA.10 - NSEI.EMA.50
Slow.Diff <- NSEI.EMA.50 - NSEI.EMA.200
addTA(Fast.Diff, col='blue', type='h',legend="10-50 MA used for in-out of market")
addTA(Slow.Diff, col='red', type='h',legend="50-200 MA give trending sense")
#addMACD(),addVo(),

summary(NSEI)


############ Arima Model
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




################ KNN Forecasting
library(tsfknn)

#Dataframe creation and model application
df <- data.frame(ds = index(NSEI),
                 y = as.numeric(NSEI[,'NSEI.Close']))
df <- tail(df, 500)
predknn <- knn_forecasting(df$y, h = 30, lags = 1:30, k = 100, msas = "MIMO")
# k --> exp heuristic

#Train set model accuracy
ro <- rolling_origin(predknn)

print(ro$global_accu)

plot(predknn, type ='1', main = "KNN Prediction")




# df <- data.frame(Date=index(NSEI),coredata(NSEI))
# df <- tail(df, 100)
# fig <- df %>% plot_ly(x = ~Date, type="candlestick",
#                       open = ~NSEI.Open, close = ~NSEI.Close,
#                       high = ~NSEI.High, low = ~NSEI.Low) 
# fig <- fig %>% layout(title = "Candlestick Chart of Nifty50")
# fig
# 







# start <- as.Date("2007-09-17")
# end <- as.Date("2021-04-16")
# getSymbols("^NSEI",src="yahoo",from=start,to=end)
# getSymbols("^IXIC",src="yahoo",from=start,to=end)
# 






# 
# df <- data.frame(Date=index(NSEI),coredata(NSEI))
# # annotation
# a <- list(text = "Stock Split",
#           x = '2014-06-06',
#           y = 1.02,
#           xref = 'x',
#           yref = 'paper',
#           xanchor = 'left',
#           showarrow = FALSE
# )
# 
# # use shapes to create a line
# l <- list(type = line,
#           x0 = '2014-06-06',
#           x1 = '2014-06-06',
#           y0 = 0,
#           y1 = 1,
#           xref = 'x',
#           yref = 'paper',
#           line = list(color = 'black',
#                       width = 0.5)
# )
# 
# fig <- df %>% plot_ly(x = ~Date, type="candlestick",
#                       open = ~NSEI.Open, close = ~NSEI.Close,
#                       high = ~NSEI.High, low = ~NSEI.Low) 
# fig <- fig %>% layout(title = "NIFTY 50",
#                       annotations = a,
#                       shapes = l)
# 
# fig





