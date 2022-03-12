install.packages('forecast')
install.packages('tsutils')

library(forecast)
library(tsutils)

Data <- ts(scan('Final Forecast.txt'),  start=c(2005,1), frequency=1)

#Setting the Horizon
h <- 3

#ARIMA Method
fit.arima <- auto.arima(Data)
print(fit.arima)
f.arima <- forecast(fit.arima,h=h) #Run the forecast function
print(f.arima) #Print the forecasted data
