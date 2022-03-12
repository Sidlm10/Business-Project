install.packages('forecast')
install.packages('tsutils')

library(forecast)
library(tsutils)

#Importing the 1st time series
Data1 <- ts(scan('Forecast.txt'),  start=c(2005,1), frequency=1)
Data1
Data1_Test <- ts(scan("Forecast_1.txt"), start=c(2019,1), frequency=1)
Data1_Test

#Setting the Horizon
h <- length(Data1_Test)

#Naive
f.naive <- rep(tail(Data1,1),h)
f.naive <- ts(f.naive,start=start(Data1_Test),frequency=frequency(Data1_Test))
print(f.naive)

#ETS Method
fit.ets <- ets(Data1)
print(fit.ets)
f.ets <- forecast(fit.ets,h=h) #Run the forecast function
print(f.ets) #Print the forecasted data

#THETA Method
fit.theta <- thetaf(Data1)
print(fit.theta)
f.theta <- forecast(fit.theta,h=h) #Run the forecast function
print(f.theta) #Print the forecasted data

#TBATS Method
fit.tbats <- tbats(Data1)
f.tbats <- forecast(fit.tbats,h=h) #Run the forecast function
print(f.tbats) #Print the forecasted data

#ARIMA Method
fit.arima <- auto.arima(Data1)
print(fit.arima)
f.arima <- forecast(fit.arima,h=h) #Run the forecast function
print(f.arima) #Print the forecasted data

Data1_all <- cbind(f.naive, f.ets$mean, f.theta$mean, f.tbats$mean, f.arima$mean)
K1 <- dim(Data1_all)[2] 
Data1_test_all <- t(tcrossprod(rep(1,K1),Data1_Test))
E1 <- Data1_test_all - Data1_all
ME1 <- apply(E1,2,mean)
MAE1 <- apply(abs(E1),2,mean)
MAPE1 <- apply(abs(E1)/Data1_test_all,2,mean)*100

res <- rbind(ME1,MAE1,MAPE1)
rownames(res) <- c("ME","MAE","MAPE%")
colnames(res) <- c("Naive", "ETS", "Theta", "TBATS", "ARIMA")
print(round(res,2))