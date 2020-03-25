library('ggplot2')
library('forecast')
library('tseries')
str(gas)

head(gas)
data(gas)

write.csv(gas, "gas_data.csv",row.names = FALSE)

#plot gas
plot(gas)
seasonplot(gas)
monthplot(gas)



#Quarterly gas data
gas_q = ts(gas, start = c(1956,1), frequency = 4)
plot(gas_q)
gas_q

#Monthly gas data(time series data)
gas_m = ts(gas , start = c(1956,1), frequency = 12)
plot(gas_m)
gas_m

#Decompose the data

gas_const = stl(gas_m, s.window = "periodic")#constant seasonality
plot(gas_const)
gas_const


gas_incon<-stl(gas_m, s.window=7) #seasonality changes
plot(gas_incon)
gas_incon

# to deseasonalize
deseasonal_demand =seasadj(gas_incon)
plot(deseasonal_demand)
deseasonal_demand
ts.plot(deseasonal_demand, gas, col=c("red", "blue"), main="Comparison of gas and Deseasonalized gas")



#Check for stationarity

#Dickey-Fuller Test 
adf.test(gas_m, alternative = "stationary")








#Differencing the time series data

count_d1 = diff(deseasonal_demand, differences = 1)# used only on deseasonalized data, d=1
plot(count_d1)

#test again if series is stationary
adf.test(count_d1, alternative = "stationary")#Differenced demand is stationary

#acf and pacf for dif time series

Acf(count_d1, main='ACF for Differenced Series')#q=9 which is taken upto lag 24
Pacf(count_d1, main='PACF for Differenced Series')#p=14 which is taken upto lag 24
acf(gas_m, lag.max = 24)
pacf(gas_m, lag.max = 24)

#From the ACF plot, there is a cut off after lag 0. This implies that q=0. PACF cuts off after lag 1. Hence p=1.

#Splitting into training and test sets
str(gas)
gastrain = window(deseasonal_demand, start=1956, end=c(1992,12))
gastest= window(deseasonal_demand, start=1993, end=c(1993,12))




# (ACF, diff, pacf) or (q,d,p)
demandARIMA1 = arima(gastrain, order=c(0,1,0))
demandARIMA1
demandARIMA2 = arima(gastrain, order=c(14,1,0))
demandARIMA2# 2nd lowest aic vale
demandARIMA3 = arima(gastrain, order=c(0,1,9))
demandARIMA3
demandARIMA4 = arima(gastrain, order=c(14,1,9))
demandARIMA4# lowest aic value

tsdisplay(residuals(demandARIMA4), lag.max=15, main='Model Residuals')# better model none of the errors are significat and touching the ble line
tsdisplay(residuals(demandARIMA2), lag.max=15, main='Model Residuals')

#There are no significant autocorrelations present. If the model is not correctly specified, that will usually be reflected in residuals in the form of trends, skeweness, or any other patterns not captured by the model. Ideally, residuals should look like white noise, meaning they are normally distributed.  Residuals plots show a smaller error range, more or less centered around 0.
# this was the manual Arima method

#Fitting with Auto ARIMA

fit<-auto.arima(gastrain, seasonal=FALSE)
fit
tsdisplay(residuals(fit), lag.max=45, main='Auto ARIMA Model Residuals')


#Auto ARIMA also fits the same p and q parameters for the model, but has a slightly lower AIC.

#Ljung box test
#H0: Residuals are independent
#Ha: Residuals are not independent

library(stats)
Box.test(demandARIMA$residuals)
Box.test(fit$residuals)


#Forecasting with the ARIMA model
fcast <- forecast(demandARIMA4, h=12)
fcast1 <- forecast(fit, h=12)
plot(fcast)
plot(fcast1)

fit1<-auto.arima(demandTS, seasonal=FALSE)
fcast2=forecast(fit1, h=12)
plot(fcast2)



#Accuracy of the forecast

f7=forecast(demandARIMA4)
accuracy(f7, gastest)

f8=forecast(fit)
accuracy(f8, gastest)




