library(ggplot2)
library(zoo)
library(forecast)

canada <- read.csv("./canada.csv", header = TRUE, sep = "")
canada$Date <- as.Date(canada$Date)
gg <- ggplot(data=canada ,aes(y=Value,x=Date)) + geom_line( colour = "blue") +
  ggtitle("Monthly Air travel to Canada")
gg + geom_vline(xintercept=as.numeric(canada$Data),linetype=4)

##Exploratory Data Analysis

#Summary statistics of the canada value dataset 
summary(canada$Value)
sd(canada$Value)

#STL
travel<-ts(canada$Value,start=c(1995,1),end=c(2016,9),frequency=12)
stl_travel<-stl(travel,s.window = "periodic")
plot(stl_travel, main="Seasonal Decomposition of Time Series by Loess for monthly Canada Travels")
#stl shows that there is a seasonal trend
#The bar on the seasonal panel is only a bit larger than the data panel, meaning the
#seasoning signal is large relative to the variation in the data
# The trend panel has larger box than the data and seasonal panels, meaning 
# the variation attrivuted to the trend is smaller than seasonal component, which
# doesn't exists a trend 

#In the data panel, there is no obvious sign of non-stationary, so we don't need to 
# take differencing for now. 

#Plotting
m <-c('J','F','M','A','M','J','J','A','S','O','N','D')
plot(canada$Value,ylab='Number of arrivals',xlab='Time Index', type = 'l', 
     main="Monthly Air arrivals to Canada")
points(canada$Value,pch=m)
#The peak of the traveling season is July and August
#Fit log series
plot(log(canada$Value),ylab='Number of Arrivals',xlab='Time', type = 'l',
     main="Logarithm of Monthly air travel to Canada")
points(log(canada$Value),pch=m)

#ACF for MA
acf(canada$Value, main = "ACF for air travel to Canada")
acf(log(canada$Value),lag.max=36, main = "ACF for log Air Travel to Canada") 
#the acf appear to be seasonality 
acf(diff(log(canada$Value),12),lag.max=36, ci.type='ma',
      main="ACF of First Difference of logarithm of Air Travel to Canada")
acf(diff(diff(log(canada$Value)),12),lag.max=36,ci.type='ma',
    main="ACF of Twice Difference of logarithm of Air Travel to Canada") 

#PACF for AR 
pacf(diff(log(canada$Value),12),lag.max=36,
     main="PACF of First Difference of logarithm of Air Travel to Canada") 
pacf(diff(diff(log(canada$Value)),12),lag.max=36,
     main="PACF of Twice Difference of logarithm of Air Travel to Canada") 

plot(diff(diff(log(canada$Value)),lag=12),xlab='Time', ylab ="Log of arrivals", 
     type = 'l', main = "Twice Difference of logarithm of air travel to Canada") 

eacf(log(canada$Value))
#eacf the circle ones are surrounded by AR 2 and MA 2

##ARIMA Models

data <- ts(canada$Value, freq = 12)
#run auto arima
auto <- auto.arima(log(data))
auto
tsdisplay(residuals(auto))
tsdiag(auto)
acf(residuals(auto), lag.max = 36)
#Auto ARIMA shows that there is no differencing and it's equivalent to ARMA(3,1)

#Without seasonal effect (should I ignore the seasonal effect ???)
f1 <- arima(log(data), order = c(1,1,1))
tsdisplay(residuals(f1))
tsdiag(f1)
acf(residuals(f1), lag.max = 36)

f2 <- arima(log(data), order = c(2,1,2))
tsdisplay(residuals(f2))
tsdiag(f2)
acf(residuals(f2), lag.max = 36)

f3 <- arima(log(data), order = c(2,1,0))
tsdisplay(residuals(f3))
tsdiag(f3)
acf(residuals(f3), lag.max = 36)

#With seasonal effect 
#First model with seasonal effect 
fit <- arima(log(data), order = c(2,0,2), seasonal=list(order = c(0,1,1), period = 12))
tsdisplay(residuals(fit))
tsdiag(fit)
acf(residuals(fit2), lag.max = 36)
pacf(residuals(fit2))

#Second model with seasonal effect 
fit1 <-arima(log(data), order=c(0,1,1), seasonal=list(order = c(0,1,1), period = 12))
tsdisplay(residuals(fit1))
tsdiag(fit1)
plot(fit1)
acf(residuals(fit1), lag.max = 36)

#Third model with seasonal effect
fit2 <- arima(log(data), order=c(2,0,1), seasonal=list(order = c(0,1,1), period = 12))
tsdisplay(residuals(fit2))
tsdiag(fit2)
plot(fit2)
acf(residuals(fit2), lag.max = 36)
McLeod.Li.test(fit2) #above 0.5 of p-values meaning didn't reject H0. 
#no arch effect so don't need arch-garch 

#try sin and cosine  and compare the forecast 

resModel <- residuals(fit2)
#Normal QQ Plot for residuals
qqnorm(resModel)
qqline(resModel)
hist(resModel,xlab='Standardized Residuals for the fitted model')
#shapiro.test(resModel)


# From Frank Davenport
funggcast <- function(dn,fcast){ 
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
  
}

#canada_date <- window(canada$Date)
#model_fit <- arima(canada_date, order=c(2,0,1), seasonal=list(order = c(0,1,1), period = 12))
# how come this one doesn't have drift??

best_model <- Arima(log(canada$Value), order = c(2,0,1))
#why i can only forecast the auto.arima??????? 
#plot(forecast(best_model))
plot(forecast(best_model, h = 12))
abline(h=auto$coef["intercept"], lty=2) #draw a horizontal line in the mean
model_forecast <- forecast(auto)
model_df <- funggcast(auto, model_forecast)
#error!! 

ggplot_forecast <- function(pd) {
  p1a<-ggplot(data=pd,aes(x=date,y=observed))
  p1a<-p1a+geom_line(col='red')
  p1a<-p1a+geom_line(aes(y=fitted),col='blue')
  p1a<-p1a+geom_line(aes(y=forecast))+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25)
  p1a<-p1a+scale_x_date(date_breaks = "12 month", date_labels = "%b-%y")
  p1a<-p1a+scale_y_continuous(name='Units of Y')
  p1a<-p1a+theme(axis.text.x=element_text(size=10))
  p1a<-p1a+ggtitle("Arima Fit to Simulated Data\n(black=forecast, blue=fitted, red=data, shadow=95% conf. interval)")
  p1a
}
ggplot_forecast(model_df)

#Detech outlier
detectAO(fit2) #Additive Outliers
detectIO(fit2) #Innovative Outliers
#The magnitude (lambda) of IO is bigger so IO has more effect than AO.
#the same index 81 has an outlier but it doesn't detech the outliers at the end of series


## Spectral Analysis
p <- periodogram(resModel, main = "Periodogram for the residuals of the model")
p
?spec.pgram()
spec(resModel,main="Periodogram", kernel = kernel("daniell", c(3,3)), taper = 0.05,
     ci.plot = T)
#Can draw a flat line between the interval so there is no ambigitiy


## Arch Garch Model
garch(data, order = c(1,1))

