library(ggplot2)
library(zoo)
library(forecast)
library(lubridate)
library(plyr)

#Read and format the dataset
canada <- read.csv("./canada.csv", header = TRUE, sep = "")
canada$Date <- as.Date(canada$Date)

canada_ymd <- mutate(canada, date = ymd(canada$Date), 
       day = day(date), month = month(date), year = year(date))

#Time Series plot 
gg <- ggplot(data=canada_ymd ,aes(y=Value,x=Date)) + geom_line( colour = canada_ymd$month) +
  ggtitle("Monthly Air travel to Canada")
gg + geom_vline(xintercept=as.numeric(canada_ymd$Data),linetype=4) 

#Plot by months 
travel<-ts(canada$Value,start=c(1996,1),end=c(2016,9),frequency=12)
m <-c('J','F','M','A','M','J','J','A','S','O','N','D')
plot(travel, ylab='Number of arrivals',xlab='Time Index', type = 'l', 
     main="Monthly Air arrivals to Canada")
points(travel,pch=m)

##Exploratory Data Analysis

#STL
stl_travel<-stl(travel,s.window = "periodic")
plot(stl_travel, main="Seasonal Decomposition of Time Series by Loess for monthly Canada Travels")

#ACF
acf(canada$Value, lag.max = 60,  main = "ACF for air travel to Canada")
acf(log(canada$Value),lag.max=60, main = "ACF for log Air Travel to Canada") 
#the acf appear to be seasonality 
acf(diff(log(canada$Value),12),lag.max=60, ci.type='ma',
      main="ACF of First Difference of logarithm of Air Travel to Canada")
acf(diff(diff(log(canada$Value)),12),lag.max=60,ci.type='ma',
    main="ACF of Twice Difference of logarithm of Air Travel to Canada") 

#PACF 
pacf(canada$Value, lag.max = 60, main = "PACF for air trave to Canada")
pacf(log(canada$Value), lag.max = 60, main = "PACF for log Air Travel to Canada")
pacf(diff(log(canada$Value),12),lag.max=36,
     main="PACF of First Difference of logarithm of Air Travel to Canada") 
pacf(diff(diff(log(canada$Value)),12),lag.max=36,
     main="PACF of Twice Difference of logarithm of Air Travel to Canada") 

plot(diff(log(canada$Value),lag=12),xlab='Time', ylab ="Log of arrivals", 
     type = 'l', main = "Twice Difference of logarithm of air travel to Canada") 

#EACF
eacf(log(canada$Value)) #suggested AR 2 and MA 2

##ARIMA Models


data <- ts(canada$Value, freq = 12)

auto <- auto.arima(log(travel))
tsdisplay(residuals(auto))
tsdiag(auto)
acf(residuals(auto), lag.max = 36)

#First model with seasonal effect 
fit <- arima(log(data), order = c(1,1,1), seasonal=list(order = c(0,1,2), period = 12))
tsdisplay(residuals(fit))
tsdiag(fit)
acf(residuals(fit))
pacf(residuals(fit))

#Second model with seasonal effect 
fit1 <-arima(log(data), order=c(2,1,2), seasonal=list(order = c(0,1,1), period = 12))
tsdisplay(residuals(fit1))
tsdiag(fit1)
plot(fit1)
acf(residuals(fit1))
pacf(residuals(fit1))

#Third model with seasonal effect
fit2 <- arima(log(data), order=c(2,0,1), seasonal=list(order = c(0,1,2), period = 12))
tsdisplay(residuals(fit2))
tsdiag(fit2)
plot(fit2)
acf(residuals(fit2)) 
pacf(residuals(fit2))
res <- ts(resid(fit2), s=1996,f=12)
plot.ts(res,ylab="residuals of the fitted model")

#McLeod Li test
McLeod.Li.test(fit2, main = "McLeod Li Test") 

## Arch Garch Model
g <- garch(resModel, order = c(1,1))
plot(g) 
summary(g)

#Residuals
resModel <- residuals(fit2)

qqnorm(resModel) #Normal QQ Plot for residuals
qqline(resModel)
dev.off()
hist(resModel,xlab='Standardized Residuals for the fitted model')

# Frank Davenport Forecast
funggcast <- function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
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

train <- window(travel, end=c(2012,12))
fit_train_model <- Arima(train, order=c(2,0,1), 
                    seasonal=list(order = c(0,1,2), period = 12))
model_forecast <- forecast(fit_train_model, 45) #forecast the next 45 months
model_df <- funggcast(travel, model_forecast)

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

## Spectral Analysis
p <- periodogram(travel, main = "Periodogram")
spec.pgram(travel,  kernel = kernel("daniell", c(3,3)), taper = 0.05)

spec(travel,main="Smoothed Periodogram", kernel = kernel("daniell", c(3,3)), taper = 0.05,
     ci.plot = T)

#Spectral Representation
key_freq <- p$freq[which(p$spec > 10^11)]
t <- 1:length(travel)
harmonics <- do.call(cbind, lapply(key_freq, function(freq){
  cbind(cos(2 * pi * freq * t), sin(2 * pi * freq * t))
}))
reslm <- lm(travel ~ harmonics)
png(filename = "./images/sp.png")
plot(t, travel, type="l")
lines(fitted(reslm)~t, col=4, lty=2)
dev.off()

t_train <- 1:length(train)
spec_train<- periodogram(train)

