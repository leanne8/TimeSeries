library(ggplot2)
library(zoo)
library(forecast)
library(lubridate)
library(plyr)

canada <- read.csv("./canada.csv", header = TRUE, sep = "")
canada$Date <- as.Date(canada$Date)

canada_ymd <- mutate(canada, date = ymd(canada$Date), 
       day = day(date), month = month(date), year = year(date))

#Time Series plot 
png("images/ts.png")
gg <- ggplot(data=canada_ymd ,aes(y=Value,x=Date)) + geom_line( colour = canada_ymd$month) +
  ggtitle("Monthly Air travel to Canada")
gg + geom_vline(xintercept=as.numeric(canada_ymd$Data),linetype=4) 
dev.off()

#Plot by months 
travel<-ts(canada$Value,start=c(1996,1),end=c(2016,9),frequency=12)
png("images/monthly_plot.png")
m <-c('J','F','M','A','M','J','J','A','S','O','N','D')
plot(travel, ylab='Number of arrivals',xlab='Time Index', type = 'l', 
     main="Monthly Air arrivals to Canada")
points(travel,pch=m)
dev.off()

## How to test if it's stationary?? 

##Exploratory Data Analysis

summary(canada$Value)
sd(canada$Value)

#STL

png("images/stl.png")
stl_travel<-stl(travel,s.window = "periodic")
plot(stl_travel, main="Seasonal Decomposition of Time Series by Loess for monthly Canada Travels")
dev.off()
g
#In the data panel, there is no obvious sign of non-stationary, so we don't need to 
# take differencing for now. ????
# we look at the residuals in the stl and see if there's need of differencing 

#Because the graph doesn't seem like it get effect when the variation increases with the level of the series,
#so there is no need of taking logs  ??????
#no log 

#which acf and pacf to pick?? the regular one without log and difference?? 

#ACF for MA

acf(canada$Value, lag.max = 60,  main = "ACF for air travel to Canada")
acf(log(canada$Value),lag.max=60, main = "ACF for log Air Travel to Canada") 
#the acf appear to be seasonality 
acf(diff(log(canada$Value),12),lag.max=60, ci.type='ma',
      main="ACF of First Difference of logarithm of Air Travel to Canada")
acf(diff(diff(log(canada$Value)),12),lag.max=60,ci.type='ma',
    main="ACF of Twice Difference of logarithm of Air Travel to Canada") 

#PACF for AR 
pacf(canada$Value, lag.max = 60, main = "PACF for air trave to Canada")
pacf(log(canada$Value), lag.max = 60, main = "PACF for log Air Travel to Canada")
pacf(diff(log(canada$Value),12),lag.max=36,
     main="PACF of First Difference of logarithm of Air Travel to Canada") 
pacf(diff(diff(log(canada$Value)),12),lag.max=36,
     main="PACF of Twice Difference of logarithm of Air Travel to Canada") 

plot(diff(log(canada$Value),lag=12),xlab='Time', ylab ="Log of arrivals", 
     type = 'l', main = "Twice Difference of logarithm of air travel to Canada") 

eacf(log(canada$Value))
#eacf the circle ones are surrounded by AR 2 and MA 2

##ARIMA Models

#put the canada data into month matrix 
data <- ts(canada$Value, freq = 12)
#run auto arima
auto <- auto.arima(log(travel))
auto
tsdisplay(residuals(auto))
tsdiag(auto)
acf(residuals(auto), lag.max = 36)
#Auto ARIMA shows that there is no differencing and it's equivalent to ARMA(3,1)

#With seasonal effect 
#First model with seasonal effect 
fit <- arima(log(data), order = c(2,0,2), seasonal=list(order = c(2,1,2), period = 12))
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
acf(residuals(fit2)) #compare back to the original acf and pacf 
#Fit2 has the smallest AIC 
##check p and q with auto

#Residual plot of the fitted model
res <- ts(resid(fit2), s=1996,f=12)
plot.ts(res,ylab="residuals of the fitted model")

McLeod.Li.test(fit2) 
#above 0.5 of p-values meaning didn't reject H0. 
#no arch effect so don't need arch-garch 

#try sin and cosine  and compare the forecast 

resModel <- residuals(fit2)
#Normal QQ Plot for residuals
qqnorm(resModel)
qqline(resModel)
# There are a few outlier in the beginnings 
hist(resModel,xlab='Standardized Residuals for the fitted model')
#p-value is lower than 0.5 so it doesnt reject null ??
shapiro.test(resModel)

# From Frank Davenport
funggcast <- function(dn,fcast){ 
  
 # en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=c(2014,9)))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=c(2014,9))))
  
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

#cut off the date in earlier term, to forecast more point
train <- window(travel, end=c(2014,9))
fit_train_model <- Arima(train, order=c(2,0,1), 
                    seasonal=list(order = c(0,1,2), period = 12), include.drift=T)
plot(forecast(fit_train_model))

model_forecast <- forecast(fit_train_model)
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

png("images/frank_forecast.png")
ggplot_forecast(model_df)
dev.off()


#Detech outlier
detectAO(fit2) #Additive Outliers
detectIO(fit2) #Innovative Outliers
#The magnitude (lambda) of IO is bigger so IO has more effect than AO.
#the same index 81 has an outlier but it doesn't detech the outliers at the end of series
#IO tells more story 

## Spectral Analysis
periodogram(resModel, main = "Periodogram for the residuals of the model")

spec.pgram(resModel,  kernel = kernel("daniell", c(3,3)), taper = 0.05)
spec(resModel,main="Periodogram", kernel = kernel("daniell", c(3,3)), taper = 0.05,
     ci.plot = T)
#Can draw a flat line between the interval so there is no ambigitiy
#Very stable 

## Arch Garch Model
#
g <- garch(resModel, order = c(1,1))
plot(g) #use which to select the plot
summary(g)
#put in garch equation
## Why all NA in std. error and t-value?
#computational error so there is NA value
#based on the small b1 meaning no autoregressive effect 
#that's why its not necassary to do the garch model 
#only one bump at the end so 1,1 is good 
