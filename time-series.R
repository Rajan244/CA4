#libraries have used 
library(zoo)
library(forecast)
library(tseries)
library(ggplot2)
library(scales)

#reading csv file
data <- read.csv("Dublin_PM10-2.5.csv",  header = TRUE, 
                      na.strings = c("", " ", "NA"))

#To replace the NA values with the mean values of each column
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#check still having any NA 
anyNA(data)summary(data)

#changing the data type 
d <- as.Date(as.character(data$Date), format = "%d-%b-%y") 
data$Date <- d
str(data)

#subsetting the marino data
marino_data <- subset(data, select = c(Date,Marino))
write.csv(marino_data, 'marino_data.csv')
str(marino_data)

#Histrogram of Marino Area
hist(m_data, breaks = 100, main = "Hisogram of Marino Area",col="blue")

#########################################################################################
#Time-series 
########################################################################################
#time series graph of Marino Area
marino_time_series <- ggplot(marino_data,aes(x = Date, y = Marino))+
  geom_line(color = "Red", size = 0)+geom_smooth()+
  scale_x_date(date_breaks = '1 years',labels = date_format("%b-%Y"))+ 
  labs(title = "PM10-2.5 Air Pollution Index In Marino Area(Dublin)", 
       subtitle = "From Jan 2011 to Dec 2012", x = "", y = "Pollution Rate") + 
  theme_minimal()
marino_time_series
 
marino_data[which.max(marino_data)] #to varify with the chart
marino_data[which.min(marino_data)] #to varify with the chart

m_data <- read.zoo(marino_data,format = "%Y-%m-%d",index.column = "Date")
head(m_data)

#monthly poltting of the year 2011
marino_2011 <- window(m_data, start= "2011-01-01", end = "2011-12-31")
monthplot(marino_2011, ylab = "Units", xlab = "Months", xaxt = "n",
          main= "Seasonal Deviation plot : PM 10 & 2.5 Air pollution (year 2011)") 
axis(1,at=1:12, labels = month.abb, cex = 0.8)

# monthly poltting of the year 2012
marino_2012 <- window(m_data, start= "2012-01-01", end = "2012-12-31")
monthplot(marino_2012, ylab = "Units", xlab = "Months", xaxt = "n",
          main= "Seasonal Deviation plot : PM 10 & 2.5 Air pollution (year 2012)")
axis(1, at= 1:12, labels = month.abb, cex = 0.8)

#Calculating the quantile of data
quantile(m_data, probs = 0.01)

#######################################################################################
#Using Arima Forecasting
#######################################################################################
 
#Here, we are storing the data into a time series object  
marino_ts <- ts(marino_data$Marino, start = c(2011,1), end = c(2012, 12), frequency = 12)

#decomposing seasonal data
marino_decom <- decompose(marino_ts)
plot(marino_decom)

#making the data stationary 
marino_diff1 <- diff(marino_ts, differences = 1)
plot.ts(marino_diff1)

marino_diff2 <- diff(marino_ts, differences = 2)
plot.ts(marino_diff2)

########################################################################################
#ARIMA MODEL
#Now the data is stationary enough to do any kind of test
#######################################################################################

#ACF, PACF test
acf(marino_diff1, lag.max = 20)# Determine the value q = 0 #when we are not appling anything into series, when we are not tranformered series to stasnary 
pacf(marino_diff1, lag.max = 20) # Determine the value p = 0

#in case of D, we will take d = 1 because we are using first oder difference


#Fitting the ARIMA model
#AR I MA 
#c(p, d, q)
marino_model <- arima(marino_ts, order = c(1,0,0), seasonal = list(order =c(1,0,0)))
marino_model

#Automated Forecasting
marino_arima <- auto.arima(marino_ts, stepwise = FALSE, trace = TRUE)
marino_arima

#Ljung-box
Box.test(marino_arima$residuals, lag = 20, type= "Ljung-Box") 
checkresiduals(marino_model, lag =20)

#after Dec 2012, predicting next 12 months future values from ARIMA
marino_model_forecast %>% forecast(marino_model, h = 12) %>% autoplot()
marino_model_forecast

#Predictive Accuracy
accuracy(marino_arima)
