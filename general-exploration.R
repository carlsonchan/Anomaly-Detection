getwd()
setwd("C://Users/Carlson/Desktop/cmpt-318-group4")
getwd()

install.packages("lubridate")
install.packages("ggplot2")
library(lubridate)
library(ggplot2)
source("general-functions.R")

DataDf <- read.table("train.txt", header = T, sep = ",")
#DataDf$day <- weekdays(as.Date(DataDf$Date,'%d/%m/%Y'))
#DataDf$Month<-returnMonth(as.Date(DataDf$Date))
#DataDf$Hour<-returnHour(DataDf$Time)
#DataDf$year<-returnYear(DataDf$Date)
#DataDf$day<-returnDay(DataDf$Date)


# Assume highest energy consumption is during after work hours
afterWork<-DataDf[which(DataDf$Hour>=18 & DataDf$Hour<=23),] #06:00pm - 11:00pm

# Global Active Power
meanGlobalActive<-returnMean(afterWork,'Global_active_power')
ggplot(meanGlobalActive, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) +labs(y = "Average Global Active Power per day between 6PM-11PM")

#Global reactive power
meanGlobalReactive<-returnMean(afterWork, 'Global_reactive_power')
ggplot(meanGlobalReactive, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) + labs(y = "Average Global Reactive Power per day between 6PM-11PM")

#Voltage
meanVoltage<-returnMean(afterWork, 'Voltage')
ggplot(meanVoltage, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) + labs(y = "Average Voltage per day between 6PM-11PM")

#Global Intensity
meanGlobalIntensity<-returnMean(afterWork, 'Global_intensity')
ggplot(meanGlobalIntensity, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) +labs(y = "Average Global Intensity per day between 6PM-11PM")

#Functions for Data Exploration
cor(DataDf$Global_reactive_power, DataDf$Global_active_power, use = "complete.obs", method = "pearson")
mean_GA <- mean(afterWork$Global_active_power, na.rm = TRUE)
median_GA <- median(afterWork$Global_active_power, na.rm = TRUE)
sd_GA <- sd(afterWork$Global_active_power, na.rm = TRUE)

mean_GR <- mean(afterWork$Global_reactive_power, na.rm = TRUE)
median_GR <- median(afterWork$Global_reactive_power, na.rm = TRUE)
sd_GR <- sd(afterWork$Global_reactive_power, na.rm = TRUE)

mean_GI <- mean(afterWork$Global_intensity, na.rm = TRUE)
median_GI <- median(afterWork$Global_intensity, na.rm = TRUE)
sd_GI <- sd(afterWork$Global_intensity, na.rm = TRUE)

mean_V <- mean(afterWork$Voltage, na.rm = TRUE)
median_V <- median(afterWork$Voltage, na.rm = TRUE)
sd_V <- sd(afterWork$Voltage, na.rm = TRUE)

