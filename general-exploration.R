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
DataDf$Hour<-returnHour(DataDf$Time)
#DataDf$year<-returnYear(DataDf$Date)
#DataDf$day<-returnDay(DataDf$Date)


# Assume highest energy consumption is during after work hours
afterWork<-DataDf[which(DataDf$Hour>=18 & DataDf$Hour<=23),] #06:00pm - 11:00pm
summer[which(summer$year==2007),]$Average
# Global Active Power
meanGlobalActive<-returnMean(afterWork,'Global_active_power')

mean_GA <- mean(meanGlobalActive$Average, na.rm = TRUE)
median_GA <- median(meanGlobalActive$Average, na.rm = TRUE)
sd_GA <- sd(meanGlobalActive$Average, na.rm = TRUE)
max_GA <- max(meanGlobalActive$Average)
min_GA <- min(meanGlobalActive$Average)

ggplot(meanGlobalActive, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) +labs(y = "Average Global Active Power per day between 6PM-11PM")

# Zoomed in version of the summer boxplot
summer <- meanGlobalActive[which(meanGlobalActive$Month>=5 & meanGlobalActive$Month<=8),] # Extracting summer momnth May - Aug
ggplot(summer, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) + labs(x = "Summer Months of 2007-2009") + labs(y = "Average Global Active Power")
ggplot(summer, aes(x=,Date, y=Average, group=1))+facet_wrap(~year)+geom_line()

# Change number to extract specific year's summer for line graph
summer2007<-summer[which(summer$year==2007),]
ggplot(summer2007, aes(x=,Date, y=Average, group=1))+geom_line() +labs(y = "Average Global Active Power Per day")+labs(x = "May 1st 2007 - Aug 31st")




#Global reactive power
meanGlobalReactive<-returnMean(afterWork, 'Global_reactive_power')

mean_GR <- mean(meanGlobalReactive$Average, na.rm = TRUE)
median_GR <- median(meanGlobalReactive$Average, na.rm = TRUE)
sd_GR <- sd(meanGlobalReactive$Average, na.rm = TRUE)
max_GR <- max(meanGlobalReactive$Average)
min_GR <- min(meanGlobalReactive$Average)

ggplot(meanGlobalReactive, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) + labs(y = "Average Global Reactive Power per day between 6PM-11PM")

#Voltage
meanVoltage<-returnMean(afterWork, 'Voltage')

mean_GV <- mean(meanVoltage$Average, na.rm = TRUE)
median_GV <- median(meanVoltage$Average, na.rm = TRUE)
sd_GV <- sd(meanVoltage$Average, na.rm = TRUE)
max_GV <- max(meanVoltage$Average)
min_GV <- min(meanVoltage$Average)

ggplot(meanVoltage, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) + labs(y = "Average Voltage per day between 6PM-11PM")

#Global Intensity
meanGlobalIntensity<-returnMean(afterWork, 'Global_intensity')

mean_GI <- mean(meanGlobalIntensity$Average, na.rm = TRUE)
median_GI <- median(meanGlobalIntensity$Average, na.rm = TRUE)
sd_GI <- sd(meanGlobalIntensity$Average, na.rm = TRUE)
max_GI <- max(meanGlobalIntensity$Average)
min_GI <- min(meanGlobalIntensity$Average)

ggplot(meanGlobalIntensity, aes(x=,month, y=Average)) + geom_boxplot() + facet_wrap(~year) +labs(y = "Average Global Intensity per day between 6PM-11PM")

# Feature correlation
cor(afterWork$Global_active_power, afterWork$Global_reactive_power, use = "complete.obs", method = "pearson")
cor(afterWork$Global_active_power, afterWork$Global_intensity, use = "complete.obs", method = "pearson")
cor(afterWork$Global_active_power, afterWork$Voltage, use = "complete.obs", method = "pearson")
cor(afterWork$Global_reactive_power, afterWork$Global_intensity, use = "complete.obs", method = "pearson")
cor(afterWork$Global_reactive_power, afterWork$Voltage, use = "complete.obs", method = "pearson")
cor(afterWork$Global_intensity, afterWork$Voltage, use = "complete.obs", method = "pearson")

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

