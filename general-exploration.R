getwd()
setwd("D://R-Playground/cmpt-318-group4")
getwd()

install.packages("lubridate")
install.package("ggplot2")
library(lubridate)
library(ggplot2)
source("general-functions.R")

DataDf <- read.table("test.txt", header = T, sep = ",")
DataDf$day <- weekdays(as.Date(DataDf$Date,'%d/%m/%Y'))
DataDf$Month<-returnMonth(as.Date(DataDf$Date))
DataDf$Hour<-returnHour(DataDf$Time)
DataDf$year<-returnYear(DataDf$Date)
DataDf$day<-returnDay(DataDf$Date)
#Splitting the data into seasons 
summer<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8),]
winter<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12 & DataDf$day=='Friday'),]
spring<-DataDf[which(DataDf$Month>=1 & DataDf$Month<=4 & DataDf$day=='Friday'),]

#Splitting the seasons into day and night
summerday<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$Hour>=6 & DataDf$Hour<=20),]#6:00am- 8:59pm
summernight<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$Hour>=21 & DataDf$Hour<=23),] #9:00pm-11:00pm
summerdawn<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$Hour>=0 & DataDf$Hour<=5),] #12:00am- 6:00am

winterday<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12  & DataDf$Hour>=8 & DataDf$Hour<=18),] #8:00am- 6:59pm
winternight<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12  & DataDf$Hour>=19 & DataDf$Hour<=23),] #7:00pm-11:00pm
winterdawn<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12  & DataDf$Hour>=0 & DataDf$Hour<=7),] #12:00am- 7:00pm

# Assume highest energy consumption is during after work hours
afterWork<-DataDf[which(DataDf$Hour>=17 & DataDf$Hour<=22),] #05:00pm - 10:00pm

#Global Active Power
newDataDF<-afterWork[,c('Date','Time','Global_active_power')]
cleanData<- newDataDF[complete.cases(newDataDF),]


meanGlobalActive <-data.frame(aggregate(cleanData[,3], list(cleanData$Date),mean))
meanGlobalActive <- meanGlobalActive[order(as.Date(meanGlobalActive$Group.1, format="%d/%m/%Y")),]
colnames(meanGlobalActive)[1] <- "Date"
colnames(meanGlobalActive)[2] <- "Average"
meanGlobalActive$day <- weekdays(as.Date(meanGlobalActive$Date,'%d/%m/%Y'))
meanGlobalActive$Month<-returnMonth(as.Date(meanGlobalActive$Date))
meanGlobalActive$year<-returnYear(meanGlobalActive$Date)
meanGlobalActive$day<-returnDay(meanGlobalActive$Date)
summer<-meanGlobalActive[which(meanGlobalActive$Month>=5 & meanGlobalActive$Month<=8),]


meanGlobalActive$month <- factor(strftime(meanGlobalActive$Date,"%b"),levels=month.abb)
ggplot(meanGlobalActive, aes(x=,month, y=Average)) + geom_boxplot() + labs(y = "Average Global Active Power per day")

#Global reactive power
newDataDF<-afterWork[,c('Date','Time','Global_reactive_power')]
cleanData<- newDataDF[complete.cases(newDataDF),]


meanGlobalReactive <-data.frame(aggregate(cleanData[,3], list(cleanData$Date),mean))
meanGlobalReactive <- meanGlobalReactive[order(as.Date(meanGlobalReactive$Group.1, format="%d/%m/%Y")),]
colnames(meanGlobalReactive)[1] <- "Date"
colnames(meanGlobalReactive)[2] <- "Average"
meanGlobalReactive$day <- weekdays(as.Date(meanGlobalReactive$Date,'%d/%m/%Y'))
meanGlobalReactive$Month<-returnMonth(as.Date(meanGlobalReactive$Date))
meanGlobalReactive$year<-returnYear(meanGlobalReactive$Date)
meanGlobalReactive$day<-returnDay(meanGlobalReactive$Date)
summer<-meanGlobalReactive[which(meanGlobalReactive$Month>=5 & meanGlobalReactive$Month<=8),]


meanGlobalReactive$month <- factor(strftime(meanGlobalReactive$Date,"%b"),levels=month.abb)
ggplot(meanGlobalReactive, aes(x=,month, y=Average)) + geom_boxplot() + labs(y = "Average Global Reactive Power per day")



#Voltage
newDataDF<-afterWork[,c('Date','Time','Voltage')]
cleanData<- newDataDF[complete.cases(newDataDF),]


meanVoltage <-data.frame(aggregate(cleanData[,3], list(cleanData$Date),mean))
meanVoltage <- meanVoltage[order(as.Date(meanVoltage$Group.1, format="%d/%m/%Y")),]
colnames(meanVoltage)[1] <- "Date"
colnames(meanVoltage)[2] <- "Average"
meanVoltage$day <- weekdays(as.Date(meanVoltage$Date,'%d/%m/%Y'))
meanVoltage$Month<-returnMonth(as.Date(meanVoltage$Date))
meanVoltage$year<-returnYear(meanVoltage$Date)
meanVoltage$day<-returnDay(meanVoltage$Date)
summer<-meanVoltage[which(meanVoltage$Month>=5 & meanVoltage$Month<=8),]


meanVoltage$month <- factor(strftime(meanVoltage$Date,"%b"),levels=month.abb)
ggplot(meanVoltage, aes(x=,month, y=Average)) + geom_boxplot() + labs(y = "Average Voltage per day")



#Functions for Data Exploration
cor(DataDf$Global_reactive_power, DataDf$Global_active_power, use = "complete.obs", method = "pearson")
v_mean <- mean(DataDf$Global_reactive_power, na.rm = TRUE)
v_median <- median(DataDf$Global_reactive_power, na.rm = TRUE)
v_sd <- sd(DataDf$Global_reactive_power, na.rm = TRUE)
v_mean2 <- mean(DataDf$Global_active_power, na.rm = TRUE)
v_median2 <- median(DataDf$Global_active_power, na.rm = TRUE)
v_sd2 <- sd(DataDf$Global_active_power, na.rm = TRUE)
