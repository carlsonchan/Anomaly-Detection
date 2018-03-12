#this is to get data from a specific night
library(xts)
library(dplyr)
library(ggplot2)
library(lubridate)
apply.hourly <- function(x, FUN,...) {
  ep <- endpoints(x, 'hours')
  period.apply(x, ep, FUN, ...)
}

DataDf<- read.table("/Users/zhouenwei/Desktop/test2.txt",header=T,sep=",")
#get the first and third column
train <- DataDf[, c(1,3)]
#rename the column in the train
colnames(train) <- c("DateTime", "Global_active_power")
#the format in our txt like "1/12/2009 16:30:00":day/month/year time
train$DateTime <- paste(DataDf$Date,DataDf$Time)
#change the format to year-month-day using the function POSIXct
train$DateTime <- as.POSIXct(train$DateTime, format='%d/%m/%Y %H:%M:%S')
#get the time
train$Time<-paste(DataDf$Time)
train <- na.omit(train)

#GAP is the feature "Global_active_power"
trainDateAndGAP <- xts(train$Global_active_power, order.by = train$DateTime)
minutes <- fortify(trainDateAndGAP)
hourly <- fortify(apply.hourly(trainDateAndGAP,mean))
daily <- fortify(apply.daily(trainDateAndGAP,mean))
monthly <- fortify(apply.monthly(trainDateAndGAP,mean)) 
#define night time for a specific day 
#(to be continued...)may consider the date as variable in a loop and get the data for all nights
night <- minutes%>% filter(Index>= '2009-12-14 18:00:00' & Index<= '2009-12-14 23:59:00')
colnames(night)<-c("Date","Global_active_power")
ggplot()+
  layer(data = night, mapping = aes(x=Date, y=Global_active_power), geom = "point",stat="identity", position = position_identity())

