library(zoo)
library(xts)
library(dplyr)
library(ggplot2)
library(lubridate)
class(DataDf)
library(ggplot2)
head(DataDf)
set.seed(150)
setwd("/Users/zeeshan/Desktop/cmpt-318-group4")
getwd()

returnMonth <- function(a){
  mydate=as.POSIXct(a)
  x<-format(mydate,"%m")
  new<-as.numeric(x)
  return (new)
}
returnHour<-function(a){
  Time<-factor(a)
  Time_1<-hms(as.character(Time))
  update<-hour(Time_1)
  return (update)
  
}
returnMinute<-function(a){
  Time<-factor(a)
  Time_1<-hms(as.character(Time))
  update<-minute(Time_1)
  return (update)
}

returnYear<-function(a){
  x <- format(as.Date(a, format="%d/%m/%Y"),"%Y")
  new<-as.numeric(x)
  return (new)
}

#Created Function for Point Anomalies using the vector array to compare the values
findPoint<-function(a,value){
  v<-as.vector(a)
  x<-c(v$x)
  for (i in 1:length(x)) { 
    if(i+1==length(x)){
      break
    }
    t1=x[i]
    t2=x[i+1]
    diff=t2-t1
    if(diff>value){
      print("Anomaly")
    }
    if(diff<value){
      print("Normal")
    }
  }
}

find_point<-function(train,test,col){
  find_min<-min(train[,col],na.rm=TRUE)
  find_max<-max(train[,col],na.rm=TRUE)
  x<-c(test[,col])
  for(i in 1:length(x)){
    if(is.na(x[i])){
      test$set[i]="NOISE"
    }
    else if (x[i]>find_max){ # If the value is greater than the max of the training set we put it as an anomaly
      test$set[i]="Anomaly"
    }
    else if (x[i]<find_min)
    {
      test$set[i]="Anomaly"
    }
    else
    {
      test$set[i]="Normal"
    }
  }
  return(test)
}

require(zoo)
#Adding Columns to Dataset to help with the Data
DataDf <- read.table("train.txt", header = T, sep = ",")
DataDf$day <- weekdays(as.Date(DataDf$Date,'%d/%m/%Y'))
DataDf$Month<-returnMonth(as.Date(DataDf$Date))
DataDf$Hour<-returnHour(DataDf$Time)
DataDf$year<-returnYear(DataDf$Date)

#Splitting the data into seasons 
summer<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$day=='Friday' & DataDf$year>=2007 & DataDf$year<=2008),]
winter<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12 & DataDf$day=='Friday'),]
spring<-DataDf[which(DataDf$Month>=1 & DataDf$Month<=4 & DataDf$day=='Friday'),]

#Splitting the data into training and test dataset for the summer
dt = sort(sample(nrow(summer), nrow(summer)*.7))
train<-summer[dt,]
test<-summer[-dt,]

#Finding Point Anomalies
p_a<-zoo(c(summer$Global_active_power))
x<-rollapply(p_a,width=15,by=14,FUN=mean,align="left")
update<-data.frame(x)

#Finding Anomalies using Max and Min of Training Set 
b<-find_point(train,test,'Voltage')

#Writing the data frame to a file
write.table(x,"x.txt",sep="\t",row.names=TRUE)
write.table(summer,"summer.txt",sep="\t",row.names=TRUE)
write.table(b,"testanomaly.txt",sep="\t",row.names=TRUE)
write.table(train,"updated_train.txt",sep="\t",row.names=TRUE)


############################################### Global Reactive power and active power 

ggplot()+
  layer(data = summer, mapping = aes(x=Voltage, y=Global_active_power), geom = "point",stat="identity", position = position_identity())

cor(DataDf$Global_reactive_power, DataDf$Global_active_power, use = "complete.obs", method = "pearson")
v_mean <- mean(DataDf$Global_reactive_power, na.rm = TRUE)
v_median <- median(DataDf$Global_reactive_power, na.rm = TRUE)
v_sd <- sd(DataDf$Global_reactive_power, na.rm = TRUE)
v_mean2 <- mean(DataDf$Global_active_power, na.rm = TRUE)
v_median2 <- median(DataDf$Global_active_power, na.rm = TRUE)
v_sd2 <- sd(DataDf$Global_active_power, na.rm = TRUE)

print(v_mean)
print(v_median)
print(v_sd)
print(v_mean2)
print(v_median2)
print(v_sd2)

