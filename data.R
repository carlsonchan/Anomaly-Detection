library(zoo)
library(xts)
library(dplyr)
library(ggplot2)
library(lubridate)
class(DataDf)
library(ggplot2)
library("depmixS4")
head(DataDf)
set.seed(1)
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
returnDay<-function(a){
  x <- format(as.Date(a, format="%d/%m/%Y"),"%d")
  new<-as.numeric(x)
  return (new)
}

#Moving Average Point Anomaly Detection

movingAverage<-function(average_vector,data,windowsize,threshold,column){
  v<-as.vector(average_vector)
  x<-c(v$x)
  y<-as.vector(data)
  y_new<-c(data[,column])
  initial=windowsize+1
  index<-1
  for (i in initial:length(y_new)){
    average<-x[index]
    data_point<-y_new[i]
    diff=data_point-average
    if(is.na(diff)){
      i=i+1
    }
    else if( diff>threshold){
      data$detection[i]<-"Anomaly"
    }
    else {
      data$detection[i]<-"Normal"
    }
    index=index+1
  }
  return (data)
}

#Creating a Validation DataFrame

validation_set<-function(test,window){
  
  numberofrow=nrow(test)
  y=numberofrow/window
  x=0.70*y #Finding the number of days to select
  x=floor(x)
  rowselection=x*window #After getting the number of days we multiply * window size
  new_row<-rowselection+1
  train<-test[1:rowselection,]
  vald<-test[new_row:numberofrow,]
  MyList<- list("Train"=train,"Val"=vald) 
  return(MyList)
  

}
#Finding Outliers in the Dataset
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
#Adding Columns to Dataset to help subset the Data
DataDf <- read.table("train.txt", header = T, sep = ",")
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


#Exploring the Data
# newrow <- apply(summer, 1, function(x){any(is.na(x))}) #Cleaning the Data Here/ Finding Rows that Have NA
# final <- summer[!row.has.na,]
Explore<-summerday[which(summerday$Month==5 & summerday$day>=1  & summerday$day<=10 & summerday$year>=2007),]#6:00am- 8:59pm
ggplot()+
  layer(data = Explore, mapping = aes(x=Time, y=Global_active_power, color = Date), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_continuous() +
  scale_color_hue()

#Finding Moving Average Vector for a variable
p_a<-zoo(c(summer$Global_active_power))
x<-rollapply(p_a,width=15,by=1,FUN=mean,align="left")
update<-data.frame(x)

#Finding Anomalies using Max and Min of Training Set 
#b<-find_point(train,test,'Voltage')
d<-movingAverage(update,summer,15,0.7,'Global_active_power')


#Exploring the Dataset
ggplot()+
  layer(data = DataDf, mapping = aes(x=Date, y=Voltage), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_continuous()

#Training HMM
list=validation_set(summerday,900) # Make sure to change the window if the dataset i s changed 
val<-list$Val #30% OF THE DATA
train<-list$Train #70% OF THE DATA
mod1 <- depmix(response = train$Global_active_power ~ 1, data = train, nstates = 8)
fm1 <- fit(mod1)
summary(fm1)
print(fm1)


#Writing the data frame to a file
write.table(update,"vector.txt",sep="\t",row.names=TRUE)
write.table(summer$Global_active_power,"summer.txt",sep="\t",row.names=TRUE)
write.table(b,"testanomaly.txt",sep="\t",row.names=TRUE)
write.table(update,"updated_train.txt",sep="\t",row.names=TRUE)
write.table(d,"AverageAnomalies.txt",sep="\t",row.names=TRUE)
############################################### Global Reactive power and active power 

#Functions for Data Exploration
cor(DataDf$Global_reactive_power, DataDf$Global_active_power, use = "complete.obs", method = "pearson")
v_mean <- mean(DataDf$Global_reactive_power, na.rm = TRUE)
v_median <- median(DataDf$Global_reactive_power, na.rm = TRUE)
v_sd <- sd(DataDf$Global_reactive_power, na.rm = TRUE)
v_mean2 <- mean(DataDf$Global_active_power, na.rm = TRUE)
v_median2 <- median(DataDf$Global_active_power, na.rm = TRUE)
v_sd2 <- sd(DataDf$Global_active_power, na.rm = TRUE)


