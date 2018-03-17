library(zoo)
library(xts)
library(dplyr)
library(ggplot2)
library(lubridate)
class(DataDf)
library(ggplot2)
head(DataDf)
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

require(zoo)


#Printing Friday evening 
DataDf <- read.table("train.txt", header = T, sep = ",")
DataDf$day <- weekdays(as.Date(DataDf$Date,'%d/%m/%Y'))
DataDf$Month<-returnMonth(as.Date(DataDf$Date))
DataDf$Hour<-returnHour(DataDf$Time)
DataDf$year<-returnYear(DataDf$Date)

#Splitting the data into seasons 
summer<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8 & DataDf$Hour>=16 & DataDf$day=='Friday' & DataDf$year>=2007 & DataDf$year<=2008),]
winter<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12 & DataDf$day=='Friday' ),]
spring<-DataDf[which(DataDf$Month>=1 & DataDf$Month<=4 & DataDf$day=='Friday'),]

#Finding Point Anomalies
p_a<-zoo(c(summer$Global_active_power))
x<-rollapply(p_a,width=15,by=14,FUN=mean,align="left")
update<-data.frame(x)
#x<-findPoint(update,0.5)

#Writing the data frame to a file
write.table(x,"x.txt",sep="\t",row.names=TRUE)
write.table(summer,"summer.txt",sep="\t",row.names=TRUE)


newdata <- DataDf[ which(DataDf$day=='Friday' & DataDf$Hour>=16), ]
class(newdata)
write.table(newdata,"Update.txt",sep="\t",row.names=TRUE)
#train <- newdata[, c(1,3)]
print(newdata)
#colnames(train) <- c("DateTime", "Global_active_power")
#train$DateTime <- paste(train$Date, train$Time)



############################################### Global Reactive power and active power 

ggplot()+
  layer(data = summer, mapping = aes(x=Global_reactive_power, y=Global_active_power), geom = "point",stat="identity", position = position_identity())

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
print("Global Active Results")
print(v_mean2)
print(v_median2)
print(v_sd2)
############################################### Time and Voltage

ggplot()+
  layer(data = DataDf, mapping = aes(x=Time, y=Voltage), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_continuous()
v_mean3 <- mean(DataDf$Voltage, na.rm = TRUE)
v_median3 <- median(DataDf$Voltage, na.rm = TRUE)
v_sd3 <- sd(DataDf$Voltage, na.rm = TRUE)
print(v_mean3)
print(v_median3)
print(v_sd3)

##########################Global Active Power and Global Intensity 

ggplot()+
  layer(data = DataDf, mapping = aes(x=Global_active_power, y=Global_intensity), geom = "point",stat="identity", position = position_identity())

v_mean3 <- mean(DataDf$Global_intensity, na.rm = TRUE)
v_median3 <- median(DataDf$Global_intensity, na.rm = TRUE)
v_sd3 <- sd(DataDf$Global_intensity, na.rm = TRUE)
print(v_mean3)
print(v_median3)
print(v_sd3)
cor(DataDf$Global_active_power, DataDf$Global_intensity, use = "complete.obs", method = "pearson")

################## Gloabl Reactive Power and Global Intensity 
ggplot()+
  layer(data = DataDf, mapping = aes(x=Global_reactive_power, y=Global_intensity), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_continuous()
cor(DataDf$Global_reactive_power, DataDf$Global_intensity, use = "complete.obs", method = "pearson")
v_mean3 <- mean(DataDf$Global_intensity, na.rm = TRUE)
v_median3 <- median(DataDf$Global_intensity, na.rm = TRUE)
v_sd3 <- sd(DataDf$Global_intensity, na.rm = TRUE)
print(v_mean3)
print(v_median3)
print(v_sd3)

