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
returnNormal_Hourly<-function(frame){
  normal_count_hourly<-matrix(0,1,24)
  
  #anomaly_count_in_hour<-(0:23)
  for(i in 1:nrow(frame)){
    if (is.na(frame[i,]$detection)){}
    else if(frame[i,]$detection=="Normal"){
      normal_count_hourly[frame[i,]$Hour+1]=normal_count_hourly[frame[i,]$Hour+1]+1;
    }
   # else if(frame[i,]$detection=="Anomaly"){
    #  anomaly_count_in_hour[frame[i,]$Hour+1]=anomaly_count_in_hour[frame[i,]$Hour+1]+1;
    
    #}
  }
 
  return(normal_count_hourly)
}
returnAnomaly_Hourly<-function(frame){
  anomaly_count_hourly<-matrix(0,1,24)
  for(i in 1:nrow(frame)){
    if (is.na(frame[i,]$detection)){}
    else if(frame[i,]$detection=="Anomaly"){
      anomaly_count_hourly[frame[i,]$Hour+1]=anomaly_count_hourly[frame[i,]$Hour+1]+1;
    }
  }
  
  return(anomaly_count_hourly)
}
#Moving Average Point Anomaly Detection

movingAverage<-function(average_vector,sd_vector,data,windowsize,threshold,column){
  v<-as.vector(average_vector)
  v_1<-as.vector(sd_vector)
  x<-c(v$x)
  x_1<-c(v_1$x)
  y<-as.vector(data)
  y_new<-c(data[,column])   #global_active_power
  initial=windowsize+1
  index<-1
  number_null=0;
  point_Hour<-c(data[,13])
  trigger=0;
  trigger1=0; #first day, only use once
  for (i in initial:length(y_new)){
    average<-x[index]
    data_point<-y_new[i]
    if(point_Hour[i]==0&trigger==1&trigger1==1){ #goes into next day
      trigger=0
      for(j in 0:15){ #every 15min data of a new day, set as normal
        if(is.na(y_new[i+j])){
          data$detection[i+j]<-"Noise"
        }
        else{
          data$detection[i+j]<-"Normal"
        }
      }
      i=i+16 # start 16th of the new day
      index=index+15
      data_point<-y_new[i]
      average<-x[index]
      
      diff=data_point-average
      diff=abs(diff)
      threshold=3*x_1[index]
      
      if(is.na(diff)){
        #i=i+1
        #number_null=number_null+1;
        data$detection[i]<-"Noise"
      }
      else if( diff>threshold){   #can replace threshold by 3*x_1[index]
        data$detection[i]<-"Anomaly"
      }
      else{
        data$detection[i]<-"Normal"
      }
      index=index+1
    }
    else{  #>=17th minute of each day
        
        if(point_Hour[i]>22&trigger==0){
          trigger=1;
          trigger1=1;
        }
      diff=data_point-average
      diff=abs(diff)
      threshold=3*x_1[index]
      if(is.na(diff)){
        #i=i+1
        #number_null=number_null+1;
        data$detection[i]<-"Noise"
      }
      else if( diff>3*x_1[index]){   #can replace threshold by 3*x_1[index]
        data$detection[i]<-"Anomaly"
      }
      else {
        data$detection[i]<-"Normal"
      }
      index=index+1
    }
  }  
  #assume first window data as normal except for the Noise
  for(i in 1:15)
  {
    if(is.na(y_new[i])){
      data$detection[i]<-"Noise"
    }
    else{
      data$detection[i]<-"Normal"
    }
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
  vald<-test[1:rowselection,]
  train<-test[new_row:numberofrow,]
  MyList<- list("Val"=vald,"Train"=train) 
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
DataDf <- read.table("/Users/zhouenwei/Desktop/trainData.txt", header = T, sep = ",")
DataDf$day <- weekdays(as.Date(DataDf$Date,'%d/%m/%Y'))
DataDf$Month<-returnMonth(as.Date(DataDf$Date))
DataDf$Hour<-returnHour(DataDf$Time)
DataDf$year<-returnYear(DataDf$Date)
#DataDf$day<-returnDay(DataDf$Date)
#Splitting the data into seasons 
#summer<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8),]
winter<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12),]
spring<-DataDf[which(DataDf$Month>=1 & DataDf$Month<=4),]
summer<-DataDf[(which(DataDf$Month>=5&DataDf$Month<=8 & DataDf$day=='Saturday')),]
summer<-DataDf[(which(DataDf$Month>=5 & DataDf$Month<=8 & (DataDf$day=='Monday'|DataDf$day=='Tuesday'|DataDf$day=='Wednesday'|DataDf$day=='Thursday'|DataDf$day=='Friday'))),]
#winter<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12 & DataDf$day=='Friday'),]
#spring<-DataDf[which(DataDf$Month>=1 & DataDf$Month<=4 & DataDf$day=='Friday'),]

#Splitting the seasons into day and night
summerday<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$Hour>=6 & DataDf$Hour<=20&DataDf$day=='Saturday'),]#6:00am- 8:59pm
summernight<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$Hour>=21 & DataDf$Hour<=23 &DataDf$day=='Saturday'),] #9:00pm-11:00pm
summerdawn<-DataDf[which(DataDf$Month>=5 & DataDf$Month<=8  & DataDf$Hour>=0 & DataDf$Hour<=6),] #12:00am- 6:00am


winterday<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12  & DataDf$Hour>=8 & DataDf$Hour<19),] #8:00am- 6:59pm
winternight<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12  & DataDf$Hour>=19 & DataDf$Hour<=23),] #7:00pm-11:00pm
winterdawn<-DataDf[which(DataDf$Month>=9 & DataDf$Month<=12  & DataDf$Hour>=0 & DataDf$Hour<=7),] #12:00am- 7:00pm


#Exploring the Data
# newrow <- apply(summer, 1, function(x){any(is.na(x))}) #Cleaning the Data Here/ Finding Rows that Have NA
# final <- summer[!row.has.na,]
#Explore<-summerday[which(summerday$Month==5 & summerday$day>=1  & summerday$day<=10 & summerday$year>=2007),]#6:00am- 8:59pm
#ggplot()+
 # layer(data = Explore, mapping = aes(x=Time, y=Global_active_power, color = Date), geom = "point",stat="identity", position = position_identity()) +
#  coord_cartesian() +
#  scale_x_discrete() +
#  scale_y_continuous() +
#  scale_color_hue()

#Finding Anomalies using Max and Min of Training Set
test <- read.table("/Users/zhouenwei/Desktop/test.txt", header = T, sep = ",")
test$day <- weekdays(as.Date(test$Date,'%d/%m/%Y'))
test$Month<-returnMonth(as.Date(test$Date))
test$Hour<-returnHour(test$Time)
test$year<-returnYear(test$Date)
#summer includes day and night
b<-find_point(summer,summerTest,'Global_active_power')
#summerday
summerdayTest<-test[which(test$Month>=5 & test$Month<=8  & test$Hour>=6 & test$Hour<20&test$day=='Saturday'),]#6:00am- 8:59pm
b<-find_point(summerday,summerdayTest,'Global_active_power') 
#summernight
summernightTest<-test[which(test$Month>=5 & test$Month<=8  & test$Hour>=21 & test$Hour<=23 & test$day=='Saturday'),] #9:00pm-11:00pm
b<-find_point(summernight,summernightTest,'Global_active_power') 

ggplot()+
  layer(data = b, mapping = aes(x=Time, y=Global_active_power, color = set), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_continuous() +
  scale_color_hue()




#Finding Moving Average Vector for a variable
years<-DataDf$year
print(years<-years[!duplicated(years)])
user_input_year <- readline(prompt="Enter a year to explore: ")
user_input_season<-readline(prompt="Enter the season to explore:")
if(user_input_season=="summer"){
  specific_season_of_year<-summer[which(summer$year==user_input_year),]
}
if(user_input_season=='winter'){
  specific_season_of_year<-winter[which(summer$year==user_input_year),]
}
if(user_input_season=='spring'){
  specific_season_of_year<-spring[which(summer$year==user_input_year),]
}
write.table(specific_season_of_year,"summer.txt",sep="\t",row.names=TRUE)
p_a<-zoo(c(specific_season_of_year$Global_active_power))
x<-rollapply(p_a,width=15,by=1,FUN=mean,align="left",na.rm=TRUE)
x_1<-rollapply(p_a,width=15,by=1,FUN=sd,align="left",na.rm=TRUE)
update<-data.frame(x)
update_1<-data.frame(x_1)
d<-movingAverage(update,update_1,specific_season_of_year,15,0.7,"Global_active_power")







#Exploring the Dataset
#Explore<-d[which(d$Month==5 & d$day>=1  & d$day<=1 & d$year==2007),]
#replace the next line by above line to check the first day data
#Explore<-d[which(d$Month==5 & d$day>=27  & d$day<=27 & d$year==2007),]
#user_input_month<- readline(prompt="Enter the month to explore: ")
#user_input_day <- readline(prompt="Enter the start day to explore: ")
#user_input_end_explore <- readline(prompt="Enter the end day to explore: ")
Explore<-d[(which(d$Month>=5 & d$Month<=8 & (d$day=='Monday'|d$day=='Tuesday'|d$day=='Wednesday'|d$day=='Thursday'|d$day=='Friday'))),]   # Month Data
ggplot()+
  layer(data = Explore, mapping = aes(x=Time, y=Global_active_power, color=detection), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_continuous()+
  scale_color_hue()

#To show the moving average point
moving_avg_first_day<-update$x[1:1426]   #first day of the explore
time_first_day<-d$Time[1:1426]
moving_avg_first_day<-matrix(moving_avg_first_day,1426,1)
time_first_day<-matrix(time_first_day,1426,1)
moving_avg_first_day<-cbind(moving_avg_first_day,time_first_day)
colnames(moving_avg_first_day)<-c("Moving_avg","Time")
moving_avg_first_day<-data.frame(moving_avg_first_day)
ggplot()+
  layer(data = moving_avg_first_day, mapping = aes(x=Time, y=Moving_avg), geom = "point",stat="identity", position = position_identity()) +
  coord_cartesian() +
  scale_x_discrete() +
  scale_y_discrete()+
  
  scale_color_hue()

#For the smooth graph
#ggplot( moving_avg_first_day,mapping = aes(x=Time, y=Moving_avg,group=1))+
  #geom_point()+
  #geom_smooth()+
  #coord_cartesian() +
  #scale_x_discrete() +
  #scale_y_discrete()+
  #scale_color_hue()


Explore<-d[(which(d$Month>=5 & d$Month<=8 & (d$day=='Monday'|d$day=='Tuesday'|d$day=='Wednesday'|d$day=='Thursday'|d$day=='Friday'))),]
Normal_Counts_Hourly<-returnNormal_Hourly(Explore)
Anomaly_Counts_Hourly<-returnAnomaly_Hourly(Explore)
counts<-matrix(Anomaly_Counts_Hourly,1,24)
#counts<-rbind(counts,Anomaly_Counts_Hourly)
#counts <- table(Normal_Counts_Hourly[1], Anomaly_Counts_Hourly[1])
#colnames(counts)=c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","12-13","12-13","12-13","12-13","12-13","12-13")
barplot(counts, col=colors()[c(270)] , ylim=c(0,300),border="white", space=0.40, font.axis=2, xlab="Time:hourly",ylab="Anomalous data",main="Anomalous data over time")
#barplot(counts, col=colors()[c(270)] , ylim=c(0,2000),border="white", space=0.40, font.axis=2, xlab="Time:hourly",ylab="Detection number",main="Anomaly over time")



#Training HMM
list=validation_set(summerday,900)
val<-list$Val
train<-list$Train
print(nrow(summerday_validation))
mod1 <- depmix(response = train$Global_active_power ~ 1, data = train, nstates = 9)
fm1 <- fit(mod1)
summary(fm1)
print(fm1)


#Writing the data frame to a file
write.table(summerday,"weekend.txt",sep="\t",row.names=TRUE)
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

