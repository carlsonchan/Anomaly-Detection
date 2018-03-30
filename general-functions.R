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

returnMean<-function(df,column){
  # Extract 3 column we need
  newDataDF<-df[,c('Date','Time',column)]
  
  # Remove values that are NA
  cleanData<- newDataDF[complete.cases(newDataDF),]
  
  # Aggregate the mean per day
  meanData <-data.frame(aggregate(cleanData[,3], list(cleanData$Date),mean))
  # Reformat date
  meanData <- meanData[order(as.Date(meanData$Group.1, format="%d/%m/%Y")),]
  # Rename column
  colnames(meanData)[1] <- "Date"
  colnames(meanData)[2] <- "Average"
  
  meanData$Month<-returnMonth(as.Date(meanData$Date))
  meanData$year<-returnYear(meanData$Date)
  
  #Sorting months
  meanData$month <- factor(strftime(meanData$Date,"%b"),levels=month.abb)
  return (meanData)
}
