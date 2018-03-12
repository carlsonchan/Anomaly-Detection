getwd()
setwd("D://R-Playground")

getwd()

DataDf <- read.table("test.txt", header = T, sep = ",")
class(DataDf)

install.packages("lubridate")
library(lubridate)
mydate <- DataDf$Date
myNewDate <- dmy(mydate)
DataDf$Date <- myNewDate

# Copy out time from dataframe into a variable
myTime <- DataDf$Time
# Convert the time to time format instead of characters
myNewTime <- as.POSIXct(myTime,format="%H:%M:%S")
# Since POSIXct appends date to the formatted time, i truncated the value for only time
x <- format(myNewTime,"%H:%M:%S")
DataDf$Time <- x

# Aggregate Global active power and get the mean each day
meanPower <-data.frame(aggregate(DataDf[,3], list(DataDf$Date),mean))

# Plotting mean to look for random drops
plot(meanPower$x, type = "o")  ## Index plot

# Split data set into month (list)
newDataDf<-split(DataDf, format(DataDf$Date, "%Y-%m"))
# Use newDataDf$`2009-12` to view specific month
month(newDataDf$`2009-12`$Date[1])
# newDataDf[[1]][[1]][1] to find out the month in the first list item, first index will switch the month
month(newDataDf[[3]][[1]][1])

# To merge month try the following:
#try do.call(c, list(l1, l2)) or append(l1, l2)