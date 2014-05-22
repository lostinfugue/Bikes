## Uses stringr package

library(stringr)
library(plyr)

setwd("/Users/jeffreyli/Desktop/Bikes/Source")

## Load data in chunks
## Read File of Raw data
## Reshapes and compiles into master dataset
cleanData <- function(arg1){
  
  print("Processing ")
  print(arg1)
  
  data = read.csv(arg1, colClasses = "character")
  
  ## Regularize column names to lowercase
  colnames(data) <- tolower(colnames(data))
  
  ## If station name and station ID combined in station name variable
  ## Split them into variables stationID
  if(!("end.terminal" %in% colnames(data))) {
    
    ## Get rid of observations with missing data
    #data$Start.station[(grepl('( \\()|(\\))', data$Start.station)==FALSE)]
    data <- data[(grepl('( \\()|(\\))', data$start.station)==TRUE),]
    data <- data[(grepl('( \\()|(\\))', data$end.station)==TRUE),]
    
    ## Split start/end station name and ID into separate variables
    foo <- data.frame(do.call('rbind', strsplit(as.character(data$start.station),'( \\()|(\\))', fixed=FALSE)))[, 1:2]
    data[c("start.station", "start.terminal")] <- foo
    foo <- data.frame(do.call('rbind', strsplit(as.character(data$end.station),'( \\()|(\\))', fixed=FALSE)))[, 1:2]
    data[c("end.station", "end.terminal")] <- foo 
  }
  
  ## Regularize date variable name
  if("start.time" %in% colnames(data)) {
    data <- rename(data, c("start.time"="start.date"))
  }
  if("end.time" %in% colnames(data)) {
    data <- rename(data, c("end.time"="end.date"))
  }
  
  ## Split Date into Date and Time
  foo <- data.frame(do.call('rbind', strsplit(as.character(data$start.date),' ',)))
  data[c("start.date", "start.time")] <- foo
  foo <- data.frame(do.call('rbind', strsplit(as.character(data$end.date),' ',)))
  data[c("end.date", "end.time")] <- foo
  rm(foo)
  
  ## Get two data sets of station visits: (1) start and (2) end (w/ stations/times)
  ## Generalize (remove start and end identification)
  starts <- data[c("start.station","start.terminal","start.date","start.time")]
  starts <- rename(starts, c("start.station"="stationName", "start.terminal"="stationID", "start.date"="date", "start.time"="time"))
  ends <- data[c("end.station","end.terminal","end.date","end.time")]
  ends <- rename(starts, c("end.station"="stationName", "end.terminal"="stationID", "end.date"="date", "end.time"="time"))
  ## Append ends to starts, resulting in a list of all bike station visits
  newdata <- rbind(starts, ends) # A dataset of ALL visits to bike stations
  rm(starts, ends)

  
  ## Get a dataset with a tally of visits for each station / date
  newdata["ones"] <- list(rep(1, length(newdata[,1])))
  sums <- aggregate(newdata$ones, by=list(stationName=newdata$stationName, date = newdata$date, stationID=newdata$stationID), FUN=sum)
  sums <- rename(sums, c("x"="visits")) 
  
  ## Create date variables
  sums$date <- as.Date(sums$date,format='%m/%d/%Y')
  sums$weekdays <- weekdays(sums$date)
  sums["weekend"] <- grepl("Sunday|Saturday", sums$weekdays)
  sums$month <- as.factor((format(sums$date, "%b")))
  sums$year <- as.factor(as.numeric(format(sums$date, "%Y")))
  
  return(sums)
}

## Processing Data:
## Read in all files and compile dataset for 2010q4-2014q1
## raw data located in "../Data/"
files <- list.files(path="../Data", pattern="*quarter.csv", full.names=T, recursive=FALSE)

## Calls to Clean function: Iterative vs. functional versions

## (1) Iterative version
ptm <- proc.time()
i = 0
for (file in files) {
  i <- i+1
  if (i == 1){
    dataset <- cleanData(file)
  }
  else {
    dataset <- rbind(dataset, cleanData(file))
  }
}
proc.time() - ptm

## (2) Functional Version - about the same performance, but less natural
## Takes in files from file list and combines them into a dataset
readAll <- function(filelist){
  if (length(filelist) <= 0) {
    return(data.frame())
  }
  else {
    
    ## Initialize dataset on first file
    dataset <- cleanData(filelist[1])
    
    ## Define tail-recursive function
    readRest <- function(fileno, data) {
      if (fileno > length(filelist)) return(data)
      else {
        return(readRest(fileno + 1,
                        rbind(data, cleanData(filelist[fileno]))))
      }
    }
    
    ## Call recursive function
    dataset <- readRest(2, dataset)
    return(dataset)
  }
}

ptm <- proc.time()
dataset <- readAll(files[1:3])
proc.time() - ptm

## Analysis ##

## Basic Relationships
print(summary(dataset))
fit <- lm(visits ~ weekend + month, data=dataset)
print(summary(fit)) # show results

meanDailyVisits <- aggregate(dataset$visit, by=list(stationName=dataset$stationName, stationID=dataset$stationID), FUN=mean)
meanDailyVisits <- rename(meanDailyVisits, c("x"="visits"))
latlon <- read.csv("../Locations2.csv")
merged <- merge(latlon, meanDailyVisits)
write.csv(merged, "../locationsAndVisits.csv")

#boxplot(visits ~ weekdays, data = dataset)
#boxplot(visits ~ weekend, data = dataset)


months <- c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
dataset$month <- factor(dataset$month, levels = months)
boxplot(visits ~ month, data = dataset)