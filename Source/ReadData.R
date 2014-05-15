## Uses stringr package

library(stringr)

setwd("/Users/jeffreyli/Desktop/Bikes/Source")

## Load data in chunks

## Read File
## Raw Data
data = read.csv("../Data/2010-4th-quarter.csv", colClasses = "character")


## Cleaning

## Regularize colum names to lowercase
colnames(data) <- tolower(colnames(data))

## Get rid of observations with missing data
#data$Start.station[(grepl('( \\()|(\\))', data$Start.station)==FALSE)]
data <- data[(grepl('( \\()|(\\))', data$start.station)==TRUE),]
data <- data[(grepl('( \\()|(\\))', data$end.station)==TRUE),]


## Split start/end station name and ID into separate variables
foo <- data.frame(do.call('rbind', strsplit(as.character(data$start.station),'( \\()|(\\))', fixed=FALSE)))[, 1:2]
data[c("start.station", "start.terminal")] <- foo
foo <- data.frame(do.call('rbind', strsplit(as.character(data$end.station),'( \\()|(\\))', fixed=FALSE)))[, 1:2]
data[c("end.station", "end.terminal")] <- foo

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


## Basic Relationships
summary(sums)
fit <- lm(visits ~ weekend + month, data=sums)
summary(fit) # show results

boxplot(visits ~ weekdays, data = sums)
boxplot(visits ~ weekend, data = sums)
boxplot(visits ~ month, data = sums)

## Export sums dataset as csv
write.csv(sums, "../ProcessedData/2010-4q.csv")

# Clear dataset
remove(data)

