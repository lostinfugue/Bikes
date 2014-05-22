## Load libraries

#install.packages("XML")
#install.packages("stringr")
library(XML)
library(stringr)

## Load website code using XML package
URL <- "http://www.opm.gov/policy-data-oversight/snow-dismissal-procedures/status-archives/"
html <- htmlTreeParse(URL, useInternalNodes=T)



## Extract dates and statuses from HTML Tree
dates <- xpathSApply(html, "//div[@class='StatusDateContainer']")
statuses <- xpathSApply(html, "//div[@class='StatusNameContainer']/a")


## Create data frame
data <- data.frame(date = vector(mode = "character", length = length(statuses)), 
                   statuses = vector(mode = "character", length = length(statuses)), 
                   status = vector(mode = "numeric", length = length(statuses)),
                   stringsAsFactors = F)

## Fill in data frame with dates and statuses
for (i in 1:length(statuses)) 
{
  data$date[i] <- xmlToList(dates[[i]])$text
  data$statuses[i] <- xmlToList(statuses[[i]])$text
  # 0: OPEN
  if (grepl("[O|o]pen", data$statuses[i])) {
    data$status[i] <- 0
  }
  # 1: DELAYED
  if (grepl("[D|d]elayed", data$statuses[i])) {
    data$status[i] <- 1
  }
  # 3: CLOSED
  else if (grepl("[C|c]losed", data$statuses[i])) {
    data$status[i] <- 2
  }
}

## Add column of factors for status
data["status"] <- factor(data$status, labels = c("Open", "Delayed", "Closed"))

## Process date variable
for (i in 1:length(data$date)) {
  ## Trim excess off date strings
  data$date[i] <- gsub("\r\n                    \t\t|\r\n                    \t","",
                     data$date[i])
}

## Convert to date variable
data$date <- as.Date(data$date, format='%B %d, %Y')

## Drop data that's not in time frame (before 2010)
data <- subset(data, data$date > as.Date('2010-10-1'))


## Merge data into bikes dataset
new <- merge(data, dataset, by.dataset = date, by.data = date, all.y = T)

## Fill in missing status data with "Open"
new$status[is.na(new$status)] <- "Open"
summary(new$status)

## Fill in days of gov't shutdown
new["shutdown"] <- (new$date < as.Date('2013-10-17') & new$date >= as.Date('2013-10-1'))
summary(new$shutdown)

fitShutNotClosed <- lm(visits ~ weekend + month + status + shutdown, data=new)
print(summary(fitShutNotClosed)) # show results

# Option to include gov't shutdown as closed
new$status2 <- new$status
new$status2[new$shutdown] <- "Closed"

fitShutIsClosed <- lm(visits ~ weekend + month + status2 + shutdown, data=new)
summary(fitShutIsClosed) # show results

new$Predicted  <- predict(fitShutNotClosed, newdata=new)
new$Predicted2 <- predict(fitShutIsClosed, newdata=new)

summary(new$Predicted2)



### Get locations to make lat/ long vars
distinctlocations <- unique(dataset$stationName)
write.csv(distinctlocations, file = "../Data/locationslist.csv")

## Get longitudes and latitudes for stations as follows:
## 1) Paste a column of "Washington, DC" next to station names in locationslist.csv
## 2) save as a .csv
## 3) Access using text editor.  copy and paste into following website:
## http://www.findlatitudeandlongitude.com/batch-geocode/#.U30SQFhdVHv (as of 5/21/2014)
## With output settings: address in, longitude, latitude
## 4) Copy and paste output into csv

## Load resulting stations, longitudes, latitudes
lonlat <- read.csv("../Data/locationslonlat.csv", header = F)
library(plyr)
lonlat <- rename(lonlat, c("V1"="stationName", "V2"="latitude", "V3"="longitude"))

## Remove ",Washington, D.C."
lonlat$stationName <- gsub(",\"Washington, D.C.\"","",lonlat$stationName)

## Remove errors: rm if longtidue/latitude is outside certain range (outliers)
medlat <- median(lonlat$latitude)
medlon <- median(lonlat$longitude)
#### (Subset)
lonlat <- lonlat[abs(lonlat$longitude - medlon) < 1 & abs(lonlat$latitude - medlat) < 1,]

## Merge with Bikes data
combined <- merge(new, lonlat, by.new = stationName, by.lonlat = stationName, all.x = T)


## scale longitude/lat
combined$lon.scaled <- (combined$longitude - mean(combined$longitude, na.rm=T))/(max(combined$longitude, na.rm=T)-min(combined$longitude, na.rm=T))*10
combined$lat.scaled <- (combined$latitude - mean(combined$latitude, na.rm=T))/(max(combined$latitude, na.rm=T)-min(combined$latitude, na.rm=T))*10
summary(combined$lat.scaled)
summary(combined$lon.scaled)


## Add quadratic factors for lon/lat (and scale them)
combined$lonsq <- combined$longitude*combined$longitude
combined$lonsq.scaled <- (combined$lonsq - mean(combined$lonsq, na.rm=T))/(max(combined$lonsq, na.rm=T)-min(combined$lonsq, na.rm=T))
combined$latsq <- combined$latitude*combined$latitude
combined$latsq.scaled <- (combined$latsq - mean(combined$latsq, na.rm=T))/(max(combined$latsq, na.rm=T)-min(combined$latsq, na.rm=T))
summary(combined$lonsq.scaled)
summary(combined$latsq.scaled)


## Analysis: Using abs val errors

# model 1

## What would the model predict on the data prior to government shutdown?
#subset before and after datasets
before <- combined[combined$date < as.Date('2013-10-1'),]
shutdown <- combined[combined$date >= as.Date('2013-10-1') & combined$date <= as.Date('2013-10-16'),]


## Make model based on data before shutdown
fitPriorToShutdown <- lm(visits ~ weekend + month + year + status2 + lon.scaled + lonsq.scaled + lat.scaled + latsq.scaled, data=before)
summary(fitPriorToShutdown)
## Use prior model to predict bike visits during shutdown
predict <- shutdown
predict$predictOutOfSample <- predict(fitPriorToShutdown, shutdown)
predict$error <- abs(predict$predictOutOfSample - predict$visits)

summary(predict$error)


## Visualize data
meanDailyErrors <- aggregate(list(predict$visits, predict$error), by=list(stationName=predict$stationName, stationID=predict$stationID), FUN=mean)
meanDailyErrors <- rename(meanDailyErrors, c("x"="averageError"))
colnames(meanDailyErrors)[3] <- "avgVisits"
colnames(meanDailyErrors)[4] <- "avgError"
meanDailyErrors["avgErrorScaled"] <- meanDailyErrors$avgError/meanDailyErrors$avgVisits

merged <- merge(lonlat, meanDailyErrors)
write.csv(merged,"../Data/ErrorMapping.csv")



# model 2 : Using non-absolute value error

## What would the model predict on the data prior to government shutdown?
#subset before and after datasets
before <- combined[combined$date < as.Date('2013-10-1'),]
shutdown <- combined[combined$date >= as.Date('2013-10-1') & combined$date <= as.Date('2013-10-16'),]


## Make model based on data before shutdown
fitPriorToShutdown <- lm(visits ~ weekend + month + year + status2 + lon.scaled + lonsq.scaled + lat.scaled + latsq.scaled, data=before)
summary(fitPriorToShutdown)
## Use prior model to predict bike visits during shutdown
predict <- shutdown
predict$predictOutOfSample <- predict(fitPriorToShutdown, shutdown)
predict$error <- predict$visits - predict$predictOutOfSample

summary(predict$error)


## Visualize data
meanDailyErrors <- aggregate(list(predict$visits, predict$error), by=list(stationName=predict$stationName, stationID=predict$stationID), FUN=mean)
meanDailyErrors <- rename(meanDailyErrors, c("x"="averageError"))
colnames(meanDailyErrors)[3] <- "avgVisits"
colnames(meanDailyErrors)[4] <- "avgError"
meanDailyErrors["avgErrorScaled"] <- meanDailyErrors$avgError/meanDailyErrors$avgVisits

merged <- merge(lonlat, meanDailyErrors)
write.csv(merged[1:100,],"../Data/ErrorMappingNotAbs1-100.csv")
write.csv(merged[101:200,],"../Data/ErrorMappingNotAbs101-200.csv")
write.csv(merged[200,],"../Data/ErrorMappingNotAbs200-.csv")

## Visualize using Google Maps Engine Lite