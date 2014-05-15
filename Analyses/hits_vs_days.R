# Using prepared data from stata

setwd("/Users/jeffreyli/Desktop/Bikes/Source")

data <- read.csv("../Data/2010-4th-quarter-done.csv")
data$date <- as.Date(data$date,format='%m/%d/%Y')
data$weekdays <- weekdays(data$date)
data$month <- as.factor(as.numeric(format(data$date, "%m")))
data$year <- as.factor(as.numeric(format(data$date, "%Y")))
data["weekend"] <- grepl("Sunday|Saturday", data$weekdays)

fit <- lm(count ~ weekend + month, data=data)
summary(fit) # show results

plot(count ~ weekdays, data = data)
boxplot(count ~ weekend, data = data)
