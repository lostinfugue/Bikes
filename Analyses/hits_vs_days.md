Capital Bikeshare in DC
========================================================
# Sept. - Dec. 2010

### Dependencies:
### Options: Analysis cached



## Load the data



## Here's a summary of the data:


```r
summary(data)
```

```
##       date                          stationName     stationID    
##  Min.   :2010-09-15   14th & R St NW      : 104   Min.   :31000  
##  1st Qu.:2010-10-23   14th & V St NW      : 104   1st Qu.:31108  
##  Median :2010-11-14   18th & Bell St      : 104   Median :31218  
##  Mean   :2010-11-13   10th & U St NW      : 103   Mean   :31287  
##  3rd Qu.:2010-12-07   14th & Harvard St NW: 103   3rd Qu.:31503  
##  Max.   :2010-12-31   15th & P St NW      : 103   Max.   :31999  
##                       (Other)             :7831                  
##      count         weekdays         month       year       weekend       
##  Min.   :  2.0   Length:8452        9 : 544   2010:8452   Mode :logical  
##  1st Qu.:  8.0   Class :character   10:2373               FALSE:6201     
##  Median : 20.0   Mode  :character   11:2851               TRUE :2251     
##  Mean   : 27.9                      12:2684               NA's :0        
##  3rd Qu.: 40.0                                                           
##  Max.   :236.0                                                           
## 
```


## Relating bike station hits to month and weekday vs. weekend:


```r
fit <- lm(count ~ weekend + month, data = data)
summary(fit)  # show results
```

```
## 
## Call:
## lm(formula = count ~ weekend + month, data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -34.05 -18.05  -6.85  12.02 199.95 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   14.846      1.113   13.34  < 2e-16 ***
## weekendTRUE    3.035      0.637    4.76  1.9e-06 ***
## month10       15.136      1.228   12.32  < 2e-16 ***
## month11       18.174      1.207   15.05  < 2e-16 ***
## month12        5.923      1.213    4.88  1.1e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.8 on 8447 degrees of freedom
## Multiple R-squared:  0.0559,	Adjusted R-squared:  0.0554 
## F-statistic:  125 on 4 and 8447 DF,  p-value: <2e-16
```


## Plot Relationships

```r
plot(data$weekend, data$count, main = "Station Hits vs. Weekday/Weekend", xlab = "Weekday (1) or Weekend(2)", 
    ylab = "Number of Station Hits")
abline(fit[1], fit$weekendTRUE)
```

```
## Warning: only using the first two of 5 regression coefficients
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r

plot(data$month, data$count, main = "Station Hits vs. Month", xlab = "Month", 
    ylab = "Number of Station Hits")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 


<iframe src="https://mapsengine.google.com/map/embed?mid=zBlpnPnIr12s.k-xTvWnNNEQg" width="800" height="480"></iframe>

Thanks!  
Jeff
