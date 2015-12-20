# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# 1.load data
rdata <- read.csv("activity.csv")  
# 2.process/transmform...hum no
rdata$date <- as.Date(rdata$date)
rdata$steps <- as.numeric(rdata$steps)
rdata$interval <- as.numeric(rdata$interval)
```

## What is mean total number of steps taken per day?

```r
# get a list of each date
dates <- levels(factor(rdata$date))
# 1.calculate and 2.barchart the number of steps taken per day
tsteps <- sapply(dates, function(x) sum(rdata[rdata$date==x,"steps"], na.rm=TRUE))
barplot(tsteps, main="Total steps", xlab="date", ylab="n steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# 3. mean? median?
mean(tsteps)
```

```
## [1] 9354.23
```

```r
median(tsteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
# 1. time series of average number of steps taken
intervals <- levels(factor(rdata$interval))
tsteps_ave <- aggregate(steps~interval, data=rdata, FUN=mean, na.action=na.omit)
plot(tsteps_ave, main="Average steps during the day", xlab="interval", ylab="average steps over time", type = 'l')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# 2. when was the maximum?
maxatindex <- which(tsteps_ave$steps == max(tsteps_ave$steps, na.rm = TRUE))
tsteps_ave[maxatindex,]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
# 1. number of missing values
sum(is.na(rdata$steps))
```

```
## [1] 2304
```

```r
# 2. filling in all of the missing values in the dataset
d <- rdata
for(i in 1:length(d$steps))
{
  d$steps[i] <- if(!is.na(d$steps[i])) d$steps[i] else
    {tsteps_ave$steps[which(tsteps_ave$interval == d$interval[i])]}
}
# 4. histogram? mean? median?
ttsteps <- sapply(dates, function(x) sum(d[d$date==x,"steps"]))
barplot(ttsteps, main="Total steps", xlab="date", ylab="n steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
ttotal_steps = aggregate(steps~date, data=rdata, FUN=sum, na.action=na.omit)
mean(ttotal_steps$steps)
```

```
## [1] 10766.19
```

```r
median(ttotal_steps$steps)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
# 1. Create new factor weekday and weekend
d$weekend <- chron::is.weekend(d$date)
d$weekend <- factor(d$weekend, levels=c(TRUE,FALSE), labels=c('weekend','weekday'))
# 2. panel plot containing time series plot of interval
library(ggplot2)
ggplot(data=d, aes(x=interval, y=steps)) +
  geom_line() + ylab("Number of steps") +
  facet_wrap(~weekend, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
