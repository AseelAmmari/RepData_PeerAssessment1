---
output: 
  html_document: 
    keep_md: yes
---
**Loading and preprocessing the data**  

```r
getwd()
```

```
## [1] "C:/Users/ASEEL/Desktop"
```

```r
setwd("C:/Users/ASEEL/Desktop")
getwd()
```

```
## [1] "C:/Users/ASEEL/Desktop"
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)
Data <- read.csv("activity.csv")
```

more details 

```
##  [1] 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06
##  [7] 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12
## [13] 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18
## [19] 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24
## [25] 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30
## [31] 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05
## [37] 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11
## [43] 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17
## [49] 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23
## [55] 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29
## [61] 2012-11-30
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```
##    steps            date          interval      
##  Mode :logical   Mode :logical   Mode :logical  
##  FALSE:15264     FALSE:17568     FALSE:17568    
##  TRUE :2304
```
**What is mean total number of steps taken per day?**


```r
par(mfrow=c(1,1))
sumD <- data.frame(date=unique(Data$date),steps=tapply(Data$steps,Data$date,sum,na.rm=TRUE))
hist(sumD$steps,xlab = "Number of Steps",main="Steps per Day",ylab="frequancy")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
sumD is the total number of steps per eachday 
and the mean and the median for the previous data are 

```r
mean(sumD$steps)
```

```
## [1] 9354.23
```

```r
median(sumD$steps)
```

```
## [1] 10395
```
**What is the average daily activity pattern?**


```r
sumI <- data.frame(Interval=as.integer(unique(Data$interval)),M=tapply(Data$steps,Data$interval,mean,na.rm=TRUE))
plot(sumI$Interval,sumI$M,type="l",xlab="5-mins Intervals",ylab="Avg num of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 

```r
maxstepsI <- max(sumI$M)
maxstepsI
```

```
## [1] 206.1698
```

```r
sumI[sumI$M==maxstepsI,]
```

```
##     Interval        M
## 835      835 206.1698
```

** Imputing missing values**


```r
summary(is.na(Data$steps))
```

```
##    Mode   FALSE    TRUE 
## logical   15264    2304
```

```r
sum(is.na(Data$steps))
```

```
## [1] 2304
```
    
and if we make tha same  histogram without these missing data 

```r
sumDM <- data.frame(date=unique(Data$date),steps=tapply(Data$steps,Data$date,sum))
hist(sumDM$steps,xlab = "Number of Steps",main="Steps per Day(with missing data)",ylab="frequancy")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



```r
Data$date <- as.Date(Data$date)
clean <- data.frame(Data[!is.na(Data$steps),])
bad <- Data[is.na(Data$steps),]
dataM <- data.frame(interval=unique(clean$interval),mean=tapply(clean$steps,clean$interval,mean))
clean2 = data.frame(merge(bad[,c("steps","date","interval")],dataM[ ,c("interval","mean")]))
clean2[,2] <- clean2[,4]
bad2<- clean2[,1:3]

newNA<- data.frame(steps=bad2[,2],date=bad2[,3],interval=bad2[,1])
newdata <- rbind(clean,newNA)
```

**Are there differences in activity patterns between weekdays and weekends?**


```r
Data$date <- as.Date(Data$date,"%Y %m %A")
head(Data$date)
```

```
## [1] "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01"
## [6] "2012-10-01"
```

```r
Wday<- function(d){
  wd <- weekdays(d)
  ifelse(wd =="Saturday" | wd=="Sunday","weekend","weekday")}
date <- as.Date(date)
w <- data.frame(WD=sapply(Data$date,Wday),interval=Data$interval,steps=Data$steps)
WD <- w[w$WD== "weekday" ,]
WE <- w[w$WD== "weekend" ,]
WD <- data.frame(Interval=unique(WD$interval),steps=tapply(WD$steps,WD$interval,sum,na.rm=TRUE))
WE <- data.frame(Interval=unique(WE$interval),steps=tapply(WE$steps,WE$interval,sum,na.rm=TRUE))
par(mfrow=c(1,2))
xyplot(WD$steps~WD$Interval,type="l",xlab = "Intervals", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
xyplot(WE$steps~WE$Interval,type="l",xlab = "Intervals", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-2.png)<!-- -->


