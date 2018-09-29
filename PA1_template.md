
##Reproducible Research:Project-1

####The following code downloads and unzip the data file.

```r
project1url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filename<-"Rep_activity.zip"
if(!file.exists(filename)){download.file(project1url,destfile = filename)}
unzip(filename)
```
####Loading and processing the data

```r
  repdata<-read.csv("activity.csv",header = TRUE,sep=",")
  dim(repdata)
```

```
## [1] 17568     3
```

```r
  names(repdata)
```

```
## [1] "steps"    "date"     "interval"
```

```r
  str(repdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

**1. What is mean total number of steps taken per day?**

*For this part of the assignment, you can ignore the missing values in the dataset.*

*1.1 Calculate the total number of steps taken per day.*

```r
  step_day<-data.frame(Totalstepsday=with(repdata,tapply(steps,date,sum)),date=levels(repdata$date),row.names = NULL)
library(dplyr)  
as_tibble(step_day)
```

```
## # A tibble: 61 x 2
##    Totalstepsday date      
##            <int> <fct>     
##  1            NA 2012-10-01
##  2           126 2012-10-02
##  3         11352 2012-10-03
##  4         12116 2012-10-04
##  5         13294 2012-10-05
##  6         15420 2012-10-06
##  7         11015 2012-10-07
##  8            NA 2012-10-08
##  9         12811 2012-10-09
## 10          9900 2012-10-10
## # ... with 51 more rows
```
*1.2 Make a histogram of the total number of steps taken each day.*

```r
  with(step_day,hist(Totalstepsday,col="green",xlab="steps",main=" Total steps each day"))
```

![plot of chunk Question1.2](figure/Question1.2-1.png)

```r
dev.copy(png,file="step_original_data.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

*1.3 Calculate and report the mean and median of the total number of steps taken per day.*

```r
print("Mean");with(step_day,mean(Totalstepsday,na.rm=T))
```

```
## [1] "Mean"
```

```
## [1] 10766.19
```

```r
print("Median");with(step_day,median(Totalstepsday,na.rm=T))  
```

```
## [1] "Median"
```

```
## [1] 10765
```
The mean and median differ slightly in values.

**2. What is the average daily activity pattern?**

*2.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).*

```r
pattern<-data.frame(interval=levels(factor(repdata$interval)),mean=with(repdata,tapply(steps,factor(interval),mean,na.rm=TRUE)),row.names = NULL)

pattern<-transform(pattern,interval=as.integer(as.character(interval)))

with(pattern,plot(interval,mean,type="l",xlab="5-minute Inetrval",ylab="Average number of steps", col="red",main="Time series plot"))
```

![plot of chunk Question2.1](figure/Question2.1-1.png)

```r
dev.copy(png,file="activity_pattern.png")
```

```
## png 
##   3
```

```r
  dev.off()
```

```
## png 
##   2
```

*2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

```r
with(pattern,interval[which.max(mean)])
```

```
## [1] 835
```
**3. Imputing missing values.**

*3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA).*

```r
  missing_value<-table(is.na(repdata))
  print("Total Number of Missing Values:")
```

```
## [1] "Total Number of Missing Values:"
```

```r
  print(missing_value[2])
```

```
## TRUE 
## 2304
```
*3.2 Devise a strategy for filling in all of the missing values in the dataset.*

(The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.)

```r
  as_tibble(repdata)   #Original data.
```

```
## # A tibble: 17,568 x 3
##    steps date       interval
##    <int> <fct>         <int>
##  1    NA 2012-10-01        0
##  2    NA 2012-10-01        5
##  3    NA 2012-10-01       10
##  4    NA 2012-10-01       15
##  5    NA 2012-10-01       20
##  6    NA 2012-10-01       25
##  7    NA 2012-10-01       30
##  8    NA 2012-10-01       35
##  9    NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

```r
  as_tibble(pattern)   # data with mean for 5-minute interval.
```

```
## # A tibble: 288 x 2
##    interval   mean
##       <int>  <dbl>
##  1        0 1.72  
##  2        5 0.340 
##  3       10 0.132 
##  4       15 0.151 
##  5       20 0.0755
##  6       25 2.09  
##  7       30 0.528 
##  8       35 0.868 
##  9       40 0     
## 10       45 1.47  
## # ... with 278 more rows
```

```r
  steps_na<-which(is.na(repdata$steps))  #index of NA
  filler<-ifelse(repdata[steps_na,"interval"]==pattern$interval,pattern$mean)
```
The missing values are filled/replaced by mean for 5-minute interval.

*3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.*

```r
  rep<-repdata
  rep[steps_na,"steps"]=filler
  as_tibble(rep)  # data with NA replaced by mean for 5-minute interval.
```

```
## # A tibble: 17,568 x 3
##     steps date       interval
##     <dbl> <fct>         <int>
##  1 1.72   2012-10-01        0
##  2 0.340  2012-10-01        5
##  3 0.132  2012-10-01       10
##  4 0.151  2012-10-01       15
##  5 0.0755 2012-10-01       20
##  6 2.09   2012-10-01       25
##  7 0.528  2012-10-01       30
##  8 0.868  2012-10-01       35
##  9 0      2012-10-01       40
## 10 1.47   2012-10-01       45
## # ... with 17,558 more rows
```
*3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

```r
rep_step_day<-data.frame(Totalstepsday=with(rep,tapply(steps,date,sum)),date=levels(rep$date),row.names = NULL)

as_tibble(rep_step_day)
```

```
## # A tibble: 61 x 2
##    Totalstepsday date      
##            <dbl> <fct>     
##  1        10766. 2012-10-01
##  2          126  2012-10-02
##  3        11352  2012-10-03
##  4        12116  2012-10-04
##  5        13294  2012-10-05
##  6        15420  2012-10-06
##  7        11015  2012-10-07
##  8        10766. 2012-10-08
##  9        12811  2012-10-09
## 10         9900  2012-10-10
## # ... with 51 more rows
```

```r
with(rep_step_day,hist(Totalstepsday,col="wheat",xlab="Steps",main=" Total number of steps each day"))
```

![plot of chunk Question3.4](figure/Question3.4-1.png)

```r
dev.copy(png,file="step_filled_data.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
summary(rep_step_day)
```

```
##  Totalstepsday           date   
##  Min.   :   41   2012-10-01: 1  
##  1st Qu.: 9819   2012-10-02: 1  
##  Median :10766   2012-10-03: 1  
##  Mean   :10766   2012-10-04: 1  
##  3rd Qu.:12811   2012-10-05: 1  
##  Max.   :21194   2012-10-06: 1  
##                  (Other)   :55
```
The mean and median for new data is same and these values do not vary much from the original data. In fact mean for newdata is integer part of the mean for original one.

**4. Are there differences in activity patterns between weekdays and weekends?**
(For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.)


*4.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*

```r
  rep_week<-rep%>%as_tibble()%>%mutate(days=weekdays(as.Date(date)))
  #creating labels of weekday and weekend.
  week<-data.frame(days=levels(factor(rep_week$days)))
  week<-mutate(week,num=levels(week$days)<-c(5,1,6,7,4,2,3))
  week<-mutate(week,week=factor(num<6,labels=c("weekend","weekday")))
  #adding a column in object rep of weekday or weekend.
  rep_week<-rep_week%>%as_tibble()%>%mutate(week=factor(days,levels = week$days,labels = week$week))
  rep_week
```

```
## # A tibble: 17,568 x 5
##     steps date       interval days   week   
##     <dbl> <fct>         <int> <chr>  <fct>  
##  1 1.72   2012-10-01        0 Monday weekday
##  2 0.340  2012-10-01        5 Monday weekday
##  3 0.132  2012-10-01       10 Monday weekday
##  4 0.151  2012-10-01       15 Monday weekday
##  5 0.0755 2012-10-01       20 Monday weekday
##  6 2.09   2012-10-01       25 Monday weekday
##  7 0.528  2012-10-01       30 Monday weekday
##  8 0.868  2012-10-01       35 Monday weekday
##  9 0      2012-10-01       40 Monday weekday
## 10 1.47   2012-10-01       45 Monday weekday
## # ... with 17,558 more rows
```
*4.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*

```r
  week_mean<-rep_week%>%group_by(interval,week)%>%summarise(mean=mean(steps,na.rm=TRUE))
  week_mean
```

```
## # A tibble: 576 x 3
## # Groups:   interval [?]
##    interval week       mean
##       <int> <fct>     <dbl>
##  1        0 weekday 2.25   
##  2        0 weekend 0.215  
##  3        5 weekday 0.445  
##  4        5 weekend 0.0425 
##  5       10 weekday 0.173  
##  6       10 weekend 0.0165 
##  7       15 weekday 0.198  
##  8       15 weekend 0.0189 
##  9       20 weekday 0.0990 
## 10       20 weekend 0.00943
## # ... with 566 more rows
```

```r
  library(lattice)
  xyplot(mean~interval|week,data=week_mean,layout=c(1,2),type="l",lty=1)
```

![plot of chunk Question4.2](figure/Question4.2-1.png)

```r
  dev.copy(png,file="steps_weekday_weekend.png")
```

```
## png 
##   3
```

```r
  dev.off()
```

```
## png 
##   2
```
