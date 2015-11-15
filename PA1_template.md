# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(plyr)
read.csv('./activity.csv',header=TRUE,colClasses=c('integer','Date','integer'))->x
y<-ddply(x,"date",summarise,sum(steps))
z<-ddply(x,'interval',summarise,mean(steps,na.rm=TRUE))
```

```r
## What is mean total number of steps taken per day?
```

```r
hist(y[,2],breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(y[,2],na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(y[,2],na.rm=TRUE)
```

```
## [1] 10765
```

```r
## What is the average daily activity pattern?
```

```r
plot(z[,1],z[,2],type='l',ylim=c(0,200))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
## Imputing missing values
```

```r
z[z$..1==max(z[,2]),1]
```

```
## [1] 835
```

```r
w<-x
for ( i in seq(1,nrow(w))) {
  if (is.na(w[i,1])) {w[i,1]<-z[z$interval==w[i,3],2] } }

u<-ddply(x,"date",summarise,sum(steps))

hist(u[,2],breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean(u[,2],na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(u[,2],na.rm=TRUE)
```

```
## [1] 10765
```

```r
## Are there differences in activity patterns between weekdays and weekends?
```


