# Peer-graded Assignment: Course Project 1 (Reproducible Research)
Michael Baradas  
July 4, 2018  
==================================================================



## Loading and preprocessing the data


```r
#Installing required packages
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("chron")

#Loading library packages
library(ggplot2)
library(dplyr)
library(chron)
```


1. Load the data (i.e. read.csv())


```r
adata <- read.csv("activity.csv", header = TRUE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
head(adata)
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

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
#Calculating total number of steps taken per date
totalsteps <- aggregate(steps ~ date, adata, FUN = sum)
head(totalsteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Make a histogram of the total number of steps taken each day


```r
#Plotting histogram with hist()
par(mfrow = c(1,1))
par(mar = c(5, 4, 2, 1), las = 1)
hist(totalsteps$steps,
     col = "red",
     xlab = "Frequency",
     ylab = "Steps",
     main = "Total Number of Steps Taken Each Day")
```

![plot of chunk makehistogram](figure/makehistogram-1.png)

3. Calculate and report the **Mean** and **Median** of the total number of steps taken per day


```r
adatamean <- mean(totalsteps$steps)
adatamedian <- median(totalsteps$steps)

#Mean total number of steps taken per day
adatamean
```

```
## [1] 10766.19
```

```r
#Median total number of steps taken per day
adatamedian
```

```
## [1] 10765
```

The total number of steps taken per day **Mean** is **1.0766189 &times; 10<sup>4</sup>**, and the **Median** is **10765**.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
totalinterval <- aggregate(steps ~ interval, adata, FUN = sum)
#Plotting line graph with plot()
par(mar = c(5, 4, 2, 1), las = 1)
plot(totalinterval$interval,
     totalinterval$steps,
     col = "red",
     type = "l",
     lwd = 2,
     xlab = "Interval",
     ylab = "Total Steps",
     main = "Total Steps vs 5-Minute Interval")
```

![plot of chunk makeplot](figure/makeplot-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#Get the 5-minute interval with the maximum number of steps
maxsteps <- filter(totalinterval, steps == max(steps))
maxsteps
```

```
##   interval steps
## 1      835 10927
```

The maximum number of steps is 10927 steps, happening in the 835 5-minute interval.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
#Counting rows with missing values (NAs)
adatana <- table(is.na(adata))
adatana
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

```r
adatanatrue <- table(is.na(adata))["TRUE"]
adatanatrue
```

```
## TRUE 
## 2304
```

The total number of rows with missing values (NAs) are 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
#Calculate the mean of the original 5-minute interval data
meaninterval <- aggregate(steps ~ interval, adata, FUN = mean)

#Merge the meaninterval with the original data
adatanew <- merge(x = adata, y = meaninterval, by = "interval")

#Replace the missing NA values with the mean
adatanew$steps <- ifelse(is.na(adatanew$steps.x), adatanew$steps.y, adatanew$steps.x)

#New data set
head(adatanew)
```

```
##   interval steps.x       date  steps.y    steps
## 1        0      NA 2012-10-01 1.716981 1.716981
## 2        0       0 2012-11-23 1.716981 0.000000
## 3        0       0 2012-10-28 1.716981 0.000000
## 4        0       0 2012-11-06 1.716981 0.000000
## 5        0       0 2012-11-24 1.716981 0.000000
## 6        0       0 2012-11-15 1.716981 0.000000
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#Select the required columns and save the new data set
adatanew <- select(adatanew, steps, date, interval)

#New data set replacing the NAs with the mean
head(adatanew)
```

```
##      steps       date interval
## 1 1.716981 2012-10-01        0
## 2 0.000000 2012-11-23        0
## 3 0.000000 2012-10-28        0
## 4 0.000000 2012-11-06        0
## 5 0.000000 2012-11-24        0
## 6 0.000000 2012-11-15        0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the **Mean** and **Median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Total steps over date
totalsteps_new<- aggregate(steps ~ date, adatanew, FUN = sum)

#Plotting histogram
#Set panel parameters for one row and two columns
par(mfrow = c(1,2))
par(mar = c(5, 4, 2, 1), las = 1)

#Calculate maximum ylim limit for histogram
rng_new <- max((hist(totalsteps_new$steps, plot = FALSE))$counts)
rng_orig <- max((hist(totalsteps$steps, plot = FALSE))$counts)
rng <- max(rng_new, rng_orig)

#Histogram with new values
hist(totalsteps_new$steps, 
     col = "blue",
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,rng),
     main = "Total Number of Steps Taken Each Day \n(Replaced Missing NA Values with Mean)",
     cex.main = 0.75)

#Histogram with  orginal values
hist(totalsteps$steps, 
     col = "red", 
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0, rng),
     main = "Total Number Of Steps Taken Each day \n(Orginal Data)",
     cex.main = 0.75)
```

![plot of chunk makenewhist](figure/makenewhist-1.png)

```r
# Reset panel to default
par(mfrow = c(1,1))
```


```r
adatamean_new <- mean(totalsteps_new$steps)
adatamedian_new <- median(totalsteps_new$steps)

#Compare the Means
paste("New Mean:", round(adatamean_new, 2), "," ,  
      " Original Mean:", round(adatamean, 2),"," , 
      " Difference:", round(adatamean_new -  adatamean, 2))
```

```
## [1] "New Mean: 10766.19 ,  Original Mean: 10766.19 ,  Difference: 0"
```

```r
#Compare the Medians
paste("New Median:", adatamedian_new, ",", 
      " Original Median:", adatamedian,"," ,  
      " Difference:",round(adatamedian_new -adatamedian, 2))
```

```
## [1] "New Median: 10766.1886792453 ,  Original Median: 10765 ,  Difference: 1.19"
```

```r
newmediandiff <- round(adatamedian_new - adatamedian, 2)
```

The **Mean** will be the same. However the **New Median** will differ from the **Original Median** by 1.19.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
#Determine if weekend with library(chron)
library(chron)
table(is.weekend(adatanew$date))
```

```
## 
## FALSE  TRUE 
## 12960  4608
```

```r
#Create new factor variable "dayweek" to indicate weekday or weekend day
adatanew$dayweek <- ifelse(is.weekend(adatanew$date), "weekend", "weekday")

#Count Weekdays and Weekends
table(adatanew$dayweek)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
#New data set with new factor variable dayweek
head(adatanew)
```

```
##      steps       date interval dayweek
## 1 1.716981 2012-10-01        0 weekday
## 2 0.000000 2012-11-23        0 weekday
## 3 0.000000 2012-10-28        0 weekend
## 4 0.000000 2012-11-06        0 weekday
## 5 0.000000 2012-11-24        0 weekend
## 6 0.000000 2012-11-15        0 weekday
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#Mean of total steps over interval and day of week
meaninterval_new<- aggregate(steps ~ interval + dayweek, adatanew, FUN = mean)

#New data
head(meaninterval_new)
```

```
##   interval dayweek      steps
## 1        0 weekday 2.25115304
## 2        5 weekday 0.44528302
## 3       10 weekday 0.17316562
## 4       15 weekday 0.19790356
## 5       20 weekday 0.09895178
## 6       25 weekday 1.59035639
```

```r
#Plot time series data with ggplot
par(mfrow = c(1,1))
par(mar = c(5, 4, 2, 1), las = 1)
ggplot(meaninterval_new, 
       aes(x = interval, y = steps)) + 
  geom_line(color = "blue", size = 1) + 
  facet_wrap(~dayweek, nrow = 2) +
  labs(x = "Interval", 
       y = "Number of Steps")
```

![plot of chunk plotsimuldata](figure/plotsimuldata-1.png)

```r
par(mfrow = c(1,1))
```



