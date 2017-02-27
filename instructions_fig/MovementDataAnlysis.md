Collection and processing of data.
----------------------------------

    if(!dir.exists('./data')) {
        dir.create('./data')
    }

    fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
    download.file(fileUrl, destfile = './data/data.zip')
    unzip('./data/data.zip', exdir = './data')

    activity <- read.csv("data/activity.csv")
    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?
-------------------------------------------------

    SumSteps <- tapply(activity$steps, activity$date, sum)
    library(reshape2)
    SumStepsMelted <- melt(SumSteps)
    hist(SumStepsMelted$value, breaks = 20, xlab = "Total steps taken in a day", main = "", col = "orange")

![](MovementDataAnlysis_files/figure-markdown_strict/unnamed-chunk-2-1.png)

### And the mean and median of the total steps in a day?

    mean(SumStepsMelted$value, na.rm = TRUE)

    ## [1] 10766.19

    median(SumStepsMelted$value, na.rm = TRUE)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

### Time series plot of 5 min intervals and average number of steps taken.

    ActAvg <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
    ActAvgMelt <- melt(ActAvg)
    names(ActAvgMelt) <- c("Interval", "AvgSteps")
    plot(AvgSteps ~ Interval, data = ActAvgMelt, type = "l", col = "red", main = "Averge daily steps", xlab = "5 min interval")

![](MovementDataAnlysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### And the mean and median total number of steps in a day?

    ActAvgMelt[ActAvgMelt$AvgSteps == max(ActAvgMelt$AvgSteps), ]

    ##     Interval AvgSteps
    ## 104      835 206.1698

### So the maximal mean daily steps occurs at the 835th 5 minute interval

There are days and intervals where data are missing. This could affect the analysis.
------------------------------------------------------------------------------------

    sum(is.na(activity$steps))/nrow(activity)

    ## [1] 0.1311475

### So 13% of the observations are not actually measured.

Impute missing values
---------------------

### Create new data set with imputed missing values, using mean without NAs, for comparison.

    ActImpute <- activity
    ActImpute$steps[is.na(ActImpute$steps)] <- mean(ActImpute$steps, na.rm = TRUE)

### Make a histogram of the total number of steps taken each day.

    ActImputeSteps <- tapply(ActImpute$steps, ActImpute$date, sum)
    library(reshape2)
    ImputeMelt <- melt(ActImputeSteps)
    names(ImputeMelt) <- c("Date", "SumOfSteps")
    MeanSteps <- mean(ImputeMelt$SumOfSteps)
    MedianSteps <- median(ImputeMelt$SumOfSteps)
    hist(ImputeMelt$SumOfSteps, breaks = 20, xlab = "Total steps taken in a day", main = "", col = "red")

![](MovementDataAnlysis_files/figure-markdown_strict/unnamed-chunk-8-1.png)

### Report mean and median of total number of steps taken per day.

    mean(ImputeMelt$SumOfSteps, na.rm = TRUE)

    ## [1] 10766.19

    median(ImputeMelt$SumOfSteps, na.rm = TRUE)

    ## [1] 10766.19

### Compare these values to when the NAs were just removed, above: mean = 10766.19 and median = 10765. Not a great deal of difference.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Looking only at the imputed data create a new column which tells whether a date is a weekday or weekend day.

    ActImpute$date <- as.Date(ActImpute$date)
    ActImpute$weekdays <- weekdays(ActImpute$date)
    ActImpute$weeks[(ActImpute$weekdays == "Saturday" | ActImpute$weekdays == "Sunday")] <- "weekend"
    ActImpute$weeks[!(ActImpute$weekdays == "Saturday" | ActImpute$weekdays == "Sunday")] <- "weekdays"
    library(plyr)
    WeekData <- ddply(ActImpute, c("interval", "weeks"), function(x) apply(x[1], 2, mean))
    str(WeekData)

    ## 'data.frame':    576 obs. of  3 variables:
    ##  $ interval: int  0 0 5 5 10 10 15 15 20 20 ...
    ##  $ weeks   : chr  "weekdays" "weekend" "weekdays" "weekend" ...
    ##  $ steps   : num  7.01 4.67 5.38 4.67 5.14 ...

### Panel plot of time series of 5-minute intervals and average steps taken, averaged across all weekday days or weekend days.

    library(lattice)
    xyplot(steps ~ interval | weeks, data = WeekData, type = "l", xlab = "interval", ylab = "steps", layout = c(1, 2))

![](MovementDataAnlysis_files/figure-markdown_strict/unnamed-chunk-11-1.png)

### Notice activity is spread across the day during the weekend and concentrated roughly before midday during the week.
