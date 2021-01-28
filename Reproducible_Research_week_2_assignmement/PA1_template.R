library(ggplot2)
library(rmarkdown)
 
wd <- getwd() # will default to your WD
#let us list the webiste where we find our data
website <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#now we need to go get our data
download.file(website, file.path(wd, "activity.zip"))

#now unzip into memory of R and or WD----------------------------------------------------------------------
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")


## What is mean total number of steps taken per day? ---------------------------

Tsteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(Tsteps, binwidth=1000, xlab="total number of steps taken each day")
mean(Tsteps, na.rm=TRUE)
median(Tsteps, na.rm=TRUE)


## What is the average daily activity pattern? ---------------------------------

Avgs <- aggregate(x=list(steps=data$steps), 
                  by = list(interval=data$interval), 
                  FUN = mean, na.rm = TRUE)

ggplot(data=Avgs, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")

Avgs[which.max(Avgs$steps),]


## Imputing missing values -------------------------------------------------------
missingVaules <- is.na(data$steps)
    # How many missing - Calculate and report the total number of missing values
    table(missingVaules)
    # Devise a strategy for filling in all of the missing values in the dataset.
    fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
            filled <- c(steps)
        else
            filled <- (Avgs[Avgs$interval==interval, "steps"])
        return(filled)
    }
    #Create a new dataset that is equal to the original dataset but with the missing data filled in.
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
    #Make a histogram of the total number of steps taken each day and Calculate and report the mean and median
    total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
    mean(total.steps)
    median(total.steps)


## Are there differences in activity patterns between weekdays and weekends?-----
weekday.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}

    #Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN = weekday.weekend)
    
    # Make a panel plot containing a time series plot
    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")

    

render("PA1_template.Rmd", output_file = "PA1_template.md", output_dir = wd)














