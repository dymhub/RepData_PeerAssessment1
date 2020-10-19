---
title: "Reproducible Research Assignment 1"
author: "JDB"
date: "19/10/2020"
output: 
  html_document: 
    keep_md: yes
---



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
The data for this assignment can be downloaded from the course web site:

+ Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K] 

The variables included in this dataset are:

+ steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
+ date: The date on which the measurement was taken in YYYY-MM-DD format
+ interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

This Markdown file will now run through the data processing code on a question by question basis. Before the code required for the questions is reported, some intitial set up is required. To begin an working directory must be set up which only contains the unzipped data.

Next, the following code must be run to load the relvant libraries.

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
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
```

Now all the prerequisites have been met to allow proccessing of the data as per the assignment.

### 1. Code for reading in the dataset and/or processing the data
<br />


```r
# Define function to read data
read.data <- function(file.list){
        # List files in Working Directory 
        files <- list.files()
        
        # Read data file & convert to tibble
        df <- read.csv(files[file.list]) %>%
                tbl_df() %>%
                mutate(date = ymd(date))
}

# Execute function
df <- read.data(1)
```
<br />
<br />
<br />
<br />


### 2. Histogram of the total number of steps taken each day
<br />


```r
# Define function which takes the data and summarises the total number of steps recorded each day
total.steps <- function(df){
        # Remove NA values, group by date and then sum the total number of steps by date
        tot_step <- na.omit(df) %>%
                group_by(date) %>%
                summarise(Total = sum(steps))
}

# Execute function using df which was generated in question 1
tot_step <- total.steps(df)

# Generate histogram using the tot_step data frame
hist(tot_step$Total, 
     breaks = 20, 
     xlab =  "Total Steps", 
     main = "Histogram - Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
<br />
<br />
<br />
<br />

### 3. Mean and median number of steps taken each day
<br />

```r
# Display a summary of total steps recorded per day
summary(tot_step$Total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```
<br />
<br />
<br />
<br />

### 4. Time series plot of the average number of steps taken
<br />

```r
# Define function which takes a data frame and summarises the average steps taken in each interval
mean.steps <- function(df){
        # Remove NA values, group by interval and take the mean of the steps recorded for each interval
        mean_step <- na.omit(df) %>%
                group_by(interval) %>%
                summarise(Average = mean(steps)) 
}

# Execute function using data frame from question 1
mean_step <- mean.steps(df)

# Plot a graph which shows the mean steps per interval
plot(mean_step$interval, 
     mean_step$Average, 
     xlab = "Interval", 
     ylab = "Average Steps", 
     main = "Average steps over time",
     type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
<br />
<br />
<br />
<br />

### 5.The 5-minute interval that, on average, contains the maximum number of steps
<br />


```r
# Sort the data frame of interval means by Highest first
Maximum <- arrange(mean_step, desc(Average))

# Report the first row of the new data frame
Maximum[1,]
```

```
## # A tibble: 1 x 2
##   interval Average
##      <int>   <dbl>
## 1      835    206.
```
<br />
<br />
<br />
<br />

### 6.Code to describe and show a strategy for imputing missing data
<br />


```r
# Define a function which replaces the NA values of the original data frame with median values per interval based on the rest of the data
NA.steps <- function(df){
        # Remove NA values, group by interval, calculate the median for each interval and arrange by intervals.
        median_step <- na.omit(df) %>%
                group_by(interval) %>%
                summarise(Median = median(steps)) %>%
                arrange(interval)
        
        # Label the complete observations (No NAs), filter by non-complete cases (NAs), arrange by date and overwrite the steps              column with 8 repetitions (8 days) of the medians previously caluculted.
        NA_df <- mutate(df, Complete = complete.cases(df)) %>%
                filter(Complete == FALSE) %>%
                arrange(date) %>%
                mutate(steps = rep(median_step$Median,8)) %>%
                select(-Complete)
        # Combine the estimated data with the original data frame minus the NA observations. 
        bind_rows(NA_df, na.omit(df))
        
}

# Calculate & report the number of NA rows in data frame
No_NA <- nrow(df) - sum(complete.cases(df))
No_NA
```

```
## [1] 2304
```

```r
# Exectute function using data frame from question 1
NA_df <- NA.steps(df)
```
<br />
<br />
<br />
<br />

### 7.Histogram of the total number of steps taken each day after missing values are imputed
<br />


```r
# Execture the total.steps function using the updated data frame from question 6
Total.NA.steps <- total.steps(NA_df)

# Generate histogram of total steps per day using updated data frame
hist(Total.NA.steps$Total, 
     breaks = 20, 
     xlab =  "Total Steps", 
     main = "Histogram - Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
# Report mean and median of Total steps per day data from new data frame
summary(Total.NA.steps$Total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10395    9504   12811   21194
```
<br />
<br />
<br />
<br />

### 8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
<br />



```r
# Define a function which groups the data by weekdays and weekends
day.steps <- function(df){
        # Add a column to the original data frame which includes the day for each date
        day.steps <- mutate(df, Days = weekdays(date)) 
        # Split the data frame into Weekends and week days
        # Filter the data frame by weekends and append a column which specifies day type ie. Weekend
        Weekend.steps <- filter(day.steps, Days == "Saturday" | Days == "Sunday") %>%
                mutate(Weekend = "Weekend")
        # Repeat for weekdays
        Weekday.steps <- filter(day.steps, Days != "Saturday" & Days != "Sunday") %>%
                mutate(Weekend = "Weekday")
        # Combine the two data frames and calulate the mean steps per day type
        day.steps <- bind_rows(Weekend.steps, Weekday.steps) %>%
                group_by(Weekend, interval) %>%
                summarise(Average = mean(steps))
}

# Execute the function using the data frame with estimated NA values
Day.steps <- day.steps(NA_df)

# Plot a graph of average steps per interval split by day type
 g <- ggplot(Day.steps, aes(x = interval, y = Average))
 g + geom_line() +
         facet_grid(Weekend ~.) +
         ggtitle("Averge steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
<br />
<br />
<br />
<br />
 
### 9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

<br />

Below is tall of the raw code used in this assignment. The entirity of the code is found in the report above.


```r
# Set working directory
setwd("~/Documents/R/Reproducible Research/Project 1")

#load libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Functions
########## 
read.data <- function(file.list){
        # List files in WD
        files <- list.files()
        # Read data, remove NAs & convert to tibble
        df <- read.csv(files[file.list]) %>%
                tbl_df() %>%
                mutate(date = ymd(date))
}
total.steps <- function(df){
        tot_step <- na.omit(df) %>%
                group_by(date) %>%
                summarise(Total = sum(steps))
}
mean.steps <- function(df){
        mean_step <- na.omit(df) %>%
                group_by(interval) %>%
                summarise(Average = mean(steps))
}
median.steps <- function(df){
        median_step <- na.omit(df) %>%
                group_by(interval) %>%
                summarise(Median = median(steps))
}
NA.steps <- function(df){
        median_step <- na.omit(df) %>%
                group_by(interval) %>%
                summarise(Median = median(steps)) %>%
                arrange(interval)
        NA_df <- mutate(df, Complete = complete.cases(df)) %>%
                filter(Complete == FALSE) %>%
                arrange(date) %>%
                mutate(steps = rep(median_step$Median,8)) %>%
                select(-Complete)
                bind_rows(NA_df, na.omit(df))
}
day.steps <- function(df){
        day.steps <- mutate(df, Days = weekdays(date)) 
        Weekend.steps <- filter(day.steps, Days == "Saturday" | Days == "Sunday") %>%
                mutate(Weekend = "Weekend")
        Weekday.steps <- filter(day.steps, Days != "Saturday" & Days != "Sunday") %>%
                mutate(Weekend = "Weekday")
        day.steps <- bind_rows(Weekend.steps, Weekday.steps) %>%
                group_by(Weekend, interval) %>%
                summarise(Average = mean(steps))
}
        
        
        
        


#######
# Read data & convert to tibble
df <- read.data(1)

#######
# TOTAL STEPS PER DAY
# Calculate total steps per day
tot_step <- total.steps(df)

# Create Historgram
hist(tot_step$Total, 
     breaks = 20, 
     xlab =  "Total Steps", 
     main = "Histogram - Total steps per day")

# Summary
summary(tot_step$Total)

#######
# MEAN STEPS PER INTERVAL
# Calculate mean steps per interval
mean_step <- mean.steps(df)

# Plot time graph of mean steps
plot(mean_step$interval, 
     mean_step$Average, 
     xlab = "Interval", 
     ylab = "Average Steps", 
     main = "Average steps over time",
     type = "l")

Maximum <- mean_step[1,]


#######
# NA's
# Calculate number of NA rows in df
No_NA <- nrow(df) - sum(complete.cases(df))
NA_df <- NA.steps(df)
Total.NA.steps <- total.steps(NA_df)
hist(Total.NA.steps$Total, 
     breaks = 20, 
     xlab =  "Total Steps", 
     main = "Histogram - Total steps per day")
summary(Total.NA.steps)

#######
# WEEKDAY VS WEEKEND
# Split data in to Weekdays and Weekends
Day.steps <- day.steps(NA_df)

# Plot time graph of mean steps
 g <- ggplot(Day.steps, aes(x = interval, y = Average))
 g + geom_line() +
         facet_grid(Weekend ~.) +
         ggtitle("Averge steps per interval")
```

