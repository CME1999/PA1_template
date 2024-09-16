---
title: "Personal Activity Monitoring Data Analysis"
author: "Claudia Morilla"
date: "2024-09-16"
output: html_document
---
# Loading and Preprocessing the Data

## Load necessary packages


``` r
library(ggplot2)
library(dplyr)
```

## Load the dataset

``` r
activity_data <- read.csv("C:/Users/klaum/OneDrive/Desktop/activity.csv")
```

## Inspect the dataset structure

``` r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Check the summary of the data

``` r
summary(activity_data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
# Mean Total Number of Steps Per Day

## Calculate total steps per day (ignoring NAs)

``` r
total_steps_per_day <- activity_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))
```
## Plot histogram of total steps per day

``` r
ggplot(total_steps_per_day, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Total Steps Per Day", x = "Total Steps", y = "Frequency")
```


<img src="Peer-Review-assingment-1_files/figure-html/unnamed-chunk-48-1.png" width="672" />
## Calculate mean and median of total steps per day

``` r
mean_steps <- mean(total_steps_per_day$total_steps)
median_steps <- median(total_steps_per_day$total_steps)
```
## Print mean and median

``` r
mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```
# Average Daily Activity Pattern

## Average number of steps per 5-minute interval across all days

``` r
avg_steps_per_interval <- activity_data %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))
```
## Time series plot of average steps per 5-minute interval

``` r
ggplot(avg_steps_per_interval, aes(x = interval, y = avg_steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern", x = "5-minute Interval", y = "Average Steps")
```


<img src="Peer-Review-assingment-1_files/figure-html/unnamed-chunk-52-1.png" width="672" />
## Identify the 5-minute interval with the maximum average steps

``` r
max_interval <- avg_steps_per_interval[which.max(avg_steps_per_interval$avg_steps), ]
max_interval
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```
# Imputing Missing Data

## Total number of missing values

``` r
total_na <- sum(is.na(activity_data$steps))
```
## Impute missing values using the mean for each 5-minute interval

``` r
imputed_data <- activity_data %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```
## Calculate total steps per day with imputed data

``` r
total_steps_imputed <- imputed_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
```
## Plot histogram of total steps per day after imputing missing data

``` r
ggplot(total_steps_imputed, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "black") +
  labs(title = "Total Steps Per Day (Imputed Data)", x = "Total Steps", y = "Frequency")
```


<img src="Peer-Review-assingment-1_files/figure-html/unnamed-chunk-57-1.png" width="672" />
## Calculate mean and median after imputation

``` r
mean_steps_imputed <- mean(total_steps_imputed$total_steps)
median_steps_imputed <- median(total_steps_imputed$total_steps)
```
## Print mean and median after imputation

``` r
mean_steps_imputed
```

```
## [1] 10766.19
```

``` r
median_steps_imputed
```

```
## [1] 10766.19
```
# Weekday vs. Weekend Activity Patterns

## Create a factor variable for weekday vs weekend

``` r
imputed_data$date <- as.Date(imputed_data$date)
imputed_data$day_type <- ifelse(weekdays(imputed_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
```
## Average number of steps for each 5-minute interval split by weekday and weekend

``` r
avg_steps_weekday_weekend <- imputed_data %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```
## Panel plot comparing weekday and weekend activity patterns

``` r
ggplot(avg_steps_weekday_weekend, aes(x = interval, y = avg_steps, color = day_type)) +
  geom_line() +
  facet_wrap(~ day_type, ncol = 1) +
  labs(title = "Activity Patterns: Weekday vs Weekend", x = "5-minute Interval", y = "Average Steps")
```


<img src="Peer-Review-assingment-1_files/figure-html/unnamed-chunk-62-1.png" width="672" />

# Conclusion
- The mean and median number of steps taken per day increased after imputing missing values, from approximately 9354 and 10395 to 10766 for both metrics. 
- The most active 5-minute interval was at interval 835, where an average of 206 steps were taken.
- Activity patterns differed between weekdays and weekends, with noticeable differences in the time of day when the most steps were taken.



