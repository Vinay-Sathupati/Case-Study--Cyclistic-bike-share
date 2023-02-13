---
title: "Case Study- Cyclistic bike-share"
output: html_document
date: "`r Sys.Date()`"
---

## Introduction

This case study is my Google Data Analytics Certificate course final project. In this project I will be analyzing public dataset provided by course using R programming language.

## Scenario

You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

Lily Moreno (the director of marketing and my manager) has assigned you the first question to answer: How do annual members and casual riders use Cyclistic bikes differently?

## Step 01 - ASK

Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ. We will analyze the Cyclistic historical bike trip data to identify trends. Find the differences between the casual riders and annual members.

## Step 02 - PREPARE

I will be using Cyclistic’s historical trip data. The data has been made available by Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement). Datasets are available [here](https://divvy-tripdata.s3.amazonaws.com/index.html). I will be using data for the last 12 months - Jan 2022 to Dec 2022.

### Install required packages

```{r load packages}
library(tidyverse) #helps wrangle data
library(ggplot2)   #helps visualize data
library(janitor)   #helps examining and cleaning data 
library(lubridate) #helps wrangle date attributes
library(plyr)      #helps data cleaning and transformation
```

### Collect Data

Import data -12 csv files, each representing 1 of the 12 months of trip data.
```{r Import Data}
y22_jan <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202201-divvy-tripdata.csv")
y22_feb <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202202-divvy-tripdata.csv")
y22_mar <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202203-divvy-tripdata.csv")
y22_apr <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202204-divvy-tripdata.csv")
y22_may <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202205-divvy-tripdata.csv")
y22_jun <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202206-divvy-tripdata.csv")
y22_jul <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202207-divvy-tripdata.csv")
y22_aug <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202208-divvy-tripdata.csv")
y22_sep <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202209-divvy-tripdata.csv")
y22_oct <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202210-divvy-tripdata.csv")
y22_nov <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202211-divvy-tripdata.csv")
y22_dec <- read_csv("C:\\Users\\sathu\\OneDrive\\Desktop\\Case Study\\Data\\202212-divvy-tripdata.csv")
```

### Inspecting Data

```{r Inspect Data}
str(y22_jan)
str(y22_feb)
str(y22_mar)
str(y22_apr)
str(y22_may)
str(y22_jun)
str(y22_jul)
str(y22_aug)
str(y22_sep)
str(y22_oct)
str(y22_nov)
str(y22_dec)
```
### Merge Data

```{r}
y22_merged <- bind_rows(y22_jan, y22_feb, y22_mar, y22_apr, y22_may, y22_jun, y22_jul, y22_aug, y22_sep, y22_oct, y22_nov, y22_dec)
```


Inspect the new table that has been created

```{r}
colnames(y22_merged) 
nrow(y22_merged)  
dim(y22_merged)  
head(y22_merged)
tail(y22_merged)
str(y22_merged)
summary(y22_merged)
```

## Step 03 - PROCESS

Documentation of any cleaning or manipulation of data. Transforming the data so you can work with it effectively.


Add columns that list day of the week and month.This will allow us to aggregate ride data for each day and each month. We will add "day_of_week" and "month". More on date formats are found [here](https://www.statmethods.net/input/dates.html).

```{r}
y22_merged$month <- format(as.Date(y22_merged$started_at), "%b")
y22_merged$day_of_week <- format(as.Date(y22_merged$started_at), "%A")
```

We will want to add a calculated field for length of ride since the data did not have the "trip duration" column. We will add "ride_length"(in seconds) to the entire dataframe for consistency.

```{r}
y22_merged$ride_length <- difftime(y22_merged$ended_at, 
                                   y22_merged$started_at)
```

View and inspect the structure of columns

```{r}
str(y22_merged)
```
Convert "ride_length" from Factor to numeric so we can run calculations on the data.

```{r}
y22_merged$ride_length <- as.numeric(as.character(y22_merged$ride_length))
is.numeric(y22_merged$ride_length)
```
### Remove "Bad" Data

The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was zero or negative.

We will create a new dataframe since data is being removed. [Delete or Drop rows conditions](https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/).

```{r}
y22_merged_clean<- y22_merged[!(y22_merged$ride_length <= 0),]
```

## Step 04 - ANALYZE

Perform calculations.Identify trends and relationships.

Conducting descriptive analysis on "ride_length".

```{r}
summary(y22_merged_clean$ride_length)
```

Compare members and casual users.

```{r}
aggregate(y22_merged_clean$ride_length ~ y22_merged_clean$member_casual, FUN = mean)
aggregate(y22_merged_clean$ride_length ~ y22_merged_clean$member_casual, FUN = median)
aggregate(y22_merged_clean$ride_length ~ y22_merged_clean$member_casual, FUN = max)
aggregate(y22_merged_clean$ride_length ~ y22_merged_clean$member_casual, FUN = min)
```

See the average ride time by each day for members vs casual users.

```{r}
y22_merged_clean$day_of_week <- ordered(y22_merged_clean$day_of_week, 
                                        levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  aggregate(y22_merged_clean$ride_length ~ y22_merged_clean$member_casual +
            y22_merged_clean$day_of_week, FUN = mean )
```

analyze ridership data by type and weekday

```{r}
y22_merged_clean %>% 
  mutate(weekday = wday(started_at, label= TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  dplyr::summarise(number_of_rides= n(), .groups="drop",
                   average_duration= mean(ride_length)) %>% 
  arrange(member_casual,weekday)
```


### Visualizing data

Let's visualize members and casuals by the total ride taken

```{r}
ggplot(data = y22_merged_clean)+
  geom_bar(mapping= aes(x= member_casual, fill= member_casual)) +
  labs(title = "Members Vs Casual")
```

Let's Visualize the number of rides by rider type

```{r}
y22_merged_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  dplyr::summarise(number_of_rides = n(), .groups="drop",
                   average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Total rides in a day")
```

Let's create a visualization for average duration

```{r}
y22_merged_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  dplyr::summarise(number_of_rides = n(), .groups="drop",
                   average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average ride time in a day")
```


Let's visualize the total rides taken by members and casuals by month




```{r}
  y22_merged_clean$month <- ordered(y22_merged_clean$month, 
                                        levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
y22_merged_clean %>% 
  group_by(member_casual, month) %>% 
  dplyr::summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x= month, y= number_of_rides, fill= member_casual))+
  geom_col(position = "dodge") +
  labs(title = "Number of rides in a month")
```


## Step 05 - SHARE

Present your findings. Ensure your work is accessible.
You can view my work [here](https://github.com/Vinay-Sathupati/Case-Study--Cyclistic-bike-share.git).


### Findings:
* membership people are more compared with casual members.
* members renting bikes show consistency throughout the week. Whereas, casual are lowest during week and higher than members during weekend.
* This data also shows a seasonal pattern as Chicago's climate is typically continental with cold winters, warm summers. Lowest usage of bikes is in winters and Highest during summer.

## Step 06 - ACT

#### My recommendations:
* Provide weekend and/or seasonal memberships.
* provide discounts during summer, since bike usage is peak during this season.
* Host campaigns, events to attract more members to get membership.
