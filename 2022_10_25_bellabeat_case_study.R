#Business Obective: Analyze smart device usage data to gain insight into how consumers use non-Bellabeat smart devices. Apply insights to Time smartwatch in presentation. 

#Turn all of the Kaggle Fitbit daily csvs into variables

library(tidyverse)
library(dplyr)
library(skimr)
library(ggplot2)
library(janitor)
library(lubridate)
library(reshape2)
library(scales)

activity <- read.csv("dailyActivity_merged.csv")

intensities <- read.csv("dailyIntensities_merged.csv")

calories <- read.csv("dailyCalories_merged.csv")

sleep <- read.csv("sleepDay_merged.csv")

weight <- read.csv("weightLoginfo_merged.csv")

steps <- read.csv("dailySteps_merged.csv")

#Validate >30 unique Ids
activity_unique_id <-
  activity %>%
  unique() %>%
  distinct(Id) %>%
  print()

intensities_unique_id <-
  intensities %>%
  unique() %>%
  distinct(Id) %>%
  print()

calories_unique_id <-
  calories %>%
  unique() %>%
  distinct(Id) %>%
  print()

sleep_distinct_id <-
  sleep %>%
  unique %>%
  distinct(Id) %>%
  print()

weight_distinct_id <-
  weight %>%
  unique %>%
  distinct(Id) %>%
  print()

steps_distinct_id <- 
  steps %>%
  unique %>%
  distinct(Id) %>%
  print()

##Unique ids: activity = 33, intensities = 33, calories = 33, sleep = 24, weight = 8, steps = 33
##Excluding any datasets with n<30, because 30 is the sample size which begins to represent the population.

#Cleaning usable activity, intensities, calories, and steps

activity_cleaned <-
  activity %>%
  unique() %>%
  clean_names() %>%
  mutate(activity_cleaned, date_cleaned = mdy(activity_cleaned$activity_date)) %>%
  view()

intensities_cleaned <- unique(intensities)

calories_cleaned <- unique(calories)

steps_cleaned <- unique(steps)

#Verifying no null values

sum(is.na(activity_cleaned))

sum(is.na(intensities_cleaned))

sum(is.na(calories_cleaned))

sum(is.na(steps_cleaned))


#dailyActivity_merged.csv contains collated data from intensities, calories, and steps, so I will use activity_cleaned for any new analyses
#Issues include units not being clearly defined, low sample size (n = 33)

#Summary of dailyActivity_merged dataset
##Notable that there are data points which have the min_distance = 0, min_calories = 0, and/or min_steps = 0.
#This is improbable that there are zero calories burned or distance travelled in a day accounting for regular human activity and calories burned during brain function.
#Going to run preliminary graphs and correlation coefficients to compare with a round of filtration.

activity_summarized <- summarise(activity_cleaned, avg_distance = mean(total_distance), max_distance = max(total_distance), min_distance = min(total_distance),
                                 avg_calories = mean(calories), max_calories = max(calories), min_calories = min(calories), avg_steps = mean(total_steps),
                                 max_steps = max(total_steps), min_steps = min(total_steps))
view(activity_summarized)

#Graphing Total Distance vs Intensities
##Calculating Correlation Coefficients
#Total distance vs very active minute correlation coefficient = 0.681
#Total distance vs fairly active minute correlation coefficient = 0.463
#Total distance vs lightly active minute correlation coefficient = 0.0.516
#Total distance vs sedentary minute correlation coefficient = -0.288

ggplot(activity_cleaned,aes(x = total_distance,y = very_active_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Very Active versus Total Distance Travelled") +
  ylab("Minutes")

cor(activity_cleaned$total_distance, activity_cleaned$very_active_minutes)

#Repeating for Very Active, Fairly Active, Lightly Active, and Sedentary Minutes against Total Distance

ggplot(activity_cleaned,aes(x = total_distance,y = fairly_active_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Fairly Active versus Total Distance Travelled") +
  ylab("Minutes")

cor(activity_cleaned$total_distance, activity_cleaned$fairly_active_minutes)

ggplot(activity_cleaned,aes(x = total_distance,y = lightly_active_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Lightly Active versus Total Distance Travelled") +
  ylab("Minutes")

cor(activity_cleaned$total_distance, activity_cleaned$lightly_active_minutes)

ggplot(activity_cleaned,aes(x = total_distance,y = sedentary_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Sedentary versus Total Distance Travelled") +
  ylab("Minutes")

cor(activity_cleaned$total_distance, activity_cleaned$sedentary_minutes)

#Analyzing based on Calories

ggplot(activity_cleaned,aes(x = total_distance,y = calories)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Total Distance Travelled versus Calories Burned")

cor(activity_cleaned$total_distance, activity_cleaned$calories)

##Based on the total_minutes, not all of the minutes in a day (n = 1440) are being counted, which may affect the results.
#Filtering data based on entries that account for all the time in a day.

###Another objective to examine is to see which days users are the most active proportionally.
##Necessary components are total time spent in a day, time spent in each category
#Turning dates into categorical days of the week and grouping by weekday

activity_long <- mutate(activity_cleaned, total_minutes = rowSums(activity_cleaned[11:14])) %>%
  mutate(activity_cleaned, active_minutes = rowSums(activity_cleaned[11:13])) %>%
  mutate(activity_cleaned, proportion_active = (active_minutes/total_minutes)) %>%
  mutate(activity_cleaned, weekday = wday(activity_cleaned$activity_date, label = TRUE))

filtered_activity <- filter(activity_long, total_minutes == 1440 & sedentary_minutes <= 1439 & total_distance >= 1)
view(filtered_activity)

###Run same graphs with filtered dataset

##Repeat for total distance vs. different intensities
#Total distance vs very active minute correlation coefficient = 0.731
#Total distance vs fairly active minute correlation coefficient = 0.308
#Total distance vs lightly active minute correlation coefficient = 0.255
#Total distance vs sedentary minute correlation coefficient = -0.556
#Total distance vs calories correlation coefficient = 0.746

ggplot(filtered_activity,aes(x = total_distance,y = very_active_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Very Active versus Total Distance Travelled") +
  ylab("Minutes")

cor(filtered_activity$total_distance, filtered_activity$very_active_minutes)

ggplot(filtered_activity,aes(x = total_distance,y = fairly_active_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Fairly Active versus Total Distance Travelled") +
  ylab("Minutes")

cor(filtered_activity$total_distance, filtered_activity$fairly_active_minutes)

ggplot(filtered_activity,aes(x = total_distance,y = lightly_active_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Lightly Active versus Total Distance Travelled") +
  ylab("Minutes")

cor(filtered_activity$total_distance, filtered_activity$lightly_active_minutes)

ggplot(filtered_activity,aes(x = total_distance,y = sedentary_minutes)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Spent Sedentary versus Total Distance Travelled") +
  ylab("Minutes")

cor(filtered_activity$total_distance, filtered_activity$sedentary_minutes)

ggplot(filtered_activity,aes(x = total_distance,y = calories)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Total Distance Travelled versus Calories Burned")

cor(filtered_activity$total_distance, filtered_activity$calories)

###Total distance to intensities seems to have inconsistent correlation coefficients, so I will focus on the correlation of total steps / distance to calories burnt because the confidence
###interval after filtering the data seems to have improved to a very strong correlation.

##Summarizing activity by weekday

activity_day_summary <- activity_long %>% 
  group_by(weekday) %>% 
  summarize(avg_minutes_active = mean(active_minutes), very_active = mean(very_active_minutes), fairly_active = mean(fairly_active_minutes), lightly_active = mean(lightly_active_minutes)) %>%
  view()

##uSING reshape2 to transform data from long to wide for graph

weekday_activity_long <- melt(activity_day_summary, id.vars = c("weekday","avg_minutes_active"))
colnames(weekday_activity_long)[3] = "intensity"
colnames(weekday_activity_long)[4] = "minutes_intensity"
view(weekday_activity_long)

percentage_activity <- mutate(weekday_activity_long, intensity_percentage = percent(minutes_intensity / avg_minutes_active, accuracy = .01)) %>%
  dplyr::group_by(weekday)
view(percentage_activity)

##Plotting activity intensity breakdown based on day and percentage.

ggplot(percentage_activity, aes(fill=intensity, y=minutes_intensity, x=weekday)) + 
  geom_bar(position="stack", stat = "identity") +
  geom_text(aes(label = intensity_percentage), size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Average Activity Based by Day and Intensity", subtitle = "Intensity proportions based on the average total activity divided by average intensity durations") +
  xlab("Day") +
  ylab("Average Activity (Minutes)")

##Interesting to see that after lightly active being the most common activity intensity, the proportion of very active intensities seems to be higher than fairly active activities.
#Summarizing overall intensities by total minutes of activity/total number of observations

intensity_summary <- summarise(filtered_activity, avg_very_active_minutes = mean(filtered_activity$very_active_minutes), 
                               avg_fairly_active_minutes = mean(filtered_activity$fairly_active_minutes), 
                               avg_lightly_active_minutes = mean(filtered_activity$lightly_active_minutes),
                               avg_sedentary_minutes = mean(filtered_activity$sedentary_minutes)) %>%
  mutate(intensity_summary, total_minutes = rowSums(intensity_summary[1:4]))

