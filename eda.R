library(tidymodels)
library(ISLR)
library(ISLR2)
library(discrim)
library(poissonreg)
library(corrr)
library(klaR)
library(dplyr)
library(ggplot2)
library(glmnet)
library(janitor)
setwd("/Users/shobhanashreedhar/Desktop")
weather <- read.csv("weatherdatanew.csv") %>% clean_names()
# We clean the variable names to make the code easier to write

weather %>% 
  dplyr::select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot(type = 'lower', diag = FALSE, 
           method = 'color')
# Correlation matrix of all numeric variables in order to ascertain which correlations stand out

weather$date <- as.Date(weather$date, format="%m/%d/%y")
# Cleaning date values
weather <- weather %>% mutate(year_month=format(date, "%y/%m"), .before=date)
# Adding year_month variable in order to create monthly averages for weather values
humidity_avg <- aggregate(average_humidity ~ year_month, weather, mean)
# Aggregate means of monthly humidity in a new data frame
humidity_avg
# View data frame to ensure everything is correct
h <- ggplot(data=humidity_avg, aes(x=year_month, y=average_humidity, group=1)) + 
  geom_line() + labs(y="Average Humidity", x="Month")  
# Store plot for monthly average humidity
h + theme(axis.text.x = element_text(angle = 90))
# Show plot while rotating x-axis values 90 degrees to prevent crowding
# We are viewing distribution of humidity because we want to use this as our response
# Humidity is our response because it has the most natural variation in Santa Barbara
# and has implications for climate change, epidemiology, electronics, and other major
# aspects of modern life.

temp_avg <- aggregate(average_temperature_o_f ~ year_month, weather, mean)
# Aggregate means of monthly humidity in a new data frame
temp_avg
# View data frame to ensure everything is correct
dew_avg <- aggregate(average_dew_point_o_f ~ year_month, weather, mean)

total1 <- merge(humidity_avg, temp_avg, by="year_month")
total <- merge(total1, dew_avg, by="year_month")
total
ggplot(data=total, aes(x=year_month)) + 
  geom_line(aes(y = average_temperature_o_f, group=1, color="Temperature")) + 
  geom_line(aes(y = average_dew_point_o_f, group=1, color="Dew Point")) + 
  theme(axis.text.x = element_text(angle = 90)) + labs(y="Degrees Fahrenheit", x="Month")

precip_month <- aggregate(precipitation_in ~ year_month, weather, sum)

ggplot(data=precip_month, aes(x=year_month)) + geom_line(aes(y=precipitation_in, group=1)) + 
  theme(axis.text.x = element_text(angle = 90)) + labs(y="Total Precipitation", x="Month")

ggplot(data=weather, aes(x=date)) + geom_point(aes(y=average_pressure_in_hg, group=1)) + 
  theme(axis.text.x = element_text(angle = 90)) + labs(y="Average Pressure (inHg)", x="Date")


ggplot(data=weather, aes(x=date)) + geom_line(aes(y=average_wind_speed_mph, group=1, color="Average Wind Speed")) +
  geom_line(aes(y=maximum_wind_speed_mph, group=1, color="Maximum Wind Speed")) +
  theme(axis.text.x = element_text(angle=90)) + labs(y="Miles per Hour", x="Date")