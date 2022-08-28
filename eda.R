# Exploratory Data Analysis<br /><br />


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
library(parsnip)
library(janitor)
library(corrplot)
library(randomForest)
library(rpart)
library(rpart.plot)


### Reading in Data

weather <- read.csv("weatherdatanew.csv") %>% clean_names()
# We clean the variable names to make the code easier to write
weather$date <- as.Date(weather$date, format="%m/%d/%y")
# Cleaning date values
weather <- weather %>% mutate(year_month=format(date, "%y/%m"), .before=date)
# Adding year_month variable in order to create monthly averages for weather values

### Correlation Matrix<br /><br />

weather %>% 
  dplyr::select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot(type = 'lower', diag = FALSE, 
           method = 'color')
# Correlation matrix of all numeric variables in order to ascertain which correlations stand out

### Exploring Individual Variables<br /><br />
  
### Average Monthly Humidity<br /><br />

humidity_avg <- aggregate(average_humidity ~ year_month, weather, mean)
# Aggregate means of monthly humidity in a new data frame
h <- ggplot(data=humidity_avg, aes(x=year_month, y=average_humidity, group=1)) + 
  geom_line() + labs(y="Average Humidity", x="Year/Month")  
# Store plot for monthly average humidity
h + theme(axis.text.x = element_text(angle = 90))
# Show plot while rotating x-axis values 90 degrees to prevent crowding

### Average Monthly Temperature/Dew Point<br /><br />
  
temp_avg <- aggregate(average_temperature_o_f ~ year_month, weather, mean)
# Aggregate means of monthly temp in a new data frame
dew_avg <- aggregate(average_dew_point_o_f ~ year_month, weather, mean)
# Aggregate means of monthly dew point in a new data frame

total1 <- merge(humidity_avg, temp_avg, by="year_month")
total <- merge(total1, dew_avg, by="year_month")
ggplot(data=total, aes(x=year_month)) + 
  geom_line(aes(y = average_temperature_o_f, group=1, color="Temperature")) + 
  geom_line(aes(y = average_dew_point_o_f, group=1, color="Dew Point")) + 
  theme(axis.text.x = element_text(angle = 90)) + labs(y="Degrees Fahrenheit", x="Year/Month")


### Total Monthly Precipitation<br /><br />
  
precip_month <- aggregate(precipitation_in ~ year_month, weather, sum)

ggplot(data=precip_month, aes(x=year_month)) + geom_line(aes(y=precipitation_in, group=1)) + 
  theme(axis.text.x = element_text(angle = 90)) + labs(y="Total Precipitation", x="Year/Month")

### Daily Average Pressure<br /><br />
  
ggplot(data=weather, aes(x=date)) + geom_point(aes(y=average_pressure_in_hg, group=1)) + 
  theme(axis.text.x = element_text(angle = 90)) + labs(y="Average Pressure (inHg)", x="Date")

### Daily Average and Maximum Wind Speed<br /><br />
  
ggplot(data=weather, aes(x=date)) + geom_line(aes(y=average_wind_speed_mph, group=1, color="Average Wind Speed")) +
  geom_line(aes(y=maximum_wind_speed_mph, group=1, color="Maximum Wind Speed")) +
  theme(axis.text.x = element_text(angle=90)) + labs(y="Miles per Hour", x="Date")