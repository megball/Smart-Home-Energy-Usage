###C3T1 Deep Analytics and Visualization
##Megan Ball, 4/3/19


###############
# Project notes
###############

# Summarize project
#Time-series forecasting and visualization script


###############
# Housekeeping
###############

# Clear all variables from R
rm(list = ls())

# Set working directory
getwd()
setwd("C:/Users/Megan/Documents/Data Analytics/C3T1")
dir()

################
# Load packages
################

install.packages("RMySQL")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("plotly")
install.packages("mime")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")

library(RMySQL)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(mime)
library(ggplot2)
library(ggfortify)
library(forecast)

###############
# Import data
##############

## Create a database connection
con = dbConnect(MySQL(), user='username', password='password', dbname='database', host='hostname')


## List the tables contained in the database
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'yr_2006')
dbListFields(con,'yr_2007')
dbListFields(con,'yr_2008')
dbListFields(con,'yr_2009')
dbListFields(con,'yr_2010')

## pull data for each year for just Date, Time, and sub-metering
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

#sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

#review data
str(yr_2006)
summary(yr_2006)
head(yr_2006) #starts 12/16/06
tail(yr_2006) #ends 12/31/06

str(yr_2007)
head(yr_2007)
tail(yr_2007)

head(yr_2008)
tail(yr_2008)

head(yr_2009)
tail(yr_2009)

head(yr_2010)
tail(yr_2010) #ends 11/26/10

################
# Evaluate & Format data
################

## Combine tables into one dataframe using dplyr
energy <- bind_rows(yr_2007, yr_2008, yr_2009, yr_2010)
head(energy)
tail(energy)


## Combine Date and Time attribute values in a new attribute column
energy <-cbind(energy,paste(energy$Date,energy$Time), stringsAsFactors=FALSE)
View(energy)
colnames(energy)[6] <-"DateTime"
View(energy)

## Move the DateTime attribute within the dataset
energy <- energy[,c(ncol(energy), 1:(ncol(energy)-1))]
head(energy)

## Convert DateTime from character to POSIXct 
energy$DateTime <- as.POSIXct(energy$DateTime,
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "Europe/Paris")

## Inspect the data types
str(energy)

## Create "year" attribute with lubridate

energy$year <- year(energy$DateTime)
energy$month <- month(energy$DateTime)
energy$weekday <- weekdays(energy$DateTime)
energy$hour <- hour(energy$DateTime)
energy$day <- day(energy$DateTime)
energy$week <- week(energy$DateTime)
energy$minute <- minute(energy$DateTime)
energy$quarter <- quarter(energy$DateTime, with_year = TRUE, fiscal_start = 1)
str(energy)


#check for missing values
any(is.na(energy))
# Use summary to identify where NAs are 
summary(energy)
#240 NA's in dataset


#statistics of data
# Sub_metering_1   Sub_metering_2   Sub_metering_3
#Min.   : 0.000   Min.   : 0.000   Min.   : 0.000
#1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000
#Median : 0.000   Median : 0.000   Median : 1.000
#Mean   : 1.121   Mean   : 1.289   Mean   : 6.448
#3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000
#Max.   :88.000   Max.   :80.000   Max.   :31.000


#calculate standard deviation
sd(energy$Sub_metering_1) #[1] 6.14737 (mean + 3sd = ~7)
sd(energy$Sub_metering_2) #[1] 5.786206 (mean + 3sd = ~7)
sd(energy$Sub_metering_3) #[1] 8.434118 (mean + 3sd = ~15)
#conclusions from means & sd & max - data has at least some outliers

## -------- Save pre-processed dataset --------##

# Save file (export to other programs), or
# Save object (for R only)
write.csv(energy, file = "energy.csv")

################
# Visualize data
################

#visualize distribution and number of outliers
hist(energy$Sub_metering_1)
hist(energy$Sub_metering_2)
hist(energy$Sub_metering_3)
#distributions are all heavily skewed to the left

## Plot all of sub-meter 1
plot(energy$Sub_metering_1)

#############
## Subsets
#############

## Subset the second week of 2008 - All Observations
houseWeek <- filter(energy, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(energy, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Energy Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

########
#  Day subset
########

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- energy %>%
  filter(year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50)) %>%
  mutate(SM1 = Sub_metering_1 / 1000, # Total kWh per day
         SM2 = Sub_metering_2 / 1000,  
         SM3 = Sub_metering_3 / 1000)
head(houseDay10)


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Energy Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))


houseDay8 <- energy %>%
  filter(year == 2008 & month == 1 & day == 8 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50)) %>%
  mutate(SM1 = Sub_metering_1 / 1000, # Total kWh per day
         SM2 = Sub_metering_2 / 1000,  
         SM3 = Sub_metering_3 / 1000)
head(houseDay8)

plot_ly(houseDay8, x = ~houseDay8$DateTime, y = ~houseDay8$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay8$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay8$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Energy Consumption January 8th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))

########
#  Week subset 
########

## Subset the week of January 6th-12th 2008 - 1 day frequency
houseWeekday <- energy %>%
  filter(year == 2008 & month == 1 & (day == 6 | day == 7 | day == 8 | day == 9 | day == 10 | day == 11 | day == 12)) %>%
  mutate(SM1 = Sub_metering_1 / 1000, # Total kWh per day
         SM2 = Sub_metering_2 / 1000,  
         SM3 = Sub_metering_3 / 1000)
head(houseWeekday)

plot_ly(houseWeekday, x = ~houseWeekday$DateTime, y = ~houseWeekday$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeekday$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeekday$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 6th - 12th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))

#aggregate use by day for week of 1/6 - 1/12
houseWd <- energy %>%
  filter(Date >= "2008-01-06" & Date <= "2008-01-12") %>%
  group_by(year, month, day, Date) %>%  # Group data 
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per day
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(day)) # Remove the any rows that have NA 

plot_ly(houseWd, x = ~houseWd$Date, y = ~houseWd$SM1, name = 'Kitchen', type = 'bar') %>%
  add_trace(y = ~houseWd$SM2, name = 'Laundry Room') %>%
  add_trace(y = ~houseWd$SM3, name = 'Water Heater & AC') %>%
  layout(title = "Energy Consumption (kWh) January 6th - January 12th 2008",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Power (kWh)"))  

########
#  Monthly subset
########

###create annual monthly use subset for later use
monthly.sum <- energy %>%
  filter(year==2007 | year==2008 | year==2009 | year == 2010) %>%
  group_by(month, year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance



##create annual monthly avg subset for later use
monthly.avg <- monthly.sum %>%
  filter(year==2007 | year==2008 | year==2009) %>%
  group_by(month) %>%  # Group data by Year
  summarize(SM1 = mean(SM1, na.rm = TRUE), # Total kWh per hour
            SM2 = mean(SM2, na.rm = TRUE), 
            SM3 = mean(SM3, na.rm = TRUE),
            DateTime = first(DateTime))   # To verify date of first instance

plot_ly(monthly.avg, x = ~monthly.avg$month, y = ~monthly.avg$SM1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~monthly.avg$SM2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~monthly.avg$SM3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Monthly Power Consumption (avg) 2007 - 2009",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (kWh)"))

########
#  Annual subset for January only from 2007 - 2009
########

January <- energy %>%
  filter((year==2007 | year==2008 | year==2009) & month == 1) %>%
  group_by(year) %>%  # Group data by Year
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

plot_ly(January, x = ~January$year, y = ~January$SM1, name = 'Kitchen', type = 'bar') %>%
  add_trace(y = ~January$SM2, name = 'Laundry Room') %>%
  add_trace(y = ~January$SM3, name = 'Water Heater & AC') %>%
  layout(title = "Energy consumption (kWh) for January 2007 - 2009",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (kWh)"))  

########
#  Quarterly subset
########

quarterly.sum <- energy %>%
  filter(year==2007 | year==2008 | year==2009 | year == 2010) %>%
  group_by(quarter, year) %>%  # Group data by quarter
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance
#exclude 4th quarter of 2010 since it's not complete
quarterly.sum1 <- quarterly.sum[ !(quarterly.sum$quarter %in% c(2010.4)), ]

plot_ly(quarterly.sum, x = ~quarterly.sum$quarter, y = ~quarterly.sum$SM1, name = 'Kitchen', type = 'bar') %>%
  add_trace(y = ~quarterly.sum$SM2, name = 'Laundry Room') %>%
  add_trace(y = ~quarterly.sum$SM3, name = 'Water Heater & AC') %>%
  layout(title = "Energy consumption (kWh) for Each Quarter, 2007 - 2010",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Power (kWh)"))  

########
#  Daily subset to visualize whole trend
########

daily.sum <- energy %>%
  filter(year==2007 | year==2008 | year==2009 | year == 2010) %>%
  group_by(day, month, year) %>%  # Group data by day
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE), 3), # Total kW per hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE), 3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE), 3),
            DateTime = first(DateTime))   # To verify date of first instance


######
# Pie charts
######

#Percentage of total power use over a month by each sub-meter.

Januarypct <- energy %>%
  filter( year==2008  & month == 1) %>%
  group_by(month) %>%  # Group data by month
  summarize(Kitchen = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            Laundry_Room = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            Water_Heater_AC = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

Januarypct2 <- gather(Januarypct, key ="Submeter", value = "Energy", "Kitchen", "Laundry_Room", "Water_Heater_AC", na.rm = TRUE)

colors <- c('#1f77b4', '#ff7f0e', '#2ca02c')

plot_ly(Januarypct2, labels = ~Submeter, values = ~Energy, type = 'pie', sort = FALSE, marker = list(colors = colors)) %>%
  layout(title = 'January 2008 energy by sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


Julypct <- energy %>%
  filter( year==2009  & month == 7) %>%
  group_by(month) %>%  # Group data by month
  summarize(Kitchen = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            Laundry_Room = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            Water_Heater_AC = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

Julypct <- gather(Julypct, key ="Submeter", value = "Energy", "Kitchen", "Laundry_Room", "Water_Heater_AC", na.rm = TRUE)

plot_ly(Julypct, labels = ~Submeter, values = ~Energy, type = 'pie', sort = FALSE, marker = list(colors = colors)) %>%
  layout(title = 'July 2008 energy by sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


Januarypctall <- energy %>%
  filter( month == 1) %>%
  group_by(month) %>%  # Group data by month
  summarize(Kitchen = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            Laundry_Room = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            Water_Heater_AC = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

Januarypctall.avg <- Januarypctall %>%
  summarize(Kitchen = round((Kitchen/4),3), # Total kWh per hour
            Laundry_Room = round((Laundry_Room/4),3), 
            Water_Heater_AC = round((Water_Heater_AC/4),3),
            DateTime = first(DateTime))   # To verify date of first instance

Januarypctall.avg <- gather(Januarypctall.avg, key ="Submeter", value = "Energy", "Kitchen", "Laundry_Room", "Water_Heater_AC", na.rm = TRUE)

plot_ly(Januarypctall.avg, labels = ~Submeter, values = ~Energy, type = 'pie', sort = FALSE, marker = list(colors = colors), textinfo = 'value+percent') %>%
  layout(title = 'January Average Energy (kWh) by Sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

Julypctall <- energy %>%
  filter( month == 7) %>%
  group_by(month) %>%  # Group data by month
  summarize(Kitchen = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total kWh per hour
            Laundry_Room = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            Water_Heater_AC = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime))   # To verify date of first instance

Julypctall.avg <- Julypctall %>%
  summarize(Kitchen = round((Kitchen/4),3), # Total kWh per hour
            Laundry_Room = round((Laundry_Room/4),3), 
            Water_Heater_AC = round((Water_Heater_AC/4),3),
            DateTime = first(DateTime))   # To verify date of first instance

Julypctall.avg <- gather(Julypctall.avg, key ="Submeter", value = "Energy", "Kitchen", "Laundry_Room", "Water_Heater_AC", na.rm = TRUE)

plot_ly(Julypctall.avg, labels = ~Submeter, values = ~Energy, type = 'pie', sort = FALSE, marker = list(colors = colors), textinfo = 'value+percent') %>%
  layout(title = 'July Energy (kWh) by Sub-meter',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#######
#Save data as time series 
#######

#save daily aggregate data as ts for any trends, seasonality, etc
tsSM1_dailysum <- ts(daily.sum$SM1, frequency = 365.25, start =c(2007,1))
tsSM1_dailysum
autoplot(tsSM1_dailysum)

tsSM2_dailysum <- ts(daily.sum$SM2, frequency = 365.25, start =c(2007,1))
tsSM2_dailysum
autoplot(tsSM2_dailysum)

tsSM3_dailysum <- ts(daily.sum$SM3, frequency = 365.25, start =c(2007,1))
tsSM3_dailysum
autoplot(tsSM3_dailysum)

#save monthly aggregate data as ts
tsSM1_monthlysum <- ts(monthly.sum$SM1, frequency=12, start=c(2007,1))
autoplot(tsSM1_monthlysum)
#save subset of monthly sum to compare for ground truth with forecast
tsSM1_monthlysum2 <- ts(monthly.sum$SM1, frequency=12, start=c(2007,1), end = c(2010,6))
autoplot(tsSM1_monthlysum2)

tsSM2_monthlysum <- ts(monthly.sum$SM2, frequency=12, start=c(2007,1))
autoplot(tsSM2_monthlysum)
#save subset of monthly sum to compare for ground truth with forecast
tsSM2_monthlysum2 <- ts(monthly.sum$SM2, frequency=12, start=c(2007,1), end = c(2010,6))
autoplot(tsSM2_monthlysum2)

tsSM3_monthlysum <- ts(monthly.sum$SM3, frequency=12, start=c(2007,1))
autoplot(tsSM3_monthlysum)
#save subset of monthly sum to compare for ground truth with forecast
tsSM3_monthlysum2 <- ts(monthly.sum$SM3, frequency=12, start=c(2007,1), end = c(2010,6))
autoplot(tsSM3_monthlysum2)

#save quarterly aggregate data as ts
tsSM1_quarterlysum <- ts(quarterly.sum1$SM1, frequency = 4, start=c(2007,1))
tsSM1_quarterlysum

tsSM2_quarterlysum <- ts(quarterly.sum1$SM2, frequency = 4, start=c(2007,1))
tsSM2_quarterlysum

tsSM3_quarterlysum <- ts(quarterly.sum1$SM3, frequency = 4, start=c(2007,1))
tsSM3_quarterlysum

##SUBMETER 3

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- energy %>% 
  filter(weekday == "Monday" & hour == 20 & minute == 0 & year < 2010) %>%
  filter(!is.na(day)) # Remove the any rows that have NA 

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
tsSM3_070809weekly

autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'black', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3 at Mondays, 8pm")

max(energy$Sub_metering_1)

##SUBMETER 1

## Subset to one observation per minute for January 24th, 2010 (period of peak usage for SM1)
house012410 <- energy %>% 
  filter(month == 1 & day == 24  & year == 2010) %>%
  filter(!is.na(day)) # Remove the any rows that have NA 

house012410

tsSM1_house012410 <- ts(house012410$Sub_metering_1, frequency = 1440)

tsSM1_house012410

#plot time series for SM1 on 1/24/10
plot_ly(house012410, x = ~house012410$DateTime, y = ~house012410$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  layout(title = "Sub-meter 1 on January 24, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Watt-hour)"))


##subset for week including January 24th 2010 to one observation per hour
house012410week <- energy %>% 
  filter(month == 1 & week == 4 & year == 2010 ) %>%
  filter(!is.na(day)) # Remove the any rows that have NA 


tsSM1_house012410week <- ts(house012410week$Sub_metering_1, frequency = 1440)
tsSM1_house012410week
plot.ts(tsSM1_house012410week, ts.colour = 'black', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1 January 22 - 29, 2010")

plot_ly(house012410week, x = ~house012410week$DateTime, y = ~house012410week$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
    add_trace(y = 65, name = 'Upper limit') %>%
      layout(title = "Sub-meter 1 from January 22 - 29, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (Watt-hour)"))


##SUBMETER 2
# Create a subset that shows the average hourly kWh used for each hour of the day during 
# January 2010. This subset should only have 24 values (it reflects the typical usage per 
# hour of day during Jan-10). Plot this subset using a line plot. 

hour.sum <- energy %>%
  filter(Date >= "2010-01-01" & Date <= "2010-01-31") %>%
  group_by(hour) %>%  # Group data by hour
  summarize(SM1 = round(sum(Sub_metering_1 / 1000, na.rm = TRUE),3), # Total K Watt-hour
            SM2 = round(sum(Sub_metering_2 / 1000, na.rm = TRUE),3), 
            SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(hour)) # Remove any row that has NA 

hour.avg <- hour.sum %>%
  group_by(hour) %>%  # Group data by hour
  summarize(SM1 = (SM1/31), #  K Watt-hour per day
            SM2 = (SM2/31), 
            SM3 = (SM3/31),
            DateTime = first(DateTime)) %>% # For filters in Tableau
  filter(!is.na(hour)) # Remove any row that has NA 

tsSM2_houravg <- ts(hour.avg$SM2, frequency = 1)
tsSM2_houravg

plot.ts(tsSM2_houravg, ts.colour = 'black', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2 Hourly Avg")

plot_ly(hour.avg, x = ~hour.avg$DateTime, y = ~hour.avg$SM2,  type = 'scatter', mode = 'lines') %>%
  layout(title = "Sub-meter 2 Hourly Average - Jan 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kWh)"))

#a single day in January 2010 to overlay
January24 <- energy %>%
  filter(Date == "2010-01-24") %>%
  group_by(hour) %>%
  summarize(SM1 = sum(Sub_metering_1/1000, na.rm = TRUE),
            SM2 = sum(Sub_metering_2/1000, na.rm = TRUE),
            SM3 = sum(Sub_metering_3/1000, na.rm = TRUE),
            DateTime = first(DateTime)) %>%
  filter(!is.na(hour))

January.hour <- merge(hour.avg, January24, by="hour")

plot_ly(January.hour, x = ~January.hour$hour, y = ~January.hour$SM2.x,  type = 'scatter', mode = 'lines', name = "Hourly average") %>%
  add_trace( y = ~January.hour$SM2.y, name = "January 24th") %>%
  layout(title = "Sub-meter 2 Hourly Data - Jan 2010",
         xaxis = list(title = "Hour"),
         yaxis = list (title = "Power (kWh)"))

############# 
# TSLM Forecast
#############

#######
###SUBMETER 3
##############

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)
#Call:
#  tslm(formula = tsSM3_070809weekly ~ trend + season)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-12.3333  -5.9097  -0.3333   5.7570  18.3333 

#Residual standard error: 8.981 on 104 degrees of freedom
#Multiple R-squared:  0.2538,	Adjusted R-squared:  -0.1193 #over-fit model?
#F-statistic: 0.6803 on 52 and 104 DF,  p-value: 0.9375

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 60 and 75
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(60, 75))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", main = "Submeter 3 Forecast for Mondays at 8pm")
legend("topleft", legend=c("60% confidence", "75% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

#######
###SUBMETER 1
##############
fitSM1_monthly <- tslm(tsSM1_monthlysum2 ~ trend + season)
summary(fitSM1_monthly)
#Residual standard error: 15.17 on 29 degrees of freedom
#Multiple R-squared:  0.3172,	Adjusted R-squared:  0.03461 
#F-statistic: 1.122 on 12 and 29 DF,  p-value: 0.3804

## Create sub-meter 3 forecast with confidence levels 85 and 90
forecastfitSM1 <- forecast(fitSM1_monthly, h=12, level=c(85,90))
summary(forecastfitSM1)
forecastfitSM1$lower
forecastfitSM1$upper

plot(forecastfitSM1, ylab= "Watt-Hours", xlab="Time", main = "Kitchen Energy Usage Forecast by Month")
legend("bottomleft", legend=c("85% confidence", "90% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

SM1monthly_subset <- monthly.sum %>%
  filter(year >= 2010 & (month == 7 | month == 8 | month == 9 | month == 10))

accuracy(forecastfitSM1, SM1monthly_subset$SM1)
#                       ME     RMSE       MAE       MPE     MAPE      MASE      ACF1
#Training set -1.688464e-16 12.60472  9.500517 -28.02191 44.43404 0.6970099 0.2505085
#Test set     -1.076390e+01 11.64301 10.763905 -58.07684 58.07684 0.7896989        NA

## Plot sub-meter 3 forecast, limit y and add labels
autoplot(forecastfitSM1, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM1_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Kitchen Energy Forecast #1 by Month (linear)") +
  guides(colour=guide_legend(title="Legend")) +
  theme(legend.position = "bottom") 

#######
###SUBMETER 2
##############
fitSM2_monthly <- tslm(tsSM2_monthlysum2 ~ trend + season)
summary(fitSM2_monthly)
#Residual standard error: 14.34 on 29 degrees of freedom
#Multiple R-squared:  0.499,	Adjusted R-squared:  0.2917 
#F-statistic: 2.407 on 12 and 29 DF,  p-value: 0.02623

## Create sub-meter 3 forecast with confidence levels 85 and 90
forecastfitSM2 <- forecast(fitSM2_monthly, h=12, level=c(85,90))
summary(forecastfitSM2)
#Error measures:
#                     ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set 7.610067e-16 11.91639 9.864671 -5.873865 20.64283 0.5511554 0.1924851


plot(forecastfitSM2, ylab= "Watt-Hours", xlab="Time", main = "Laundry Energy Usage Forecast by Month")
legend("bottomleft", legend=c("85% confidence", "90% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

accuracy(forecastfitSM2, SM1monthly_subset$SM2)
#                       ME     RMSE       MAE        MPE     MAPE      MASE      ACF1
#Training set  7.610067e-16 11.91639  9.864671  -5.873865 20.64283 0.5515431 0.1924851
#Test set     -1.241673e+01 22.09396 15.391655 -41.587033 46.52713 0.8605620        NA

## Plot sub-meter 2 forecast, limit y and add labels
autoplot(forecastfitSM2, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM2_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Laundry Energy Forecast #1 by Month (linear)") +
  guides(colour=guide_legend(title="Legend")) +
  theme(legend.position = "bottom") 

#######
###SUBMETER 3
##############
fitSM3_monthly <- tslm(tsSM3_monthlysum2 ~ trend + season)
summary(fitSM3_monthly)
#Residual standard error: 64.48 on 29 degrees of freedom
#Multiple R-squared:  0.3305,	Adjusted R-squared:  0.05341 
#F-statistic: 1.193 on 12 and 29 DF,  p-value: 0.3335

## Create sub-meter 3 forecast with confidence levels 85 and 90
forecastfitSM3 <- forecast(fitSM3_monthly, h=12, level=c(85,90))
summary(forecastfitSM3)
#Error measures:
#                     ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set 6.767074e-16 53.57859 41.86488 -6.515457 19.96845 0.5473301 0.3941572


plot(forecastfitSM3, ylab= "Watt-Hours", xlab="Time", main = "Water Heater & A/C Energy Usage Forecast by Month")
legend("bottomleft", legend=c("85% confidence", "90% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

accuracy(forecastfitSM2, SM1monthly_subset$SM3)
#                       ME      RMSE        MAE       MPE     MAPE       MASE      ACF1
#Training set 7.610067e-16  11.91639   9.864671 -5.873865 20.64283  0.5515431 0.1924851
#Test set     1.807235e+02 187.77917 180.723524 77.828356 77.82836 10.1044232        NA

## Plot sub-meter 3 forecast, limit y and add labels
autoplot(forecastfitSM3, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM3_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Water Heater & A/C Energy Forecast #1 by Month (linear)") +
  guides(colour=guide_legend(title="Legend")) +
  theme(legend.position = "bottom") 

############################
#QUARTERLY forecast
############################
fitquarterSM1 <- tslm(tsSM1_quarterlysum ~ trend + season)
summary(fitquarterSM1)
#Call:
#  tslm(formula = tsSM1_quarterlysum ~ trend + season)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-22.245 -12.069  -3.368   9.210  28.094 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  182.075     11.590  15.710 2.24e-08 ***
#  trend         -2.642      1.071  -2.465   0.0334 *  
#  season2       -3.975     12.541  -0.317   0.7578    
#season3      -56.653     12.678  -4.469   0.0012 ** 
#  season4        1.849     13.539   0.137   0.8941    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 17.67 on 10 degrees of freedom
#Multiple R-squared:  0.7978,	Adjusted R-squared:  0.7169 
#F-statistic: 9.863 on 4 and 10 DF,  p-value: 0.001687

## Create forecast for rest of 2010 and all of 2011 with confidence levels 85 and 90
forecastfitquarterSM1 <- forecast(fitquarterSM1, h=5, level=c(85,90))

#check model fit
checkresiduals(forecastfitquarterSM1)

plot(forecastfitquarterSM1, ylab= "Watt-Hours", xlab="Time", main = "Kitchen Energy Usage Forecast by Quarter")
legend("bottomleft", legend=c("85% confidence", "90% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

fitquarterSM2 <- tslm(tsSM2_quarterlysum ~ trend + season)
summary(fitquarterSM2)
#Call:
#  tslm(formula = tsSM2_quarterlysum ~ trend + season)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-29.631 -14.162   3.822  13.417  31.149 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  242.151     14.408  16.807 1.17e-08 ***
#  trend         -6.359      1.332  -4.774 0.000752 ***
#  season2      -30.593     15.591  -1.962 0.078145 .  
#season3      -56.737     15.760  -3.600 0.004848 ** 
#  season4       -3.353     16.831  -0.199 0.846097    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 21.97 on 10 degrees of freedom
#Multiple R-squared:  0.8218,	Adjusted R-squared:  0.7505 
#F-statistic: 11.53 on 4 and 10 DF,  p-value: 0.0009186

forecastfitquarterSM2 <- forecast(fitquarterSM2, h=5, level=c(85,90))
plot(forecastfitquarterSM2, ylab= "Watt-Hours", xlab="Time", main = "Laundry Energy Usage Forecast by Quarter")
legend("bottomleft", legend=c("85% confidence", "90% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

fitquarterSM3 <- tslm(tsSM3_quarterlysum ~ trend + season)
summary(fitquarterSM3)

#Call:
#  tslm(formula = tsSM3_quarterlysum ~ trend + season)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-129.53  -54.96   20.10   52.53   90.65 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  832.756     54.797  15.197 3.08e-08 ***
#  trend         17.558      5.066   3.466 0.006063 ** 
#  season2     -130.540     59.296  -2.202 0.052307 .  
#season3     -369.446     59.941  -6.163 0.000106 ***
#  season4      -30.223     64.013  -0.472 0.646962    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 83.55 on 10 degrees of freedom
#Multiple R-squared:  0.8376,	Adjusted R-squared:  0.7726 
#F-statistic: 12.89 on 4 and 10 DF,  p-value: 0.0005869

forecastfitquarterSM3 <- forecast(fitquarterSM3, h=5, level=c(85,90))
plot(forecastfitquarterSM3, ylab= "Watt-Hours", xlab="Time", main = "Water Heater & A/C Energy Usage Forecast by Quarter")
legend("topleft", legend=c("85% confidence", "90% confidence"),
       col=c("#5C6BAD", "#9FA5C7"),
       pch = c(15,15))

#######
###SUBMETER 2 monthly
##############
#fitSM2 <- tslm(tsSM2_monthlysum ~ trend + season)
#summary(fitSM2)
#Residual standard error: 13.97 on 34 degrees of freedom
#Multiple R-squared:  0.4933,	Adjusted R-squared:  0.3145 
#F-statistic: 2.759 on 12 and 34 DF,  p-value: 0.009984

## Create sub-meter 2 forecast with confidence levels 85 and 90
#forecastfitSM2 <- forecast(fitSM2, h=13, level=c(85,90))

## Plot sub-meter 3 forecast, limit y and add labels
#plot(forecastfitSM2, ylab= "kWh", xlab="Time")

#######
###SUBMETER 3
##############
#fitSM3m <- tslm(tsSM3_monthlysum ~ trend + season)
#summary(fitSM3m)
#Residual standard error: 69.19 on 34 degrees of freedom
#Multiple R-squared:  0.2059,	Adjusted R-squared:  -0.0744 
#F-statistic: 0.7346 on 12 and 34 DF,  p-value: 0.7086

## Create sub-meter 3 forecast with confidence levels 85 and 90
#forecastfitSM3m <- forecast(fitSM3m, h=13, level=c(85,90))

## Plot sub-meter 3 forecast, limit y and add labels
#plot(forecastfitSM3m, ylab= "kWh", xlab="Time")

######
# Decompose time series
######

#SUBMETER 3
## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3
plot(cbind(observed = components070809SM3weekly$random +
             components070809SM3weekly$trend + components070809SM3weekly$seasonal, trend = components070809SM3weekly$trend, seasonal = components070809SM3weekly$seasonal,
           random = components070809SM3weekly$random), main = "Decomposition of Sub-meter 3 for Mondays at 8pm") 



## Check summary statistics for decomposed sub-meter 3
summary(components070809SM3weekly$seasonal)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-6.81518 -5.92576  1.83866  0.01159  2.97328 11.63674
summary(components070809SM3weekly$trend)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  4.837   5.606   6.183   6.271   6.827   7.952      52 
summary(components070809SM3weekly$random)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-13.9204  -1.7233   0.1902   0.2046   2.1325  14.3296       5

#SUBMETER 1 Monthly avg
decomp_tsSM1 <- decompose(tsSM1_monthlysum)
decomp_tsSM1
plot(cbind(observed = decomp_tsSM1$random +
            decomp_tsSM1$trend + decomp_tsSM1$seasonal, trend = decomp_tsSM1$trend, seasonal = decomp_tsSM1$seasonal,
          random = decomp_tsSM1$random), main = "Decomposition of Monthly Average for Sub-meter 1") 

summary(decomp_tsSM1$seasonal)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-13.90569  -6.73753  -2.64963  -0.01919   6.72869  16.61424 
summary(decomp_tsSM1$trend)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  33.76   38.92   49.78   45.87   51.71   53.93      12 
summary(decomp_tsSM1$random)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-19.1791  -6.6869  -2.3492  -0.6811   5.7160  22.4975       12 

#SUBMETER 2 Monthly avg
decomp_tsSM2 <- decompose(tsSM2_monthlysum)
plot(cbind(observed = decomp_tsSM2$random +
             decomp_tsSM2$trend + decomp_tsSM2$seasonal, trend = decomp_tsSM2$trend, seasonal = decomp_tsSM2$seasonal,
           random = decomp_tsSM2$random), main = "Decomposition of Monthly Average for Sub-meter 2") 
summary(decomp_tsSM2$seasonal)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-12.8987  -8.9438  -4.3017   0.1586   7.2239  28.9270 
summary(decomp_tsSM2$random)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-28.3303  -5.8545   1.3318   0.3577   6.0022  26.2263       12 
summary(decomp_tsSM2$trend)
# 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  43.31   47.92   54.11   53.45   58.45   64.18      12 

#SUBMETER 3 Monthly avg
decomp_tsSM3 <- decompose(tsSM3_monthlysum)
plot(cbind(observed = decomp_tsSM3$random +
             decomp_tsSM3$trend + decomp_tsSM3$seasonal, trend = decomp_tsSM3$trend, seasonal = decomp_tsSM3$seasonal,
           random = decomp_tsSM3$random), main = "Decomposition of Monthly Average for Sub-meter 3") 
summary(decomp_tsSM3$seasonal)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-64.5088 -21.1519   3.3623  -0.6875  20.7009  46.7931 

decomp_tsSM1_quart <- decompose(tsSM1_quarterlysum)
plot(decomp_tsSM1_quart)
summary(decomp_tsSM1_quart)
#Length Class  Mode     
#x        15     ts     numeric  
#seasonal 15     ts     numeric  
#trend    15     ts     numeric  
#random   15     ts     numeric  
#figure    4     -none- numeric  
#type      1     -none- character

###########
##HOLTS-WINTERS FORECASTING
###########

#SUBMETER 3 TS

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

#############
#SUBMETER 1 monthly avg
tsSM1_monthly_adj <- tsSM1_monthlysum - decomp_tsSM1$seasonal
autoplot(tsSM1_monthly_adj)
plot(decompose(tsSM1_monthly_adj))

## Holt Winters Exponential Smoothing
tsSM1_HW <- HoltWinters(tsSM1_monthly_adj, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW)

## HoltWinters forecast & plot
tsSM1_HW_for <- forecast(tsSM1_HW, h=10, level = c(50, 60))
plot(tsSM1_HW_for, ylab= "Watt-Hours", xlab="Time - Sub-meter 1", main = "Kitchen Energy Usage Forecast by Month")
legend("bottomleft", legend=c("50% confidence", "60% confidence"),
       col=c("#013FA4", "#5C6BAE"),
       pch = c(15,15))
plot(tsSM1_HW_forC, ylab= "Watt-Hours", xlab="Time - Sub-meter 1", main = "Kitchen Energy Usage Forecast by Month", start(2010,12))
legend("bottomleft", legend=c("50% confidence", "60% confidence"),
       col=c("#013FA4", "#5C6BAE"),
       pch = c(15,15))

autoplot(tsSM1_HW, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM1_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Kitchen Energy Forecast by Month") +
  guides(colour=guide_legend(title="Legend")) +
  theme(legend.position = "bottom") 

#########
#submeter 1 without removing seasonality, but removing trend
fitSM1 <- HoltWinters(tsSM1_monthlysum2, beta = FALSE)
summary(fitSM1)
tsSM1_HW_for <- forecast(fitSM1, h=12, level = c(85, 90))
plot(tsSM1_HW_for)

autoplot(tsSM1_HW_for, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM1_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Kitchen Energy Forecast #2 by Month (smoothing)") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position = "bottom")

#########
#submeter 2 without removing seasonality, but removing trend
fitSM2 <- HoltWinters(tsSM2_monthlysum2, beta=FALSE)
summary(fitSM2)
tsSM2_HW_for <- forecast(fitSM2, h=12, level = c(85, 90))
plot(tsSM2_HW_for)

autoplot(tsSM2_HW_for, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM2_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Laundry Energy Forecast #2 by Month (smoothing)") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position = "bottom")

#########
#submeter 3 without removing seasonality, but removing trend
fitSM3 <- HoltWinters(tsSM3_monthlysum2, beta=FALSE)
summary(fitSM3)
tsSM3_HW_for <- forecast(fitSM3, h=12, level = c(85, 90))
plot(tsSM3_HW_for)

autoplot(tsSM3_HW_for, series="Monthly Forecast", PI=TRUE) +
  autolayer(tsSM3_monthlysum, series = "Actual Monthly Usage")  +
  xlab("Time") +
  ylab("Watt-Hours") +
  ggtitle("Water Heater & A/C Energy Forecast #2 by Month (smoothing)") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position = "bottom")
