# Attempt to see what "triggers" a mosquito visit

# -- constants --

# lag time expected between an inciting event and a visit to deal with it
WAIT_TIME <- 3
# period considered to be closely following after a date
DAYS_AFTER <- 2
# minimum amount of rain considered important
SIGNIFICANT_RAIN <- 0.05
# records here are from 6/1/17 - 6/1/22
# manually recorded here in case either date happened to have no records entered
EARLIEST_TIME <- as.Date('2017-06-01')
LATEST_TIME <- as.Date('2022-06-01')
# match bases with weather stations
LOCS <- data.frame(Base = c('A', 'B', 'C', 'D'),
                   Weather = c('E', 'F', 'G', 'H'))
# change to a number 1-4 to get data for different places
LOCATION_NUM <- 1
# pull data from the correct place
BASE_NAME <- LOCS[c(LOCATION_NUM),]$Base
WEATHER_NAME <- LOCS[c(LOCATION_NUM),]$Weather

# -- prepare master data frames --

source(paste0(getwd(), 'helpers/calc_season.R'))
df <- df_with_seasons()
df$Installation.Name <- ifelse(startsWith(df$Installation.Name, 'b'), 
                               'B', df$Installation.Name)
# select only visits for mosquitoes at this particular base
df <- df[df$Installation.Name == BASE_NAME
         & startsWith(df$Pest, 'Mosquito'),]
# date objects are more easily worked with than strings
df$Date <- as.Date(df$Application.Date, format = '%m/%d/%Y')

# take only weather data from this particular base
weather <- read.csv(paste0(getwd(), '/data/weather.csv'))
weather$Date <- as.Date(weather$DATE, format = '%m/%d/%Y')
weather <- weather[weather$NAME == WEATHER_NAME,]

# determine which days within this period had a mosquito visit
mos_days <- data.frame(Date = seq(EARLIEST_TIME, LATEST_TIME, by = 'days'))
mos_days$Visit <- sapply(mos_visit$Date, 
                         function(x) length(df[df$Date == x,]$Date) > 0)
# determine which days have a mosquito visit close enough afterwards
close_days <- sapply(
  seq(EARLIEST_TIME, LATEST_TIME, by = 'days'), 
  # TRUE/FALSE for weather any such date exists
  function(x) length(df[df$Date >= x + WAIT_TIME & 
                          df$Date <= x + DAYS_AFTER + WAIT_TIME + 1,]$Date) > 0)
close_days <- mos_days[close_days,]$Date
# which days are close enough is precalculated so as to make later lookup quick

# -- convenience functions --

# calculate the percentage of given dates which have a mosquito visit close
percent_close <- function(dates)
  # exploit TRUE=1; sum booleans of whether this day is a close day
  sum(sapply(dates, function(x) x %in% close_days)) / length(dates)

# determine, for some given days, the percentage which are close days by season
close_by_season <- function(dates) {
  # determine the season group to put each date into
  data <- data.frame(Date = dates, Season = sapply(dates, determine_season))
  # aggregate closeness by season
  by_season <- aggregate(Date ~ Season, data, percent_close)
  # return nicely formatted frame
  return (setNames(by_season, c('Season', 'Chance')))
}

# -- perform analysis --

# for all dates, chance that within a 7-day period a visit occurred
close_by_season(seq(EARLIEST_TIME - 1, LATEST_TIME - DAYS_AFTER, by = 'days'))

# after all mosquito visits, chance that another occurred
close_by_season(unique(df$Date))

# after all significant rains, chance that a visit occurred
close_by_season(unique(weather[which(weather$PRCP >= SIGNIFICANT_RAIN),]$Date))

BASE_NAME