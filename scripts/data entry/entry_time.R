# Visualize when visits are entered into the system by time and day

# handles the dirty work of bar-chart making
library(ggplot2)

# time constants
STANDARD_DAY <- '2017-06-01'
WEEKDAYS <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 
              'Thursday', 'Friday', 'Saturday')

# -- set up master data frame --

df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# a POSIXct object is more easily manipulated for later stuff
df$Date.Entered <- as.POSIXct(df$Date.Entered, format = '%m/%d/%Y %I:%M:%S %p')
# extract just time of day, preserving POSIXct format
df$Time <- as.POSIXct(paste(STANDARD_DAY, substr(df$Date.Entered, 12, 16)))
# weekday must be taken without conversion to standard year
df$Weekday <- factor(weekdays(as.Date(df$Date.Entered)), levels = WEEKDAYS)
# extract just date, preserving Date format
df$Date <- as.Date(paste('2020-', substr(df$Date.Entered, 6, 10)))
# force months to be factors for correct facet ordering
df$Month <- factor(month.name[as.numeric(format(df$Date, '%m'))], month.name)

# -- make bar charts --

# visits entry by time of day (geom_bar counts # of rows with each x value)
ggplot(df, aes(x = Time)) + geom_bar() + 
  # substring the labels so the arbitrary day from earlier doesn't appear
  scale_x_datetime(labels = function(x) substr(x, 12, 16)) + 
  geom_vline(xintercept = as.POSIXct(paste(STANDARD_DAY, '12:00:00 PM')),
             color = 'red') +
  # provide context to graph
  labs(title = 'When visits are entered into the system', 
       subtitle = 'Data from 06/2017-05/2022', 
       x = 'Time of entry', y = 'Number of visits')

# visits entry by day of week
ggplot(df, aes(x = Weekday)) + geom_bar() +
  labs(title = 'When visits are entered into the system', 
       subtitle = 'Data from 06/2017-05/2022',
       x = 'Day of entry', y = 'Number of visits')

# count when visits are entered by day of year
ggplot(df, aes(x = Date)) + geom_bar() + 
  # having months be separate facets allows more efficient vertical space use
  facet_wrap(~Month, scales = 'free_x') +
  # provide context to graph
  labs(title = 'When visits are entered into the system', 
       subtitle = 'Data from 06/2017-05/2022',
       x = 'Date of entry', y = 'Number of visits')