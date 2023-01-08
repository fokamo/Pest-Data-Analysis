# Visualize how far apart time of entry for one day's visit is

# -- prepare environment --

# for making graphs
library(ggplot2)

# load data and extract time information
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
df$Application.Date <- as.Date(df$Application.Date, format = '%m/%d/%Y')
df$Time.Entered <- as.POSIXct(df$Date.Entered, format = '%m/%d/%Y %I:%M:%S %p')
df$Date.Entered <- as.Date(df$Time.Entered)

# -- analyze time between entering records for the same visit --

# distance between time-of-entry for visits to the same base on the same day
by_app_date <- setNames(aggregate(
  Time.Entered ~ Application.Date + Installation.Name + Entered.by, df, 
  # either calculate range or, if only one, use NA for easy removal later
  function(x) ifelse(length(x) > 1, difftime(max(x), min(x), units = 'min'), NA)
), c('Application.Date', 'Installation.Name', 'Entered.by', 'Time.Between'))

ggplot(by_app_date[!is.na(by_app_date$Time.Between),], 
       aes(x = Time.Between)) + geom_histogram(bins = 50) + scale_y_log10() +
  labs(title = 'Time between entries for the same base & day',
       subtitle = 'Data for visits occurring between 06/2017-05/2022',
       y = 'Visits', x = 'Time between first and last entry (min)')

# -- analyze time between occurrences for visits entered on the same day --

# distance between date-of-visit for entries by the same person on the same day
by_ent_date <- setNames(aggregate(
  Application.Date ~ Date.Entered + Entered.by, df,
  function(x) ifelse(length(x) > 1, max(x) - min(x), NA)
), c('Date.Entered', 'Entered.by', 'Time.Between'))

ggplot(by_ent_date[!is.na(by_ent_date$Time.Between),], 
       aes(x = Time.Between)) + geom_histogram() + scale_y_log10() +
  labs(title = 'Time between visits entered on the same day by the same person',
       subtitle = 'Data for visits occurring between 06/2017-05/2022',
       x = 'Time between first and last visit entered (days)',
       y = 'Days of data entry')