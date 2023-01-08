# Visualize generalized weather patterns for the weather stations used

# used for color palette and time extraction
source(paste0(getwd(), '/scripts/helpers/by_month.R'))

# -- preparing main data frame --

# take only weather data from this particular base
df <- read.csv(paste0(getwd(), '/data/weather.csv'))
df <- extract_time(df, 'DATE')
df$Day <- as.integer(format(df$Date, '%d'))
# for proper ordering of facets
df$Month <- factor(df$Month, levels = month.name)

# -- the workhorse function --

# Plots rain and temperature data for a particular location
# @param - name: the name of the base in visits.csv; a string
# @param - station: the name of the weather station; a string
plot_weather <- function(name, station) {
  # load in data for just this base (without changing the global)
  df <- df[df$NAME == station,]
  
  # when plotting rain, subset directly in the ggplot to retain temp data
  ggplot(df[!is.na(df$PRCP) & df$PRCP > 0,], 
         aes(x = Day, y = PRCP, group = Year, color = Year)) + 
    geom_point() + facet_wrap(~Month) +
    # readability and prettiness
    scale_color_manual(values = COLORBLIND_PALETTE_NO_YELLOW) +
    labs(title = paste('Rain in', name), y = 'Precipitation (in)') + 
    theme(legend.title = element_blank())
  
  ggsave(paste0(getwd(), '/plots/weather/', name, '-rain.png'))
  
  # need to have temperature data
  df <- df[!(is.na(df$TMAX) | is.na(df$TMIN)),]
  
  # create room for every day to have an across-years average
  yr_avg <- unique(df[, c('Month', 'Day')])
  yr_avg$Year <- 'avg'
  # calculate those averages
  yr_avg$TMAX <- apply(yr_avg, 1, 
                       function(x) mean(df[df$Month == x[1] 
                                           & df$Day == as.numeric(x[2]),]$TMAX))
  yr_avg$TMIN <- apply(yr_avg, 1,
                       function(x) mean(df[df$Month == x[1] 
                                           & df$Day == as.numeric(x[2]),]$TMIN))
  yr_avg <- yr_avg[,c(4, 5, 3, 1, 2)]
  # slot year averages into the main dataframe
  df <- rbind(df[,c('TMAX', 'TMIN', 'Year', 'Month', 'Day')], yr_avg)
  
  # same idea as before: plot different years together, with facets by month
  ggplot(df, aes(x = Day, group = Year, color = Year, linetype = Year)) +
    facet_wrap(~Month) + geom_line(aes(y = TMAX)) + geom_line(aes(y = TMIN)) +
    # readability and prettiness
    scale_color_manual(values = COLORBLIND_PALETTE_NO_YELLOW) +
    # non-average lines are not solid, for further distinction of average
    scale_linetype_manual(values = c(rep('dashed', 6), 'solid')) +
    labs(title = paste('Temperature in', name), y = 'Temperature (F)') +
    theme(legend.title = element_blank())
  
  ggsave(paste0(getwd(), '/plots/weather/', name, '-temp.png'))
}

# -- actually make graphs --

# match names with weather stations
locs <- data.frame(Names = c('A', 'B', 'C', 'D'), Stations = unique(df$NAME))
# make plots for each
apply(locs, 1, function(x) plot_weather(x[1], x[2]))