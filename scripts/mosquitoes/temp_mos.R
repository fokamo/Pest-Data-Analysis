# Visualize relationships between temperature and mosquito visits

# for the by-month graphs
source(paste0(getwd(), '/scripts/helpers/by_month.R'))
# for arranging two ggplots next to each other
library(ggpubr)

# -- prepare data frames --

visits <- read.csv(paste0(getwd(), '/data/visits.csv'))
visits <- extract_time(visits) 
# there's only one day of this so it messes up the analysis
visits <- visits[!(visits$Month == 'June' & visits$Year == '2022'),]
months <- unique(visits[, c('Year', 'Month')])

temp <- read.csv(paste0(getwd(), '/data/weather.csv'))
temp <- extract_time(temp, 'DATE')

# -- main function & helper to plot for mosquitoes & temp --

# Make a modified by_month plot which is meant to be squished into half a figure
# @param - data: a data.frame, as in by_month
# @param - title: a string, as in by_month
# @param - y_lab: a string, as in by_month
# @return - a ggplot just like by_month, but with only every third label and
#           the font sizes bumped up a notch
small_by_month <- function(data, title, y_lab) {
  plot_by_month(data, title, y_lab) + theme(text = element_text(size = 20)) +
    scale_x_discrete(limits = month.name, labels = EVERY_THIRD_MONTH)
}

# Plot mosquito visits & max/min temperature on the same graph
# @param - base_name: the name of the base; a string
# @param - weather_name: the name of the weather station; a string
plot_base <- function(base_name, weather_name) {
  # load data for this specific place
  visits <- visits[visits$Installation.Name == base_name,]
  temp <- temp[temp$NAME == weather_name,]
  
  # count the number of visits during each month which were for mosquitoes
  months$y <- apply(months, 1, 
                    function(x) sum(startsWith(visits[visits$Year == x[1] 
                                                      & visits$Month == x[2],
                                                      ]$Pest, 'Mosquito')))
  
  # plot with just mosquito visit data
  visits_plot <- small_by_month(months, 'Mosquito visits', 'Visits')

  # update the y value to plot to be the average maximum temperature
  months$y <- apply(months, 1, 
                    function(x) mean(temp[temp$Year == x[1] 
                                          & temp$Month == x[2],]$TMAX, 
                                     na.rm = TRUE))
  # some months there is no weather data; ignore them
  months <- months[!is.na(months$y),]
  # set up plot with temperature
  temp_plot <- small_by_month(months, 'Temperature', 'Degrees (F)')
  
  # add in minimum temperature
  months$y <- apply(months, 1, 
                    function(x) mean(temp[temp$Year == x[1] 
                                          & temp$Month == x[2],]$TMIN, 
                                     na.rm = TRUE))
  temp_plot <- add_year_lines(temp_plot, months)
  
  # combine plots
  plot <- ggarrange(visits_plot, temp_plot, legend = 'right', 
                    common.legend = TRUE, ncol = 2)
  annotate_figure(plot, top = text_grob(base_name, size = 20)) 
  
  # save final result
  ggsave(paste0(getwd(), '/plots/special_bases/temp-mos-sbs-', 
                base_name, '.png'), width = 20, height = 10, units = 'cm')
}

# -- actually make graphs --

# combine three very close base names
visits$Installation.Name <- ifelse(startsWith(visits$Installation.Name, 'b'), 
                                   'B', visits$Installation.Name)

# match bases with weather stations
locs <- data.frame(Base = c('A', 'B', 'C', 'D'),
                   Weather = c('E', 'F', 'G', 'H'))

# make plots for each
apply(locs, 1, function(x) plot_base(x[1], x[2]))