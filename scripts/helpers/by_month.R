# Plots a y variable by month with years separated out and an average provided
# Meant for further use:
# - EVERY_THIRD_MONTH: shortened month labels for use on a small x axis
# - extract_time(): prepare a data.frame for use here by making $Year and $Month
# - add_year_lines(): add lines by year on top of a pre-made graph
# - plot_by_month(): create a by-month graph of a pre-processed data-set

# used to draw the graphs
library(ggplot2)

# colors with high contrast between each other and the background of the graph
COLORBLIND_PALETTE_NO_YELLOW <- c('#E69F00', '#56B4E9', '#009E73', 
                                  '#0072B2', '#D55E00', '#CC79A7', '#000000')
# pre-calculated, modified axis labels if there isn't much horizontal space
EVERY_THIRD_MONTH <- sapply(1:12, function(x) 
  ifelse(x %in% (1:4 * 3), month.abb, ''))

# Create Year and Month columns for the given data frame
# @param - data: a data.frame which has a column with dates (year, month, day)
# @param - date_name: the name of the column with dates; a string. Defaults to
#                     what the visits.csv data-set uses.
# @return - a data.frame exactly like data but with three columns added:
#           $Date with a date object from the date_name column,  
#           $Year with the four-digit year from $Date,
#           $Month with the English name for the month in the $Date object
extract_time <- function(data, date_name = 'Application.Date') {
  # process into a date object for easier manipulation
  data$Date <- as.Date(data[[date_name]], format = '%m/%d/%Y')
  # format has some codes which match certain parts of a date
  data$Year <- as.factor(format(data$Date, '%Y'))
  data$Month <- month.name[as.numeric(format(data$Date, '%m'))]
  # this will override the default alphabetical ordering
  data$Month <- factor(data$Month, levels = month.name)
  data
}

# -- plotting functions --

# Add lines for each year and the average of all years to a graph
# @param - graph: the graph to draw lines on; a ggplot object
# @param - data: the data to plot; a data.frame. Must have Year, Month (using
#                English month names), and y (dependent variable) columns
# @return - a ggplot object exactly like graph but with lines drawn on top
#           for all years (as separate series) and an average across all years,
#           using data$y for y values
add_year_lines <- function(graph, data) {
  # create space for each month's average across all years
  month_avg <- data.frame(Year = rep('avg', length(month.name)),
                          Month = month.name)
  # calculate average, ignoring any years for which data does not exist
  month_avg$y <- sapply(month.name, 
                        function(x) sum(data[data$Month == x,]$y) /
                          length(unique(data[data$Month == x,]$Year)))
  # the average line is thicker to draw attention
  month_avg$Size <- 2
  
  data$Size <- 1
  data <- rbind(data, month_avg)
  
  # add on new lines to graph and return this updated graph
  graph + 
    geom_line(data = data, aes(x = Month, y = y, size = Size, 
                               group = Year, color = Year))
}

# Plot some variable by month, with each year and the average as separate lines
# @param - data: the data to plot; a data.frame. Must have Year, Month (using
#                English month names), and y (dependent variable) columns
# @param - title: the title of the finished graph; a string
# @param - y_lab: the y-axis' label; a string
# @return - a ggplot object which is a lineplot of data. The x-axis is by month,
#           the y-axis is from data$y and labeled with y_lab, and each year
#           (plus the average across all years) is a separate series
plot_by_month <- function(data, title, y_lab) {
  # create base graph to draw on top of
  graph <- ggplot(data, aes(x = Month)) + 
    # force the colorblind-friendly scale to be used
    scale_color_manual(values = COLORBLIND_PALETTE_NO_YELLOW) +
    # force months to appear in the right order
    scale_x_discrete(limits = month.name, 
                     # longer labels run into each other
                     labels = function(x) substr(x, 1, 3)) +
    # allow size scales to be used directly
    scale_size_identity() + 
    # provide context to graph
    labs(title = title, y = y_lab) +
    theme(text = element_text(size = 15))
  
  add_year_lines(graph, data) 
}