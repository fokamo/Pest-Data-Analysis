# Creates common specialized graphs the way I like them - see help.txt
# Meant for further use:
# - pies(): make faceted pie charts to specification
# - single_pie: make a single pie chart to specification
# - dot_lines(): make a dot-line chart to specification

# used to draw the graphs
library(ggplot2)

# -- preparing environment --

MAX_PIES <- 12
COLORBLIND_PALETTE <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442',
                        '#0072B2', '#D55E00', '#CC79A7', '#000000')
# a standard year to convert dates to
STANDARD_YEAR <- 2020

# Combine low-incidence values in a column into a "misc." group
# @param - data: the data.frame to alter
# @param - col: the name of the column to reduce unique value count of; a string
# @param - max_unique: the maximum number of unique values to have in the end
# @return - a copy of data.frame with any low-incidence values in $col replaced
#           with 'misc', and no other changes
combine_misc <- function(data, col, max_unique) {
  # count each value's incidence
  col_count <- table(data[[col]])
  # if there are few enough unique values no combination is needed
  if (length(col_count) <= max_unique) return (data)
  
  # find the maximum count for a value to be combined into other
  other_count <- col_count[order(col_count, decreasing = TRUE)[max_unique]]
  # determine which column values will be combined
  others <- names(col_count[col_count <= other_count])
  # replace any low-incidence column values with other
  data[[col]] <- sapply(data[[col]], 
                        function(x) ifelse(x %in% others, 'misc', x))
  data
}

# -- plotting functions --

# Make faceted pie charts with consistent coloring of slices
# @param - data: the data to chart; a data.frame
# @param - facet_by: the variable to associate with pies (splits up the pie
#                    charts); a string. Must be a column in data
# @param - color_by: the variable to associate with colors (splits up the slices
#                    and labels the legend); a string. Must be a column in data
# @param - plot_title: what to title the plot; a string
# @param - pie_cols: the number of columns of pies to use; defaults to NULL.
#                    Must be a valid value for facet_wrap's ncol parameter
# @return - a ggplot object with faceted pie charts made to order
pies <- function(data, facet_by, color_by, plot_title, pie_cols = NULL) {
  # reduce number of colors and facets to what can be handled
  data <- combine_misc(data, facet_by, MAX_PIES)
  data <- combine_misc(data, color_by, length(COLORBLIND_PALETTE))
  
  # rename for easier chart-making (ggplot doesn't like variable names)
  colnames(data)[colnames(data) == facet_by] <- 'Facet'
  colnames(data)[colnames(data) == color_by] <- 'Color'
  
  # standardize all bar heights (x) so they turn into identically-sized pies
  ggplot(data, aes(x = factor(1), fill = Color)) + 
    facet_wrap(~Facet, ncol = pie_cols, labeller = label_wrap_gen()) + 
    # each 'bar' is partitioned into proportional sections by fill
    geom_bar(width = 1, position = 'fill') + 
    # wrap bars into circles and set their colors to be readable
    coord_polar('y') + scale_fill_manual(values = COLORBLIND_PALETTE) +
    # gets rid of the default grid background in its entirety
    labs(title = plot_title, fill = color_by) + theme_void() +
    # centered title works nicely with de-themed pies
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

# Same as pies, but for making a single pie out of all of the data
# @param - data: the data to chart; a data.frame
# @param - color_by: the variable to associate with colors (splits up the slices
#                    and labels the legend); a string. Must be a column in data
# @param - plot_title: what to title the plot; a string
# @return - a ggplot object with faceted pie charts made to order
single_pie <- function(data, color_by, plot_title) {
  # need to facet by something, and this will mean only one facet is made
  data$Blank <- ''
  pies(data, 'Blank', color_by, plot_title)
}

# Make a dot plot displaying when pests visits occurred in some data
# @param - data: the data to chart; a data.frame
# @param - lines: the variable to split horizontal lines by; a string. Must be a
#                 column in data
# @param - plot_title: what to title the plot; a string
# @return - a ggplot object with dot line plots faceted by year
dot_lines <- function(data, lines, plot_title) {
  # combine low-incidence categories
  data <- combine_misc(data, lines, length(COLORBLIND_PALETTE))
  
  # extract useful time components
  data$Date <- as.Date(data$Application.Date, format = '%m/%d/%Y')
  data$Year <- format(data$Date, '%Y')
  # converting all dates to the same year allows standard plot limits
  data$Date <- as.Date(strftime(data$Date, format = paste(STANDARD_YEAR, 
                                                          '%m-%d', sep = '-')))
  
  # rename for easier chart-making (ggplot doesn't like variable names)
  colnames(data)[colnames(data) == lines] <- 'Line'
  
  # base dot plot with each pest's dots on its own horizontal line
  ggplot(data, aes(x = Date, y = Line, color = Line)) + geom_point() +
    # standardize the x bounds so spacing between dots is directly comparable
    scale_x_date(limits = c(as.Date(paste(STANDARD_YEAR, '01-01', sep = '-')), 
                            as.Date(paste(STANDARD_YEAR, '12-31', sep = '-'))),
                 # shortened month names for x axis labels
                 date_labels = '%b') + 
    scale_y_discrete(limits = rev) +
    # split each year onto its own plot to compare between them
    facet_wrap(~Year) + scale_color_manual(values = COLORBLIND_PALETTE) +
    # axis labels are just distracting
    labs(title = plot_title, x = '', y = '', color = lines)
}