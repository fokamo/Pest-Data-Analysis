# Builds the basic background that maps of this data need to go on top of
# Meant for further use:
# - locs: all places which have data in visits.csv, associated with lat & long
# - STATES: all states which have locations in them; lowercased
# - base_map(): generates the base map
# - plot_locs(): plots all locations on the base map

# used to draw the maps
library(ggplot2)

# some global-scope variables for use downstream
locs <- read.csv(paste0(getwd(), '/data/locations.csv'))
STATES <- unique(locs$State)

# Create a background map which can be drawn on top of
# @param - xlims: the maximum and minimum longitude values to show on the map;
#                 a vector of doubles. Defaults to wide enough to show all bases
# @param - ylims: the maximum and minimum latitude values to show on the map;
#                 a vector of doubles. Defaults to long enough to show all bases
# @return - a ggplot object with locs as its data and x,y pre-set to Long,Lat;
#           has state and county borders pre-drawn using map_data maps
base_map <- function(xlims = c(0, 1), ylims = c(0, 1)) {
  ggplot(locs, aes(x = Long, y = Lat)) +
    # county borders
    geom_polygon(data = subset(map_data('county'), region %in% STATES), 
                 aes(x = long, y = lat, group = group), 
                 color = 'black', fill = 'white') +
    # state borders
    geom_polygon(data = subset(map_data('state'), region %in% STATES), 
                 aes(x = long, y = lat, group = group), 
                 # states are transparent so county bounds can be seen
                 color = 'blue', fill = NA) +
    # shave off edges and scale graph to fix stretching
    coord_fixed(xlim = xlims, ylim = ylims, ratio = 1.3)
}

# Basic visualization of locations
# @return - a ggplot object with all locations plotted in red onto the base_map
plot_locs <- function() {
  # plot each location on top of the map
  base_map() + geom_point(color = 'red') +
    labs(title = 'Locations of bases with pest-control data')
}