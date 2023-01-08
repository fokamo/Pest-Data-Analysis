# Visualize how number of pest visits might relate to density around the base

# for making graphs
library(ggplot2)

# -- set up data frame --

# load from files
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
locs <- read.csv(paste0(getwd(), '/data/locations.csv'))

# count visits for specific kinds of pests by location
df <- aggregate(Pest ~ Installation.Name, df, 
                function(x) c(Mosquitoes = sum(startsWith(x, 'Mosquitoes')), 
                              Rodents = sum(x %in% c('Rats', 'Mice'))))
# clean up the messy matrix output from aggregate
df <- do.call(data.frame, df)
colnames(df) <- c('Place', 'Mosquitoes', 'Rodents')
df <- merge(df, locs)

# -- workhorse functions --

# Make a bar chart showing visits for some pests by base, colored by density
# @param - data: the data to plot; a data.frame
# @param - pest: the pest to plot visits for; a string. Must be a column in data
# @param - density: the density to color by; a string. Must be a column in data
# @return - a ggplot object with a horizontal colored bar chart
density_bars <- function(data, pest, density) {
  # rename for easier use in aesthetics
  colnames(data)[colnames(data) == density] <- 'Density'
  
  ggplot(data[data$Pest > 1,], aes(x = Pest, y = Place, fill = Density)) + 
    # use counts for bar length, then log-scale because some bases have a lot
    geom_col() + scale_x_log10() +
    labs(title = paste('Visits for', pest, 'by density of', density), x = pest)
}

# Make bar charts for some pest by base, colored by densities
# @param - pest: the pest to analyzer; a string. Must be a column in by_loc
# Makes charts for each form of density in locs
density_pest <- function(pest) {
  # rename for easier use
  colnames(df)[colnames(df) == pest] <- 'Pest'
  # ensure bars will be ordered by length
  df <- df[order(df$Pest),]
  df$Place <- factor(df$Place, levels = df$Place)
  
  # make both possible bar charts
  print(density_bars(df, pest, 'Development'))
  print(density_bars(df, pest, 'Greenery'))
}

# -- actually make graphs --

density_pest('Mosquitoes')
density_pest('Rodents')