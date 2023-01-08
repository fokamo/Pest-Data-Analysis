# Visualize mosquito visits at specific bases

# for making maps
source(paste0(getwd(), '/scripts/helpers/base_map.R'))
# for making graphs
source(paste0(getwd(), '/scripts/helpers/chart_maker.R'))

# -- prepare master data frame --

df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# combine three locations which are very near each other
df$Installation.Name <- ifelse(startsWith(df$Installation.Name, 'b'), 
                               'B', df$Installation.Name)
# this analysis is only interested in mosquito visits at these specific places
df <- df[df$Installation.Name %in% c('A', 'B', 'C', 'D') &
           startsWith(df$Pest, 'Mosquito'),]

# -- make plots --

#  plot these bases on a map
base_map() + 
  geom_point(data = locs[locs$Place %in% df$Installation.Name,], 
             color = 'red', size = 4)

# dot plot for all mosquitoes
dot_lines(df, 'Installation.Name', 'Mosquito visits at bases of interest') +
  theme(text = element_text(size = 20), legend.position = 'none')

# Pie charts for the special bases customized to accommodate having less facets
# Parameters are similar to chart_maker::pies
# @param - data: the data to chart; a data.frame
# @param - plot_title: what to title the plot; a string
# @param - facet_by: the variable to associate with pies; a string. Must be a 
#                    column in data and defaults to Site
# @return - a ggplot object with faceted pie charts made to order
special_pies <- function(data, plot_title, facet_by = 'Site') {
  # pie visits by site for all bases
  pies(data, 'Installation.Name', facet_by, plot_title, 2) +
    # special set-up since facet # is known and small: larger text
    theme(text = element_text(size = 15), strip.text = element_text(size = 20),
          # move the legend into the empty facet spot
          legend.position = c(0.75, 0.25))
}

# pie chart of all mosquito visits by site for the bases
special_pies(df, 'Sites with mosquito visits')

# pie chart of just surveillance mosquito visits for the bases
special_pies(df[df$Operation == 'Surveillance/Monitoring *',], 
             'Sites with mosquito surveillance visits') + 
  # only two facets in this one so no empty spot to slot the legend into
  theme(legend.position = 'top')

# pie chart of operations conducted
special_pies(df, 'Mosquito operations conducted', 'Operation')