# Visualize the relative frequency of different pest control visits by season 

# -- prepare environment --

# for making graphs
library(ggplot2)

# minimum number of visits to consider a pest significant
MIN_VISITS <- 10

# load pest visits with seasons attached
source(paste0(getwd(), '/scripts/helpers/calc_season.R'))
df <- df_with_seasons()
df <- df[!(df$Pest %in% c('Mixed Grasses and Weeds', 'Algae and Aquatic Weeds', 
                          'Disease of Ornamentals and Turf', 'Woody Vegetation',
                          'Broad-leaved Weeds', 'Grasses', 'Decay Fungi', '', 
                          'Moss')),]

# -- helper functions --

# Determine whether a particular pest is seasonal
# @param - pest: the pest to investigate; a string
# @param - data: the data.frame to analyze; must have $Pest and $Season columns
# @return - a boolean indicating whether this pest is seasonal or not
is_seasonal <- function(pest, data) {
  # count the number of visits in each season
  season_count <- table(data[data$Pest == pest,]$Season)
  # if any seasons are missing, this is definitely seasonal
  if (length(season_count) < length(SEASONS)) return (TRUE)
  # otherwise at least one season must have double the visits of another
  return (max(season_count) / 2 >= min(season_count))
}

# Make a seasonality graph showing relative pest visits between seasons
# @param - data: the data.frame to analyze; must have $Season and $Pest columns
# @param - name: what to title the graph; a string
# @return - a ggplot object with the completed graph
relative_seasonality <- function(data, name) {
  ggplot(data, aes(x = Season)) + 
    # each faceted count-bar chart has an axis relative to only its own data
    geom_bar() + 
    facet_wrap(~Pest, scales = 'free_y', 
               labeller = label_wrap_gen(width = 20)) + 
    # order seasons and shorten their names for a readable x axis
    scale_x_discrete(limits = SEASONS, labels = function(x) substr(x, 1, 3)) +
    labs(title = paste('Pest control visits by pest & season:', name))
}

# -- perform analysis --

# make bar charts showing relative numbers of visits by pest for a location
lapply(unique(df$Installation.Name), function(base) {
  df <- df[df$Installation.Name == base,]
  relative_seasonality(df, base)
  
  # ggsave will attempt to make all plots the same size; stop that
  h <- wrap_dims(length(unique(df$Pest)))[1] * 1.5 + 1.5
  w <- wrap_dims(length(unique(df$Pest)))[2] * 2 + 1
  
  # navigate to the correct folder before saving
  ggsave(paste0(getwd(), '/plots/seasonality/bases/', base, '.png', sep = ''),
         width = w, height = h)
})

# shallow seasonality investigation
relative_seasonality(df, 'all bases')

# select pests which are both common and seasonal
pests <- table(df$Pest)
pests <- names(pests[pests >= MIN_VISITS])
pests <- pests[sapply(pests, is_seasonal, df)]

# another seasonality investigation, with font size pumped up for visibility
relative_seasonality(df[df$Pest %in% pests,], 'all seasonal pests') +
  theme(axis.text = element_blank(), strip.text = element_text(size = 20))