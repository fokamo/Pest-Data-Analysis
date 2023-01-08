# Visualize how pest control actions have changed over the years

# for making graphs
library(ggplot2)

# -- prepare environment --

# Very Important Pests - zoomed in on later in the analysis
VIPs <- c('Ants, nuisance', 'Bees', 'Birds', 'Cockroaches', 'Fruit Flies', 
          'Mice', 'Mosquitoes - Adult', 'Mosquitoes - Larval/pupal', 'Rats')
# identity columns: if the same between two records, they're from the same visit
ID_COLS <- c('Application.Date', 'Installation.Name', 'Applicator.Name', 'Pest')
# a string to use to indicate all bases should be included in analysis
ALL <- 'all bases'
# reasons to visit from the data dump that aren't pests
IGNORE <- c('', 'Mixed Grasses and Weeds', 'Woody Vegetation', 'Decay Fungi',
            'Disease of Ornamentals and Turf', 'Broad-leaved Weeds', 'Grasses',
            'Moss', 'Algae and Aquatic Weeds')

# load data & extract year; this is all the analysis needs
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
df$Year <- format(as.Date(df$Application.Date, format = '%m/%d/%Y'), '%y')
df <- df[!(df$Pest %in% IGNORE),]

# -- workhorse function --

# Plot visits by year with different pests separated into facets
# @param - base: the base to focus on; a string
plot_years <- function(base) {
  # locally load in data for just this base
  df <- df[df$Installation.Name == base,]
  # ggsave will attempt to make all plots the same size; stop that
  h <- wrap_dims(length(unique(df$Pest)))[1] * 3.5 + 4
  w <- wrap_dims(length(unique(df$Pest)))[2] * 5.5 + 3
  
  # graph counting data-line record separately
  ggplot(df, aes(x = Year)) + 
    # split up graphs by pest and give them different y axes
    geom_bar() + facet_wrap(~Pest, scales = 'free_y') +
    labs(title = 'Number of pest records', x = 'Year', y = 'Records',
         subtitle = paste('Data from 06/2017-05/2022; 2017 and 2022', 
                          'only have half a year of data'))
  
  ggsave(paste0(getwd(), '/plots/pest_years/record_count/', base, '.png'),
         width = w, height = h, units = 'cm')
  
  # remove multiple records which refer to the same day
  df <- df[!duplicated(df[,ID_COLS]),]
  
  # graph counting visits per location-person-days
  ggplot(df, aes(x = Year)) + 
    geom_bar() + facet_wrap(~Pest, scales = 'free_y') +
    labs(title = 'Number of location-person-days of pest visits', 
         x = 'Year', y = 'Records',
         subtitle = paste('Data from 06/2017-05/2022; 2017 and 2022', 
                          'only have half a year of data'))
  ggsave(paste0(getwd(), '/plots/pest_years/unique_days/', base, '.png'),
         width = w, height = h, units = 'cm')
}

# -- perform actual analysis --

# make graphs for each base individually (this line takes a while)
sapply(unique(df$Installation.Name), plot_years)

# make aggregated graph
df$Installation.Name <- ALL
plot_years(ALL)

# make aggregated graph zoomed in on VIPs
df <- df[df$Pest %in% VIPs,]
df$Installation.Name <- paste(ALL, '(VIPs)')
plot_years(paste(ALL, '(VIPs)'))