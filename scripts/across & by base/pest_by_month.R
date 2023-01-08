# Plot visits for a particular reason by location

# for plotting the by-month graphs
source(paste0(getwd(), '/scripts/helpers/by_month.R'))

# identity columns: if the same between two records, they're from the same visit
ID_COLS <- c('Application.Date', 'Installation.Name', 'Applicator.Name')

# -- prepare master data frame --

# overall data
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# set up data for time analysis
df <- extract_time(df)
# there's only one day of this so it messes up the analysis
df <- df[!(df$Month == 'June' & df$Year == '2022'),]
# determine all months represented
all_months <- unique(df[, c('Year', 'Month')])

# -- actually make plots --

# Plot visits over time for a particular place
# @param - data: the data to plot; a data.frame. Must have Year, Month (using
#                English month names) columns
# @param - name: the name to give to this plot; a string
# @param - folder: the folder to save this plot into; a string
plot_place <- function(data, name, folder) {
  # count visits by year and month to feed plot_by_month function
  all_months$y <- apply(all_months, 1, 
                        function(x) length(unique(data[data$Year == x[1]
                                                       & data$Month == x[2],
                                                       ]$Application.Date)))
  # plot with raw number of visits
  plot_by_month(all_months, paste('Entries for', folder, 'at', name), 'Count') +
    labs(subtitle = 'Data from 06/2017-05/2022')
  ggsave(paste0(getwd(), '/plots/', folder, '/', name, '.png'), 
         width = 20, height = 15, units = 'cm')
  
  data <- data[!duplicated(data[,ID_COLS]),]
  # plot with unique visit days
  plot_by_month(all_months, paste('Days with entries for', folder, 'at', name), 
                'Count') + labs(subtitle = 'Data from 06/2017-05/2022')
  ggsave(paste0(getwd(), '/plots/', folder, '/unique_days/', name, '.png'), 
         width = 20, height = 15, units = 'cm')
}

# Plot visits by month & year for some specific pests
# @param - pest: the pest to plot for; a string 
# @param - folder: the folder to save plots into; a string
# @param - min_visits: the minimum visits a place should have by these pests;
#                      an integer. Defaults to 10
pest_by_month <- function(pest, folder, min_visits = 10) {
  # can safely subset within here since it won't change the global permanently
  df <- df[df$Pest == pest,]
  
  # select places which had at least the minimum number of visits for this pest
  places <- table(df$Installation.Name)
  places <- names(places)[places > min_visits]
  
  # plot each base separately
  sapply(places, 
         function(x) plot_place(df[df$Installation.Name == x,], x, folder))
  # plot overall
  plot_place(df, 'all bases', folder)
}

pest_by_month('All Pests (Surveillance only)', 'Surveillance', 1)
df[startsWith(df$Pest, 'Mosquitoes'),]$Pest <- 'Mosquitoes'
pest_by_month('Mosquitoes', 'Mosquito')
df <- df[df$Operation == 'Surveillance/Monitoring *',]
pest_by_month('Mosquitoes', 'Mosquito Surveillance')