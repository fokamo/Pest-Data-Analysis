# Visualize some pests' visits which are concentrated to one base/time

# -- set up --

# used for time extraction
source(paste0(getwd(), '/scripts/helpers/by_month.R'))
# used for miscellaneous combination
source(paste0(getwd(), '/scripts/helpers/chart_maker.R'))

# pests whose graphs I found funny
PESTS <- c('Bees', 'Spiders', 'Other Pests', 'Crickets', 'Fleas',
           'Raccoons', 'Squirrels', 'Darkling Beetles')

df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# filter for only these pests, and process those for time information
df <- extract_time(df[df$Pest %in% PESTS,])

# -- convenience functions -- 

# Plot a single pest across all dates in the data
# @param - pest: which pest to plot data for; a string present in df$Pest
# @param - qualifier: what is being plotted on the y axis; a string. Must work
#                     as a y axis label and in title as "X for [pest] visits"
plot_single <- function(pest, qualifier) {
  # filter for only this pest, combine low-incidence locations into misc.
  ggplot(combine_misc(df[df$Pest == pest,], 'Installation.Name', 
                      length(COLORBLIND_PALETTE)), 
         # time is x axis, stacked bars split up by place, facets by years
         aes(x = Month, fill = Installation.Name)) + facet_wrap(~Year) +
    # bar chart with heights determined via count, stacked colors from palette
    geom_bar() + scale_fill_manual(values = COLORBLIND_PALETTE) +
    # keep order of bars the same, but only label every third for space reasons
    scale_x_discrete(limits = month.name, labels = EVERY_THIRD_MONTH) +
    # label so the final chart has enough context to be understandable
    labs(title = paste0(qualifier, ' for "', pest, '" visits'),
        x = 'Date', y = qualifier, fill = 'Place')
  ggsave(paste0(getwd(), '/plots/concentrated/', qualifier, pest, '.png'),
         width = 20, height = 10, units = 'cm')
}

# Plot all funny pests across all dates in the data
# @param - qualifier: what is being plotted on the y axis; a string. Must work
#                    in title as "X for funny pests"
plot_all <- function(qualifier) {
  # combine low-incidence locations into misc.
  ggplot(combine_misc(df, 'Installation.Name', length(COLORBLIND_PALETTE)), 
         # time is x axis, stacked bars split up by place
         aes(x = Month, fill = Installation.Name)) + 
    # the grid of plots has different pests going up-down and years left-right
    facet_grid(rows = vars(Pest), cols = vars(Year), scales = 'free_y',
               # splits the pest names onto multiple lines if too long
               labeller = label_wrap_gen(width = 2, multi_line = TRUE)) +
    # bar chart with heights determined via count, stacked colors from palette
    geom_bar() + scale_fill_manual(values = COLORBLIND_PALETTE) +
    # keep order of bars the same, but only label every third for space reasons
    scale_x_discrete(limits = month.name, labels = EVERY_THIRD_MONTH) +
    # label so the final chart has enough context to be understandable
    labs(title = paste(qualifier, 'for funny pests'),
         x = 'Month', y = qualifier, fill = 'Place')
  ggsave(paste0(getwd(), '/plots/concentrated/', qualifier, ' all.png'), 
         width = 25, height = 15, units = 'cm')
}

# Run all the plot-making functions on df as it stands
# @param - qualifier: what qualifier to pass along to plot_single and plot_all
run_plots <- function(qualifier) {
  sapply(PESTS, plot_single, qualifier)
  plot_all(qualifier)
}

# Plot visits for a particular pest over two months
# @param - pest: which pest to plot data for; a string present in df$Pest
# @param - year: the year to zoom in on; a string present in df$Year
# @param - month1: one of the months to zoom in on; a string present in df$Month
# @param - month2: the other month to zoom in on; a string present in df$Month
plot_two_months <- function(pest, year, month1, month2) {
  # filter for only this pest/time, combine low-incidence locations into misc.
  ggplot(combine_misc(df[df$Pest == pest & df$Year == year 
                         & df$Month %in% c(month1, month2),], 
                      'Installation.Name', length(COLORBLIND_PALETTE)), 
         # time is x axis, stacked bars split up by place
         aes(x = Date, fill = Installation.Name)) + 
    # bar chart with heights determined via count, stacked colors from palette
    geom_bar() + facet_wrap(~Month, scales = 'free_x') + 
    scale_fill_manual(values = COLORBLIND_PALETTE) + 
    # substring the automatic date labels to leave only day number behind
    scale_x_date(labels = function(x) substr(x, 9, 10)) +
    # label so the final chart has enough context to be understandable
    labs(title = paste0('Entries for "', pest, '"'), 
         subtitle = paste(month1, 'and', month2, year, 
                          'had an unusual number of visits entered for', pest), 
         x = 'Day', y = 'Entries', fill = 'Place')
  ggsave(paste0(getwd(), '/plots/concentrated/two-', pest, '.png'),
         width = 20, height = 10, units = 'cm')
}

# initially plot by entries
run_plots('Entries')
plot_two_months('Spiders', '2020', 'January', 'February')
plot_two_months('Squirrels', '2017', 'September', 'October')

# combine all entries which occurred on the same day
df <- df[!duplicated(df[,c('Application.Date', 'Installation.Name', 'Pest')]),]
run_plots('Unique days')