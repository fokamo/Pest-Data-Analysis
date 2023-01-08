# Investigate entries where the pesticide name is missing

# for making graphs
library(ggplot2)

# -- prepare data frames --

# master data frame
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# for shortness of later references
colnames(df)[colnames(df) == 'Installation.Name'] <- 'Place'
df$Missing <- df$Quantity.Concentrate != '' & df$Pesticide.Name == ''
# child data frame with only pesticide-use records
used <- df[df$Quantity.Concentrate != '',]

# aggregated statistics
stats <- aggregate(Missing ~ Place, used, function(x) 100 * mean(x))
colnames(stats) <- c('Place', 'Percent')
# count of entries with a missing name
stats$Missing <- sapply(stats$Place,
                        function(x) sum(used[used$Place == x,]$Missing))
# count of overall pesticide uses
stats$Uses <- sapply(stats$Place, function(x) sum(used$Place == x))
stats$Has <- stats$Missing != 0

# -- visualize --

# order bars by how many entries they have missing a name
stats$Place <- factor(stats$Place, levels = stats$Place[order(stats$Missing)])

# amounts & frequency of missing names, by base
ggplot(stats[stats$Missing != 0,], 
       aes(y = Place, x = Missing, fill = Percent)) + 
  # log-scaled bars make the 1s disappear, so a dot is placed for clarity
  geom_col() + geom_point(color = 'red') + scale_x_log10() +
  # colorblind friendly gradient with a long legend for readability
  scale_fill_viridis_c() + guides(fill = guide_colorbar(barwidth = 20)) +
  labs(title = 'Records recording an amount used but not a pesticide name',
       subtitle = 'Data from 06/2017-05/2022', x = 'Entries', y = '', 
       fill = 'Percent among all entires reporting pesticide use') +
  theme(text = element_text(size = 20), legend.position = 'top')

# order bars by how many entries they have
stats$Place <- factor(stats$Place, levels = stats$Place[order(stats$Uses)])

# amount of pesticide applications & whether any messed up and left out name
ggplot(stats, aes(x = Place, y = Uses, fill = Has)) + 
  geom_col() + geom_point() + scale_y_log10() +
  scale_x_discrete(limits = stats$Place[order(stats$Uses)]) +
  labs(title = 'Are any entries missing a pesticide name?', 
       subtitle = 'Data from 06/2017-05/2022', y = 'Pesticide entries',
       fill = 'Has such an entry') + 
  theme(text = element_text(size = 20), axis.text.x = element_blank())