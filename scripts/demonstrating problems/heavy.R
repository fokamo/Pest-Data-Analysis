# Find "heavy hitters" - bases with more than half of one pest's records

# for making the graph
library(ggplot2)

# -- build heavy-hitter data frame --

# master data frame
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# allows indexing into the table to work
df[df$Pest == '',]$Pest <- ' '
count <- table(df$Pest)

hh <- data.frame(Pest = unique(df$Pest))
# location with majority of visits
hh$Loc <- sapply(hh$Pest, function(x)
  names(which.max(table(df[df$Pest == x,]$Installation.Name)))[1])
# share of visits for this pest which went to majority location
hh$Share <- apply(hh, 1, function(x) 
  sum(df[df$Pest == x[1],]$Installation.Name == x[2]) / count[x[1]])

# -- visualize significant heavy hitters --

# heavy hitters must bre at least half of a significant number of visits
hh <- hh[hh$Share > .5,]
hh <- hh[sapply(hh$Pest, function(x) count[x] > 50),]

# fix labels to include location and show up in increasing order
hh$Pest <- paste0(hh$Pest, '\n(', hh$Loc, ')')
hh$Pest <- factor(hh$Pest, levels = hh$Pest[order(hh$Share)])

# partially filled pie charts
ggplot(hh, aes(x = '', y = Share)) + facet_wrap(~Pest) +
  geom_bar(stat = 'identity', width = 1, color = 'white') + 
  coord_polar('y', start = 0) + theme_void() +
  labs(title = 'Common pests where one base makes up a majority of the entries', 
       subtitle = 'Data from 06/2017-05/2022; pests with at least 50 entries') + 
  # centered title works nicely with de-themed pies
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))