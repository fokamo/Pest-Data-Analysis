# Visualize locations where contracts were used, and their average lag times

# data to perform analysis on & map to plot analysis on
source(paste0(getwd(), '/scripts/helpers/base_map.R'))
df <- read.csv(paste0(getwd(), '/data/visits.csv'))

# -- building up dataframe --

# for ease of use
colnames(df)[colnames(df) == 'Installation.Name'] <- 'Place'
colnames(df)[colnames(df) == 'Contract...In.House'] <- 'Contract'
# convert to date objects to utilize subtraction
df$Lag <- as.numeric(as.Date(df$Date.Entered, format = '%m/%d/%Y') 
                     - as.Date(df$Application.Date, format = '%m/%d/%Y'))

# build up all information about visits under each kind of contract at bases
place_contract <- aggregate(Lag ~ Place + Contract, df, mean)
colnames(place_contract) <- c('Place', 'Contract', 'Lag')
# percentage of visits under this contract
place_contract$Percent <- apply(place_contract, 1, 
                                function(x) 
                                  sum(df[df$Place == x[1],]$Contract == x[2]) 
                                / sum(df$Place == x[1]))
# fetch coordinates so as to plot on map
place_contract$Long <- sapply(place_contract$Place, 
                              function(x) locs[locs$Place == x,]$Long)
place_contract$Lat <- sapply(place_contract$Place, 
                            function(x) locs[locs$Place == x,]$Lat)

# -- making maps --

# visualizing both percent of visits and average lag
base_map() +
  geom_point(aes(x = Long, y = Lat, size = Percent, color = Lag), 
             data = place_contract) +
  # separate each kind of contract's dots
  facet_wrap(~Contract) +
  labs(title = 'Pest-control visits on military bases by kind of contract', 
       size = '% of visits under this contract',
       color = 'avg. lag between visits & data entry')

# visualizing where each kind of contract is
base_map() +
  geom_point(aes(x = Long, y = Lat), data = place_contract, color = 'red') +
  facet_wrap(~Contract) +
  labs(title = 'Locations of contract use for pest control visits')