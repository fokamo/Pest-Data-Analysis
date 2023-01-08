# Disconnected, random exploration of what this data could do

# load all visits where the pest is known
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
df <- df[df$Pest != '',]

# -- mosquito percentage mapping --

# for making maps on top of
source(paste0(getwd(), '/scripts/helpers/base_map.R'))

# calculate visits to each base which were for mosquitoes
locs$Mos <- sapply(locs$Place, 
                   function(x) sum(df[startsWith(df$Pest, 'Mosquitoes'),
                                      ]$Installation.Name == x))

# map areas by their mosquito percentage
base_map() + 
  geom_point(aes(size = Mos), data = locs[locs$Mos > 0,], color='red') +
  labs(title = 'Visits which were for mosquitoes')
# same map, with outliers removed to better see variation
base_map() +
  geom_point(aes(size = Mos), color = 'red',
             data = locs[locs$Mos > 0 & locs$Mos < 1000,]) +
  labs(title = 'Visits which were for mosquitoes')

# combining mosquitoes within df
df$Pest <- ifelse(startsWith(df$Pest, 'Mosquito'), 'Mosquito', df$Pest)

# -- most common pest mapping --

# remove nonpest visits, then remove any bases which have no visits left
df <- df[!(df$Pest %in% 
             c('Mixed Grasses and Weeds', 'Algae and Aquatic Weeds', 'Grasses', 
               'Disease of Ornamentals and Turf', 'Broad-leaved Weeds', 'Moss',
               'All Pests (Surveillance only)', 'Woody Vegetation', 
               'Decay Fungi')),]
locs <- locs[locs$Place %in% df$Installation.Name,]

# calculate the most common reason to visit each base
locs$Common <- sapply(locs$Place, 
                      function(x) 
                        names(which.max(table(
                          df[df$Installation.Name == x,]$Pest))))

# map out by most common pest - both all bases and state-by-state
base_map() + 
  geom_point(aes(fill = Common, color = Common), data = locs) +
  labs(title = 'Most common reason for a pest visit')

# zoom into only the northern bases
base_map(xlims = c(0, 1), ylims = c(0, 1)) + 
  geom_point(aes(fill = Common, color = Common), data = locs[locs$Lat > 42,]) +
  labs(title = 'Most common reason for a pest visit') +
  theme(axis.title = element_blank())

# zoom into only the southern bases
base_map(xlims = c(0, 1), ylims = c(0, 1)) + 
  geom_point(aes(fill = Common, color = Common), data = locs[locs$Lat < 42,]) +
  labs(title = 'Most common reason for a pest visit') +
  theme(axis.title = element_blank())