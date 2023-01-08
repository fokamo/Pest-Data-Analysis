# Visualize the most common pest visit by location and season

# extract season from data
source(paste0(getwd(), '/scripts/helpers/calc_season.R'))
# pre-made background map with base locations
source(paste0(getwd(), '/scripts/helpers/base_map.R'))

# -- constants -- 

# reasons to visit from the data dump that aren't pests
IGNORE <- c('', 'Mixed Grasses and Weeds', 'Woody Vegetation', 'Decay Fungi',
            'Disease of Ornamentals and Turf', 'Broad-leaved Weeds', 'Grasses',
            'Moss', 'Algae and Aquatic Weeds', 'All Pests (Surveillance only)')
# for ease of reading the map
COLORBLIND_PALETTE <- c('#E69F00', '#56B4E9', '#009E73', '#0072B2', '#D55E00')

# -- set up master data frame --

# load pest visits with seasons attached
df <- df_with_seasons()
df <- df[!(df$Pest %in% IGNORE),]

# aggregate across base and season
loc_season <- aggregate(Pest ~ Installation.Name + Season, df, 
                        # use table-count to find most common pest
                        function(x) names(which.max(table(x))))
# grab location for plotting purposes
loc_season$Lat <- sapply(loc_season$Installation.Name, 
                         function(x) locs[locs$Place == x,]$Lat)
loc_season$Long <- sapply(loc_season$Installation.Name, 
                          function(x) locs[locs$Place == x,]$Long)

# set up the 15 different point designs needed
pests <- unique(loc_season$Pest)
pests <- pests[order(pests)]
# assign a different color-shape combination to each pest
pest_shapes <- setNames(c(rep(16, 5), rep(15, 5), rep(17, 5)), pests)
pest_colors <- setNames(rep(COLORBLIND_PALETTE, 3), pests)

# -- make maps --

# getting rid of nonpests removed the eastmost base, so the map can be shrunk
base_map(xlims = c(0, 1)) + 
  geom_point(data = loc_season, aes(color = Pest, shape = Pest), size = 3) +
  # these larger maps have to be all side-by-side
  facet_wrap(~Season, ncol = 4) +
  # identical names & labels mean the colors and shapes will both affect points
  scale_color_manual(name = 'Pest', labels = pests, values = pest_colors) +   
  scale_shape_manual(name = 'Pest', labels = pests, values = pest_shapes) +
  # give context to the map
  labs(title = 'Most common reason for a pest visit') +
  # no point in having lat/long labels for the axes
  theme(axis.title = element_blank())

# take only data from northern bases
loc_season <- loc_season[loc_season$Lat > 0,]
# use only the point shape/colors associated with the reduced list of pests
pests <- unique(loc_season$Pest)
pest_shapes <- pest_shapes[pests]
pest_colors <- pest_colors[pests]

# zoom into only the northern bases
base_map(xlims = c(0, 1), ylims = c(0, 1)) + 
  geom_point(data = loc_season, aes(color = Pest, shape = Pest), size = 3) +
  # these smaller maps are best in the default 2-by-2 square wrap
  facet_wrap(~Season) +
  scale_color_manual(name = 'Pest', labels = pests, values = pest_colors) +   
  scale_shape_manual(name = 'Pest', labels = pests, values = pest_shapes) +
  labs(title = 'Most common reason for a pest visit') +
  theme(axis.title = element_blank())

# zoom in on a small area in Washington for presentation elsewhere
loc_season <- loc_season[loc_season$Lat > 0 & loc_season$Long < 0,]

# zoom into only the northern bases
base_map(xlims = c(0, 1), ylims = c(0, 1)) + 
  geom_point(data = loc_season, aes(color = Pest), size = 3) +
  # stop worrying about using the same key as earlier maps
  facet_wrap(~Season) + scale_color_manual(values = COLORBLIND_PALETTE) + 
  labs(title = 'Most common reason for a pest visit') +
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank())