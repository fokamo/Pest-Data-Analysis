# Flex chart_maker.R to explore data split up by location

# for custom charts
source(paste0(getwd(), '/scripts/helpers/chart_maker.R'))

# -- set-up --

# reasons to visit from the data dump that aren't pests
IGNORE <- c('', 'Mixed Grasses and Weeds', 'Woody Vegetation', 'Decay Fungi',
            'Disease of Ornamentals and Turf', 'Broad-leaved Weeds', 'Grasses',
            'Moss', 'Algae and Aquatic Weeds')
# a string to use to indicate all bases should be included in analysis
ALL <- 'all bases'
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# take visits for known pests
df <- df[!(df$Pest %in% IGNORE),]
# combine mosquito life stages
df$Pest <- ifelse(startsWith(df$Pest, 'Mosquitoes'), 'Mosquitoes', df$Pest)

# -- analysis functions --

# Make and save pie charts displaying which pests were surveilled for at a base
# @param - base: the base to analyze; a string
surv_pies <- function(base) {
  pies(df[df$Operation == 'Surveillance/Monitoring *' &
            df$Installation.Name == base,], 'Site', 'Pest',
       paste('Sites with surveillance visits at', base))
  
  ggsave(paste0(getwd(), '/plots/surv_sites/', base, '.png'))
}

# Make and save a dot plot displaying when pests were surveilled for at a base
# @param - base: the base to analyze; a string
surv_dots <- function(base) {
  dot_lines(df[df$Operation == 'Surveillance/Monitoring *' &
                 df$Installation.Name == base,], 'Pest',
            paste('Surveillance visits at', base))
  
  # force reasonable sizing for dot plot
  ggsave(paste0(getwd(), '/plots/surv_dots/', base, '.png'),
         width = 30, height = 12, units = 'cm')
}

# Make and save a pie chart displaying what was done to which pests at a base
# @param - base: the base to analyze; a string
all_ops <- function(base) {
  pies(df[df$Installation.Name == base,], 'Pest', 'Operation',
       paste('Operations conducted at', base))
  
  ggsave(paste0(getwd(), '/plots/pest_ops/', base, '.png'))
}

# Make and save a dot plot displaying when pest visits occurred at a base
# @param - base: the base to analyze; a string
all_dots <- function(base) {
  dot_lines(df[df$Installation.Name == base,], 'Pest',
            paste('All visits at', base))
  
  # force reasonable sizing for dot plot
  ggsave(paste0(getwd(), '/plots/overall_dots/', base, '.png'),
         width = 30, height = 12, units = 'cm')
}

# Make and save all of the above plots for a base
# @param - base: the base to analyze; a string
all_plots <- function(base) {
  # only run the surveillance plots if there is surveillance
  if (dim(df[df$Operation == 'Surveillance/Monitoring *' & 
             df$Installation.Name == base,])[1] != 0) {
    surv_pies(base)
    surv_dots(base)
  }
  all_ops(base)
  all_dots(base)
}

# -- actual analysis calling --

sapply(unique(df$Installation.Name), all_plots)

# run the same thing but treat all bases as the same
df$Installation.Name <- ALL
all_plots(ALL)