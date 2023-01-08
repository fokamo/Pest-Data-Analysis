# Determine the season given dates fall under
# Meant for further use:
# - SEASONS: all the season names in order (from Winter), similar to month.name
# - df_with_seasons(): load visits.csv with all entries' seasons pre-determined

# -- constants -- 

# this is a leap year so any date can be converted to it
STANDARD_YEAR <- 2012
# Winter/Summer Solstices, Spring/Fall Equinoxes
WS <- as.Date(paste(STANDARD_YEAR, '12-15', sep = '-'))
SE <- as.Date(paste(STANDARD_YEAR, '03-15', sep = '-'))
SS <- as.Date(paste(STANDARD_YEAR, '06-15', sep = '-'))
FE <- as.Date(paste(STANDARD_YEAR, '09-15', sep = '-'))
# similar to month.name, but for seasons
SEASONS <- c('Winter', 'Spring', 'Summer', 'Fall')

# -- determining season --

# Categorize dates by season via solstice/equinox definition
# @param - day: a Date object
# @return - the season which day falls under; a string
determine_season <- function(day) {
  # standardized year to compare with the solstice/equinox constants
  d <- as.Date(strftime(day, format = paste(STANDARD_YEAR, '%m-%d', sep = '-')))
  # determine the correct season index
  SEASONS[ifelse (d >= WS | d < SE, 1,
                  ifelse (d >= SE & d < SS, 2,
                          ifelse (d >= SS & d < FE, 3, 4)))]
}

# Load visits.csv with an added $Season column, determined from Application.Date
# @return - a data.frame exactly like visits.csv except for an added $Season col
df_with_seasons <- function() {
  df <- read.csv(paste0(getwd(), '/data/visits.csv'))
  # have to convert date to correct object format before determining season
  df$Season <- sapply(as.Date(df$Application.Date, format = '%m/%d/%y'), 
                      determine_season)
  df
}