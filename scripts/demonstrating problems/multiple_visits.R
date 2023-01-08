# Investigate what is being entered differently in multiply-reported visits

# -- set up --

df <- read.csv(paste0(getwd(), '/data/visits.csv'))

# columns assumed to be constant for all entries of a single visit
ID_COLS <- c('Application.Date', 'Installation.Name', 'Applicator.Name')
# columns which might vary between entries of a single visit
VARY_COLS <- c('Facility', 'Operation', 'Site', 'Pest', 'Pesticide.Name')
# the plurals for these are not regular, so this is a lookup table
VARY_PLURALS <- setNames(c('Facilities', 'Operations', 'Sites', 'Pests', 
                           'Pesticide names'), VARY_COLS)

# Count the number of unique values in a certain column for each visit
# @param - col: the column in question; a string of a column name in df
# @return - a data.frame with each ID'ed visit given a unique-value count
count_per_visit <- function(col) {
  # count the number of unique values for each ID'ed visit
  per <- aggregate(formula(paste(col, '~', paste(ID_COLS, collapse = ' + '))), 
                   df, function(x) length(unique(x)))
  # fix up column names
  per <- setNames(per, c(ID_COLS, 'Unique'))
  # for grouping after all the dataframes are combined
  per$Col <- col
  # return value
  per
}

# -- perform analysis --

# count number of duplicate entries per visit
entries_per_visit <- aggregate(formula(paste('Date.Entered', '~', 
                                             paste(ID_COLS, collapse = ' + '))), 
                               df, length)
colnames(entries_per_visit) <- c(ID_COLS, 'Entries')
# count number of visits per number of duplicate entries
entries_per_visit <- aggregate(Application.Date ~ Entries, 
                               entries_per_visit, length)
colnames(entries_per_visit) <- c('Entries', 'Visits')

# bar chart with frequency as height, and dots at top of bar for logged 0 vs 1
ggplot(entries_per_visit, aes(x = Entries, y = Visits)) + 
  geom_col() + geom_point(color = 'red') + scale_y_log10() +
  labs(title = 'Number of times a visit was entered', x = 'Duplicate entries') + 
  theme(text = element_text(size = 20))

# count per visit for all VARY_COLS, then merge all those dataframes top-down
per_visit <- do.call(rbind, lapply(VARY_COLS, count_per_visit))

# make a histogram for the unique values per visit
ggplot(per_visit, aes(x = Unique)) + geom_histogram(stat = 'count') +
  # separate facets for each of the columns, and log scale on y for readability
  facet_wrap(~Col, scales = 'free_x') + scale_y_log10() +
  labs(title = 'Number of unique X in a one day visit', 
       x = 'Number of unique X', y = 'Visits')

# count per visit for all VARY_COLS, then merge all those dataframes left-right
per_visits <- lapply(VARY_COLS, function(x) count_per_visit(x)[,c(1:4)])
# get rid of the id columns for all but the first dataframe
per_visits[2:5] <- lapply(per_visits[2:5], function(x) x[,4])
per_visit <- do.call(cbind, per_visits)
# column names get messed up
colnames(per_visit) <- c(ID_COLS, VARY_COLS)

# for all sets of two VARY_COLS...
lapply(combn(VARY_COLS, 2, simplify = FALSE), function(cols) {
  # temporarily rename these columns to play nice with ggplot
  colnames(per_visit)[colnames(per_visit) == cols[1]] <- 'Var1'
  colnames(per_visit)[colnames(per_visit) == cols[2]] <- 'Var2'
  
  # count occurrences of each Var1-Var2 value combo
  ggplot(data = aggregate(Application.Date ~ Var1 + Var2, per_visit, length),
         # put Var1 and Var2 on the axes, and size is by count of occurrences
         mapping = aes(x = Var1, y = Var2, size = Application.Date)) + 
    # plot all points with their sizes, scaling size by log10 to show variation
    geom_point() + scale_size(trans = 'log10') + 
    # fix all the labels to correspond to this particular column combo
    labs(title = paste(VARY_PLURALS[cols[1]], '&', VARY_PLURALS[cols[2]], 
                       'in one visit'), 
         x = cols[1], y = cols[2], size = 'Visits')
  
  # autogenerate filename and save
  ggsave(paste0(getwd(), '/plots/multi_visit/', cols[1], '-', cols[2], '.png'))
})