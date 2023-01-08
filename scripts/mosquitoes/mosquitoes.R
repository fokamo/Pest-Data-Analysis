# Investigate mosquito visits in general

# -- initial charts of data --

# import necessary stuff
source(paste0(getwd(), '/scripts/helpers/chart_maker.R'))
source(paste0(getwd(), '/scripts/helpers/by_month.R'))

# prepare data frame with mosquito data and year/month information
df <- read.csv(paste0(getwd(), '/data/visits.csv'))

# taking only mosquito visits, extract life stage & timing
df <- df[startsWith(df$Pest, 'Mosquitoes'),]
df$Pest <- substring(df$Pest, 14)
colnames(df)[colnames(df) == 'Pest'] <- 'Stage'
df <- extract_time(df)

# combine nearby bases to be consistent with later analysis
df$Installation.Name <- ifelse(
  startsWith(df$Installation.Name, 'b'), 'B', df$Installation.Name)

# Print a properly subtitled pie chart
# @param - data: the data to chart; a data.frame
# @param - facet_by: the variable to associate with pies (splits up the pie
#                    charts); a string. Must be a column in data
# @param - color_by: the variable to associate with colors (splits up the slices
#                    and labels the legend); a string. Must be a column in data
# @param - plot_title: what to title the plot; a string
# @param - pie_cols: the number of columns of pies to use; defaults to NULL.
#                    Must be a valid value for facet_wrap's ncol parameter
# @return - a ggplot object with faceted pie charts made to order
sub_pies <- function(data, facet_by, color_by, plot_title) {
  print(pies(data, facet_by, color_by, plot_title) + 
          labs(subtitle = 'Data from 6/2017-5/2022; all bases'))
}

# Make & print an array of exploratory pie charts
make_charts <- function() {
  sub_pies(df, 'Stage', 'Installation.Name', 'Bases with mosquito visits')
  sub_pies(df, 'Month', 'Operation', 'Operations conducted for mosquitoes')
  sub_pies(df, 'Stage', 'Operation', 'Operations conducted by life stage')
  sub_pies(df, 'Operation', 'Stage', 'Operations conducted by life stage')
  sub_pies(df, 'Month', 'Stage', 'Mosquito life stages targeted by month')
}

make_charts()

# -- remove contradictory entries --

# 4 cols (X1, X2, X3, X4) where each row means:
# all rows in df with X2 in X1 should have X4 in X3
correct <- data.frame(matrix(data = c(
  c('Operation', 'Larviciding', 'Stage', 'Larval/pupal'),
  c('Operation', 'Service Bait Station - Bait added', 'Operation', 'Baiting'),
  c('Pesticide.Name', 'VECTOBAC G BIOLOGICAL LARVICIDE GRANULES', 
    'Operation', 'Larviciding'),
  c('Pesticide.Name', 'KONTROL 4-4', 'Stage', 'Adult'),
  c('Pesticide.Name', 'KONTROL 4-4', 'Operation', 'Fog/ULV'),
  c('Pesticide.Name', 'Talon G', 'Stage', 'Rats'),
  c('Pesticide.Name', 'Altosid Briquets', 'Stage', 'Larval/pupal'),
  c('Active.Ingredient', 'METHOPRENE', 'Stage', 'Larval/pupal'),
  c(sapply(unique(grep('bacillus', df$Active.Ingredient, 
                       ignore.case = TRUE, value = TRUE)),
           function(x) c('Active.Ingredient', x, 'Stage', 'Larval/pupal')))
), ncol = 4, byrow = TRUE))

# all rows in df with X2 in X1 should not have X4 in X3
wrong <- data.frame(matrix(data = c(
  c('Pesticide.Name', 'AQUABACxt', 'Operation', 'Baiting'),
  c('Pesticide.Name', 'In2Mix', 'Operation', 'Larviciding')
), ncol = 4, byrow = TRUE))

# save the initial state so a comparison can be made after cleaning up data
pre_df <- table(df$Installation.Name)

# go through each rule in correct and throw out violating columns
invisible(apply(correct, 1, function(x) {
  # TRUE for columns which violate this rule, FALSE for those to be kept
  violators <- df[[x[1]]] == x[2] & df[[x[3]]] != x[4]
  df <<- df[!violators,]
}))

# similar process for wrong
invisible(apply(wrong, 1, function(x) {
  violators <- df[[x[1]]] == x[2] & df[[x[3]]] == x[4]
  df <<- df[!violators,]
}))

# -- fix things where the correct entry is known for sure --

# these two entries have comments indicating they are for traps
df[df$Operation == 'Other: Non-Chemical Control *' 
   & df$Installation.Name == 'B',
   ]$Operation <- 'Service Trap/Bait Station - No bait added *'
# the comments indicate this was servicing a trap
df[df$Operation == 'Equipment Maintenance *',
   ]$Operation <- 'Service Trap/Bait Station - No bait added *'
# these two entries have comments indicating they are for setting traps
df[df$Operation == 'Mechanical/Manual Control *' & grepl('trap', df$Comments),
   ]$Operation <- 'Service Trap/Bait Station - No bait added *'
# the comments and formulation indicate this was an aerosol can
df[df$Operation == 'Fumigation',]$Operation <- 'Manual Pesticide Application'

# now the surveillance traps have been corralled, dump into Surveillance
df[df$Operation == 'Service Trap/Bait Station - No bait added *',
   ]$Operation <- 'Surveillance/Monitoring *'
# these traps are actually for control
df[df$Pesticide.Name == 'In2Mix',
   ]$Operation <- 'Service Trap/Bait Station - No bait added *'

# these traps kill the adults which enter, as well as larvae born from them
df[df$Pesticide.Name == 'In2Mix',]$Stage <- 'All'

# make the same exploratory charts again to compare with before data cleaning
make_charts()
# make finalized dot-line chart
dot_lines(df, 'Operation', 'Mosquito operations conducted (post data cleaning)')

# -- analyze bases which had entries thrown out for self-contradictions --

# make a table of all the bases which had entries thrown out for contradicting
removed <- pre_df - table(df$Installation.Name)
removed <- removed[removed != 0]

# pie chart illustrating which bases had entries removed
pies(data.frame(Place = rep(names(removed), removed), Removed = TRUE),
     'Removed', 'Place', 'Bases with visits removed for data entry errors') +
  theme(strip.text = element_blank())

# bar chart illustrating where and what share of entries were removed
ggplot(as.data.frame(100 * removed / pre_df[names(pre_df) %in% names(removed)]), 
       aes(x = Var1, y = Freq)) + geom_col() + 
  labs(title = 'Percentage of visits which were removed for data entry errors',
       y = 'Percent', x = 'Place')