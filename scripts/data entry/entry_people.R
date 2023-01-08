# Investigate when and for when individual people enter visits

# -- prepare environment --

# for making graphs
library(ggplot2)

# load data & timing information
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
df$Application.Date <- as.Date(df$Application.Date, format = '%m/%d/%Y')
df$Date.Entered <- as.Date(df$Date.Entered, format = '%m/%d/%Y')
df$Lag <- as.numeric(df$Date.Entered - df$Application.Date)

# select only people who made at least 25 entries
people <- table(df$Entered.by)
people <- names(people[people > 25])

# -- actual analysis --

# this will take a while
sapply(people, function(name) {
  # prepare counts of application & entry dates for this specific person
  df <- df[df$Entered.by == name,]
  app <- as.data.frame(table(df$Application.Date))
  app$Type <- 'Application Date'
  ent <- as.data.frame(table(df$Date.Entered))
  ent$Type <- 'Entry Date'
  
  # create nicely formatted data for plotting
  points <- setNames(rbind(app, ent), c('Date', 'Records', 'Type'))
  points$Year <- format(as.Date(points$Date), '%Y')
  # converting all dates to the same year allows standard plot limits
  points$Date <- as.Date(strftime(points$Date, format = '2020-%m-%d'))
  
  # base dot plot with each pest's dots on its own horizontal line
  ggplot(points, aes(x = Date, y = '', size = Records)) + geom_point() +
    # standardize the x bounds so spacing between dots is directly comparable
    scale_x_date(limits = c(as.Date('2020-01-01'), as.Date('2020-12-31')),
                 # shortened month names for x axis labels
                 date_labels = '%b') + facet_grid(Type ~ Year) +
    # axis labels are just distracting
    labs(title = paste('Visits entered by', name), x = '', y = '')
  ggsave(paste0(getwd(), '/plots/lag/people/', name, '.png'),
         # dynamically change width for plots with different numbers of years
         width = 6 * length(unique(points$Year)) + 3, height = 10, units = 'cm')
})