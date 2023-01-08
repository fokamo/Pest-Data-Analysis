# Investigate products of the "Negative Report" check-box

# -- prepare environment --

# for using regex on $Comments
library(stringr)
# for simple chart-making
library(ggplot2)

# regexes for use later
VALID_YEAR <- '20[12][789012]'
VALID_MONTH <- '([A-Za-z]{3})'

# -- prepare data frame --

# load in data, extracting just the negative reports
df <- read.csv(paste0(getwd(), '/data/visits.csv'))
df <- df[grepl('No pesticides were used for the month of', df$Comments),]
print(paste(dim(df)[1], 'negative reports recorded'))

# process entry date into separate components for ease of use
df$Date.Entered <- as.Date(df$Date.Entered, '%m/%d/%Y')
df$Year.Entered <- as.numeric(format(df$Date.Entered, '%Y'))
df$Month.Entered <- as.numeric(format(df$Date.Entered, '%m'))
df$Day.Entered <- as.numeric(format(df$Date.Entered, '%d'))

# process "Application" date into separate components for ease of use
df$Application.Date <- as.Date(df$Application.Date, '%m/%d/%Y')
df$Year.Applied <- as.numeric(format(df$Application.Date, '%Y'))
df$Month.Applied <- as.numeric(format(df$Application.Date, '%m'))

# extract month from comment in a case- & abbreviation-sensitive fashion
df$Comment.Month <- str_match(df$Comments, paste('month of', VALID_MONTH))[,2]
df$Comment.Month <- str_to_title(df$Comment.Month)

# -- clean data --

# catch positive reports where someone forgot to delete the autocomment
positive_reports <- df$Pesticide.Name != ''
print(paste('Removing', sum(positive_reports), 'entries for using pesticides'))
df <- df[!positive_reports,]

# catch the same person making many entries varying only in day of 
# "Application" month, time of entry, and/or minor comment qualifications
repeat_entries <- duplicated(
  df[, c('Installation.Name', 'Department..Company', 'Office', 'Entered.by', 
         'Date.Entered', 'Comment.Month')]
)
print(paste('Removing', sum(repeat_entries), 'entries for duplication'))
df <- df[!repeat_entries,]

# if the year is given in the comment, trust Comments over Application Date
year_given <- grepl(VALID_YEAR, df$Comments)
print(paste(sum(year_given), 'entries have the year given in the comments'))
df[year_given,]$Year.Applied <- str_match(df[year_given,]$Comments, VALID_YEAR)
df[year_given,]$Month.Applied <- match(df[year_given,]$Comment.Month, month.abb)

# -- entry-time analysis --

# temporarily convert $Month.Entered from number to abbreviation
df$Month.Entered <- month.abb[df$Month.Entered]
print(paste(sum(df$Month.Entered == df$Comment.Month), 'entries where month of',
            'Application matches the month given in Comments'))

# plot by numerical day of entry, with facets split by month
ggplot(df, aes(x = Day.Entered)) + facet_wrap(~Month.Entered) + geom_bar() +
  labs(title = 'Days on which negative reports for pesticide use were entered',
       subtitle = 'Data for 06/2017-05/2022', y = 'Entries', x = 'Day')

# plot by month which report concerns
ggplot(df, aes(x = Month.Applied)) + geom_bar() +
  scale_x_discrete(limits = month.abb) +
  labs(title = 'Months with a negative report for pesticide use',
       subtitle = 'Data for 06/2017-05/2022', y = 'Entries', x = 'Month')

# -- lag analysis --

# convert everything back to numbers to do math on them
df$Comment.Month <- match(df$Comment.Month, month.abb)
df$Month.Entered <- match(df$Month.Entered, month.abb)

# calculate last day of Application month as one before first day of next
df$Expected.Date <- as.Date(
  paste(as.numeric(df$Year.Applied) + (df$Month.Applied == 12),
        (df$Month.Applied %% 12) + 1, 1, sep = '-')
) - 1
df$Lag <- as.numeric(df$Date.Entered - df$Expected.Date)

# histogram with 50-day breaks
ggplot(df, aes(x = Lag)) + geom_histogram(breaks = c(-1:11 * 50)) +
  labs(title = 'Lag between end of month of non-Application and the report',
       subtitle = 'Data for 06/2017-05/2022', y = 'Entries', x = 'Lag (days.)')

# histogram of nonpositive-lag entries
ggplot(df[df$Lag <= 0,], aes(x = Lag)) + geom_bar() +
  labs(title = 'Lag between end of month of non-Application and the report',
       subtitle = 'Data for 06/2017-05/2022', y = 'Entries', x = 'Lag (days.)')

# other fun, if not useful, visualizations
ggplot(df, aes(x = Pesticide.Name, y = Lag)) + geom_boxplot() +
  labs(title = 'Lag between end of month of non-Application and the report',
       subtitle = 'Data for 06/2017-05/2022', y = 'Lag (days.)', x = '')

ggplot(df, aes(x = Pesticide.Name, y = Lag)) + geom_violin() +
  labs(title = 'Lag between end of month of non-Application and the report',
       subtitle = 'Data for 06/2017-05/2022', y = 'Lag (days.)', x = '')