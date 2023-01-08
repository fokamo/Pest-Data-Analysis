# Find entries where the Application Date was quickly corrected

# maximum number of minutes allowed in between entries
TIME_BETWEEN <- 10

df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# convert into more usable formats
df$Time.Entered <- as.POSIXct(df$Date.Entered, format = '%m/%d/%Y %I:%M:%S %p')
df$Date.Entered <- as.Date(df$Time.Entered)
df$Application.Date <- as.Date(df$Application.Date, '%m/%d/%Y')

# creating a checksum with all the other rows is easier than matching separately
df$Checksum <- apply(df, 1,  
                     function(x) paste(x[-c(1, 27)], collapse = '&'))

# entries which had the default Application Date (the current day)
same_day <- df$Application.Date == df$Date.Entered
same_day_times <- df[same_day,]$Time.Entered
same_day_checksums <- df[same_day,]$Checksum

# looking at all other entries, determine whether they're a quick resubmit
sum(apply(df[!same_day,c('Time.Entered', 'Checksum')], 1, function(x) {
  diffs <- as.numeric(difftime(x[1], same_day_times, units = 'mins'))
  close_times <- which(diffs >= 0 & diffs <= TIME_BETWEEN)
  x[2] %in% same_day_checksums[close_times]
}))