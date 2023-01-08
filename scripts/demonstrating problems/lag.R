# Visualize the time between a pest control visit occurs and is recorded

# for making graphs
library(ggplot2)

# -- set up lag time --

df <- read.csv(paste0(getwd(), '/data/visits.csv'))
df$Lag <- as.numeric(as.Date(df$Date.Entered, format = '%m/%d/%Y') 
                     - as.Date(df$Application.Date, format = '%m/%d/%Y'))

# -- hacky lag-time histogram (uses a bar chart) --

# lag-time bins
bins <- c(-600, -500, -250, -100, -10, -1, 0, 1, 
          10, 100, 200, 300, 400, 500, 1000, 1600)

# set up boundaries for each of the bins, using -c(#) to exclude index at #
lag_bins <- data.frame(Low = bins[-c(length(bins))], High = bins[-c(1)])
lag_bins$Label <- paste('<', lag_bins$High)
# forces the bars to be ordered correctly
lag_bins$Label <- factor(lag_bins$Label, levels = lag_bins$Label)
# fill bins with correct count
lag_bins$Count <- apply(lag_bins, 1, 
                        function(x) sum(df$Lag >= as.numeric(x[1]) 
                                        & df$Lag < as.numeric(x[2])))

# basic histogram plot
ggplot(df[df$Contract...In.House != '',], aes(x = Lag)) + 
  facet_wrap(~Contract...In.House) + scale_y_log10(labels = scales::comma) +
  # historgram with bins of 100
  geom_histogram(breaks = seq(-601, 1599, 100)) + 
  # clearly mark 0
  geom_vline(aes(xintercept = 0, color = 'Zero days (no lag)')) +
  scale_color_manual(name = '', values = c('red')) +
  # make plot readable
  labs(title = 'Time between visit and data entry', 
       subtitle = 'Pest control visits which occurred from 06/2017-05/2022', 
       y = '# of Entries', x = 'Lag time in days') +
  theme(text = element_text(size = 15), legend.position = c(0.85, 0.25))

# -- analyze lag by kind (or lack) of contract --

# for ease of use
colnames(df)[colnames(df) == 'Contract...In.House'] <- 'Contract'

# aggregating stats across kind of contract
lag_stats <- aggregate(Lag ~ Contract, df, mean)
lag_stats$SD <- sapply(lag_stats$Contract, 
                       function(x) sd(df[df$Contract == x,]$Lag))
colnames(lag_stats) <- c('Contract', 'Mean', 'SD')

# barchart to show wideness of lag
ggplot(lag_stats, aes(x = Contract, y = Mean)) +
  geom_col() + geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) +
  labs(title = 'Lag time between visit and data entry', 
       x = 'On contract?', y = 'Lag')

# stats for reasonable data entry times
done_by <- aggregate(Lag ~ Contract, df, 
                     function(x) c(Day = sum(x == 0) / length(x),
                                   Week = sum(x >= 0 & x <= 7) / length(x)))
setNames(done_by, c('On contract?', 'Within'))

# add (bind) new columns to lag_stats
cbind(lag_stats, 
      # sapply puts each item as a column; make them rows
      t(
        # for each kind of contract
        sapply(lag_stats$Contract,
               # calculate quantile (returns a vector)
               function(x) quantile(df[df$Contract == x,]$Lag))
        )
      )

# -- lag for specific bases --

bases <- table(df$Installation.Name)
bases <- names(bases[bases >= 100])
bin_width <- 50
sapply(bases, function(base) {
  df <- df[df$Installation.Name == base,]
  min_bin <- (floor((min(df$Lag) + 1) / bin_width) * bin_width) - 1
  max_bin <- (ceiling((max(df$Lag) + 1) / bin_width) * bin_width) - 1
  # basic histogram plot
  ggplot(df, aes(x = Lag)) + scale_y_log10() +
    # histogram with bins of 100
    geom_histogram(breaks = seq(min_bin, max_bin, bin_width)) + 
    # clearly mark 0
    geom_vline(aes(xintercept = 0, color = 'Zero days (no lag)')) +
    scale_color_manual(name = '', values = c('red')) +
    # make plot readable
    labs(title = paste('Time between visit and data entry at', base), 
         subtitle = 'Pest control visits which occurred from 06/2017-05/2022', 
         y = '# of Entries', x = 'Lag time in days')
  ggsave(paste0(getwd(), '/plots/lag/bases/', base, '.png'))
})