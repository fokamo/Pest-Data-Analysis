# Determine whether odd PAI calculations were made around specific times

# for making the graphs
library(ggplot2)

# -- extract only entries which show oddly varying PAIs --

# master data frame
df <- read.csv(paste0(getwd(), '/data/visits.csv'))

# PAI should be determined entirely by EPA#, quantity, and weight, if applicable
PAIs <- aggregate(PAI ~ EPA.Number + Quantity.Concentrate + Weight, df, unique)
# therefore, pull all combinations of the above with multiple PAIs
PAIs <- PAIs[PAIs$EPA.Number != '' & sapply(PAIs$PAI, length) > 1,]

# throw out all irrelevant entries
df <- df[paste(df$EPA.Number, df$Quantity.Concentrate)  %in%
           paste(PAIs$EPA.Number, PAIs$Quantity.Concentrate),]
# plot entry date to see if oddness has a tendency to group around certain times
df$Date.Entered <- as.Date(df$Date.Entered, '%m/%d/%Y')
ggplot(df, aes(x = Date.Entered)) + geom_bar()

# -- extract only entries with the fluke PAI --

# all EPA Number-amounts left have only two unique PAIs
PAIs$PAI.One <- sapply(PAIs$PAI, function(x) x[1])
PAIs$PAI.Two <- sapply(PAIs$PAI, function(x) x[2])

# count the number of times PAI was calculated to mean this
PAIs$Count.One <- apply(PAIs, 1, function(x) 
  sum(df[df$EPA.Number == x[1] & df$Quantity.Concentrate == x[2] & 
           df$Weight == x[3],]$PAI == x[5]))
PAIs$Count.Two <- apply(PAIs, 1, function(x) 
  sum(df[df$EPA.Number == x[1] & df$Quantity.Concentrate == x[2] & 
           df$Weight == x[3],]$PAI == x[6]))

# need to know which PAI is the odd one
PAIs <- PAIs[PAIs$Count.One != PAIs$Count.Two,]
PAIs$Uncommon.PAI <- ifelse(PAIs$Count.One > PAIs$Count.Two, 
                            PAIs$PAI.Two, PAIs$PAI.One)
# checksums are easier to compare with each other instead of whole rows
PAIs$Checksum <- apply(PAIs, 1,  
                       function(x) paste(x[c(1,2,3,9)], collapse = '&'))
df$Checksum <- apply(df, 1,  
                     function(x) paste(x[c(15,17,18,22)], collapse = '&'))
# now plot entry date of all the fluke entries
df <- df[df$Checksum %in% PAIs$Checksum,]
ggplot(df, aes(x = Date.Entered)) + geom_bar()