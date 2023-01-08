# Visualize the time each pesticide staves off a return visit for the same pest

# for making graphs
library(ggplot2)

# -- build up data frames to reference --

df <- read.csv(paste0(getwd(), '/data/visits.csv'))
# for ease of reference, and a better ending graph
colnames(df)[colnames(df) == 'Pesticide.Name'] <- 'Pesticide'
# spellcheck one name (associated with same EPA number)
df$Pesticide <- ifelse(df$Pesticide == 'Rimon / Diamond 0.83 EC', 
                       'Rimon .83 EC', df$Pesticide)
# need to know which pest a visit was for
df <- df[df$Pest != '',]
# need to know the date a visit occurred on
df$Date <- as.Date(df$Application.Date, format = '%m/%d/%Y')
# lookup table by base & pest for all visits
base_pest <- aggregate(Date ~ Installation.Name + Pest, df, unique)

# this line will take a while to run - it calculates return time for every visit
df$Return <- apply(df, 1, function(x) {
  # do.call to flatten list without messing up dates
  visits <- do.call('c', base_pest[base_pest$Installation.Name == x[2]
                                   & base_pest$Pest == x[10],]$Date)
  # extract only visits afterwards
  visits_after <- visits[difftime(as.Date(visits), x[27]) > 0]
  # manually handle case where no visits occur after
  ifelse(length(visits_after) > 0, difftime(min(visits_after), x[27]), NA)
})

# remove visits without return times
df <- df[!is.na(df$Return),]

# -- make graph --

# aggregate across pest/herb/fung/X-icides
by_cide <- aggregate(Return ~ Pesticide, df, mean)
# sort for clearer graph
by_cide <- by_cide[order(by_cide$Return),]
# force desired ordering in graph
by_cide$Pesticide <- factor(by_cide$Pesticide, levels = by_cide$Pesticide)

# shows a nice curve in the log graph
ggplot(data = by_cide, aes(x = Pesticide, y = Return)) + 
  scale_y_log10() + geom_col() + 
  # there's too many kinds of pesticides to label them at all
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())