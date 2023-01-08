## Introduction

This is code produced during an internship with a federal agency. Due to the
nature of the work, I can't post it publicly in its original form. Thus
various things have been redacted in ways which were as small as possible while
fulfilling the spirit of the restrictions. Due to the same limitation the
documentation I can make is limited. It might not make complete sense without
knowing the content & structure of the data I use. I didn't bother making full-
fledged docs because people reading this probably couldn't use it!

Yet there's sufficient complexity here that I want documentation for myself.
What made it in here is based off of how I document in Python, which is itself
based off of some style manuals I read and what I liked myself.

Everything here investigates pest-control visits to various locations. Some
other datasets are used to supplement the main one, but at its core every script
has some purpose towards understanding what the visit data is saying. I had
quite literally no guidance on what to do initially, so a bunch of the stuff
here is just what I thought was interesting. A mentor looked over the stuff
later and requested a specific graph, so that's one of the scripts, but
basically everything here is my idea.

Not sure why you're reading this, honestly, since you can't use it. I'm posting
the code on GitHub just to prove I know R. Because this is a pretty nice big
project, I thought it would be useful. To me. Really, it isn't useful to anyone
else. I made a README since I know you're supposed to for projects.

## Script organization

Quite a few things here have dependencies. Libraries used include ggplot (for
graphing), stringr (for regex-ing, mostly on comments), and ggpubr (for the
functino ggarranged()). Then, many scripts require others, which are imported 
using source(filename) commands.

The scripts can be neatly divided into two categories: helpers, which are only
meant to be called by other scripts, and dead-ends, which produce some analysis.
Helpers exist as separate scripts because multiple dead-ends needed the same
functionality and I found it most intuitive to factor it out into its own
script.

Non-helpers are organized into various folders based on theme. All the scripts
were in a folder called "scripts", while plots were in "plots" and data in
"data". However both "plots" and "data" are probably sensitive so I'm only
sharing the scripts. Some scripts have been entirely omitted because they were
fundamentally unredactable, for example, turning on specific data points.

Helpers: base_map, by_month, calc_season, chart_maker

## Understanding individual scripts

The top of each file has a one-line description of what it does. In helpers,
this is followed by a list of things "Meant for further use" - globals exported
by the file. Most files have sections split up by headers like -- this -- (two 
dashes on either side). Individual functions explain parameters and returns.

Files are generally organized with dataframe set-up at the top: things such as
calculating the main statistic explored in the rest of the file, or subsetting
to remove parts which are not useful. After that would be helper functions.
These handle a specific part of the analysis which was easier to just factor out
from the rest. Finaly the actual analysis is performed. Here I would make maps/
graphs or print out the desired results. If applicable some global constants are
defined at the top of the file.

Each of the dead-end scripts is meant to be run by itself, and it will call/load
everything that it needs. Trying to compare scripts to each other may cause some
confusion. For example, the exact contents of the data frame in 'df' change
based on the analysis needs of that script. However, things should be consistent
between a dead-end and its helpers.
