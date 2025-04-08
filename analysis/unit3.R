# UNIT 3 --------------------------------------------------

# RESOURCES -----------------------------------------------

# https://www.data-to-viz.com/
# https://r-graph-gallery.com/


# PRINCIPLES FOR DATA VISUALIZATION -----------------------

# Purpose of plot -----------
# for presentations
### larger plots, visually engaging, less text
### visibility from a far

# for text
### plots can be more detailed
### if text might be printed, make sure that it also works in black & white


# Self-explanatory ----------
# all your plots/slides should be self-explanatory
### so if someone takes a screenshot or photo, they should be able to understand it

### Correct and precise axis labels, with units
### titles and subtitles that explain what you can see there
### captions if you need them. can also include "Lesebeispiel"
### legends

# main message of your plot should be clear
### highlighting important information with coloring, size, labels, etc. 

# ask a peer/collegue/friend when you've made a plot
### if they understand it
### and if they find it visually appealing


# accuracy ------------------
# don't distort your data
### don't start your axes somewhere else than 0


# coloring ------------------
# use intuitive colors. sometimes you might want to use
### diverging color palettes when there is some threshold
### gradients are good for continuous variables
### for categorical variables: similar things should have similar colors

# don't use red and green together in a plot
### up to 10% of men* might be colorblind (and a few women* too)
### therefore, use palettes that are colorblind friendly

# if your plot might be printed make sure that it also works in black & white
### use a gradient for continuous variables (light to dark)
### or use light and dark colors

# here's a color palette finder: https://r-graph-gallery.com/color-palette-finder 

# chart types ---------------
# don't use piecharts. they do not convey information well
# don't use 3D plots.

# stacked plots sometimes not appropriate
### categories might be hard to compare


# no clutter ----------------
# WHITESPACE is your firend!
### don't use background coloring, borders, shading
### be parsimonious with gridlines, labelling, text

# don't use too many colors/lines (in a linechart)

# don't make your plot too complicated
### double axes are problematic


# consistency ---------------
# if you have more than 1 plot in some text/work of yours, be consistent
### use the same colors, scales, styles

# axes
### also use the same axis limits and labes for similar plots
### use the same scale for the same variable in different plots


# hierarchy -----------------
# use hierarchy to highlight important messages and to keep everything else in the background
### light grey gridlines

# visual hierarchy
### coloring
### size
### transparency

# order your data
### if you have a barchart, order your bars by size


# PREP ----------------------------------------------------
# clean working environment

# library

# DATA ----------------------------------------------------
# reading in data

# PLOT 1: HISTOGRAM ---------------------------------------
# equivalized household income in Austria in 2008



# PLOT 2: DENSITY -----------------------------------------
# equivalized household income in Austria, Germany, and Italy in 2013



# PLOT 3: SIMPLE BARPLOT ----------------------------------
# share of total income that bottom 50% - middle 40% - top 10% of households hold
# in Austria, 2013



# PLOT 4: GROUPED BARPLOT ---------------------------------
# now we want to do the same but for all countries
# in a way we want to reporduce Figure 2 of teh WOorld inequality report
# https://wir2022.wid.world/executive-summary/ 



# PLOT 5 - EXERCISE ---------------------------------------
# now let's go to the INDIV file and make a graph of your choosing
# look at the data at first
head(indiv)

# a. choose your topic ------
# what do you want to display?
# some possibilities:
### average income by sex. 
### education level by sex
### income and age

# which countries and years?
### countries: AT, IT, DE
### years: 2008, 2009, 2013
### you can also compare multiple countries and years if you want to and that's what you're interested in

# what type of plot do you want to make?
### barplot, histogram, density, lineplot, etc...

# once you've chosen a type of plot look for help and examples online
### chatgpt
### https://r-graph-gallery.com/


# b. filter -----------------


# c. group and summarize ----
# you can ignore the weights


# d. make a plot ------------
# tip: start with an easy plot and then gradually improve it


# e. export the plot --------











































# SOLVED (I DID THIS BEFORE TO PREPARE FOR THE UNIT)
# DON'T LOOK AT SOLUTIONS BEFOREHAND





































# PREP ----------------------------------------------------
# clean working environment
rm(list = ls()); gc()

# library
library(tidyverse)

# DATA ----------------------------------------------------
# reading in data

hh <- readRDS("data/silc_hh_new.RDS")
indiv <- readRDS("data/silc_indiv_new.RDS")

# PLOT 1: HISTOGRAM ---------------------------------------
# equivalized household income in Austria in 2008

# prepare data
df <- hh %>% 
  filter(year == 2008, country == "AT")

# basic ggplot
ggplot(df, aes(x = heqinc)) +
  geom_histogram()

# now we gradually imporve it
ggplot(df, aes(x = heqinc)) +
  geom_histogram() +
  
  # limits
  xlim(0, 100000) +
  
  # title and axis labels
  labs(title = "Equivalized household income in Austria in 2008",
       x = "Equivalized household income (in Euro)",
       y = "Number of households") +
  
  # theme_minimal is king
  theme_minimal()
  
# export

# PLOT 2: DENSITY -----------------------------------------
# equivalized household income in Austria, Germany, and Italy in 2013

# prepare data
df <- hh %>% 
  filter(year == 2013)

# plot
ggplot(hh, aes(x = heqinc, color = country)) +
  geom_density(size = 2) +

  
  # limits
  xlim(0, 100000) +
  
  # title and axis labels
  labs(title = "Equivalized household income in Austria, Germany, and Italy in 2013",
       x = "Equivalized household income (in Euro)",
       y = "Density") +
  
  # theme_minimal is king
  theme_minimal() +
  
  # so many possibilities with colors
  # scale_color_manual(values = c("AT" = "#EF3340", "DE" = "#FFCC00", "IT" = "#008C45"),
  #                    name = "Country") +
  scale_color_manual(labels = c("AT", "DE", "IT"),
                     values = c("#94475EFF", "#364C54FF", "#E5A11FFF")) +
  # scale_color_viridis_d(labels = c("AT", "DE", "IT")) +
  # scale_color_manual(values = viridis::plasma(3)) +
  
  # y axis labels as numbers and not as text
  scale_y_continuous(labels = scales::label_number()) +
  
  # fix the guide
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#94475EFF", "#364C54FF", "#E5A11FFF"),
                                                  size = 5, color = NA)))



# PLOT 3: SIMPLE BARPLOT ----------------------------------
# share of total income that bottom 50% - middle 40% - top 10% of households
# in Austria, 2013

df <- hh %>% 
  filter(country == "AT", year == 2013) %>%
  
  #  bottom 50% - middle 40% - top 10% using weights
  arrange(hinc) %>% 
  mutate(cumsum_w = cumsum(hweight))

# check cumsum and sum
tail(df)
sum(df$hweight)
# also check with intuition: yes, tehre are about 3701302 private households in AT

# quantiles we want to know
quantile(df$cumsum_w, probs = c(0.5, 0.9))
x <- quantile(df$cumsum_w, probs = c(0.5, 0.9))[1]
y <- quantile(df$cumsum_w, probs = c(0.5, 0.9))[2]

# group variable
df$group <- ifelse(df$cumsum_w <= x, "Bottom 50%",
                   ifelse(df$cumsum_w > x & df$cumsum_w <= y, "Middle 40%",
                          "Top 10%"))
# check
table(df$group)
prop.table(table(df$group))
# yes, there are about 50, 40, and 10% in the groups

# now prepare the data for the plot
# remember, we want the share of total income, that is held by these households

dfx <- df %>% 
  group_by(group) %>% 
  summarise(suminc = sum(hinc)) %>% 
  ungroup() %>% 
  mutate(share = suminc/sum(suminc))

# make a ggplot
ggplot(dfx, aes(x = group, y = share)) +
  geom_bar(stat = "identity", fill = "grey45") +
  
  # title and axis labels
  labs(title = "Share of total income held by bottom 50% - middle 40% - top 10% of households",
       subtitle = "in Austria, 2013",
       x = "Household group",
       y = "Share of total income") +
  
  # theme_minimal is king
  theme_minimal() +
  
  # y axis labels as numbers and not as text
  scale_y_continuous(labels = scales::label_percent())
  
  # add data labels
  # geom_text(aes(label = paste0(round(share*100, digits = 1), "%")),
  #           position = position_stack(vjust = 0.5),
  #           size = 5)


# PLOT 4: GROUPED BARPLOT ---------------------------------
# now we want to do the same but for all countries
# in a way we want to reporduce Figure 2 of teh WOorld inequality report
# https://wir2022.wid.world/executive-summary/ 

# prepare data
df <- hh %>% 
  filter(year == 2013) %>% 
  arrange(hinc) %>% 
  group_by(country) %>% 
  mutate(cumsum_w = cumsum(hweight)) %>% 
  ungroup()

# check that
sum(df$hweight[df$country == "IT"]) == max(df$cumsum_w[df$country == "IT"])

df <- df %>% 
  # get quantile ratios
  group_by(country) %>%
  mutate(x = quantile(cumsum_w, probs = c(0.5, 0.9))[1],
         y = quantile(cumsum_w, probs = c(0.5, 0.9))[2])

# check that
table(df$x, df$y)
table(df$x < df$y)  

# now get quantiles
df <- df %>% 
  mutate(group = ifelse(cumsum_w <= x, "Bottom 50%",
                        ifelse(cumsum_w > x & cumsum_w <= y, "Middle 40%",
                               "Top 10%"))) %>% 
  
  # and sumamrize
  group_by(country, group) %>% 
  summarise(suminc = sum(hinc)) %>% 
  
  # share
  group_by(country) %>% 
  mutate(share = suminc/sum(suminc))


# make plot
ggplot(df, aes(x = country, y = share, fill = group)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
  
  # title and axis labels
  labs(title = "Share of total income held by bottom 50% - middle 40% - top 10% of households",
       subtitle = "in Austria, Germany, and Italy, 2013",
       x = "Household group",
       y = "Share of total income") +
  
  # theme_minimal is king
  theme_minimal() +
  
  # y axis labels as numbers and not as text
  scale_y_continuous(labels = scales::label_percent()) + 
  
  scale_fill_manual(values = c("#DB928AFF", "#BE3979FF", "#623976FF"),
                    name = "Income group")
  
  # add data labels
  # geom_text(aes(label = paste0(round(share*100, digits = 1), "%")),
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.25,
  #           size = 5)



# PLOT 5 - EXERCISE ---------------------------------------
# now let's go to the INDIV file and make a graph of your choosing
# look at the data at first
head(indiv)

# a. choose your topic ------
# what do you want to display?
# some possibilities:
### average income by sex. 
### education level by sex
### income and age

# which countries and years?
### countries: AT, IT, DE
### years: 2008, 2009, 2013
### you can also compare multiple countries and years if you want to and that's what you're interested in

# what type of plot do you want to make?
### barplot, histogram, density, lineplot, etc...

# once you've chosen a type of plot look for help and examples online
### chatgpt
### https://r-graph-gallery.com/


# b. filter -----------------


# c. group and summarize ----
# you can ignore the weights


# d. make a plot ------------
# tip: start with an easy plot and then gradually improve it


# e. export the plot --------

  
