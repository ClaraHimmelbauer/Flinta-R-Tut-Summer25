# PREP ----------------------------------------------------
# clean workspace
rm(list = ls()); gc()

# packages
packages <- c("tidyverse")
# sapply(packages, install.packages, character.only = T)
sapply(packages, library, character.only = T)

# FIRST STEP: LOOK AT README: -----------------------------

# SECOND STEP: WHAT DO WE WANT TO DO? ---------------------
### Option 1: Plot with main categories of time use data
### Option 2: Plot the share of unpaid work done by men and women
### Option 3: Time use over the course of a day of men and women
### Option 4: Share of people having less thn 3h of free time a day

# THIRD STEP: LOOK AT CODEBOOK ----------------------------

# DATA ----------------------------------------------------

dd  <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_ddfile_suf.csv")
e   <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_efile_suf.csv")
ind <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_indfile_suf.csv")

head(dd)
glimpse(e)
str(ind)

# WRANGLING: Data for all options -------------------------
df <- e[, c("HID", "PID", "DIARY", "MACT_GR", "TIMESLOT")]

# merge weights
merge <- dd[, c("HID", "PID", "DIARY", "WGHT1")]
df <- left_join(df, merge, by = c("HID", "PID", "DIARY"))

# merge sex
merge <- ind[, c("HID", "PID", "IND1")]
df <- left_join(df, merge, by = c("HID", "PID"))

# MAIN TIME USE CATEGORIES -------------------------------

dfx <- df %>% 
  
  # total time spent on main activity
  group_by(MACT_GR, IND1) %>% 
  summarise(WGHT1 = sum(WGHT1)) %>% 
  group_by(IND1) %>% 
  
  # share of time
  mutate(ant = WGHT1 / sum(WGHT1)) %>%
  
  # format to hours an dminutes
  mutate(time = ant * 24) %>% 
  mutate(h = floor(time),
         min = (time - floor(time)) * 60) %>% 
  mutate(hmin = paste0(h, ":", sprintf("%02d", round(min)))) %>%
  
  # now MACT with labels
  ungroup() %>% 
  mutate(MACT_GR = factor(MACT_GR,
                          levels = 1:8,
                          labels = c("Sleep", "Personal", "Paid work",
                                     "Education", "Unpaid work", "Volontary",
                                     "Free time", "Other"))) %>% 
  
  # sex as factor non continous and with labels
  mutate(sex = factor(IND1,
                      levels = 1:2,
                      labels = c("Male", "Female")))

# make a plot
ggplot(dfx, aes(x = MACT_GR, y = time, fill = sex, group = IND1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()

# UNPAID WORK MEN AND WOMEN -------------------------------
# can be easily derived from above

# TIME USE OVER THE DAY -----------------------------------

# make timestamps
timestamps <- seq(
  from = as.POSIXct("2024-01-01 04:00", format = "%Y-%m-%d %H:%M"),
  to   = as.POSIXct("2024-01-02 03:50", format = "%Y-%m-%d %H:%M"),
  by   = "10 min"
)
timestamps <- format(timestamps, "%H:%M")

dfx <- df %>% 
  
  # sum of activities at one point in time
  group_by(TIMESLOT, MACT_GR, IND1) %>%
  summarise(WGHT1 = sum(WGHT1)) %>% 
  
  # share of time
  group_by(TIMESLOT, IND1) %>% 
  mutate(ant = WGHT1 / sum(WGHT1)) %>% 
  
  # make factors
  ungroup() %>% 
  mutate(MACT_GR = factor(MACT_GR,
                          levels = 1:8,
                          labels = c("Sleep", "Personal", "Paid work",
                                     "Education", "Unpaid work", "Volontary",
                                     "Free time", "Other")),
         sex = factor(IND1,
                      levels = 1:2,
                      labels = c("Male", "Female")))

# make a plot
ggplot(dfx[dfx$IND1 == 2, ], aes(x = TIMESLOT, y = ant, fill = MACT_GR)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  theme_minimal() +
  
  scale_x_continuous(breaks = seq(1, 144, by = 12),
                     labels = c(timestamps[seq(1, length(timestamps), by = 12)]),
                     name = "Time of day") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Share") +
  scale_fill_manual(values = c("#99989D", "#5e7540", "#d7b772", "#edd892",
                               "#5a78c8", "#8d9af1", "#b3d58e", "#4d4e4d"),
                    name = "Main activity")


# AMOUNT OF FREE TIME PER PERSON --------------------------
# this is where weights get complicated

### we have to average out weights across the two survey dates for a person
### for reference, this is the sum of weights we want to have in the end
sum(dd$WGHT1)
sum(ind$WGHT2)

### we also don't want to drop persons with 0 free time
### number of persons
nrow(ind)

# dfx <- df[df$HID == 10002 & df$PID == 1, ]
# nrow(dfx[dfx$MACT_GR == 7, ])

dfx <- df %>% 
  
  # free time per day
  group_by(HID, PID, DIARY) %>% 
  summarise(ft = sum(MACT_GR[MACT_GR == 7]/7),
            ft2 = sum(MACT_GR == 7),
            wght = mean(WGHT1)) %>% 
  
  # average out free time across survey days
  group_by(HID, PID) %>% 
  summarise(ft = sum(ft * wght) / sum(wght),
            wght = sum(wght)) %>% 
  
  # indicator for less than 3 h
  mutate(no.ft = ifelse(ft < 3*6, 1, 0))
  
# check the sum of weights
sum(dfx$wght)

# and the number of rows
nrow(dfx)

# now calcualte the share of persons with less than 3h of free time
dfx %>%
  group_by(no.ft) %>% 
  summarise(wght = sum(wght)) %>% 
  ungroup() %>% 
  mutate(share = wght / sum(wght) * 100)
