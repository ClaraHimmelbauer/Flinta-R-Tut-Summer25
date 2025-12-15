# 0. PREP -------------------------------------------------
rm(list = ls()); gc()

# packages
packages <- c("tidyverse")
sapply(packages, library, character.only = T)

# 1. DATA -------------------------------------------------

dd <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_ddfile_suf.csv")
e <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_efile_suf.csv")
ind <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_indfile_suf.csv")
kind <- read.csv2("data/uebungsdatensatz_zve/subsample/subsample_kinder_indfile_suf.csv")

# 2. LOOK AT THE DATA -------------------------------------
# LOOK AT THE CODEBOOKS
# ASK ME QUESTIONS

# WHICH VARIABLES DO WE NEED: 
# TO CALCULATE THE DAILY HOURS
# MEN AND WOMEN - IND1
# SPEND PER DAY ON MAIN ACTIVITIES - MACT_GR
# USING ALSO THE CORRECT WEIGHTS

glimpse(dd)
glimpse(e)
glimpse(ind)

table(ind$INDC4, ind$IND5)

nrow(e)
e %>% 
  group_by(HID, PID, DIARY) %>% 
  reframe(n = n())

# 3. TIME SPENT WITH MAIN ACTIVITIES ----------------------

df <- left_join(e, dd, by = c("HID", "PID", "DIARY"))
df <- left_join(df, ind, by = c("HID", "PID"))

df <- left_join(e, dd, by = c("HID", "PID", "DIARY")) %>% 
  left_join(ind, by = c("HID", "PID"))

dim(df)


dfx <-df %>% 
  select(HID, PID, DIARY, WGHT1, IND1, MACT_GR) %>% 
  
  group_by(IND1, MACT_GR) %>% 
  reframe(sumwght = sum(WGHT1)) %>% 
  
  group_by(IND1) %>% 
  mutate(share = sumwght / sum(sumwght),
         h = share * 24,
         sex = factor(IND1, c(1,2), c("Men", "Women")),
         activity = factor(MACT_GR,
                           1:8,
                           c("Sleep",
                             "Personal Care",
                             "Paid Work",
                             "Education",
                             "Care Work",
                             "Volontary Work",
                             "Free Time",
                             "Other"))) %>% 
  ungroup()
dfx


# 4. TIME USE OVER DAY BY GENDER --------------------------

# chatgpt: i want to make a sequence from 04:00 in the morning
# to 03:50 the next moring in 10 minute intervals
# in R.
# so in the end the vector should look like 04:00, 04:10, 04:20, ..., 03:50

timestamps <- seq(
  from = as.POSIXct("2024-01-01 04:00", format = "%Y-%m-%d %H:%M"),
  to = as.POSIXct("2024-01-02 03:50", format = "%Y-%m-%d %H:%M"),
  by = "10 min"
)
timestamps <- format(timestamps, "%H:%M")
length(timestamps)

from = as.POSIXct("2024-01-01 04:00", format = "%Y-%m-%d %H:%M")

dfx <-df %>% 
  select(HID, PID, DIARY, WGHT1, IND1, MACT_GR, TIMESLOT) %>% 
  
  group_by(IND1, MACT_GR, TIMESLOT) %>% 
  reframe(sumwght = sum(WGHT1)) %>% 
  
  group_by(IND1, TIMESLOT) %>% 
  mutate(share = sumwght / sum(sumwght),
         sex = factor(IND1, c(1,2), c("Men", "Women")),
         activity = factor(MACT_GR,
                           1:8,
                           c("Sleep",
                             "Personal Care",
                             "Paid Work",
                             "Education",
                             "Care Work",
                             "Volontary Work",
                             "Free Time",
                             "Other")),
         time = factor(TIMESLOT,
                       1:144,
                       timestamps)) %>% 
  ungroup()
dfx
dim(dfx)
View(dfx)

labs <- c(timestamps[seq(1, length(timestamps), by = 12)])

ggplot(data = dfx, aes(x = time, y = share, fill = activity)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  facet_wrap(~sex, nrow = 2) +

  theme_minimal() +
  
  scale_x_discrete(breaks = labs,
                   labels = labs,
                   name = "Time of day") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Share of People With Main Activity") +
  scale_fill_manual(values = c("#99989D", "#5e7540", "#d7b772", "#edd892",
                               "#5a78c8", "#8d9af1", "#b3d58e", "#4d4e4d"),
                    name = "Main activity") +
  labs(title = "Time Use over the Course of the Day for Men and Women",
       subtitle = "Data = Austrian Time Use Survey 2021/22; Training Sample")

