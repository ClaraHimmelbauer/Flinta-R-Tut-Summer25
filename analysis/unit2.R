# UNIT 2: DATA WRANGLING ----------------------------------

# PREP ----------------------------------------------------
# clean environment
rm(list = ls())
# clear the cache
gc()

# install packages - you need to do this only once for each package
# install.packages("tidyverse")

# attach the packages - tell R that you plan to work with this packge in this session
# you need to do this every time you start R when you plan to work with this package
library(tidyverse)

# DATA ----------------------------------------------------
# when you're wokring in a project environment
silc_indiv <- readRDS("data/silc_indiv.RDS")

# when you are not working in a project environment you have to copy the whole path of the file
# be aware that paths can change and that on windows you have to change \ to /
# silc_indiv <- readRDS("C:/Users/chimmelb/OneDrive - WU Wien/Dokumente/Flinta-R-Tut-Summer25/data/silc_indiv.RDS")

# read in the household file
silc_hh <- readRDS("data/silc_hh.RDS")

# two other commands that can help you here are:
# getwd() to see the folder path of your current working directory
getwd()
# setwd("directory path") to set your working directory
# setwd("some/folder/path")

# LOOKING AT DATA -----------------------------------------
# View (with capital V) opens up a viewer so you can look at your data
View(silc_indiv)

# str() lets you see the structure of a file.
# Notice that the str() command provides you with a lot of information of the data
### number of rows (229131) and columns (17)
### names of the variables/columns
### type of the variables/columns: here we have characters, integers, and numerics
### the first elements of each column: also gives you hints towards duplicated columns
str(silc_indiv)

# another way to look at data that I like using is the head() function
# head() shows you the first elements of an object
head(silc_indiv)

# CHECKING WHETER VARIABLES ARE THE SAME ------------------
# from the str() command you might have noticed that some variables seem to be equal to each other
# we check this now

# rembember that you can check equality in this way:
3 == 1+2
3 == 4
# also, when you do that for vectors, it checks each element of the vector
a <- c(1, 2, 3, 4, 5)
b <- c(1, 3, 3, 4, 1)
a == b
# if we want to check how often elements are the same we can use the table() function
table(a == b)

# now remember, that you can access columns of a dataframe using the $ sign
# and each column of a data frame basically is a vector
# therefore, we can check the equality of two columns the following way:
table(silc_indiv$pb010 == silc_indiv$rb010)
table(silc_indiv$pb020 == silc_indiv$rb020)
table(silc_indiv$pb030 == silc_indiv$rb030)
# as we can see, this command only returns TRUE
# therefore these columns are the same - and we can discard one of each later on


# RENAMING VARIABLES --------------------------------------
str(silc_indiv)
names(silc_indiv)

# why do we want to rename columns? to know what's actually going on
# which columns do we want to rename?
### the ones that are not duplicated
### and also the onese that are not composed totally out of other columns

# renaming with tidyverse --------
# %>% pipe operator
### not referencing datasets every time
### not using quotationmarks to call on columns
silc_new <- silc_indiv %>% 
  rename(year = pb010,
         country = pb020,
         pid = pb030,
         pweight = pb040,
         birthyear = pb140,
         sex = pb150,
         educ = pe040,
         empstatus = pl040,
         workinghours = pl060,
         health = ph010,
         gross_income = py010g,
         hid = px030)
names(silc_new)

# rename them with base R --------
silc_new <- silc_indiv
colnames(silc_new) <- c("id_p",
                          "year", "country", "pid",
                          "pweight", "birthyear", "sex",
                          "educ", "empstatus", "workinghours",
                          "health", "gross_income", "hid",
                          "rb010", "rb020", "rb030", "id_h")
# only change the colnames we want to change
silc_new <- silc_indiv
names(silc_new)[2:13] <-  c("year", "country", "pid",
                            "pweight", "birthyear", "sex",
                            "educ", "empstatus", "workinghours",
                            "health", "gross_income", "hid")
names(silc_new)

# SELECTING VARIABLES -------------------------------------
# here we only want to select the variables that we actually need
# selceting with tidyverse
silc_new2 <- silc_new %>% 
  select(year, country, pid, pweight, birthyear, sex, educ,
         empstatus, workinghours, health, gross_income, hid)
names(silc_new2)
# colnames does the same as names() for dataframes
# for matrices you need to use the colname() command
# colnames(silc_new2)

# EXPORT DATA ---------------------------------------------
# export it as an RDS
saveRDS(silc_new2, "data/silc_indiv_new.RDS")
# export it as csv for german pcs
write.csv2(silc_new2, "data/silc_indiv_new.csv")
write.csv(silc_new2, "data/silc_indiv_new.csv", sep = ";")

# export for other pcs
# i'm not sure how it is for mac
write.csv(silc_new2, "data/silc_indiv_new.csv")


# EXERCISE: DO THE SAME WITH THE HOUSEHOLD FILE -----------

# what are the names of the variables?
names(silc_hh)

# are some variables the same? - doing it with base R
str(silc_hh)
table(silc_hh$hb010 == silc_hh$db010)
# do that with tidyverse
silc_hh %>% 
  # mutate lets you create new columns inside the tidyverse environment
  mutate(same010 = hb010 == db010,
         same020 = hb020 == db020,
         same030 = hb030 == db030) %>% 
  # count occurances
  count(same010, same020, same030)

# rename the variables: consult the codebook
# google (gesis - missy - silc variable coodebook)
# or go to the manual
# hb010: year
# hb020: country
# hb030: hid
# hy020: hinc (household disposeable income)
# hx040: hsize
# hx090: heqinc (household equivalized disposeable income)
# db040: region
# db090: hweight (household weight)
# db100: degurba (degree of urbanization)
silc_hh_new <- silc_hh %>% 
  rename(year = hb010,
         country = hb020,
         hid = hb030,
         hinc = hy020,
         hsize = hx040,
         heqinc = hx090,
         region = db040,
         hweight = db090,
         degurba = db100) %>% 
  # select the variables we are interested in
  select(year, country, hid, hinc, hsize, heqinc,
         region, hweight, degurba)

# export
saveRDS(silc_hh_new, "data/silc_hh_new.RDS")

# JOIN DATASET --------------------------------------------
# here we use the left_join() from the tidyverse environment
# imagine you have a dataframe/table to the left and then add variables to the right
### the left dataset is our larger one - the indiv file
### the right dataset is the household file
### our identifier variables are year, country, and hid

# this works if the identifier variables have the same names in both dataframes
data <- left_join(silc_new2, silc_hh_new,
                  by = c("year", "country", "hid"))

# do this if the columns are named differently in the two datasets
# here: the household identifier is called Hid (with capital H) instead of hid in the second dataframe
data <- left_join(silc_new2, silc_hh_new,
                  by = c("year" = "year", 
                         "country" = "country",
                         "hid" = "Hid"))

# export
saveRDS(data, "data/silc_all.RDS")
