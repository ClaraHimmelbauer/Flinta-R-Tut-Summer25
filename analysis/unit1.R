# FLINA Rtut UNIT 1 ---------------------------------------
# FLINTA R tut line 2 #####################################

# USE R AS A CALCULATOR -----------------------------------
1 + 2

# whitespaces do not matter
2-1
3 *              6

# also linebreaks do not matter if the operator is before the next line
6 /
  2

# check equalities
1 == 1       # 1 equals 1
4 == 2 * 2   # 4 equals two times two
4 > 5


# ASSIGNING VARIABLES -------------------------------------
a <- 4
b = 5

# print them = displaying the value
a
print(a)

# do calculations with variables
a * b

# case sensitivity
A
a

# OTHER DATA TYPES ----------------------------------------
class(a)

# built in R help
?class

# character
c <- "this is a character"
d <- 'hey'
class(c)
class(d)
c + d

# logical
d <- TRUE
e <- T
f <- FALSE
f <- F
class(d)
class(f)

# assign operations
g <- 2 *3
h <- 2 > 4
g
h

# DON'T DO THIS
# assining on the right side
2 -> i
# using no whitespace
i<-2

# using equal signs is okay-ish. but it's not common
i = 2

# REMOVE VARIABLIES ---------------------------------------
rm(a)
remove(b)
rm(c, d)

# clean your whole environment
rm(list = ls())
# after that you can clear your cache
# that's important for large datasets
gc()   # garbage collect


# VECTORS -------------------------------------------------
# numeric vector
# c() stands for "combine"
a <- c(2, 3, 5, 6, 10, -1, 2.4)
a
class(a)

# string vector
b <- c("a", "hello", "evening", "ciao")
class(b)


# exercise
# make a vector that has both strings and numerics in it
# what happens?
# what class does it have?
c <- c("hello", "2", 4, "no")
c
class("4")

# convert it to numeric
d <- as.numeric(c)
class(d)
d

# MATRIX --------------------------------------------------
# before that clean environment
rm(list = ls())

# assign 3 vectors, all of them have a length of 10
# the first vector should be an ID of a person
# the second vector the age
# the third vector the number of books the person read this year
a <- 1:10
b <- c(50, 17, 40, 44, 34, 25, 67, 85, 90, 78)
c <- c(3, 4, 7, 5, 9, 29, 1, 2, 2, 3)

# bind colums = cbind
BOOKS <- cbind(a, b, c)
BOOKS
class(BOOKS)

# assign names column names
colnames(BOOKS) <- c("id", "age", "books")
BOOKS

# ACCESS DIFFERENT ELEMENTS OF A VECTOR -------------------
b
# access 3rd element of vector
b[3]
# acess multiple arguments
b[c(3, 5)]

# ACCESSING A MATRIX --------------------------------------
BOOKS
# element of 5th row and 2nd column
BOOKS[5, 2]
BOOKS[c(5, 7), 2]


# DATAFRAME -----------------------------------------------
df <- data.frame(BOOKS)
df
# look at it by calling the View function (with a captial V)
View(df)

# accessing columns 
df$age

# age column is the same as vector b
df$age == b
table(df$age == b)

# new variable = new column
# ifelse(condition, if, else)
# if a person is 60 or older, "yes" that person is retired, else "no" the person is not retired
df$retired <- ifelse(df$age > 60, "yes", "no")
df

# add rows
# df_2 <- data.frame("id" = 11,
#          "age" = 20,
#          "books" = 13)
# 
# df_3 <- dplyr::bind_rows(df, df_2)
# df_3$retired <- ifelse(df_3$age > 60, "yes", "no")

# FUNCTION ------------------------------------------------
# here we want to write a function that assigns "yes"
# if a person is older than 60 and "no" otherwise

# retired = name of the function
# fucntion() = we're going to create a function
# (x) = arguments of the function. here only one number = input
# {} here we define everything that the function should do
# return = what the function should return
retired <- function(x){
  if(x > 60){
    return("yes")
  } else {
    return("no")
  }
}

retired(3)
retired(61)

# try the function on our vector
retired(df$age)

# LOOP over or vector -------------------------------------
# for i (= each element) in the vector df$age
# {} inside paranthesis stands what should happen with each element
for(i in df$age){
  x <- retired(i)
  print(x)
}

# How to use our custom function on the datafame
# if it's overwhelming it's ok

# way 1: loop
# create an empty column
df$retired2 <- NA
# loop over each row of the dataframe/element of age
# and assign the output to the new column
for(i in 1:nrow(df)){
  x <- df$age[i]
  df$retired2[i] <- retired(x)
}
df

# way 2: apply
# or sapply for simple apply
# the apply function is there to "vectorize" operations
df$retired3 <- sapply(df$age, retired)
