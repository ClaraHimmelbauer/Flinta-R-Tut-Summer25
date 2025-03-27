# FLINA Rtut UNIT 1 -----------------------------------------------------------
# FLINTA R tut line 2 #########################################################
# the two lines above are an example on how to make a heading.
# R-studio recognizes it and you can use this to navigate your code
# the naviagation is at the bottom of the script sheet.

# 1. USE R AS A CALCULATOR ----------------------------------------------------
# You can run your code by pressing Ctrl + Enter
1 + 2

# whitespaces do not matter
2-1
3 *              6

# also linebreaks do not matter if the operator is before the next line
6 /
  2

# you can use R to check equalities
1 == 1       # 1 equals 1
4 == 2 * 2   # 4 equals two times two
4 > 5


# 2. ASSIGNING VARIABLES ------------------------------------------------------
# assign a value to a variable by using <- or =
a <- 4
b = 5

# you can also assign longer an more complex lines of codes
# here, it's just a simple calcualtion
# but the lines right to the "<-" can get pretty long
b <- 2 + 3

# print them = displaying the value
a
print(a)

# do calculations with variables
a * b

# case sensitivity
A
a

# 3. DATA TYPES ---------------------------------------------------------------
# There are many data types. Here, I show you just the 3 most basic ones.
# you will encounter more data types as you continue your R-journey.

# numerics ------------------------------------------------
# a is a numeric. (because a <- 4 and 4 is a number).
class(a)
# There are two sub-types of numerics: integers and doubles
# integers are full number without decimal marks and they take up less memory and storage space

# built in R help
# by typing a question mark followed by a function you can access the built-in help page for this function
?class

# character -----------------------------------------------
# characters is text. in other programming languages it's often called "string"
# characters in R are indicated by quotation marks or apostrophes
c <- "this is a character"
d <- 'hey'
class(c)
class(d)

# you cannot do mathematical operations with text
c + d

# logical -------------------------------------------------
# there are 3 logicals: TRUE, FALSE, and NA. 
# other programming languages might call this "boolean".
d <- TRUE
e <- T
f <- FALSE
f <- F
class(d)
class(f)


# 4. SOME OTHER VARIABLE BASICS -----------------------------------------------

# assigning operations to variables is possible -----------
g <- 2 * 3
h <- 2 > 4
g
h

# DON'T do this !!! ---------------------------------------

# don't assign a variable to the right side
2 -> i
# please use whitespace
i<-2

# using equal signs is okay-ish. but it's not common
i = 2

# don't use umlauts, it will lead to encoding problems
ä <- 30

# remove variables ----------------------------------------
# rm and remove are the same
rm(a)
remove(b)

# you can remove multiple objects
rm(c, d)

# clean your whole environment
rm(list = ls())

# after you delete something, you might want to clear your cache
# the fucntion for this is gc() which stands for garbage collect
gc()
# this is especially improtant if you work with large data sets
# which take up all of your PC's memory (RAM).
# sometimes you need to gc() after each line (yeah, it's a pain in the ass)


# 5. VECTORS ------------------------------------------------------------------
# numeric vector
# c() stands for "combine"
# in a vector, we combine multiple values into one object - the vector
a <- c(2, 3, 5, 6, 10, -1, 2.4)
a
class(a)

# string vector
b <- c("a", "hello", "evening", "ciao")
class(b)


# exercise ------------------------------------------------
# make a vector that has both strings and numerics in it
# what happens? all objects are cahracter
# what class does it have? character. a vector can only have one data type
c <- c("hello", "2", 4, "no")
c
class(c)

# convert it to numeric
d <- as.numeric(c)
d
class(d)
# text cannot be converted to numerics. therefore, NAs are created.

# 6. MATRIX -------------------------------------------------------------------
# before that clean environment
rm(list = ls()); gc()

# create a matrix -----------------------------------------
# we want to create a matrix with thre columns.
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

# 7. ACCESS ELEMENTS ----------------------------------------------------------
# vector --------------------------------------------------
b
# access 3rd element of vector
b[3]
# acess multiple arguments
b[c(3, 5)]

# matrix --------------------------------------------------
BOOKS
# element of 5th row and 2nd column
BOOKS[5, 2]
BOOKS[c(5, 7), 2]


# 8. DATAFRAME ----------------------------------------------------------------
# dataframes are what makes R cool to work with
# it's like a table where each column stores another variable
# the columns can have different data types (which is not the case for matrices)

# dataframe basics ----------------------------------------
df <- data.frame(BOOKS)
df
# look at it by calling the View function (with a captial V)
View(df)

# accessing columns 
df$age
# the dataframe has two dimensions. the first dimension is the row, the second one the columns
# therefore, when we use cornered brackets to access a dataframe, we need to call both dimensions
# we want all rows for the column called "age".
# by writing nothing in front of the comma, we tell R, that we want to access all rows
df[, "age"]

# age column is the same as vector b
df$age == b
table(df$age == b)

# new columns/variables -----------------------------------
# ifelse(condition, if, else)
# if a person is 60 or older, "yes" that person is retired, else "no" the person is not retired
df$retired <- ifelse(df$age > 60, "yes", "no")
df

# add rows - more complex. this is just an example
# df_2 <- data.frame("id" = 11,
#          "age" = 20,
#          "books" = 13)
# 
# df_3 <- dplyr::bind_rows(df, df_2)
# df_3$retired <- ifelse(df_3$age > 60, "yes", "no")

# 9. FUNCTIONS ----------------------------------------------------------------
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
# this does not work because or funciton only works on one element (x)
# however, the vector x has a length of 10 and therefore 10 elements

# LOOP over or vector -------------------------------------
# for i (= each element) in the vector df$age
# {} inside paranthesis stands what should happen with each element
for(i in df$age){
  x <- retired(i)
  print(x)
}

# How to use our custom function on the datafame ----------
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
df
