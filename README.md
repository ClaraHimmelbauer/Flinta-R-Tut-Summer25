# FLINTA\* R-Tutorium Summer Term 2025

This course started as a FLINTA\* R-Tutorium in March 2025. The first 4 units covered the basics of R-langauge and were taught in a FLINTA\* context. Starting from unit 5 we will be covering more advanced and specialized topics and the Tutorium will be opened up to all students in general.

## Getting started

### Install R and R-Studio

Before you get started, make sure to install R and R-Studio. [Here](https://www.stat.colostate.edu/~jah/talks_public_html/isec2020/installRStudio.html) you can find a guide on how to do this.

### Setting up the folder structure

Working in a clean and tidy environment helps a lot when your projects get bigger. To begin with, we set up three folders:

-   `analysis` is for our R scripts

-   `data` is for storing data

-   `man` stands for manuals. Here we store help files, codebooks, methodological descriptions, etc.

### Set up an R project

Always work inside the project environment. That way, when you return to R-studio and to your project, you can pick up where you left it the last time. Also, this makes sure that internal links are still working (e.g. for loading data).

## Basic principles

### Comment your code

I R, you can use `#` to add lines of comment. Comment your code (excessively), it will help you whenever you return to a project. Or when you need bits and pieces of your code for other projects later on.

### Make headings to navigate your code

Whenever you add four or more dashes or hashes at the end of a comment, R-Studio will recognize it as a heading. You can use it to navigate your code. Furthermore, you can Enumerate your headings or use upper and lowercase letters to further structure you code. Here's an example of some headings:

```         
# 1. MAIN HEADING -------------------------------------------------------
# in this section we want to achieve understanding of struturing a code

# 1.a first subsection ---------------------------------

# 1.b this works as well ###############################
```

### Get help

When coding, you will encounter difficulties and problems. Fortunately, there are many great ways to get help. In `man/get-help.pdf` you can find more details.

1.  Read the error message
2.  Ask an AI
3.  Ask the internet
4.  Read the built in help pages
5.  Ask another person

### Use whitespace and linebreaks

The readability of your code greatly increases if you use white space and line breaks. For example:

```         
# Don't do this:
df$reg_bl<-factor(df$bundesld,levels=c(1:9),labels=c("Burgenland","Kärnten","Niederösterreich","Oberösterreich","Salzburg","Steiermark","Tirol","Vorarlberg","Wien"),ordered=F)

# do this instead:
df$reg_bl <- factor(df$bundesld,
                    levels = c(1:9),
                    labels = c("Burgenland", "Kärnten", "Niederösterreich",
                      "Oberösterreich", "Salzburg", "Steiermark",
                      "Tirol", "Vorarlberg", "Wien"),
                    ordered = F)
```

### Assign your variables to the left side

Same as before, it increases the readability of your code. Don't use `5 -> a`, but use `a <- 5` instead.

## Contact

In case you have questions, please contact me via email: `clara.himmelbauer@wu.ac.at`
