# PREP ----------------------------------------------------
# clean workspace
rm(list = ls()); gc()

# packages
packages <- c("tidyverse", "lmtest", "haven", "ggridges", "broom", "boot")
# sapply(packages, install.packages, character.only = T)
sapply(packages, library, character.only = T)

# DATA ----------------------------------------------------
silc <- readRDS("data/silc_AT23.rds")
glimpse(silc)

# DATA WRANGLING ------------------------------------------
silc$employed <- factor(silc$employed,
                        levels = c(0,1),
                        labels = c("no", "yes"))

# WHAT VARIABLES DO WE PUT IN OUR REGRESSION? -------------

# 1. do it in a loop
# 2. distinguishing between categorical and numerical data

names <- c("degurba", "sex", "age", "activity", "birthplace",
           "employed", "workinghours", "educ")

# loop basics ---------------
for(i in 1:10){
  print(i^2)
}

for(single_name in names){
  print(single_name)
}

for(i in 1:length(names)){
  single_name <- names[i]
  print(single_name)
}

# if else basics ------------

x <- c(1,2,3)
# x <- c("yes", "nope")

if(is.numeric(x)){
  print("wohoo, numeric")
} else {
  print("nope, not numeric")
}

# so let's make our loop ----

i <- 5
i <- 3

for(i in 1:length(names)){
  colname <- names[i]
  num <- is.numeric(silc[[colname]])
  
  if(num == TRUE){
    cor <- cor(silc[[colname]], silc$inc20)
    
    p <- ggplot(data = silc,
                aes(x = !!sym(colname), y = inc20, weight = wght)) +
      geom_point(color = "grey30") +
      geom_smooth(color = "slateblue") +
      labs(title = paste0("Correlation between ", colname, " and income"),
           subtitle = paste0("Cor: ", round(cor, 4)),
           x = colname,
           y = "Income vingtile") +
      theme_minimal()
    
    print(p)
    
  } else {
    
    p <- ggplot(data = silc,
                aes(y = !!sym(colname), x = inc20, weight = wght)) +
      geom_density_ridges(fill = "slateblue", alpha = 0.5) +
      labs(title = paste0("Correlation between ", colname, " and income"),
           subtitle = "Categorical data",
           y = colname,
           x = "Income vingtile") +
      coord_flip() +
      theme_minimal()
    
    print(p)
      
  }
  
}

# DEFINE ADDITIONAL VARS ----------------------------------
# DATA WRANGLING #2 ---------------------------------------

silc$agesq <- silc$age^2
silc$agecu <- silc$age^3

silc$workinghours_sq <- silc$workinghours^2
silc$workinghours_cu <- silc$workinghours^3

silc$activity2 <- ifelse(silc$activity == "unemployed", "unemployed",
                         ifelse(silc$activity == "retired", "retired", "other"))
silc$activity2 <- factor(silc$activity2)

# MODEL ---------------------------------------------------
# recommended: start simple and add gradually

# start with simple model
ols <- lm(inc20 ~
            degurba +
            sex + 
            age + 
            activity + 
            birthplace + 
            workinghours + 
            educ,
          data = silc,
          weights = wght)
summary(ols)

# add higher order terms
ols2 <- lm(inc20 ~
            degurba +
            sex + 
            age + agesq + 
            activity + 
            birthplace + 
            workinghours + workinghours_sq + workinghours_cu +
            educ,
          data = silc,
          weights = wght)
summary(ols2)

# add different variable specifications
ols3 <- lm(inc20 ~
             degurba +
             sex + 
             age + agesq +
             activity2 +
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ,
           data = silc,
           weights = wght)
summary(ols3)

# add interaction terms
ols4 <- lm(inc20 ~
             degurba +
             sex + 
             age + agesq + 
             activity + 
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ +
             degurba:birthplace +
             degurba:sex,
           data = silc,
           weights = wght)
summary(ols4)

# PRESENTING MODELS ---------------------------------------
# tables --------------------

# stargazer package
stargazer::stargazer(ols4, type = "latex")

# modelsummary package
modelsummary::modelsummary(ols)

# compare different models on screen
texreg::screenreg(list(ols, ols4))

# coefficient plot ----------
dfols <- broom::tidy(ols4, conf.int = T)

dfols$sig <- ifelse(dfols$p.value < 0.01, "1% level",
                    ifelse(dfols$p.value < 0.05, "5% level",
                           ifelse(dfols$p.value < 0.1, "10% level", "Higher")))

table(dfols$sig)

ggplot(data = dfols, aes(x = term, y = estimate, color = sig)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(title = "OLS coefficients",
       x = "Variable",
       y = "Coefficient") +
  scale_color_manual(name = "Significance level",
                     breaks = c("1% level", "5% level", "10% level", "Higher"), 
                     values = c("slateblue4", "slateblue2", "plum", "grey")) +
  theme_minimal()
