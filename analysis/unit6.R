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


# LINEARITY -----------------------------------------------
# (and Heteroskedasticity)

# residuals for y axis
# fitted values for x axis

lin <- data.frame(residuals = residuals(ols4),
                  fitted = fitted(ols4))
ggplot(lin, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(color = "slateblue2") +
  theme_minimal() +
  ylab("Residuals") +
  xlab("Fitted values") +
  labs(title = "Linearity and Heteroskedasticity")

# HETEROSKEDASTICITY --------------------------------------

# Breusch-Pagan test for heteroskedasticity
lmtest::bptest(ols4)

# yes, heteroskedasticity
# we need robust standard erros
# bootstrapping - which we do later - is one way of doing that

# NORMALITY -----------------------------------------------

ggplot(lin, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), fill = "grey") +
  geom_density(color = "slateblue", linewidth = 2) +
  stat_function(fun = dnorm,
                args = list(mean = 0,
                            sd = sd(lin$residuals)),
                color = "grey30", linetype = "dashed", size = 1.5) +
  theme_minimal() +
  labs(title = "Distribution of residuals")

# another plot possibility
qqnorm(residuals(ols4))
qqline(residuals(ols4))

# test
shapiro.test(residuals(ols4))
# no normality


# AUTOCORRELATION -----------------------------------------
# breusch-godfrey test

# does not work because of turkey
lmtest::bgtest(ols4, order = 1)

# so just to show use another model
bgtest(ols, order = 1)

# MULTICOLLINEARITY ---------------------------------------

# variance inflation factor
# highly correlated variables create high variances
car::vif(ols4)

# does not work because on observations in degurba x turkey
# add interaction terms
ols5 <- lm(inc20 ~
             degurba +
             sex + 
             age + agesq + 
             activity + 
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ +
             degurba:sex,
           data = silc,
           weights = wght)
car::vif(ols5)


# between whcih pair of variables is the correlation
X <- model.matrix(ols5)
# Drop intercept
X <- X[, -1]
# Compute correlation matrix
cor_matrix <- cor(X)
View(round(cor_matrix, 2))
# also make a heatmap/corplot out of this

# INFLUENCE -----------------------------------------------
# cooks distance shows how much influence one single observation has on the betas
# but outliers can give you additional information
# so you might consider to include them anyway
# or to censor them
# or look at why they are outliers

plot(cooks.distance(ols5))

which(cooks.distance(ols5) > 0.1)
# delete these observations from the sample
subsample <- silc[-c(100, 1739), ]
# estimate the model again
ols5 <- lm(inc20 ~
             degurba +
             sex + 
             age + agesq + 
             activity + 
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ +
             degurba:sex,
           data = subsample,
           weights = wght)
plot(cooks.distance(ols5))

# BOOTSTRAPPING -------------------------------------------

# how to make functions in R
quadratic_equation_solver <- function(a, b, c){
  x1 <- (-b + (b^2 - 4*a*c)^(1/2) ) / (2*a)
  x2 <- (-b - (b^2 - 4*a*c)^(1/2) ) / (2*a)
  return(c(x1, x2))
}
quadratic_equation_solver(a = 1, b = -3, c = 1)

# first create bootstrapping function
# you have to call it data and indices because of the boot package
boot_fn <- function(data, indices){
  
  # specify data
  d <- data[indices, ]
  
  # specify model
  model <- lm(inc20 ~
                degurba +
                sex + 
                age + agesq + 
                activity2 + 
                birthplace + 
                workinghours + workinghours_sq + workinghours_cu +
                educ +
                degurba:sex,
              data = d,
              weights = wght)
  
  # specify what to return
  return(coef(model))
}

# set seed to always draw the same numbers
# for reproducability
set.seed(123)

# bootstrap
# 1000 models in there
boot_res <- boot::boot(data = silc, statistic = boot_fn, R = 1000)

# different coefficients fo reach iteration
View(boot_res$t)

# from these 1000 coefficients, estimate the standard deviations
# iterating over colums
boot_se <- apply(boot_res$t, 2, sd)

# initial coefficients
boot_coef <- boot_res$t0

# z-values
z_values <- boot_coef / boot_se

# p-values (for significance)
p_values <- 2*(1-pnorm(abs(z_values)))

# result dataframe
boot_results <- data.frame(
  #term = names(boot_coef),
  estimate = round(boot_coef,3),
  std_error = round(boot_se,3),
  p_values = round(p_values,3)
)
boot_results

# if you want stars
make_p_stars <- function(x){
  out <- ifelse(x < 0.001, "***",
                ifelse(x < 0.01, "**",
                       ifelse(x < 0.05, "*",
                              ifelse(x < 0.1, ".", ""))))
  return(out)
}

boot_results$stars <- make_p_stars(boot_results$p_values)
boot_results
