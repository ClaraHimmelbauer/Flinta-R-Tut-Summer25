# PREP --------------------------------------------------
# clean workspace
rm(list = ls()); gc()

# packages
packages <- c("tidyverse", "lmtest", "haven", "ggridges", "broom", "boot", "survey", "spatstat", "convey")
# sapply(packages, install.packages, character.only = T)
sapply(packages, library, character.only = T)

# functions -----------------------------------------------

# cumulative density Functions
genCDF<-function(x,w) ewcdf(x=x,weights = w, normalise = FALSE)(x)/sum(w)
cutCDF<-function(x,w,q) cut(genCDF(x,w),breaks=seq(0,1,1/q),labels = as.character(1:q))

# DATA ----------------------------------------------------

silc <- readRDS("data/silc_2022.rds")
glimpse(silc)

silc<- silc |>
  filter(age>=18, workinghours>0)

# DATA WRANGLING ------------------------------------------

silc$employed <- factor(silc$employed,
                        levels = c(0,1),
                        labels = c("no", "yes"))

# Deciles
silc <- silc |>
  filter(netinc>0) |>
  mutate(across(c(netinc), 
                list("20" = ~cutCDF(., pweight, 20))))

silc$netinc_20 <- as.numeric(as.character(silc$netinc_20))


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
    cor <- cor(silc[[colname]], silc$netinc_20)
    
    p <- ggplot(data = silc,
                aes(x = !!sym(colname), y = netinc_20, weight = pweight)) +
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
                aes(y = !!sym(colname), x = netinc_20, weight = pweight)) +
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


# --------------------------------------------------------------
# EXAMPLE 1: Numerical variable: "age"
# --------------------------------------------------------------

# 1. Check if "age" is numeric
is.numeric(silc$age)
# -> TRUE

# 2. Calculate the correlation between age and income
#    We use 'complete.obs' so that missing values do not break cor()
cor_age <- cor(silc$age, silc$netinc_20, use = "complete.obs")

# 3. Create the plot
p_age <- ggplot(
  data = silc,
  aes(
    x = age,                # numerical variable on the x-axis
    y = netinc_20,              # income vingtile on the y-axis
    weight = pweight           # survey weight for representativeness
  )
) +
  geom_point(color = "grey30") +       # scatter points
  geom_smooth(color = "slateblue") +   # smooth regression line (LOESS)
  labs(
    title = "Correlation between age and income",
    subtitle = paste0("Cor: ", round(cor_age, 4)),
    x = "Age",
    y = "Income vingtile"
  ) +
  theme_minimal()

# 4. Show the plot
p_age


# DEFINE ADDITIONAL VARS ----------------------------------
# DATA WRANGLING #2 ---------------------------------------

silc$agesq <- silc$age^2
silc$agecu <- silc$age^3

silc$workinghours_sq <- silc$workinghours^2
silc$workinghours_cu <- silc$workinghours^3

silc$activity2 <- ifelse(silc$activity == "Unemployed", "Unemployed",
                         ifelse(silc$activity == "Retired", "Retired", "Other non-employed"))
silc$activity2 <- factor(silc$activity2)

# SURVEY DESIGN -------------------------------------------

silc$strata <- interaction(silc$state, silc$region, drop = TRUE)

# Individual-level survey design
silc.svy <- svydesign(
  ids = ~pid,           # Primary Sampling Unit (PSU) is the individual ID
  strata = ~strata,      # Stratification variable: 'region x state'
  weights = ~pweight,   # Use personal survey weights to ensure representation
  data = silc,       # Input dataset
  nest = TRUE
) %>% 
  convey_prep()      # Prepare for inequality analysis (e.g., Lorenz curve)

# MODEL ---------------------------------------------------
# recommended: start simple and add gradually

# start with simple model
ols <- lm(netinc_20 ~
            degurba +
            sex + 
            age + 
            activity + 
            birthplace + 
            workinghours + 
            educ,
          data = silc,
          weights = pweight)
summary(ols)

# add higher order terms
ols2 <- lm(netinc_20 ~
            degurba +
            sex + 
            age + agesq + 
            activity + 
            birthplace + 
            workinghours + workinghours_sq + workinghours_cu +
            educ,
          data = silc,
          weights = pweight)
summary(ols2)

# add different variable specifications
ols3 <- lm(netinc_20 ~
             degurba +
             sex + 
             age + agesq +
             activity2 +
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ,
           data = silc,
           weights = pweight)
summary(ols3)

# add interaction terms
ols4 <- lm(netinc_20 ~
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
           weights = pweight)
summary(ols4)


ols4_svy <- svyglm(
  netinc_20 ~
    degurba +
    sex + 
    age + agesq +
    activity +
    birthplace +
    workinghours + workinghours_sq + workinghours_cu +
    educ +
    degurba:birthplace +
    degurba:sex,
  design = silc.svy
)
summary(ols4_svy)

texreg::screenreg(list(ols4, ols4_svy))

# Tidy outputs
ols_tidy <- broom::tidy(ols4, conf.int = TRUE) %>% mutate(model = "OLS")
svy_tidy <- broom::tidy(ols4_svy, conf.int = TRUE) %>% mutate(model = "Survey")

# Combine
comp <- bind_rows(ols_tidy, svy_tidy)

# Plot Standard Errors
ggplot(comp, aes(x = term, y = std.error, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Comparison of Standard Errors: OLS vs. Survey-OLS",
       x = "Coefficient",
       y = "Standard Error") +
  scale_fill_manual(values = c("black", "steelblue"))

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
qqline(residuals(ols4), col = "red")

hist(residuals(ols4), breaks = 40, col = "gray")



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
ols5 <- lm(netinc_20 ~
             degurba +
             sex + 
             age + agesq + 
             activity + 
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ +
             degurba:sex,
           data = silc,
           weights = pweight)
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
ols5 <- lm(netinc_20 ~
             degurba +
             sex + 
             age + agesq + 
             activity + 
             birthplace + 
             workinghours + workinghours_sq + workinghours_cu +
             educ +
             degurba:sex,
           data = subsample,
           weights = pweight)
plot(cooks.distance(ols5))

