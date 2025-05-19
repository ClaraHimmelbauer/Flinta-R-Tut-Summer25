
# PREP ----------------------------------------------------
rm(list = ls()); gc()

packages <- c("tidyverse", "lmtest", "haven", "ggridges", "broom")
sapply(packages, library, character.only = T)

# DATA ----------------------------------------------------
# reading in data
silc <- readRDS("data/silc_AT23.rds")
glimpse(silc)

# WRANGLING -----------------------------------------------

silc$employed <- factor(silc$employed, c(0,1), c("no", "yes"))

# CORRELATION OF VARIABLES --------------------------------

# with y var ------------------------------------

sapply(silc, is.numeric)

# make a plot
names <- names(silc)[!(names(silc) %in% c("wght", "id", "inc20"))]
                    
for(i in 1:length(names)){
  colname <- names[i]
  num <- is.numeric(silc[[colname]])
  
  if(num){
    cor <- cor(silc[[colname]], silc$inc20)
    
    p <- ggplot(silc, aes(x = !!sym(colname), y = inc20, weight = wght)) +
      geom_point(color = "grey30") +
      geom_smooth(color = "slateblue") +
      labs(title = paste("Correlation between", colname, "and income"),
           subtitel = paste0("Correlation: ", round(cor, 2)),
           x = colname,
           y = "Income Vingtile") +
      theme_minimal()
    
  } else {
    p <- ggplot(silc, aes(y = !!sym(colname), x = inc20, weight = wght)) +
      geom_density_ridges(fill = "slateblue", alpha = 0.5) +
      labs(title = paste("Correlation between", colname, "and income"),
           subtitle = paste0("Correlation: ", "NA"),
           x = colname,
           y = "Income Vingtile") +
      coord_flip() +
      theme_minimal()
  }
  
  print(p)
}

# all variables with each other
names_fac <- names(which(sapply(silc, is.factor)))

X <- silc
for(i in names_fac){
  colname <- i
  X <- X %>% 
  mutate(dummy = 1) %>% 
  pivot_wider(
    names_from = !!sym(colname),
    values_from = dummy,
    values_fill = 0,
    names_prefix = paste0(colname, "_")
  )
}
X <- X %>% 
  select(-c(id, wght, htype_single)) %>% 
  mutate(across(everything(), as.numeric))
cor <- cor(X, use = "pairwise.complete.obs")
diag(cor) <- NA
GGally::ggcorr(cor)

# WRANGLING 2 ---------------------------------------------

silc$agesq <- silc$age^2
silc$agecu <- silc$age^3

silc$activity_retired <- ifelse(silc$activity == "retired", 1, 0)
silc$activity_unemployed <- ifelse(silc$activity == "unemployed", 1, 0)
silc$activity_educ <- ifelse(silc$activity == "education", 1, 0)

silc$workinghours_sq <- silc$workinghours^2
silc$workinghours_cu <- silc$workinghours^3

# MODEL ---------------------------------------------------

# two approaches: start with nothing and then add variables
# or start with all and then remove
# first approach is recommended
# but here in this model, well, limited dataset and stuff

names(silc)
ols <- lm(inc20 ~ degurba +
            sex + age + agesq + agecu +
            activity_retired + activity_unemployed + activity_educ +
            birthplace + employed +
            workinghours + workinghours_sq + workinghours_cu +
            educ,
          weights = wght,
          data = silc)
summary(ols)

# NICE TABLES ---------------------------------------------

# the classics: stargazer
stargazer::stargazer(ols)
stargazer::stargazer(ols, type = "text")

# the modern way: modelsummary which works for word as well
modelsummary::modelsummary(ols)

# to comapre models: texreg
texreg::screenreg(ols)


# THE COEFFICIENT PLOT ------------------------------------

dfols <- broom::tidy(ols, conf.int = T)
dfols$sig <- ifelse(dfols$p.value < 0.01, "1% level",
                    ifelse(dfols$p.value < 0.05, "5% level",
                           ifelse(dfols$p.value < 0.1, "10% level", "not significant")))

ggplot(dfols, aes(x = term, y = estimate, color = sig)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(title = "OLS Coefficients",
       x = "Variable",
       y = "Coefficient") +
  scale_color_manual(name = "Significance level", 
                     values = c("slateblue4", "plum", "grey")) +
  theme_minimal()

# ASSESSING OVERALL MODEL FIT -----------------------------

# f-Statistik, and rsquared already in there
summary(ols)

# comparing multiple models
AIC(ols); BIC(ols)

# LINEARITY -----------------------------------------------
# linearity is violated

lin <- data.frame(residuals = residuals(ols), fitted = fitted(ols))
ggplot(lin, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(color = "#5a78c8") +
  theme_minimal() +
  ylab("Residuals") +
  xlab("Fitted values") +
  labs(title = "Linearity of model")

# we want no systematic pattern between fitted values and residuals.
### linear pattern: omitted variable, misspecification, variable that entered wrongly
### u-shape/curved pattern: nonlinearity between in the relationship between predictors and the outcome
### fan shape: heteroskedasticity
# i want randomness, and no pattern in the residuals vs fitted values

# so we have a biased model in essence.
# ols assumes linear connection. if the true relationship were quadratic, then we would get biased estimates


# HETEROSKEDASTICITY --------------------------------------

# already see graph

# we have heteroskedasticity
lmtest::bptest(ols)

# general note on tests: they nearly always fail. so graphical representation is important

# NORMALITY -----------------------------------------------
ggplot(lin, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), fill = "grey") +
  geom_density(color = "#5a78c8", linewidth = 2) +
  stat_function(fun = dnorm, 
                args = list(mean = 0,
                            sd = sd(lin$residuals)),
                color = "#4d4e4d", linetype = "dashed", size = 1.5) +
  theme_minimal() +
  labs(title = "Distribution of residuals")

qqnorm(residuals(ols)); qqline(residuals(ols))
shapiro.test(residuals(ols))

# AUTOCORRELATION OF RESIDUALS ----------------------------
# leads to inefficiency

lmtest::bgtest(ols, order = 1)

# MULTICOLLINEARITY --------------------------------------
# to be expected
# variables shouldn't be interpreted independently
car::vif(ols)

# INFLUENCE -----------------------------------------------
plot(cooks.distance(ols))

# nothing too bad, but we could exclude this one observation

# BOOTSTRAPPING STANDARD ERRORS ---------------------------
