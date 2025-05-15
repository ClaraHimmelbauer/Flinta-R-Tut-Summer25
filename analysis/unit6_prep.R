
# PREP ----------------------------------------------------
rm(list = ls()); gc()

packages <- c("tidyverse", "lmtest", "haven", "ggridges")
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
