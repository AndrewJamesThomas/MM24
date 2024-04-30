library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(MLmetrics)
library(car)
library(tidymodels)
library(glmnet)

# just some more exploration of the data using a basic regression model
# nothing fancy here, but provides a baseline we can target with more advanced models
# This is based off of processed, not raw, data.

df <- read_csv("data/processed/modeling_data.csv")
df <- df %>% 
  drop_na

test <- read_csv("data/processed/test_data.csv")

# define eval functions
MAPE <- function(model) {
  resid <- model$residuals
  mape <- resid %>% 
    abs %>% 
    mean %>% 
    round(2)
  return(mape)
}


DW <- function(model) {
  resid <- model$residuals
  resid_1 <- resid[1: length(resid)-1]
  resid_2 <- resid[2: length(resid)]
  err <- sum((resid_1-resid_2)^2)
  tse <- sum(resid^2)
  return(round(err/tse, 2))
}


GetLogLoss <- function(model_, X, y, digit=2) {
  win_proba <- predict(model_, newdata=X, type="response")
  ll <- LogLoss(win_proba, y) %>% 
    round(digit)
  return(ll)
}
  

TestModel <- function(model_, test_, digits=2) {
  for (i in 2014:2023) {
    if (i != 2020) {
      test_ <- filter(test, Season==i)
      
      r <- GetLogLoss(model_, test_, test_$win, digits)
      cat("-----\nYear: ", i, "\nLog Loss: ", r, "\n-----")
    }
  }
}


# Build Basic Model -------------------------------------------------------

# do base model
basic_model <- glm(data=df, 
            win ~ offensive_index.x + offensive_index.y + defense_index.x +
            defense_index.y + MOR.x + MOR.y + POM.x + POM.y + MAS.x + MAS.y,
            family="binomial"
            )

# training results: 0.55 Log Loss
summary(basic_model)
TestModel(basic_model, test, 4)
GetLogLoss(basic_model, df, df$win)


