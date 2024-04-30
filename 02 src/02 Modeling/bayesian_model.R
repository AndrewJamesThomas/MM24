library(readr)
library(dplyr)
library(tidyr)
library(rstan)
library(MLmetrics)

# Bayesian Model is more of a POC than anything
# This might not work any better than a normal ML model, but also, it might?
# It's a simple logistic regression with priors set on some variables.
# Output: CSV with names of variables and associated coefficients.
# Improvement Opps: 
#     -- Make the fit better
#     -- Run the predictions in Stan, as opposed to manually
# Instructions: Run model, make sure model fit is good, export model coefs

df <- read_csv("01 data/02 Processed Data/modeling_data.csv")
df <- df %>% 
  drop_na %>% 
  filter(season > 2014)

test <- read_csv("01 data/02 Processed Data/test_data.csv")

# read in STAN model

independent_vars <- c(
  "offensive_index.x", 
  "offensive_index.y",
  "defense_index.x",
  "defense_index.y", 
  "MOR.x",
  "MOR.y",
  "POM.x",
  "POM.y",
  "MAS.x",
  "MAS.y")
independent_vars_matrix <- select(df, independent_vars) %>% 
  as.matrix

dependent_var <- select(df, win) %>% 
  mutate(win = as.integer(win)) %>% 
  as.vector %>% 
  .$win

stan_data <- list(
  N = dim(independent_vars_matrix)[1],
  k = dim(independent_vars_matrix)[2],
  x = independent_vars_matrix,
  y = dependent_var
)

stan_model <- stan(
  file = "02 src/02 modeling/stan_model.stan",
  data = stan_data,
  chains = 4,
  cores = 4)

# get coefs
coefs <- get_posterior_mean(stan_model)[1:10, 5]
vars <- c(independent_vars)
model_output <- tibble(variable = vars, coef=coefs)

# make prediction
logit <- function(l) {
  p <- exp(l)/(1+exp(l))
  return(p)
}

mat <- test %>% 
  select(independent_vars)
decomps <- t(t(mat)*coefs)

link <- rowSums(decomps)
pred <- sapply(link, FUN=logit)
bayes_pred_ll <- LogLoss(pred, test$win)

# basic model to beat
basic_model <- glm(data=df, 
                   win ~ offensive_index.x + offensive_index.y + defense_index.x +
                     defense_index.y + MOR.x + MOR.y + POM.x + POM.y + MAS.x + MAS.y,
                   family="binomial"
)
basic_pred <- basic_model %>% 
  predict(test, type="response")
basic_pred_ll <- LogLoss(basic_pred, test$win)

model_output
cat("Benchmark:", basic_pred_ll, "\nBayesian: ", bayes_pred_ll)

write.csv(model_output, "03 model/bayesian_model.csv", row.names=FALSE)
