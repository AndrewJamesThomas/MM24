library(readr)
library(dplyr)
library(tidyr)

# Reads in Model coefficients
# Does some basic processing on the current year's data to get it ready for predicting tourney
# Improvements: Do this in Stan instead
# Instructions: Make sure file names are updated, the season is current and the model is good
# output: Prediction probabilities for each potential matchup in long format 

YEAR <- 2024

model <- read_csv("03 model/bayesian_model.csv")
seeds <- read_csv("01 data/01 raw/2024_tourney_seeds.csv")
WTeams <- read_csv("01 data/01 raw/WTeams.csv")
MTeams <- read_csv("01 data/01 raw/MTeams.csv")
df <- read_csv("01 data/02 Processed Data/modeling_data.csv")

logit <- function(l) {
  p <- exp(l)/(1+exp(l))
  return(p)
}

# extract stats for last week
model_data <- df %>% 
  filter(season == YEAR) %>% 
  group_by(team_id) %>% 
  mutate(game_rank = dense_rank(desc(period))) %>% 
  filter(game_rank == 1) %>% 
  select(offensive_index = offensive_index.x, 
         defense_index = defense_index.x, 
         MOR = MOR.x, 
         POM = POM.x, 
         MAS = MAS.x) %>% 
  ungroup() %>% 
  inner_join(seeds, by = c("team_id" = "TeamID")) 

# create matchups for every team vs every team
model_data <- model_data %>% 
  cross_join(model_data) %>% 
  select(team_id.x, team_id.y, seed.x = Seed.x, seed.y = Seed.y, model$variable)
  
# run model
mat <- model_data %>% 
  select(model$variable) %>% 
  as.matrix()
coefs <- as.vector(model$coef)
pred <- rowSums(t(t(mat)*coefs))

output <- tibble(team_id.x = model_data$team_id.x,
       team_id.y = model_data$team_id.y,
       seed.x = model_data$seed.x,
       seed.y = model_data$seed.y,
       odds = pred,
       pred = logit(odds)) %>% 
  inner_join(MTeams, by = c("team_id.x" = "TeamID")) %>% 
  inner_join(MTeams, by = c("team_id.y" = "TeamID")) %>% 
  select(team_id.x, team_id.y, seed.x, seed.y, pred)

# export the results
write_csv(output, "01 data/03 Model Output/model_predictions.csv")
