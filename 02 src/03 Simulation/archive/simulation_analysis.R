library(readr)
library(dplyr)


MTeams <- read_csv("01 data/01 raw/MTeams.csv")
df <- read_csv("01 data/04 Simulation and Optimization/Outputs/winners.csv")
seeds <- read_csv("01 data/01 raw/2024_tourney_seeds.csv") %>% 
  filter(Tournament=="M")


predictions <- read_csv("01 data/03 Model Output/model_predictions.csv") %>% 
  left_join(MTeams, by = c("team_id.x" = "TeamID")) %>% 
  left_join(MTeams, by = c("team_id.y" = "TeamID"))

list_games <- function(data=df) {
  return(names(df))
}


team_proba <- function(game, data=df, team_names=MTeams) {
  # accepts a game and returns the probability that each team will win that game
  data <- data[game]
  names(data) <- "col"
  
  output <- data %>% 
    count(col, sort=TRUE) %>% 
    mutate(n=round((n/1000), 4)) %>% 
    left_join(team_names, by=c("col"="TeamID")) %>% 
    select(TeamName, pred=n, col) %>% 
    left_join(seeds, by=c("col"="TeamID")) %>% 
    select(-c("col", "Tournament"))
  return(output)
}

ev_calc <- function(proba, seed_1, seed_2, base_points) {
  # expected value of picking seed 1 to win
  if (seed_1 <= seed_2) {
    bonus <- 0
  } else {
    bonus <- seed_1 - seed_2
  }
  ev_1 <- proba*(bonus + base_points)
  
  # expected value of picking seed 2 to win
  if (seed_2 <= seed_1) {
    bonus <- 0
  } else {
    bonus <- seed_2 - seed_1
  }
  ev_2 <- (1-proba)*(bonus + base_points)  
  
  # output results
  cat("Expected Value of seed 1:", ev_1,
      "\nExpected Value of seed 2:", ev_2)
}

match_up <- function(seed1, seed2) {
  output <- predictions %>% 
    filter(seed.x==seed1, seed.y==seed2)
  cat("Team 1:", output[["TeamName.x"]], 
      "\nTeam 2:", output[["TeamName.y"]],
      "\nPrediction:", round(output[["pred"]],2))
}

# here's our strategy: pick the top teams in for the final four on wards
# pick the highest EVs in the first round
# pick the most favored after that
list_games()
team_proba("D17")
