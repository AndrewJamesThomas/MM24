library(readr)
library(dplyr)

df <- read_csv("01 data/04 Simulation and Optimization/Outputs/simulation.csv")

teams <- read_csv("01 data/01 raw/MTeams.csv") %>% 
  select(TeamID, TeamName)

win_proba <- function(game, data=df, team_names=teams) {
  row_count <- dim(data)[1]
  output <- df %>% 
    select(cols = game) %>% 
    count(cols, sort=TRUE) %>% 
    mutate(WinProba = round(n/row_count, 4)*100) %>% 
    left_join(team_names, by=c("cols"="TeamID")) %>% 
    select(TeamName, WinProba)
  return(output)
}

win_proba("E69")
