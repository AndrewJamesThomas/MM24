library(dplyr)
library(tidyr)
library(readr)
library(stringr)


# Step 1: Read in Data ----------------------------------------------------
# BIG THING: check that column headers are consistent. They may change YoY
# define number of simulations to run
TOTAL_SIMS <- 10000

# read in and process data

# cols: Tournament | Seed | TeamID
seeds <- read_csv("01 data/01 raw/2024_tourney_seeds.csv") %>% 
  filter(Tournament=="M")

# cols: TeamID, TeamName ...
teams <- read_csv("01 data/01 raw/MTeams.csv") %>% 
  select(TeamID, TeamName)

# cols: team_id.x, team_id.y, seed.x, seed.y, pred
predictions <- read_csv("01 data/03 Model Output/model_predictions.csv") %>% 
  left_join(teams, by = c("team_id.x" = "TeamID")) %>% 
  left_join(teams, by = c("team_id.y" = "TeamID"))

# cols: ID, base_points, prev_game_1, prev_game_2, next_game, team_1, team_2, winner
tournament = read_csv("01 data/04 Simulation and Optimization/Inputs/tournament_structure.csv")



# Step 2: Define Functions ------------------------------------------------

select_winner_ <- function(seed_1, seed_2, data=predictions) {
  # accepts two teams and, based on the prediction + a random number, simulates a winner
  random_number <- runif(1)
  match <- filter(data, seed.x == seed_1, seed.y == seed_2)
  team_1_win <- match$pred > random_number
  if (team_1_win) {
    return(seed_1)
  } else {
    return(seed_2)
  }
}

simulate_tournament_ <- function(data=tournament, seed_data=seeds) {
  # accepts tournament structure
    # Simulate the tournament
    # returns results: rows are games, columns are outcomes
    
    team_1_switch <- TRUE
    # loop through each game in the tournament
    for (i in 1:63) {
      # define the teams that are playing
      team1 <- data[[i, 6]]
      team2 <- data[[i, 7]]
      
      # select and declare the winner
      winner <- select_winner_(team1, team2)
      data[i, 8] <- winner
      
      if (i == 63) {
        # break out of loop on the last game
        break
      }
      
      # set the winner of the game as a competitor in the next game
      next_game <- data[[i, 5]]
      if (team_1_switch) {
        data[data$ID == next_game, 6] <- winner
        team_1_switch <- FALSE
      } else {
        data[data$ID == next_game, 7] <- winner
        team_1_switch <- TRUE
      }
    }
    data <- data %>% 
      left_join(seeds, by=c("winner"="Seed")) %>% 
      select(id=ID, team=TeamID)
    
    return (data)
}

repeat_simulations <- function(sims=TOTAL_SIMS){
  # repeats the simulation N times
  # each row is a simulation and each col a game
  first_run <- TRUE
  for (i in 1:sims) {
    #start progress bar
    cat(paste0(round(i / sims * 100, 2), '% completed'))
    
    # run simulation
    run <- simulate_tournament_() %>% 
      pivot_wider(names_from=id, values_from=team)

    if (first_run) {
      output <- run
      first_run <- FALSE
    } else {
      output <- bind_rows(output, run)
    }
    
    # finish progress bar
    if (i==sims) {
      cat("Done\n")
    } else {
      cat('\014')
    }
    
  }
  return(output)
}

# Run and Save Simulate ---------------------------------------------------
output <- repeat_simulations()
write_csv(output, "01 data/04 Simulation and Optimization/Outputs/simulation.csv")


