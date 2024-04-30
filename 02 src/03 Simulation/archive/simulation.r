library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# define number of simulations to run
TOTAL_SIMS <- 1000

# the most important thing here is to make sure that the input file exactly matches the tempalte

# Write Functions for Simulation ------------------------------------------
seed_bonus <- function(winner, looser, data=predictions) {
  # accepts two teams and returns the bonus if applicable
  match <- data %>% 
    filter(team_id.x == winner, team_id.y == looser) %>% 
    mutate(seed.x = str_sub(seed.x, 2, 3),
           seed.x = as.integer(seed.x),
           seed.y = str_sub(seed.y, 2, 3),
           seed.y = as.integer(seed.y))
  if (match$seed.x > match$seed.y) {
    bonus <- match$seed.x - match$seed.y
  } else {
    bonus <- 0
  }
  return(bonus)
}


select_winner <- function(seed_1, seed_2, data=predictions) {
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


run_tourney <- function(data=turney, seed_data=seeds) {
  # Simulate the tournament
  # accepts tournament structure
  # returns 4 data sets: winners, loosers, base points, bonus points
  ### columns are the games, rows are the simulations

  team_1_switch <- TRUE
  for (i in 1:63) {
    # loop through each game in the tournament
      # define the teams that are playing
      team1 <- data[[i, 6]]
      team2 <- data[[i, 7]]
  
      # select and declare the winner
      winner <- select_winner(team1, team2)
      data[i, 8] <- winner
      
      if (i == 63) {
        # break out of loop if we're on the last game
        break
      }
      
      # set up the winner of the game as a competitor in the next game
      next_game <- data[[i, 5]]
      if (team_1_switch) {
        data[data$ID == next_game, 6] <- winner
        team_1_switch <- FALSE
      } else{
        data[data$ID == next_game, 7] <- winner
        team_1_switch <- TRUE
      }
  }
  data <- data %>% 
    left_join(seed_data, by=c("team_1"="Seed")) %>% 
    left_join(seed_data, by=c("team_2"="Seed")) %>% 
    left_join(seed_data, by=c("winner"="Seed")) %>% 
    rename(team_1_id = TeamID.x,
           team_2_id = TeamID.y,
           winner_id = TeamID
           ) %>% 
    select(-c("Tournament.x", "Tournament.y", "Tournament"))
    
  return (data)
}


extract_winners <- function(turney_results) {
  results <- turney_results$winner_id
  return(results)
}

extract_loosers <- function(turney_results) {
  output <- c()
  for (i in 1:63) {
    if (turney_results[i, 8] != turney_results[i, 6]) {
      output <- c(output, turney_results[[i, 9]])
    } else {
      output <- c(output, turney_results[[i, 10]])
    }
  }
  return(output)
}


extract_base_points <- function(turney_results) {
  results <- turney_results$base_points
  return(results)
}


extract_bonus_points <- function(winners, loosers) {
  output <- c()
  for (i in 1:63) {
    team1 <- winners[[i]]
    team2 <- loosers[[i]]
    bonus <- seed_bonus(team1, team2)
    output <- c(output, bonus)
  }
  return(output)
}

simulate_tourney <- function(SIMS=TOTAL_SIMS) {
  # This could use some cleaning up
  # Repeats the tournament simulation N times
  # returns a list with the output files
  first_time <- TRUE
  for (i in 1: SIMS) {
    if (i %% 10 == 0) {
      print(i)
    }
    if (first_time) {
      results <- run_tourney()
      # winners
      winners <- extract_winners(results) %>% 
        t %>% 
        data.frame
      names(winners) <- results$ID
      
      # loosers
      loosers <- extract_loosers(results) %>% 
        t %>% 
        data.frame
      names(loosers) <- results$ID
      
      # base points
      base_points <- extract_base_points(results) %>% 
        t %>% 
        data.frame
      names(base_points) <- results$ID
      
      # bonus points
      bonus_points <- extract_bonus_points(winners, loosers) %>% 
        t %>% 
        data.frame
      names(bonus_points) <- results$ID
      
      first_time <- FALSE
    } else {
      results <- run_tourney()
      # winners
      w <- extract_winners(results) %>% 
        t %>% 
        data.frame
      names(w) <- results$ID
      winners <- bind_rows(winners, w)
      
      # loosers
      l <- extract_loosers(results) %>% 
        t %>% 
        data.frame
      names(l) <- results$ID
      loosers <- bind_rows(loosers, l)
      
      # base points
      bp <- extract_base_points(results) %>% 
        t %>% 
        data.frame
      names(bp) <- results$ID
      base_points <- bind_rows(base_points, bp)
      
      # bonus points
      bops <- extract_bonus_points(w, l) %>% 
        t %>% 
        data.frame
      names(bops) <- results$ID
      bonus_points <- bind_rows(bonus_points, bops)
    }
  }
  output <- list(
    winners = winners,
    loosers = loosers,
    base_points = base_points,
    bonus_points = bonus_points
  )
  return(output)
}

save_results <- function(final_results, file_path = "01 data/04 Simulation and Optimization/Outputs/") {
  # save results as four seperate files
    for (i in 1:4) {
    output <- final_results[i]
    path <- paste(file_path, 
                  names(output),
                  ".csv",
                  sep="")
    write_csv(output[[1]], path)
  }
}


# Run Simulation ----------------------------------------------------------

# read in and process data
seeds <- read_csv("01 data/01 raw/2024_tourney_seeds.csv") %>% 
  filter(Tournament=="M")

MTeams <- read_csv("01 data/01 raw/MTeams.csv") %>% 
  select(TeamID, TeamName)

predictions <- read_csv("01 data/03 Model Output/model_predictions.csv") %>% 
  left_join(MTeams, by = c("team_id.x" = "TeamID")) %>% 
  left_join(MTeams, by = c("team_id.y" = "TeamID"))

turney = read_csv("01 data/04 Simulation and Optimization/Inputs/tournament_structure_sam.csv")

# run simulation and save results
results <- simulate_tourney()
save_results(results)

# create team details file
team_details <- seeds %>% 
  left_join(MTeams, by="TeamID") %>% 
  select(team_name=TeamName, Seed, team_id=TeamID)
write_csv(team_details,  "01 data/04 Simulation and Optimization/Outputs/team_details.csv")


