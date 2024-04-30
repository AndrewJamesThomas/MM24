library(readr)
library(dplyr)


# Elo Functions -----------------------------------------------------------

predicted_outcome <- function(elo_1, elo_2){
  identity <- (elo_1 - elo_2)/400
  output <- 1/(1+10^identity)
  return(output)
}


update_elo <- function(elo_1, elo_2, win=TRUE, k=16){
  win <- as.integer(win)
  Ea <- predicted_outcome(elo_1, elo_2)
  elo_ <- elo_1 + k*(win-Ea)
  return(elo_)
}



#  Prep data --------------------------------------------------------------
df <- read_csv("01 data/01 raw/WRegularSeasonDetailedResults.csv")
df <- filter(df, Season >= 2014)


stats <- df %>% 
  select(season = Season,
         period = DayNum,
         team = WTeamID)

stats <- df %>% 
  select(season = Season,
         period = DayNum,
         team = LTeamID) %>% 
  bind_rows(stats) %>% 
  arrange(season, team, period) %>% 
  group_by(season, team) %>% 
  mutate(next_game = lead(period),
         elo = 2000) %>% 
  ungroup


df <- df %>% 
  select(season = Season,
         period=DayNum, 
         WTeamID,
         LTeamID)


# Update ELOs -------------------------------------------------------------
for (i in 1:nrow(df)){
  
  # loop through the games and grab elobvalues
  Season <- df[[i, 1]]
  Period <- df[[i, 2]]
  Winner <- df[[i, 3]]
  winner_next_game <- stats[stats$season == Season & 
                            stats$period == Period &
                            stats$team == Winner, 4][[1]]
  
  winner_elo <- stats[stats$season == Season & 
                      stats$period == Period &
                      stats$team == Winner, 5][[1]]
  
  
  looser <- df[[i,4]]
  looser_next_game <- stats[stats$season == Season & 
                            stats$period == Period &
                            stats$team == looser, 4][[1]]  
  
  looser_elo <- stats[stats$season == Season & 
                      stats$period == Period &
                      stats$team == looser, 5][[1]]  
  
  # get updated elo values
#  k_param = (60/Period*(0.75))*16
  k_param <- 16
  winner_elo_updated <- update_elo(winner_elo, looser_elo, win = TRUE, k=k_param)
  looser_elo_updated <- update_elo(looser_elo, winner_elo, win = FALSE, k=k_param)
  
  # assign new elo values to elo table
  if (!is.na(winner_next_game)) {
  stats[stats$season == Season & 
        stats$period == winner_next_game &
        stats$team == Winner, 5] <- winner_elo_updated
  }
  
  if (!is.na(looser_next_game)) {
    stats[stats$season == Season & 
          stats$period == looser_next_game &
          stats$team == looser, 5] <- looser_elo_updated
  }
}

stats %>% 
  filter(team == 3303) %>% 
  ggplot(data=., aes(x=period, y=elo)) +
  geom_line()

write_csv(stats, "01 data/02 Processed Data/elo_scores_w.csv")

