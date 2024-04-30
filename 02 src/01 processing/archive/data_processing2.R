library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(MLmetrics)
library(car)

# read in data
games <- read_csv("data/raw/MRegularSeasonDetailedResults.csv")
MTeams <- read_csv("data/raw/MTeams.csv")
ordinals <- read_csv("data/raw/MMasseyOrdinals.csv")
tourney <- read_csv("data/raw/MNCAATourneyCompactResults.csv")

# convert data to long
df <- games %>% 
  select(season=Season, period = DayNum, winning_team = WTeamID, 
         loosing_team = LTeamID, winner_score=WScore, looser_score = LScore,
         WLoc) %>% 
  mutate(game_id = paste(season, period, winning_team, loosing_team, sep="_"),
         winner_home = WLoc=="H", looser_home = WLoc=="L")

df_long = df %>% 
  select(game_id, season, period, team=winning_team, oppo=loosing_team, score=winner_score, oppo_score=looser_score, home=winner_home) %>% 
  bind_rows(select(df, game_id, season, period, team=loosing_team, oppo=winning_team, score=looser_score, oppo_score=winner_score, home=looser_home)) %>% 
  left_join(MTeams, by=c("team"="TeamID")) %>% 
  left_join(MTeams, by=c("oppo"="TeamID")) %>% 
  select(game_id, season, period, team_id = team, oppo_id=oppo, team=TeamName.x, oppo=TeamName.y, 
         score, oppo_score, home) %>% 
  arrange(game_id)

# get rankings table
rankings <- ordinals %>% 
  select(season=Season, period=RankingDayNum, system=SystemName, team=TeamID, rank=OrdinalRank) %>% 
  pivot_wider(names_from=system, values_from=rank) %>% 
  select(season, period, team, MAS, MOR, POM) %>% 
  filter(season >= 2014)

yrs <- seq(from=2014, to=2024)
games <- seq(1, 132)
rankings <- MTeams %>% 
  cross_join(tibble(yrs)) %>% 
  cross_join(tibble(games)) %>% 
  select(TeamID, yrs, games) %>% 
  left_join(rankings, by=c("TeamID" = "team",
                           "games" = "period",
                           "yrs" = "season")) %>% 
  fill(MAS, MOR, POM, .direction="down") %>% 
  drop_na()

# O/D Rankings
# Defensive Ranking
stats <- df_long %>% 
  group_by(team_id, season) %>% 
  mutate(running_mean = mean(score),
         running_std_dev = sd(score)) %>% 
  ungroup() %>% 
  select(game_id, season, period, team_id, home, running_mean, running_std_dev) %>% 
  left_join(df_long, by=c("team_id", "season")) %>% 
  mutate(adj_score=(score-running_mean)/running_std_dev) %>% 
  group_by(oppo_id, oppo, season) %>% 
  summarize(defense_index=mean(adj_score)*-100)

# Offensive Ranking
stats <- df_long %>% 
  group_by(team_id, season) %>% 
  mutate(running_mean = mean(oppo_score),
         running_std_dev = sd(oppo_score)) %>% 
  ungroup() %>% 
  select(game_id, season, period, team_id, home, running_mean, running_std_dev) %>% 
  left_join(df_long, by=c("team_id", "season", "period")) %>% 
  mutate(adj_score=(oppo_score-running_mean)/running_std_dev) %>% 
  group_by(oppo_id, oppo, season) %>% 
  summarize(offensive_index=cummean(adj_score)*100) %>% 
  left_join(stats, by=c("oppo_id", "season")) %>% 
  select(team_id=oppo_id, team=oppo.x, season, period, 
         offensive_index, defense_index)


# Modeling
modeling <- df_long %>% 
  left_join(stats, by=c("season", "team_id", "period")) %>% 
  left_join(stats, by=c("season", "oppo_id"="team_id", "period")) %>% 
  left_join(rankings, by=c("season" = "yrs",
                           "period" = "games",
                           "team_id" = "TeamID")) %>%
  left_join(rankings, by=c("season" = "yrs",
                           "period" = "games",
                           "oppo_id" = "TeamID")) %>% 
  select(game_id, team_id, oppo_id, season, period, team = team.x, oppo,
         score, home, oppo_score, starts_with("offensive_index"), starts_with("defense_index"),
         MAS.x, MAS.y, MOR.x, MOR.y, POM.x, POM.y) %>% 
  filter(season >= 2014) %>% 
  mutate(win = score > oppo_score)

# Tourney testing data
test <- tourney %>% 
  filter(Season >= 2014) %>% 
  left_join(filter(stats, game_number==1), 
            by=c("WTeamID" = "team_id", 
                 "Season" = "season")) %>% 
  left_join(filter(stats, game_number==1), 
            by=c("LTeamID" = "team_id", 
                 "Season" = "season")) %>% 
  left_join(filter(rankings, games==132), by=c("Season"="yrs", "WTeamID"="TeamID")) %>% 
  left_join(filter(rankings, games==132), by=c("Season"="yrs", "LTeamID"="TeamID")) %>% 
  mutate(home=FALSE, win=TRUE)


# save data
write.csv(modeling, file="data/processed/modeling_data.csv", row.names = FALSE)
write.csv(test, file="data/processed/test_data.csv", row.names = FALSE)

