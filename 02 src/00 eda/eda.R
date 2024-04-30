library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(MLmetrics)
library(car)

# just some basic Exploration of the data
# no need to redo in future versions unless rebuilding model from scratch
# so maybe redo.

games <- read_csv("data/raw/MRegularSeasonDetailedResults.csv")
tourney <- read_csv("data/raw/MNCAATourneyDetailedResults.csv")

MTeams <- read_csv("data/raw/MTeams.csv")
ordinals <- read_csv("data/raw/MMasseyOrdinals.csv")


# plot points per team per game
df <- games %>% 
  select(season=Season, period = DayNum, winning_team = WTeamID, 
         loosing_team = LTeamID, winner_score=WScore, looser_score = LScore,
         WLoc) %>% 
  mutate(game_id = paste(season, period, winning_team, loosing_team, sep="_"),
         winner_home = WLoc=="H", looser_home = WLoc=="L")

df_long = df %>% 
  select(game_id, season, period, team=winning_team, score=winner_score) %>% 
  bind_rows(select(df, game_id, season, period, team=loosing_team, score=looser_score)) %>% 
  left_join(MTeams, by=c("team"="TeamID")) %>% 
  select(-c("FirstD1Season", "LastD1Season")) %>% 
  arrange(team, season, period)


# graph timeseries
gg <- df_long %>% 
  filter(team==1160, season==2023) %>% 
ggplot(data=., aes(x=period, y=score)) +
  geom_line() +
  theme_minimal() 


# build initial model
# create rankings table
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


# build modelling data
df_model <- df %>% 
  select(game_id, season, period, 
         team=winning_team, score=winner_score,
         oppo=loosing_team, oppo_score=looser_score,
         home=winner_home) %>% 
  bind_rows(select(df, game_id, season, period, 
                   team=loosing_team, score=looser_score,
                   oppo=winning_team, oppo_score=winner_score,
                   home=looser_home)) %>% 
  left_join(MTeams, by=c("team"="TeamID")) %>% 
  select(-c("FirstD1Season", "LastD1Season")) %>%
  left_join(MTeams, by=c("oppo"="TeamID")) %>% 
  select(-c("FirstD1Season", "LastD1Season")) %>%
  rename(team_name=TeamName.x, oppo_name=TeamName.y) %>% 
  arrange(team, season, period) %>% 
  mutate(win=score>oppo_score) %>% 
  left_join(rankings, by=c("team" = "TeamID",
                           "period" = "games",
                           "season" = "yrs")) %>% 
  left_join(rankings, by=c("oppo" = "TeamID",
                           "period" = "games",
                           "season" = "yrs")) %>% 
  filter(season>2014)

dependents <- select(df_model, score, oppo_score, win)
independents <- select(df_model, home, MAS.x, MOR.x, POM.x, MAS.y, MOR.y, POM.y)

# do model..go!
model1 <- lm(data=df_model, 
    score ~ home + MAS.x + MOR.x + POM.x + MOR.y + POM.y) 
durbinWatsonTest(model1) #DW 1.6
mean(abs(model1$residuals)) #MAPE 8.7









# graphing
# get decomps
coefs <- model1$coefficients[2:length(model1$coefficients)]

mat <- df_model[c("home", "MAS.x", "MOR.x", "POM.x", "MOR.y", "POM.y")] %>% 
  as.matrix()

decomps <- t(t(mat)*coefs)

### Graph 1: get contributions
contributions <- colSums(decomps, na.rm=TRUE)
contr <- round((contributions**2/sum(contributions**2))*100, 2)
contr <- tibble(var = names(contr), value=contr)

gg <- ggplot(data=contr, aes(x=var, y=value)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_bar(stat="identity")
ggplotly(gg)


### Graph 2: actual/predictions
results <- df_model %>% 
  bind_cols(tibble(pred=predict(model1, df_model))) %>% 
  mutate(error = pred-score)

gg <- results %>% 
  filter(team==1101, season==2024) %>% 
  select(period, score, pred, error) %>% 
ggplot(data=.) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_line(aes(x=period, y=score)) +
  geom_line(aes(x=period, y=pred), color="blue") +
  geom_col(aes(x=period, y=error, fill=error < 0)) +
  scale_fill_manual(values=c("dark green", "red"), guide=FALSE)
ggplotly(gg)

### Graph 3: total volume decomp
decomp_results <- df_model %>% 
  bind_cols(tibble(pred=predict(model1, df_model))) %>% 
  select(team, season, period, pred, score) %>% 
  bind_cols(decomps) %>% 
  pivot_longer(cols=c(home, MAS.x, MOR.x, POM.x, MOR.y, POM.y), names_to = "variable", values_to="value")
  

gg <- decomp_results %>% 
  filter(team==1101, season==2024, !(variable %in% c("MOR.y", "POM.x"))) %>% 
  ggplot(data=.) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_bar(aes(x=period, y=value, fill=variable), position="stack", stat="identity") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(x=period, y=score)) +
  geom_line(aes(x=period, y=pred), color="blue")
ggplotly(gg)
