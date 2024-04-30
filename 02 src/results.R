library(dplyr)
library(readxl)
library(MLmetrics)

df <- read_csv("01 data/03 Model Output/model_predictions.csv")
teams <- read_csv("01 data/01 raw/MTeams.csv")



out <- df %>% 
  left_join(teams, by=c("team_id.x" = "TeamID")) %>% 
  left_join(teams, by=c("team_id.y" = "TeamID")) %>% 
  select(TeamName.x, TeamName.y, pred)


write.csv(out, "01 data/03 Model Output/model_predictions_readable.csv")

results <- read_excel("04 report/results.xlsx")
results$win <- 1
LogLoss(results$pred, results$win) # 0.57
Accuracy(results$pred > 0.5, results$win) # 72%
