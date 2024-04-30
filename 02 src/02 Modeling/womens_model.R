library(readr)
library(dplyr)
library(tidymodels)
library(MLmetrics)


df <- read_csv("01 data/02 Processed Data/modeling_data_W.csv")
test <- read_csv("01 data/02 Processed Data/test_data_W.csv")
elo <- read_csv("01 data/02 Processed Data/elo_scores_w.csv")


# prep data ---------------------------------------------------------------
df <- df %>% 
  left_join(elo, by = c("team_id" = "team", 
                        "season" = "season",
                        "period" = "period")) %>% 
  left_join(elo, by = c("oppo_id" = "team", 
                        "season" = "season",
                        "period" = "period")) %>% 
  select(season, period, team_id, oppo_id, win, 
         offensive_index.x, offensive_index.y, 
         defense_index.x, defense_index.y, elo.x, elo.y) %>% 
    drop_na

# Build Model -------------------------------------------------------------
# split data
set.seed(123)
init_split <- initial_split(df, prop=0.9)
df_train <- training(init_split)
df_test <- testing(init_split)
df_fold <- vfold_cv(df_train, v=10)

# just run a GLM ----------------------------------------------------------
model <- glm(formula=win ~ offensive_index.x + offensive_index.y + 
               defense_index.x + defense_index.y + elo.x + elo.y,
             data=df_train,
             family="binomial")
pred_test <- predict(model, df_test, type="response") %>% as.vector
true_test <- df_test$win %>% as.integer()
LogLoss(pred_test, true_test) # 0.57; passable

final_model <- glm(formula=win ~ offensive_index.x + offensive_index.y + 
                     defense_index.x + defense_index.y + elo.x + elo.y,
                   data=df,
                   family="binomial")

saveRDS(final_model, "03 model/womens_model.rds")

