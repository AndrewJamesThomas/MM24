import pandas as pd
from pyomo.environ import *
import warnings
warnings.simplefilter(action='ignore')

points = pd.read_csv("data/simulation/simulated_tournament_base_points.csv")
points = points.drop(['Unnamed: 0'], axis=1)

winners = pd.read_csv("data/simulation/simulated_tournament_winner.csv")
winners = winners.drop(['Unnamed: 0'], axis=1)

details = pd.read_csv("data/simulation/team_details.csv")
teams = details["team_id"]

games = winners.columns
sims = points.index


def objective():
    return sum([sum([points.loc[i, g] *
                     model.dv[(winners.loc[i, g], g)]
                for g in games]) for i in sims])


# initialize optimization model
model = ConcreteModel()

# establish DVs and objective
model.dv = Var(teams, games, domain=Binary)
model.points = Objective(expr=objective(),
                         sense=maximize)

# set up constraints
model.cons = ConstraintList()

# make sure each game gets one and only one winner
for g in games:
    model.cons.add(sum([model.dv[t, g] for t in teams]) == 1)


# make sure each game is dependent on winning the previous game in the structure
def tournament_rule_winner(model, team, primary_game, dependent_game_1, dependent_game_2):
    return model.dv[team, primary_game] <= (model.dv[team, dependent_game_1] + model.dv[team, dependent_game_2])


for t in teams:
    model.cons.add(tournament_rule_winner(model, t, "E69", "E51", "E52"))

    model.cons.add(tournament_rule_winner(model, t, "E51", "A41", "B41"))
    model.cons.add(tournament_rule_winner(model, t, "E52", "C41", "D41"))

    model.cons.add(tournament_rule_winner(model, t, "A41", "A31", "A32"))
    model.cons.add(tournament_rule_winner(model, t, "B41", "B31", "B32"))
    model.cons.add(tournament_rule_winner(model, t, "C41", "C31", "C32"))
    model.cons.add(tournament_rule_winner(model, t, "D41", "D31", "D32"))

    model.cons.add(tournament_rule_winner(model, t, "A31", "A21", "A22"))
    model.cons.add(tournament_rule_winner(model, t, "A32", "A23", "A24"))
    model.cons.add(tournament_rule_winner(model, t, "B31", "B21", "B22"))
    model.cons.add(tournament_rule_winner(model, t, "B32", "B23", "B24"))
    model.cons.add(tournament_rule_winner(model, t, "C31", "C21", "C22"))
    model.cons.add(tournament_rule_winner(model, t, "C32", "C23", "C24"))
    model.cons.add(tournament_rule_winner(model, t, "D31", "D21", "D22"))
    model.cons.add(tournament_rule_winner(model, t, "D32", "D23", "D24"))

    model.cons.add(tournament_rule_winner(model, t, "A21", "A11", "A12"))
    model.cons.add(tournament_rule_winner(model, t, "A22", "A13", "A14"))
    model.cons.add(tournament_rule_winner(model, t, "A23", "A15", "A16"))
    model.cons.add(tournament_rule_winner(model, t, "A24", "A17", "A18"))
    model.cons.add(tournament_rule_winner(model, t, "B21", "B11", "B12"))
    model.cons.add(tournament_rule_winner(model, t, "B22", "B13", "B14"))
    model.cons.add(tournament_rule_winner(model, t, "B23", "B15", "B16"))
    model.cons.add(tournament_rule_winner(model, t, "B24", "B17", "B18"))
    model.cons.add(tournament_rule_winner(model, t, "C21", "C11", "C12"))
    model.cons.add(tournament_rule_winner(model, t, "C22", "C13", "C14"))
    model.cons.add(tournament_rule_winner(model, t, "C23", "C15", "C16"))
    model.cons.add(tournament_rule_winner(model, t, "C24", "C17", "C18"))
    model.cons.add(tournament_rule_winner(model, t, "D21", "D11", "D12"))
    model.cons.add(tournament_rule_winner(model, t, "D22", "D13", "D14"))
    model.cons.add(tournament_rule_winner(model, t, "D23", "D15", "D16"))
    model.cons.add(tournament_rule_winner(model, t, "D24", "D17", "D18"))


# Solver the problem
print("Solving model")
SolverFactory("glpk").solve(model)

# save output
print ("saving model")
output = pd.DataFrame([(model.dv[i](), i[0], i[1]) for i in model.dv])\
    .pivot(index=1, columns=2)[0]\
    .reset_index()\
    .merge(details, left_on=1, right_on="team_id")\
    .drop([1, 'seed', 'team_id'], axis=1)\
    .set_index("team_name")


output[[i for i in output.keys()]].to_csv("data/results/espn/optimal_results_espn.csv", index=True)
