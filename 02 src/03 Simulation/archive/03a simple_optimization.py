import pandas as pd
from pyomo.environ import *
import warnings
warnings.simplefilter(action='ignore')

points = pd.read_csv("01 data/04 Simulation and Optimization/Outputs/base_points.csv")

winners = pd.read_csv("01 data/04 Simulation and Optimization/Outputs/winners.csv")

details = pd.read_csv("01 data/04 Simulation and Optimization/Outputs/team_details.csv")

bonus = pd.read_csv("01 data/04 Simulation and Optimization/Outputs/bonus_points.csv")
bonus = bonus + 1

looser = pd.read_csv("01 data/04 Simulation and Optimization/Outputs/loosers.csv")

teams = details["team_id"]

games_short = winners.columns
games = list([games_short + "_W"][0]) + list([games_short + "_L"][0])

sims = points.index


def objective():
    return sum([sum([points.loc[i, g] *
                     model.dv[(winners.loc[i, g], (g + "_W"))] +
                     bonus.loc[i, g] *
                     model.dv[(looser.loc[i, g], (g + "_L"))]
                for g in games_short]) for i in sims])


# initialize optimization model

model = ConcreteModel()

# establish DVs and objective
model.dv = Var(teams, games, domain=Binary)
model.points = Objective(expr=objective(),
                         sense=maximize)

# set up constraints
model.cons = ConstraintList()
for g in games:
    # each game must have one and only one winner
    model.cons.add(sum([model.dv[t, g] for t in teams]) == 1)


# each game winner must have also won the dependent games
def tournament_rule_winner(model, team, primary_game, dependent_game_1, dependent_game_2):
    return model.dv[team, primary_game + "_W"] <= (model.dv[team, dependent_game_1 + "_W"] + model.dv[team, dependent_game_2 + "_W"])


def tournament_rule_looser(model, team, primary_game, dependent_game_1, dependent_game_2):
    return model.dv[team, primary_game + "_L"] <= (model.dv[team, dependent_game_1 + "_W"] + model.dv[team, dependent_game_2 + "_W"])


# Team can't be both the winner and the looser of a game
def winner_looser_rule(model, team, game):
    w = game + "_W"
    l = game + "_L"
    return (model.dv[team, w] + model.dv[team, l]) <= 1


# Round one teams can only be won by the actual teams
def round_one_teams(game, team1, team2, model=model):
    w = game + "_W"
    l = game + "_L"
    return (model.dv[team1, w] + model.dv[team1, l] + model.dv[team2, w] + model.dv[team2, l]) == 2


for t in teams:
    for g in set([i[:3] for i in games]):
        model.cons.add(winner_looser_rule(model, t, g))


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


for t in teams:
    model.cons.add(tournament_rule_looser(model, t, "E69", "E51", "E52"))

    model.cons.add(tournament_rule_looser(model, t, "E51", "A41", "B41"))
    model.cons.add(tournament_rule_looser(model, t, "E52", "C41", "D41"))

    model.cons.add(tournament_rule_looser(model, t, "A41", "A31", "A32"))
    model.cons.add(tournament_rule_looser(model, t, "B41", "B31", "B32"))
    model.cons.add(tournament_rule_looser(model, t, "C41", "C31", "C32"))
    model.cons.add(tournament_rule_looser(model, t, "D41", "D31", "D32"))

    model.cons.add(tournament_rule_looser(model, t, "A31", "A21", "A22"))
    model.cons.add(tournament_rule_looser(model, t, "A32", "A23", "A24"))
    model.cons.add(tournament_rule_looser(model, t, "B31", "B21", "B22"))
    model.cons.add(tournament_rule_looser(model, t, "B32", "B23", "B24"))
    model.cons.add(tournament_rule_looser(model, t, "C31", "C21", "C22"))
    model.cons.add(tournament_rule_looser(model, t, "C32", "C23", "C24"))
    model.cons.add(tournament_rule_looser(model, t, "D31", "D21", "D22"))
    model.cons.add(tournament_rule_looser(model, t, "D32", "D23", "D24"))

    model.cons.add(tournament_rule_looser(model, t, "A21", "A11", "A12"))
    model.cons.add(tournament_rule_looser(model, t, "A22", "A13", "A14"))
    model.cons.add(tournament_rule_looser(model, t, "A23", "A15", "A16"))
    model.cons.add(tournament_rule_looser(model, t, "A24", "A17", "A18"))
    model.cons.add(tournament_rule_looser(model, t, "B21", "B11", "B12"))
    model.cons.add(tournament_rule_looser(model, t, "B22", "B13", "B14"))
    model.cons.add(tournament_rule_looser(model, t, "B23", "B15", "B16"))
    model.cons.add(tournament_rule_looser(model, t, "B24", "B17", "B18"))
    model.cons.add(tournament_rule_looser(model, t, "C21", "C11", "C12"))
    model.cons.add(tournament_rule_looser(model, t, "C22", "C13", "C14"))
    model.cons.add(tournament_rule_looser(model, t, "C23", "C15", "C16"))
    model.cons.add(tournament_rule_looser(model, t, "C24", "C17", "C18"))
    model.cons.add(tournament_rule_looser(model, t, "D21", "D11", "D12"))
    model.cons.add(tournament_rule_looser(model, t, "D22", "D13", "D14"))
    model.cons.add(tournament_rule_looser(model, t, "D23", "D15", "D16"))
    model.cons.add(tournament_rule_looser(model, t, "D24", "D17", "D18"))


# establish round 1 games
model.cons.add(round_one_teams("A11", 1211, 1209))
model.cons.add(round_one_teams("A12", 1129, 1272))
model.cons.add(round_one_teams("A13", 1163, 1308))
model.cons.add(round_one_teams("A14", 1116, 1436))
model.cons.add(round_one_teams("A15", 1104, 1323))
model.cons.add(round_one_teams("A16", 1403, 1286))
model.cons.add(round_one_teams("A17", 1277, 1172))
model.cons.add(round_one_teams("A18", 1181, 1168))

model.cons.add(round_one_teams("B11", 1124, 1313))
model.cons.add(round_one_teams("B12", 1314, 1266))
model.cons.add(round_one_teams("B13", 1388, 1231))
model.cons.add(round_one_teams("B14", 1417, 1103))
model.cons.add(round_one_teams("B15", 1400, 1439))
model.cons.add(round_one_teams("B16", 1345, 1463))
model.cons.add(round_one_teams("B17", 1293, 1362))
model.cons.add(round_one_teams("B18", 1246, 1389))

model.cons.add(round_one_teams("C11", 1112, 1460))
model.cons.add(round_one_teams("C12", 1371, 1395))
model.cons.add(round_one_teams("C13", 1222, 1412))
model.cons.add(round_one_teams("C14", 1228, 1151))
model.cons.add(round_one_teams("C15", 1161, 1276))
model.cons.add(round_one_teams("C16", 1397, 1255))
model.cons.add(round_one_teams("C17", 1326, 1260))
model.cons.add(round_one_teams("C18", 1437, 1174))

model.cons.add(round_one_teams("D11", 1242, 1411))
model.cons.add(round_one_teams("D12", 1361, 1166))
model.cons.add(round_one_teams("D13", 1234, 1350))
model.cons.add(round_one_teams("D14", 1344, 1355))
model.cons.add(round_one_teams("D15", 1261, 1235))
model.cons.add(round_one_teams("D16", 1458, 1159))
model.cons.add(round_one_teams("D17", 1425, 1274))
model.cons.add(round_one_teams("D18", 1120, 1240))

# Solver the problem
print("Solving model")
SolverFactory("glpk").solve(model)

# save output
print("saving model")
output = pd.DataFrame([(model.dv[i](), i[0], i[1]) for i in model.dv])\
    .pivot(index=1, columns=2)[0]\
    .reset_index()\
    .merge(details, left_on=1, right_on="team_id")\
    .drop([1, 'seed', 'team_id'], axis=1)\
    .set_index("team_name")

output[[i for i in output.keys() if i[3:]=="_W"]].to_csv("data/results/optimal_results.csv", index=True)

# export results distribution
"""
pd.DataFrame([sum([points.loc[i, g] * model.dv[(winners.loc[i, g], g)]() for g in games]) for i in sims])\
    .to_csv("data/results/optimal_distribution.csv", index=False)
"""