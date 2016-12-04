library(data.table)
library(BradleyTerry2)

setwd("~/Desktop/Duke/Fall_2016/Categorical_Data/STA841")
match = fread("match_db.csv")


results = match[,.("home.team" = home_team_api_id, "away.team" = away_team_api_id, 
                    "home.goal" = home_team_goal, "away.goal" = away_team_goal)]
results = results[home.goal != away.goal]

team.ids = unique(results$home.team)

team1 = t(combn(team.ids,2))
team2 = cbind(team1[,2], team1[,1])
team = rbind(team1,team2)
results.dat = data.table(matrix(0, nrow = nrow(team), ncol = 4))
colnames(results.dat) = c("home.team", "away.team", "home.wins", "away.wins")
results.dat$home.team = team[,1]
results.dat$away.team = team[,2]



for (i in 1:nrow(results)){
  print(i)
  home = results$home.team[i]
  away = results$away.team[i]
  if (results$home.goal[i] > results$away.goal[i]){
    results.dat[home.team == home & away.team == away]$home.wins = 
      results.dat[home.team == home & away.team == away]$home.wins + 1
  }
  if (results$home.goal[i] < results$away.goal[i]){
    results.dat[home.team == home & away.team == away]$away.wins = 
      results.dat[home.team == home & away.team == away]$away.wins + 1
  }
}

save(results.dat, file = "~/Desktop/Duke/Fall_2016/Categorical_Data/STA841/results.Rdata")
load("~/Desktop/Duke/Fall_2016/Categorical_Data/STA841/results.Rdata")

results.dat = results.dat[home.wins != 0 | away.wins != 0]
results.dat$home.team = as.factor(results.dat$home.team)
results.dat$away.team = as.factor(results.dat$away.team)

#Simple Model - Ignoring Home Advantage
btModel1 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.dat, id = "team")

#Incorporating "Home Advantage" Effect
results.dat = data.frame(results.dat)
results.dat$home.team <- data.frame(team = results.dat$home.team, at.home = 1)
results.dat$away.team <- data.frame(team = results.dat$away.team, at.home = 0)
btModel2 = update(btModel1, formula = ~ team + at.home)

#Chi Squared Test?
anova(btModel1, btModel2)

results.eff = list()
results.eff$predictors = attrs

contests1 = data.frame("winner" = results[home.goal > away.goal]$home.team, 
                       "loser" = results[home.goal > away.goal]$away.team)
contests2 = data.frame("winner" = results[home.goal < away.goal]$away.team, 
                       "loser" = results[home.goal < away.goal]$home.team)
contests = rbind(contests1, contests2)
contests$winner = as.factor(contests$winner)
contests$loser = as.factor(contests$loser)

results.eff$contests = contests

btModel3 = BTm(1, winner, loser, formula = ~ buildUpPlaySpeed[..] + buildUpPlaySpeedClass[..] + 
                 buildUpPlayDribblingClass[..] + buildUpPlayPassing[..] +
                 buildUpPlayPassingClass[..] + buildUpPlayPositioningClass[..] + chanceCreationPassing[..] +
                 chanceCreationPassingClass[..] + chanceCreationCrossing[..] + chanceCreationCrossingClass[..] +
                 chanceCreationShooting[..] + chanceCreationShootingClass[..] + chanceCreationPositioningClass[..] +
                 defencePressure[..] + defencePressureClass[..] + defenceAggression[..] + defenceAggressionClass[..] +
                 defenceTeamWidth[..] + defenceTeamWidthClass[..] + defenceDefenderLineClass[..], data = results.eff)

coef(btModel3)
