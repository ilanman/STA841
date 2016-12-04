library(data.table)
library(BradleyTerry2)

setwd("~/Desktop/Duke/Fall_2016/Categorical_Data/STA841")
match = fread("match_db.csv")

resultsO = match[,.(match_api_id, "Team_A" = home_team_api_id, "Team_B" = away_team_api_id, 
                   "A_Goal" = home_team_goal, "B_Goal" = away_team_goal)]
resultsO = resultsO[A_Goal != B_Goal]


#Home Wins
res_table = matrix(0, nrow = uniqueN(resultsO$Team_A), ncol = uniqueN(resultsO$Team_B))
rownames(res_table) = unique(resultsO$Team_A)
colnames(res_table) = unique(resultsO$Team_A)

results = resultsO[Team_A == 9987 | Team_A == 10000]

t = rbind(results[Team_A == 9987 & Team_B == 10000],
results[Team_A == 10000 & Team_B == 9987])
res_table = matrix(0, nrow = uniqueN(t$Team_A), ncol = uniqueN(t$Team_B))
rownames(res_table) = unique(t$Team_A)
colnames(res_table) = unique(t$Team_A)


for (i in 1:nrow(t)){
  idx_A = which(row.names(res_table) == t$Team_A[i])
  idx_B = which(row.names(res_table) == t$Team_B[i])
  if (t$A_Goal[i] > t$B_Goal[i]){
    res_table[idx_A,idx_B] = res_table[idx_A,idx_B] + 1
  }
  #if (t$A_Goal[i] < t$B_Goal[i]){
  #  res_table[idx_B, idx_A] = res_table[idx_B, idx_A] + 1
  #}
}


for (i in 1:nrow(results)){
  idx_A = which(row.names(res_table) == results$Team_A[i])
  idx_B = which(row.names(res_table) == results$Team_B[i])
  if (results$A_Goal[i] > results$B_Goal[i]){
    res_table[idx_A,idx_B] = res_table[idx_A,idx_B] + 1
  }
  if (results$A_Goal[i] < results$B_Goal[i]){
    res_table[idx_B, idx_A] = res_table[idx_B, idx_A] + 1
  }
}

#Away Wins
res_table2 = matrix(0, nrow = uniqueN(results$Team_A), ncol = uniqueN(results$Team_B))
rownames(res_table2) = unique(results$Team_A)
colnames(res_table2) = unique(results$Team_A)

for (i in 1:nrow(results)){
  idx_A = which(row.names(res_table2) == results$Team_A[i])
  idx_B = which(row.names(res_table2) == results$Team_B[i])
  if (results$A_Goal[i] > results$B_Goal[i]){
    res_table[idx_A,idx_B] = res_table[idx_A,idx_B] + 1
  }
  if (results$A_Goal[i] < results$B_Goal[i]){
    res_table2[idx_B, idx_A] = res_table2[idx_B, idx_A] + 1
  
  }
}

#Home
results.sf1 = matrix(0, nrow = nrow(res_table)*ncol(res_table), ncol = 4)
colnames(results.sf1) = c("home.team", "away.team", "home.wins", "away.wins")
k = 1
for (i in 1:nrow(res_table)){
  for (j in 1:ncol(res_table)){
    A = row.names(res_table)[i]
    B = colnames(res_table)[j]
    results.sf1[k,] = as.numeric(c(A,B,res_table[i,j],res_table[j,i]))
    k = k+1  
  }
}

#Away
results.sf2 = matrix(0, nrow = nrow(res_table2)*ncol(res_table2), ncol = 4)
colnames(results.sf2) = c("home.team", "away.team", "home.wins", "away.wins")
k = 1
for (i in 1:nrow(res_table2)){
  for (j in 1:ncol(res_table2)){
    A = row.names(res_table2)[i]
    B = colnames(res_table2)[j]
    results.sf2[k,] = as.numeric(c(B,A,res_table2[j,i],res_table2[i,j]))
    k = k+1  
  }
}

results.sf1 = data.frame(results.sf1)
setDT(results.sf1)

results.sf = rbind(results.sf1, results.sf2)
results.sf = data.frame(results.sf)
setDT(results.sf)

#results.sf = results.sf[-which(results.sf$home.wins == 0 & results.sf$away.wins == 0),]
results.sf$home.team = as.factor(results.sf$home.team)
results.sf$away.team = as.factor(results.sf$away.team)

results.sf = results.sf[home.wins != 0 & away.wins != 0]


btModel1 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.sf, id = "team")
coefs = sort(coef(btModel), decreasing = T)
#BTabilities(btModel)
head(coefs)
#home team effect
results.sf = data.frame(results.sf)

results.sf$home.team <- data.frame(team = results.sf$home.team, at.home = 1)
results.sf$away.team <- data.frame(team = results.sf$away.team, at.home = 0)
btModel2 = update(btModel1, formula = ~ team + at.home)

coefs2 = sort(coef(btModel2), decreasing = T)

anova(btModel1, btModel2)

#team level effects
results.eff = list()
results.eff$predictors = attrs

contests = data.frame(matrix(0, nrow = nrow(results.sf), ncol = 2))
colnames(contests) = c("winner", "loser")
setDT(results)
contests1 = data.frame("winner" = results[home_team_goal > away_team_goal]$home_team_api, 
                       "loser" = results[home_team_goal > away_team_goal]$away_team_api)
contests2 = data.frame("winner" = results[home_team_goal < away_team_goal]$away_team_api, 
                       "loser" = results[home_team_goal < away_team_goal]$home_team_api)
contests = rbind(contests1, contests2)
contests$winner = as.factor(contests$winner)
contests$loser = as.factor(contests$loser)

results.eff$contests = contests

lizModel = BTm(1, winner, loser, formula = ~ buildUpPlaySpeed[..] + buildUpPlaySpeedClass[..] + 
                  buildUpPlayDribblingClass[..] + buildUpPlayPassing[..] +
                  buildUpPlayPassingClass[..] + buildUpPlayPositioningClass[..] + chanceCreationPassing[..] +
                  chanceCreationPassingClass[..] + chanceCreationCrossing[..] + chanceCreationCrossingClass[..] +
                  chanceCreationShooting[..] + chanceCreationShootingClass[..] + chanceCreationPositioningClass[..] +
                  defencePressure[..] + defencePressureClass[..] + defenceAggression[..] + defenceAggressionClass[..] +
                  defenceTeamWidth[..] + defenceTeamWidthClass[..] + defenceDefenderLineClass[..] +  (1|..), data = results.eff)

coef(lizModel)
