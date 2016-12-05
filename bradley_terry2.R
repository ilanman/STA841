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

idx = sample(1:nrow(results.dat), 0.7*nrow(results.dat))
train.dat = results.dat[idx,]
test.dat = results.dat[-idx,]

#Home Advantage
train.dat = data.frame(train.dat)
train.dat$home.team <- data.frame(team = train.dat$home.team, at.home = 1)
train.dat$away.team <- data.frame(team = train.dat$away.team, at.home = 0)
btModel2.train = BTm(cbind(home.wins, away.wins), home.team, away.team, data = train.dat, id = "team", formula = ~ team + at.home)

test.dat = data.frame(test.dat)
test.dat$home.team <- data.frame(team = test.dat$home.team, at.home = 1)
test.dat$away.team <- data.frame(team = test.dat$away.team, at.home = 0)

predict(btModel2.train, newdata = test.dat, type = "response")

test.dat2 = data.frame(test.dat$home.team$team, test.dat$away.team$team)
#test.dat2 = data.frame(test.dat2)
colnames(test.dat2) = c("home.team", "away.team")
test.dat2$home.team = as.factor(test.dat2$home.team)
test.dat2$away.team = as.factor(test.dat2$away.team)


btModel2 = update(btModel1, formula = ~ team + at.home)

#Simple Model - Ignoring Home Advantage
btModel1 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.dat, id = "team", formula = ~ team)
btModel2 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.dat, id = "team", formula = ~ team + at.home)

#Incorporating "Home Advantage" Effect
results.dat = data.frame(results.dat)
results.dat$home.team <- data.frame(team = results.dat$home.team, at.home = 1)
results.dat$away.team <- data.frame(team = results.dat$away.team, at.home = 0)
btModel2 = update(btModel1, formula = ~ team + at.home)

#Chi Squared Test?
anova(btModel1, btModel2)

results.eff = list()
results.eff$predictors = attrs
#results.eff$contests = results.dat

contests1 = data.frame("winner" = results[home.goal > away.goal]$home.team, 
                       "loser" = results[home.goal > away.goal]$away.team)
contests2 = data.frame("winner" = results[home.goal < away.goal]$away.team, 
                       "loser" = results[home.goal < away.goal]$home.team)
contests = rbind(contests1, contests2)
contests$winner = as.factor(contests$winner)
contests$loser = as.factor(contests$loser)
results.eff$contests = contests


results.eff$home.team = results.dat$home.team
results.eff$away.team = results.dat$away.team
results.eff$home.wins = results.dat$home.wins
results.eff$away.wins = results.dat$away.wins



btModel3 = BTm(1, winner, loser, formula = ~ buildUpPlaySpeed[..] + buildUpPlayPassing[..] +
                + chanceCreationPassing[..] + chanceCreationCrossing[..] + chanceCreationShooting[..] +
                 defencePressure[..] + defenceAggression[..] + defenceTeamWidth[..], data = results.eff)

btModel3 = BTm(1, winner, loser, formula = ~  buildUpPlaySpeed[..], data = results.eff)
btModel3 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.eff, id = "team", formula = ~ team + at.home + buildUpPLaySpeed[team])



getProb = function(team1, team2, model, alpha = NULL){
  
  coefs = coef(model)
  coefs = coefs[c(paste0("team", team1), paste0("team", team2))]
  diff = coefs[1] - coefs[2]
  if (is.null(alpha)) {
    return(exp(diff)/(1 + exp(diff)))
} else {
  return(exp(diff + alpha)/(1 + exp(diff + alpha)))
  
}
  
  
}

getProb(9772, 9985, btModel1, 0.546)
getProb(9804, 8639, btModel1, 0.546)
getProb(9830, 7819, btModel1, 0.546)
getProb(6631, 9773, btModel1, 0.546)
getProb(9925, 9798, btModel1, 0.546)

par(mfrow = c(1,1))
hist(residuals(btModel, type = "pearson"), breaks = 20, col = 1, main = "Pearson's Residuals", xlab = "Values")
hist(residuals(btModel1, type = "deviance"), breaks = 20, col = 1, main = "Deviance's Residuals", xlab = "Values")

sum(residuals(btModel2, type = "deviance")^2)/btModel1$df.residual


attrs_red = attrs[,.(team_api_id, buildUpPlaySpeed, buildUpPlayDribbling)]
attrs_red$buildUpPlayDribbling[which(is.na(attrs_red$buildUpPlayDribbling))] = mean(attrs_red$buildUpPlayDribbling[which(!is.na(attrs_red$buildUpPlayDribbling))])
setDT(results.dat$home.team)
setDT(results.dat$away.team)
setkey(results.dat$home.team, team)
setkey(attrs_red, team_api_id)
results.dat$home.team = attrs_red[results.dat$home.team]
results.dat$away.team = attrs_red[results.dat$away.team]
names(results.dat$home.team)[1] = "team"
names(results.dat$away.team)[1] = "team"

btModel3 = BTm(cbind(home.wins, away.wins), home.team, away.team, data = results.dat, id = "team", formula = ~ team + at.home + buildUpPlaySpeed + buildUpPlayDribbling)

#predict

library(ROCR)
coefs = coef(btModel2.train)
at.home = coefs["at.home"]
p.win = rep(0, nrow(test.dat))
for (i in 1:nrow(test.dat)){
  home.team = as.character(test.dat$home.team$team[iin])
  away.team = as.character(test.dat$away.team$team[i])
  beta.home = coefs[paste0("team", home.team)]
  beta.away = coefs[paste0("team", away.team)]
  param = beta.home - beta.away + at.home
  p = exp(param)/(1 + exp(param))
  if (is.na(p)) {
    p.win[i] = NA
    next
  }
  if (p >= 0.5){
    p.win[i] = 1
  } else {
    p.win[i] = 0
  }
}

truth = rep(0, sum(test.dat$home.wins) + sum(test.dat$away.wins))
pred = rep(0, sum(test.dat$home.wins) + sum(test.dat$away.wins))
k = 1
for (i in 1:nrow(test.dat)){
  for (j in 1:test.dat$home.wins[i]){
    truth[k] = test.dat$home.wins[i]
    pred[k] = p.win[i]
    k = k+1
  }
  for (j in 1:test.dat$away.wins[i]){
    truth[k] = 0
    pred[k] = p.win[i]
    k = k+1
  }
  
}

for (i in 1:length(truth)){
  if (truth[i] > 0){
    truth[i] = 1
    
  }
  
}

plot(performance(prediction(p.win, truth.vec),'tpr',
                 x.measure = 'fpr'),main="ROC curve")
grid(col='gray')
abline(a=0,b=1,lty=8)
auc = performance(prediction(p.win, truth.vec),'auc')@y.values[[1]]
truth.vec = rep(0, length(p.win))
for (i in 1:nrow(test.dat)){
  if (test.dat$home.wins[i] > test.dat$away.wins[i]){
    truth.vec[i] = 1
  } else {
    truth.vec[i] = 0
  }
}
bad_idx = which(is.na(p.win))
p.win = p.win[-bad_idx]
truth.vec = truth.vec[-bad_idx]

t = confusionMatrix(p.win,truth.vec)$table
