library(plyr)
library(ggplot2)
library(MASS)

setwd("/Users/ilanman/Downloads")
match = read.csv("match_db.csv")
nrow(match)
ncol(match)
head(match)
new_match = match[,c(1:11,86:118)]
head(new_match,100)

# number of teams
length(unique(match$home_team_api_id))
length(unique(match$away_team_api_id))

# number of leagues
length(unique(match$name))

# just matches, home/away and goals scored
results = data.frame(cbind(match_id = match$match_api_id, 
                           home_team_api = match$home_team_api_id, 
                           away_team_api = match$away_team_api_id,
                           home_team_goal = match$home_team_goal, 
                           away_team_goal = match$away_team_goal))

results$winner = ifelse(results$home_team_goal>results$away_team_goal, 
                        results$home_team_api, 
                        ifelse(results$home_team_goal<results$away_team_goal, results$away_team_api, "tie"))

head(results)

count_winners = data.frame(table(results$winner))
count_winners[,1] = as.numeric(as.character(count_winners[,1]))

names(count_winners) <- c("Team","Winner")
# find top 10 winners
count_winners[order(count_winners[,2], decreasing = T),][1:11,]

# find top 10 losers
count_winners[order(count_winners[,2], decreasing = F),][1:10,]

count_total_temp = data.frame(num_home = table(results$home_team_api), 
                              num_away = table(results$away_team_api))

count_total <- data.frame(cbind(n = as.numeric(as.character(count_total_temp$num_home.Var1)), 
                                count_total_temp$num_home.Freq + count_total_temp$num_away.Freq))

count_winners[,1] = as.numeric(as.character(count_winners[,1]))

names(count_total)<-c("Team","Total")

all_counts <- join(count_total, count_winners, by='Team')
all_counts$Proportion = all_counts$Winner/all_counts$Total

# top 10 winning percentage
all_counts[order(all_counts[,4], decreasing = T),][1:10,]

# number of goals scored, home and away
goals_scored = data.frame(cbind(aggregate(results$home_team_goal, by=list(results$home_team_api), FUN=sum),
                                aggregate(results$away_team_goal, by=list(results$away_team_api), FUN=sum)))

goals_scored$Group.1.1 <- NULL
names(goals_scored) <- c('Team','Home_goals','Away_goals')



