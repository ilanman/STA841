library(reshape2)
library(plyr)
library(ggplot2)
library(MASS)
library(data.table)

#setwd("/Users/ilanman/Downloads")
setwd("~/Desktop/Duke/Fall_2016/Categorical_Data/STA841")
match = fread("match_db.csv")
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

names(count_total)<-c("Team","Total_games")

all_counts <- join(count_total, count_winners, by='Team')
all_counts$Proportion = all_counts$Winner/all_counts$Total

# top 10 winning percentage
all_counts[order(all_counts[,4], decreasing = T),][1:10,]

# number of goals scored, home and away
goals_scored = data.frame(cbind(aggregate(results$home_team_goal, by=list(results$home_team_api), FUN=sum),
                                aggregate(results$away_team_goal, by=list(results$away_team_api), FUN=sum)))

goals_scored$Group.1.1 <- NULL
names(goals_scored) <- c('Team','Home_goals','Away_goals')

goals_scored$Total_goals = goals_scored$Home_goals + goals_scored$Away_goals

goals_scored[order(goals_scored$Total_goals, decreasing = T),][1:10,]

goals_wins_df = join(goals_scored, all_counts, by = 'Team')

temp_df = goals_wins_df[order(goals_wins_df$Proportion, decreasing = T),][1:15,]
temp_df$Proportion_home_goals = temp_df$Home_goals/temp_df$Total_goals
temp_df$Proportion_away_goals = temp_df$Away_goals/temp_df$Total_goals
temp_df

prop_df = melt(temp_df, id.vars='Team')[91:120,]
ggplot(prop_df, aes(x = factor(Team), y = value, fill = variable)) + 
  geom_bar(stat = "identity", width=0.75) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  ggtitle("Proportion of goals that are home vs away") + ylab("Proportion of goals") + xlab("Team")

prop_df = melt(temp_df, id.vars='Team')[1:30,]
ggplot(prop_df, aes(x = factor(Team), y = value, fill = variable)) + 
  geom_bar(position = 'dodge', stat = "identity", width=0.75) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  ggtitle("Home vs Away goals by count") + ylab("Number of goals") + xlab("Team")


setDT(all_counts)
all_counts[order(-Proportion)]
