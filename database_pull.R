library(RSQLite)


con = dbConnect(SQLite(), dbname="~/Desktop/Duke/Fall_2016/Categorical_Data/STA841/database.sqlite")

dbListTables(con)

team = data.table(dbGetQuery(con,"SELECT * FROM Team"))
attrs = data.table(dbGetQuery(con,"SELECT * FROM Team_Attributes"))
attrs = attrs[,.(buildUpPlaySpeed, buildUpPlaySpeedClass, buildUpPlayDribbling, buildUpPlayDribblingClass, buildUpPlayPassing,
         buildUpPlayPassingClass, buildUpPlayPositioningClass, chanceCreationPassing, chanceCreationPassingClass,
         chanceCreationCrossing, chanceCreationCrossingClass, chanceCreationShooting, chanceCreationShootingClass,
         chanceCreationPositioningClass, defencePressure, defencePressureClass, defenceAggression, defenceAggressionClass,
         defenceAggressionClass, defenceTeamWidth, defenceTeamWidthClass, defenceDefenderLineClass),
      by = team_api_id, mult = "first"]
attrs = attrs[,.SD[1], by = team_api_id]
attrs = data.frame(attrs)
rownames(attrs) = attrs$team_api_id
attrs$team_api_id = NULL
setDT(attrs)
names(attrs)
par(mfrow = c(2,2))
hist(attrs$buildUpPlaySpeed, xlab = "Value", main = "Build Up Play Speed", breaks = 20, col = 1)
hist(attrs$buildUpPlayPassing, xlab = "Value", main = "Build Up Play Passing", breaks = 20, col = 1)
hist(attrs$chanceCreationPassing, xlab = "Value", main = "Chance Creation Passing", breaks = 20, col = 1)
hist(attrs$chanceCreationCrossing, xlab = "Value", main = "Chance Creation Crossing", breaks = 20, col = 1)
hist(attrs$chanceCreationShooting, xlab = "Value", main = "Chance Creation Shooting", breaks = 20, col = 1)
hist(attrs$defencePressure, xlab = "Value", main = "Defense Pressure", breaks = 20, col = 1)
hist(attrs$defenceAggression, xlab = "Value", main = "Defence Aggression", breaks = 20, col = 1)
hist(attrs$defenceTeamWidth, xlab = "Value", main = "Defence Team Width", breaks = 20, col = 1)

team
