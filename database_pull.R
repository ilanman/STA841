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
attrs$id = NULL  
attrs$team_fifa_api_id = NULL
attrs$date = NULL
attrs = data.frame(attrs)
rownames(attrs) = attrs$team_api_id
attrs$team_api_id = NULL
