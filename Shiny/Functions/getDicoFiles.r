library(data.table)
library(stringr)

#-------------------------------------------------------------------------------
getGamesId <-  function(selectedTeam = NULL, selectedPlayer, DT = nbaData) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team the player played for
  # - selectedPlayer : selectedPlayer
  # - DT : data tabke where to find Data (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesIdDT <- DT[(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer),
                  .(game_id, Home, onFloorHome, Away, onFloorAway)]
  
  # We identify the team the player played for 
  gamesIdDT <- gamesIdDT[, team := ifelse(grepl(selectedPlayer, onFloorHome), Home, Away)]
  
  # We only conserve Data related to selectedTeam, not old team or new team
  if (!is.null(selectedTeam)) {  
    gamesId <- unique(gamesIdDT[team == selectedTeam, game_id])
  } else {
    gamesId <- unique(gamesIdDT[, game_id])
  }
  
  return(gamesId)
  
}

#-------------------------------------------------------------------------------
transformData <- function(season, seasonType = "Regular Season", filePattern = "combined-stats", 
                       colDelete = c("date", "play_length", "play_id", "away", "home", "num", 
                                     "opponent", "outof", "possession", "reason", "original_x", "original_y", "description"),
                       path = "Shiny/CombinedGames") {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  # - season : Must be a YYYY-YYYY pattern
  # - filePattern : Specific file pattern (default) 
  # - colDelete : Vector with variable names (default)
  #-------------------------------------------------------------------------------
  season1 <- str_extract(string = season, pattern = "[0-9]{4}(?=-)")
  season2 <- str_extract(string = season, pattern = "(?<=-)[0-9]{4}")
  file <- list.files(path)[grepl(filePattern, list.files(path)) & grepl(season1, list.files(path)) & grepl(season2, list.files(path))]
  
  # Return a data.table with cleaned variables
  nbaDT <- readRDS(paste(path, "/", file, sep = ""))
  nbaDT <- nbaDT[, .SD, .SDcols = setdiff(colnames(nbaDT), colDelete)]
  
  # We filter on season type
  nbaDT <- nbaDT[data_set %like% seasonType]
  
    # We gather a1, a2, ...., h1, h2, .....
  nbaDT[, onFloorHome := paste(h1, h2, h3, h4, h5, sep = "|")]
  nbaDT[, onFloorAway := paste(a1, a2, a3, a4, a5, sep = "|")]
  nbaDT[, ":=" (a1=NULL, a2=NULL, a3=NULL, a4=NULL, a5=NULL, h1=NULL, h2=NULL, h3=NULL, h4=NULL, h5=NULL)]
  
  return(nbaDT)
  
}

#-------------------------------------------------------------------------------
getNBAcalendar <- function(season, DT, path = "Shiny/AllGames") {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  # - team : a vector with teams names (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # we list files in the directory 
  gamesVect <- fread(paste(path, "/", season, "/games.csv", sep = ""), header = FALSE)
  
  # operations to identify each games gathered in a DT
  Games <- unlist(str_extract_all(string = gamesVect, pattern = ".{7}(?=.csv)"))
  Home <- unlist(str_extract_all(string = Games, pattern = "[A-Z]{3}$"))
  Away <- unlist(str_extract_all(string = Games, pattern = "^[A-Z]{3}"))
  Dates <- unlist(str_extract_all(string = gamesVect, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  Id <- unlist(str_extract_all(string = gamesVect, pattern = "[0-9]{8}(?=-)"))
  
  CalendarDT <- data.table(game_id = as.integer(Id),
                           Date = as.Date(Dates, '%Y-%m-%d'),
                           Home = Home,
                           Away = Away)
  
  # We filter on the result of the 4th period or overtime & select Regular or Playoff games
  nbaDT <- DT[event_type == "end of period" & period >= 4, 
              .(game_id, data_set, period, home_score, away_score)]
  
  # We find ultimate period
  gameResultDT <- nbaDT[, .(period=max(period)), by = c("game_id", "data_set")]
  nbaDT <- merge(gameResultDT, nbaDT, 
                 by = c("game_id", "period", "data_set"), all.x = TRUE, all.y = FALSE)
  
  # We merge score and calendar
  CalendarDT <- merge(CalendarDT, nbaDT,
                      by = "game_id", all.x = FALSE, all.y = TRUE)
  
  # We add winner or loser
  CalendarDT[, Winner := ifelse(home_score > away_score, Home, Away)]
  CalendarDT[, Loser := ifelse(home_score > away_score, Away, Home)]
  
  return(CalendarDT)
  
}

getPlayerMinutesManual <- function(selectedTeam, selectedPlayer, nbaDT = nbaDataDT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : selecte a team
  # - selectedPlayer : select a player
  # - DT : data table where to look for the Data (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesId <- getGamesId(selectedTeam, selectedPlayer, nbaDT)
  
  # we filter the nbaData
  playerTimeDT <- nbaDT[((entered == selectedPlayer | left == selectedPlayer & event_type == "substitution") | 
                        (event_type %in% c("start of period", "timeout", "end of period"))) & 
                       game_id %in% gamesId, 
                     .(game_id, period, onFloorHome, onFloorAway, elapsed, entered, left)][order(game_id, period)]
  
  # creation of a new variable indicating if the player is a starter at each event or not
  playerTimeDT <- playerTimeDT[, isStarter := ifelse((onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer) , TRUE, FALSE)]
  
  # We convert time on a total seconds played basis
  playerTimeDT[, time := ifelse(period < 5, (period - 1) * 12 * 60, 4 * 12 * 60 + (period - 5) * 5 * 60) 
               + as.integer(str_sub(string = elapsed,start =  3, end =  4)) * 60 
               + as.integer(str_sub(string = elapsed, 6, 7))]
  
  # we simplify the Dataet
  playerTimeDT <- playerTimeDT[, .(game_id, period, time, isStarter, entered, left)]
  
  # We create a new variable where we shift values of time played to calculate a difference for the length of each play
  playerTimeDT[, nextTime := shift(time, -1)]
  
  # We clean values (end of game nextTime is the same as value time and cannot be zero or NA for last line), then we calculate the time length for each play
  playerTimeDT[nextTime == 0 | is.na(nextTime), nextTime := time]
  playerTimeDT[, playTime := nextTime - time]
  
  # We gather time played per period and calculate the mean (per min)
  playerTimeDT <- playerTimeDT[isStarter == TRUE]
  playerTimeDT <- playerTimeDT[, .(minTot = round(sum(playTime)/60, 2)), by = game_id]
  
  finalMinDT <- data.table(minPlayer = round(sum(playerTimeDT$minTot), 0), GamesPlayed = nrow(playerTimeDT))
  finalMinDT <- finalMinDT[, minMeanPlayer := round(minPlayer / GamesPlayed, 1)]

  return(finalMinDT)
  
}

#-------------------------------------------------------------------------------
getTeamPoss <- function(selectedTeam, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team to be selected
  # - DT : data table where to look for the Data
  #-------------------------------------------------------------------------------
  # We filter Data
  gamesId <- unique(DT[team %like% selectedTeam, game_id])
  DTteam <- DT[game_id %in% gamesId]
  
  # We calculate minutes played by the team
  minTeamDT <- DTteam[, .(minTeam = (max(period)-4) * 5 * 5 + 5 * 48), by = game_id]
  totMinTeam <- sum(minTeamDT[, minTeam])
  
  # calculate FG inputs for possession formula
  fgDT <- DTteam[points %in% c(0, 1, 2, 3)]
  
  totTeamFGA <- nrow(fgDT[team == selectedTeam & !(type %like% "free throw")])
  totOppFGA <- nrow(fgDT[team != selectedTeam & !(type %like% "free throw")])
  
  totTeamFGM <- nrow(fgDT[team == selectedTeam & points %in% c(2, 3)])
  totOppFGM <- nrow(fgDT[team != selectedTeam & points %in% c(2, 3)])
  
  totTeamFTA <- nrow(fgDT[team == selectedTeam & type %like% "free throw"])
  totOppFTA <- nrow(fgDT[team != selectedTeam & type %like% "free throw"])
  
  # We calculate rebounds inputs for possession formula
  rebDT <- DTteam[event_type == "rebound" & player != ""]
  
  reboundsOffTeam <- nrow(rebDT[type == "rebound offensive" & team == selectedTeam])
  reboundsOffOpp <- nrow(rebDT[type == "rebound offensive" & team != selectedTeam])
  
  reboundsDefTeam <- nrow(rebDT[type == "rebound defensive" & team == selectedTeam])
  reboundsDefOpp <- nrow(rebDT[type == "rebound defensive" & team != selectedTeam])
  
  # We calculate turnovers for possession formula
  toDT <- DTteam[event_type == "turnover"]
  
  totToTeam <- nrow(toDT[team == selectedTeam])
  totToOpp <- nrow(toDT[team != selectedTeam])
  
  # Possession formula
  teamPoss <- 0.5 * ((totTeamFGA + 0.4 * totTeamFTA - 1.07 * (reboundsOffTeam / (reboundsOffTeam + reboundsDefOpp)) * (totTeamFGA - totTeamFGM) + totToTeam) + 
                       (totOppFGA + 0.4 * totOppFTA - 1.07 * (reboundsOffOpp / (reboundsOffOpp + reboundsDefTeam)) * (totOppFGA - totOppFGM) + totToOpp))
  
  teamPossPerGame <- round(teamPoss / length(gamesId), 2)
  
  # We calculate pace
  teamPace <- round(teamPoss / (totMinTeam / 5) * 48, 2)
  
  return(teamPace)
  
}

####################################################################### INPUTS ################################################################
season = "2020-2021"
seasonType = "Regular Season"

####################################################################### 1ST : NBA Data FILE ################################################################
# DT where Data are taken
nbaDataDT <- transformData(season = season, seasonType = seasonType)

# We save the modified file
saveRDS(nbaDataDT, paste("Shiny/Dictionary/", season, "/", seasonType, "/nbaData.rds", sep = ""))

####################################################################### 2ND : NBA CALENDAR ################################################################
nbaCalendarDT <- getNBAcalendar(season, nbaDataDT)

# We save the modified file
saveRDS(nbaCalendarDT, paste("Shiny/Dictionary/", season, "/", seasonType, "/nbaCalendar.rds", sep = ""))

# We merge Dataets
nbaDataDT <- merge(nbaDataDT, nbaCalendarDT[, .(game_id, Date, Home, Away)], by = "game_id")

####################################################################### 3RD : MINUTES FILE ################################################################
# DT where players and teams vectors are taken
dicoPlayerTeam <- fread(paste("Shiny/Dictionary/", season, "/", seasonType, "/dicoPlayers.csv", sep = ""))

# lapply to all players
minutesList <- mapply(getPlayerMinutesManual, dicoPlayerTeam$team, dicoPlayerTeam$player, SIMPLIFY = FALSE)
TotMin <- sapply(minutesList, function(x) {x$minPlayer})
GamesPlayed <- sapply(minutesList, function(x) {x$GamesPlayed})
MeanMin <- sapply(minutesList, function(x) {x$minMeanPlayer})

#  Creation of a DT with main infos
finalMinDT <- data.table(player = dicoPlayerTeam$player, team = dicoPlayerTeam$team, 
                         TotMin = TotMin, TotalGames = GamesPlayed, AvgMin = MeanMin)

# We create a file
fwrite(x = finalMinDT, file = paste("Shiny/Dictionary/", season, "/", seasonType, "/minutesSummary.csv", sep = ""), append = FALSE)

####################################################################### 4TH : TEAM STATS FILE ################################################################
# script to calculate ranks per stats for teams
teamPart <- unique(nbaDataDT[team != "", team])
teamStats <- lapply(X = teamPart, FUN = function(x) {

    tmpDT <- nbaDataDT[(Home %like% x | Away %like% x)]
    nbGames <- length(unique(tmpDT[, game_id]))
    
    ptsTeam <- round(sum(tmpDT[team == x & points %in% c(1, 2, 3), points])/nbGames, 2)
    ptsOpp <- round(sum(tmpDT[team != x & points %in% c(1, 2, 3), points])/nbGames, 2)
    
    astTeam <- round(nrow(tmpDT[assist != "" & team == x])/nbGames, 2)
    astOpp <- round(nrow(tmpDT[assist != "" & team != x])/nbGames, 2)  
    
    rebTeam <- round(nrow(tmpDT[event_type == "rebound" & team == x & player != ""])/nbGames, 2)
    rebOpp <- round(nrow(tmpDT[event_type == "rebound" & team != x & player != ""])/nbGames, 2)
    
    blkTeam <- round(nrow(tmpDT[block != "" & team != x])/nbGames, 2)
    blkOpp <- round(nrow(tmpDT[block != "" & team == x])/nbGames, 2)
    
    stlTeam <- round(nrow(tmpDT[steal != "" & team != x])/nbGames, 2)
    stlOpp <- round(nrow(tmpDT[steal != "" & team == x])/nbGames, 2)
    
    toTeam <- round(nrow(tmpDT[event_type == "turnover" & team == x])/nbGames, 2)
    toOpp <- round(nrow(tmpDT[event_type == "turnover" & team != x])/nbGames, 2)
    
    madeFGTeam <- nrow(tmpDT[result == "made" & points %in% c(2, 3) & team == x])
    attFGTeam <- nrow(tmpDT[result != "" & !(type %like% "free throw") & team == x])
                        
    madeFGOpp <- nrow(tmpDT[result == "made" & points %in% c(2, 3) & team != x])
    attFGOpp <- nrow(tmpDT[result != "" & !(type %like% "free throw") & team != x])
    
    made3Team <- nrow(tmpDT[result == "made" & points == 3 & team == x])
    att3Team <- nrow(tmpDT[result != "" & type %like% "3pt" & team == x])
    
    made3Opp <- nrow(tmpDT[result == "made" & points == 3 & team != x])
    att3Opp <- nrow(tmpDT[result != "" & type %like% "3pt" & team != x])
    
    fgTeam <- round(100 * nrow(tmpDT[result == "made" & points %in% c(2, 3) & team == x]) / nrow(tmpDT[result != "" & !(type %like% "free throw") & team == x]), 2)
    fgOpp <- round(100 * nrow(tmpDT[result == "made" & points %in% c(2, 3) & team != x]) / nrow(tmpDT[result != "" & !(type %like% "free throw") & team != x]), 2)
    
    threeTeam <- round(100 * nrow(tmpDT[result == "made" & points == 3 & team == x]) / nrow(tmpDT[result != "" & type %like% "3pt" & team == x]), 2)
    threeOpp <- round(100 * nrow(tmpDT[result == "made" & points == 3 & team != x]) / nrow(tmpDT[result != "" & type %like% "3pt" & team != x]), 2)
    
    ftTeam <- round(100 * nrow(tmpDT[result == "made" & points == 1 & team == x]) / nrow(tmpDT[result != "" & type %like% "free throw" & team == x]), 2)
    
    teamPoss <- getTeamPoss(selectedTeam = x, DT = tmpDT)
    
    return(data.table(Possession = teamPoss,
                      Points = ptsTeam, "Opp. Points" = ptsOpp, 
                      Assists = astTeam, "Opp. Assists" = astOpp, 
                      Rebounds = rebTeam, "Opp. Rebounds" = rebOpp, 
                      Blocks = blkTeam, "Opp. Blocks" = blkOpp, 
                      Steals = stlTeam, "Opp. Steals" = stlOpp, 
                      Turnovers = toTeam, "Opp. Turnovers" = toOpp, 
                      "All FGM" = madeFGTeam, "All FGA" = attFGTeam, "All FG%" = fgTeam,  
                      "Opp. All FGM" = madeFGOpp, "Opp. All FGA" = attFGOpp, "Opp. All FG%" = fgOpp, 
                      "3FGM" = made3Team, "3FGA" = att3Team, "3FG%" = threeTeam,  
                      "Opp. 3FGM" = made3Opp, "Opp. 3FGA" = att3Opp, "Opp. 3FG%" = threeOpp,
                      "Free Throws" = ftTeam
                      
                      ))
  
})

# We name lists with team names
names(teamStats) <- teamPart
leagueDT <- rbindlist(l = teamStats, idcol = "Team")

# Loop to add stats rank
for (ncol in seq(2, length(leagueDT))) {
  nameColumn <- colnames(leagueDT[, ..ncol])
  if ((!grepl("Opp", nameColumn) & nameColumn != "Turnovers") | nameColumn == "Opp. Turnovers") {
    leagueDT <- leagueDT[order(-get(nameColumn))]
    leagueDT[, paste(nameColumn, " - Rank", sep = "") := .I]
  } else if ((grepl("Opp.", nameColumn) & nameColumn != "Opp. Turnovers") | nameColumn == "Turnovers") {
    leagueDT <- leagueDT[order(get(nameColumn))]
    leagueDT[, paste(nameColumn ," - Rank", sep = "") := .I]
  }
}

# We create the excel file
fwrite(x = leagueDT, file = paste("Shiny/Dictionary/", season, "/", seasonType, "/teamStatSummary.csv", sep = ""), append = FALSE)
