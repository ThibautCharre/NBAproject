library(data.table)
library(stringr)

#-------------------------------------------------------------------------------
getGamesId <-  function(selectedTeam = NULL, selectedPlayer, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team the player played for
  # - selectedPlayer : selectedPlayer
  # - DT : data tabke where to find datas (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesIdDT <- DT[(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer),
                  .(game_id, Home, onFloorHome, Away, onFloorAway)]
  
  # We identify the team the player played for 
  gamesIdDT <- gamesIdDT[, team := ifelse(grepl(selectedPlayer, onFloorHome), Home, Away)]
  
  # We only conserve datas related to selectedTeam, not old team or new team
  if (!is.null(selectedTeam)) {  
    gamesId <- unique(gamesIdDT[team == selectedTeam, game_id])
  } else {
    gamesId <- unique(gamesIdDT[, game_id])
  }
  
  return(gamesId)
  
}

#-------------------------------------------------------------------------------
getDatasForMinutesManual <- function(season, seasonType = "Regular Season", filePattern = "combined-stats", 
                       colDelete = c("date", "remaining_time", "play_length", "play_id", "away", "home", "num", 
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
  
  return(nbaDT)
  
}

#-------------------------------------------------------------------------------
getNBAcalendar <- function(season, DT = nbaDatas, path = "Shiny/AllGames") {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  # - team : a vector with teams names (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # we list files in the directory 
  filesName <- list.files(paste(path, "/", season, sep = ""))
  
  # operations to identify each games gathered in a DT
  Games <- unlist(str_extract_all(string = filesName, pattern = ".{7}(?=.csv)"))
  Home <- unlist(str_extract_all(string = Games, pattern = "[A-Z]{3}$"))
  Away <- unlist(str_extract_all(string = Games, pattern = "^[A-Z]{3}"))
  Dates <- unlist(str_extract_all(string = filesName, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  Id <- unlist(str_extract_all(string = filesName, pattern = "[0-9]{8}(?=-)"))
  
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

getPlayerMinutesManual <- function(selectedTeam, selectedPlayer, nbaDT = nbaDatasDT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : selecte a team
  # - selectedPlayer : select a player
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesId <- getGamesId(selectedTeam, selectedPlayer, nbaDT)
  
  # we filter the nbaDatas
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
  
  # we simplify the dataset
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

# INPUTS
season = "2021-2022"
seasonType = "Playoffs"

# DT where datas are taken 
nbaDatasDT <- getDatasForMinutesManual(season = season, seasonType = seasonType)
nbaCalendarDT <- getNBAcalendar(season, nbaDatasDT)

# We merge datasets
nbaDatasDT <- merge(nbaDatasDT, nbaCalendarDT[, .(game_id, Date, Home, Away)], by = "game_id") 

# DT where players and teams vectors are taken
dicoPlayerTeam <- fread(paste("Shiny/Dictionary/", season, "/", seasonType, "/dicoPlayers.csv", sep = ""))

# lapply to all players
ltest <- mapply(getPlayerMinutesManual, dicoPlayerTeam$team, dicoPlayerTeam$player, SIMPLIFY = FALSE)
TotMin <- sapply(ltest, function(x) {x$minPlayer})
GamesPlayed <- sapply(ltest, function(x) {x$GamesPlayed})
MeanMin <- sapply(ltest, function(x) {x$minMeanPlayer})

#  Creation of a DT with main infos
finalMinDT <- data.table(player = dicoPlayerTeam$player, team = dicoPlayerTeam$team, 
                         TotMin = TotMin, TotalGames = GamesPlayed, AvgMin = MeanMin)

# We create a file
fwrite(x = finalMinDT, file = paste("Shiny/Dictionary/", season, "/", seasonType, "/minutesSummary.csv", sep = ""), append = FALSE)
