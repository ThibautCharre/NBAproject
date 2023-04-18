#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------

##############
# CREATE OUTPUTS : 6th page Shiny - Bonus
##############
# Object
# getTeamCustomDT

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

getTeamCustomDT <- function(typeStat, teamOpp, DT) {
  
  # We extract columns filtered by stats
  colStat <- paste(typeStat, collapse = "|")
  DT <- DT[, .SD, .SDcols = c("Team", colnames(DT)[colnames(DT) %like% colStat])]
  
  # We extract team, opp or both columns
  if (teamOpp == "Opp") {
    colSelected <- colnames(DT)[colnames(DT) %like% "Opp"]
    DT <- DT[, .SD, .SDcols = c("Team", colSelected)]
  }  else if (teamOpp == "Team") {
    colSelected <- colnames(DT)[!(colnames(DT) %like% "Opp")]
    DT <- DT[, .SD, .SDcols = colSelected]
  } else {
    DT <- DT
  }
  
  return(DT)
  
}

#-------------------------------------------------------------------------------
getClutchRank <- function(minLeft = 4, secLeft = 0, diffScore = 5, minGame = 3, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - timeLeft : at which point time we consider a clutch moment
  # - diffScore : difference between scores to consider game is close
  # - DT
  #-------------------------------------------------------------------------------
  # We filter general datas
  DT <- DT[period >= 4]
  
  # We add the minute variable and seconds variable
  DT <- DT[, minute := as.integer(str_sub(remaining_time, 3, 4)) + round(as.integer(str_sub(remaining_time, 6, 7)) / 60, 2)]
  
  # We convert the selected time left
  timeLeft <- minLeft + round(secLeft / 60, 2)
  
  # Games where a clutch momentum is played
  nearTimeDT <- DT[period == 4 & minute >= timeLeft, .(targetTime = min(minute)), by = game_id]
  
  # We add target time
  DT <- merge(DT, nearTimeDT, by = "game_id", all.x = TRUE, all.y = FALSE)
  
  # We find game clutch and eliminate all datas not in the time window
  gameClutch <- unique(DT[period == 4 & minute == targetTime & abs(away_score - home_score) <= diffScore, game_id])
  
  # We filter datas based on clutch games vector
  gameClutchDT <- DT[game_id %in% gameClutch & ((period == 4 & minute <= targetTime) | period > 4)]
  
  ###########
  # 1/ POINTS
  ###########
  
  # We calculate PPG and number of closed games per player
  pointsPlayerDT <- gameClutchDT[points %in% c(1, 2, 3), .(GamesPlayed = uniqueN(game_id), TotalPoints = sum(points)), by = player]
  
  # We add PPG by dividing total points by umber of clutch games
  pointsPlayerDT <- pointsPlayerDT[, PPG := round(TotalPoints / GamesPlayed, 2)]
  
  ############
  # 2/ FIELD GOAL
  ############
  
  # We focus on shots
  shootingPlayerDT <- gameClutchDT[(event_type == "shot"|event_type == "free throw")]
  
  # Same thing for Field Goal
  shootingPlayerDT <- shootingPlayerDT[, type := ifelse(grepl("3pt", type), "3pt Shots", 
                                                        ifelse(grepl("free throw", type), 
                                                               "Free Throws", "2pt Shots"))]
  
  # We aggregate shots by type and result for the selected player
  shootingPlayerDT <- shootingPlayerDT[, .(Total = .N), by = c("player", "type", "result")]
  
  # dcast to provide a better data table
  shootingPlayerDT <- dcast(shootingPlayerDT, player ~ type + result, value.var = "Total", fill = 0)
  
  # If NaN or NA then we force value to 0
  shootingPlayerDT[is.na(shootingPlayerDT)] <- 0
  
  # FG% and we keep total attempted
  shootingPlayerDT[, ":=" 
                   (FG2pts = round(100 * (get("2pt Shots_made") / (get("2pt Shots_made") + get("2pt Shots_missed"))), 2),
                     FG3pts = round(100 * (get("3pt Shots_made") / (get("3pt Shots_made") + get("3pt Shots_missed"))), 2),
                     FGFTpts = round(100 * (get("Free Throws_made") / (get("Free Throws_made") + get("Free Throws_missed"))), 2),
                     ATT2pts = (get("2pt Shots_made") + get("2pt Shots_missed")),
                     ATT3pts = (get("3pt Shots_made") + get("3pt Shots_missed")),
                     ATTFTpts = (get("Free Throws_made") + get("Free Throws_missed")))]
  
  # Only important variables selected and we clean NA values
  shootingPlayerDT <- shootingPlayerDT[, .(player, ATT2pts, FG2pts, ATT3pts, FG3pts, ATTFTpts, FGFTpts)]
  shootingPlayerDT[is.na(shootingPlayerDT)] <- 0
  
  #####################
  # POINTS + FIELD GOAL
  #####################
  # We merge both previous datas
  playerScoringDT <- merge(pointsPlayerDT, shootingPlayerDT, 
                           by = c("player"), all.x = TRUE, all.y = FALSE)
  
  # We change columns names
  colnames(playerScoringDT) <- c("Player", "GP", "PTS", "PPG", "2PA", "2P%", "3PA", "3P%", "FTA", "FT%")
  
  #####################
  # FILTERING OF MIN GAMES
  #####################
  playerScoringDT <- playerScoringDT[GP >= minGame]
  
  #####################
  # FINAL OBJECT (depending on player selection or not)
  #####################
  #playerScoringWL <- merge(playerScoringDT, playerwinLossDT, by = "player", all = TRUE)
  
  return(playerScoringDT[order(-PPG)])
  
}