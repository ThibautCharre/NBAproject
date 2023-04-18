#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------

##############
# CREATE OUTPUTS : 4th page Shiny - Shooting charts
##############
# Object 1 & 2
# getPlayerGlobalShooting
# getPlayerGlobalShootingGraph

# Object 3, 3bis & 4
# getPlayerShotsDT
# getShotChart (voir court.R)

# Obejct 4
# getAssistsShotsPlayer

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
getPlayerGlobalShooting <- function(selectedTeam, selectedPlayer, startDate = NULL, endDate = NULL, DTcalendar, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #------------------------------------------------------------------------------- 
  # Returns object by default
  if (selectedPlayer == "Player") {
    
    shootingCalDT <- data.table(Date = "-", "W/L" = "-", Opp = "-", "2PM" = "-", "2PA" = "-", "3PM" = "-", "3PA" = "-", "FTM" = "-", "FTA" = "-")
    shootingFinalDT <- data.table(Type = c("2pts Shots", "3pts Shots", "Free Throws", "Effective Shots", "True Shooting"), 
                                  FieldGoal = c(0, 0, 0, 0, 0))
    
    return(list(calDT = shootingCalDT, graphDT = shootingFinalDT))
  }
  
  # We filter DT based on dates
  if (!is.null(startDate) & !is.null(endDate)) {
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We take into account games where player was on the floor
  gamesIds <- unique(DT[(Home == selectedTeam & onFloorHome %like% selectedPlayer) | (Away == selectedTeam & onFloorAway %like% selectedPlayer), game_id])
  
  # We stop if no games
  if (length(gamesIds) == 0) {
    
    shootingCalDT <- data.table(Date = "-", "W/L" = "-", Opp = "-", "2PM" = "-", "2PA" = "-", "3PM" = "-", "3PA" = "-", "FTM" = "-", "FTA" = "-")
    shootingFinalDT <- data.table(Type = c("2pts Shots", "3pts Shots", "Free Throws", "Effective Shots", "True Shooting"), 
                                  FieldGoal = c(0, 0, 0, 0, 0))
    
    return(list(calDT = shootingCalDT, graphDT = shootingFinalDT))
  }
  
  # We download datas and add directly the opponent and the place of the game
  shootingDT <- DT[player == selectedPlayer & team == selectedTeam & event_type %in% c("free throw", "shot"), .(game_id, Date, Home, Away, points, type, result)]
  shootingDT <- shootingDT[, oppTeam := ifelse(Home == selectedTeam, paste("@-", Away, sep = ""), paste("h-", Home, sep = ""))]
  
  # We isolate total points of the player
  totPoints <- sum(shootingDT[, "points"])
  
  # We create categories
  shootingDT <- shootingDT[, typeShot := ifelse(test = grepl('3pt', type), yes = '3pt Shots', 
                                                no = ifelse(grepl("free throw", type), "Free Throws", "2pt Shots"))]
  
  # We aggregate nb of shots by result and type of shot
  shootingDT <- shootingDT[, .(Total = .N), by = c("game_id", "Date", "oppTeam", "typeShot", "result")]
  
  # dcast to provide results by columns
  shootingDT <- dcast(data = shootingDT, formula = game_id + Date + oppTeam ~ typeShot + result, value.var = "Total")
  
  # We check if all shoot type and results are there, if not, we create and force value to zero
  missingTypeShots <- setdiff(c("game_id", "Date", "oppTeam", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed"), 
                              colnames(shootingDT))
  if (length(missingTypeShots) > 0) { 
    for (i in seq(1, length(missingTypeShots))) {
      shootingDT <- shootingDT[, missingTypeShots[i] := 0]
    }
  }
  
  # We find games for which a player played but did not shoot at all
  missing_gamesShoot <- setdiff(gamesIds, unique(shootingDT[, game_id]))
  if (length(missing_gamesShoot) > 0) {
    
    missing_gamesDT <- unique(DT[game_id %in% missing_gamesShoot, .(game_id, Date, Home, Away)])
    
    # Add of opp team varible and columns selection
    missing_gamesDT <- missing_gamesDT[, oppTeam := ifelse(Home == selectedTeam, paste("@-", Away, sep = ""), paste("h-", Home, sep = ""))]
    missing_gamesDT <- missing_gamesDT[, .(game_id, Date, oppTeam)]
    
    # We fixe values to zero for the added games
    missing_gamesDT <- missing_gamesDT[, ":=" ("2pt Shots_made" = 0, "2pt Shots_missed" = 0, "3pt Shots_made" = 0, "3pt Shots_missed" = 0, "Free Throws_made" = 0, "Free Throws_missed" = 0)]
    
    # We combine the added DT to the main shooting DT
    shootingDT <- rbind(shootingDT, missing_gamesDT)
    
  }
  
  # We replace NA by zero
  shootingDT[is.na(shootingDT)] <- 0
  
  # We merge to calendar to get win or loss factor
  shootingDT <- merge(shootingDT, DTcalendar[, .(game_id, Winner)], by = "game_id", all.x = TRUE, all.y = FALSE)
  shootingDT <- shootingDT[, resultGame := ifelse(Winner == selectedTeam, "W", "L")]
  
  # We reorganize columns order
  shootingDT <- shootingDT[, .SD, .SDcols = c("Date", "resultGame", "oppTeam", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed")]
  
  # shooting calendar dt
  shootingCalDT <- copy(shootingDT)
  colnames(shootingCalDT) <- c("Date", "W/L", "Opp", "2PM", "2PA", "3PM", "3PA", "FTM", "FTA")
  shootingCalDT <- shootingCalDT[, ":=" ("2PA" = get("2PA") + get("2PM"), "3PA" = get("3PA") + get("3PM"), "FTA" = get("FTA") + get("FTM"))]
  shootingCalDT <- shootingCalDT[order(-Date)]
  
  # We aggregate nb of shots by result and type of shot
  shootingGraphDT <- data.table("2PM" = sum(shootingCalDT[, "2PM"]), "2PA" = sum(shootingCalDT[, "2PA"]), "3PM" = sum(shootingCalDT[, "3PM"]), "3PA" = sum(shootingCalDT[, "3PA"]), 
                                "FTM" = sum(shootingCalDT[, "FTM"]), "FTA" = sum(shootingCalDT[, "FTA"]))
  
  # We create the variables FG as percent
  shootingGraphDT <- shootingGraphDT[, ":=" (twoFG = round(100 * get("2PM") / get("2PA"), 2),
                                   threeFG = round(100 * get("3PM") / get("3PA"), 2),
                                   FtFG = round(100 * get("FTM") / get("FTA"), 2))]
  
  
  # Creation of the variable efficient field goal
  shootingGraphDT <- shootingGraphDT[, eFG := round(100 * (get("2PM") + get("3PM") + 0.5 * get("3PM")) / (get("2PA") + get("3PA")), 2)]
  
  # Creation of the variable true shooting per cent
  shootingGraphDT <- shootingGraphDT[, tFG := round(100 * totPoints / (2 * (get("2PA") + get("3PA") + 0.44 * get("FTA"))), 2)]
  
  # we filter datas
  shootingFilteredDT <- shootingGraphDT[, .(twoFG, threeFG, FtFG, eFG, tFG)]
  
  shootingFinalDT <- data.table(Type = c("2pts Shots", "3pts Shots", "Free Throws", "Effective Shots", "True Shooting"), 
                                FieldGoal = c(shootingFilteredDT$twoFG, shootingFilteredDT$threeFG, shootingFilteredDT$FtFG, shootingFilteredDT$eFG, shootingFilteredDT$tFG))
  
  return(list(calDT = shootingCalDT, graphDT = shootingFinalDT))
  
}

#-------------------------------------------------------------------------------
getPlayerGlobalShootingGraph <- function(selectedPlayer, startDate = NULL, endDate = NULL, DTshots) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We dl datas
  shootingDT <- DTshots
  
  # We do a factorization
  shootingDT$Type <- factor(shootingDT$Type, levels = c("2pts Shots", "3pts Shots", "Free Throws", "Effective Shots", "True Shooting"))
  
  # We create the graph
  fig <- plot_ly(data = shootingDT, type = "bar", 
                 x = ~Type, y = ~FieldGoal,
                 texttemplate = ~paste("<b>", FieldGoal, "%"), textposition = "inside",
                 marker = list(color = brewer.pal(5, "Paired"), opacity = 0.6),
                 hoverinfo = "none",
                 showlegend = FALSE)
  
  fig <- layout(p = fig, 
                title = paste("<b>", selectedPlayer, " </b>- Shooting Percent"),
                xaxis = list(title = "", tickangle = -45), 
                yaxis = list(title = "", 
                             range = c(0, 100), 
                             zeroline = FALSE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = FALSE), 
                plot_bgcolor = "rgba(255, 255, 255, 0)",
                paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  fig <- config(p = fig, displayModeBar = FALSE)
  
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getPlayerShotDT <- function(selectedPlayer, selectedTeam, startDate = NULL, endDate = NULL, distLongShot = 27, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We download datas
  if (is.null(startDate)|is.null(endDate)) {
    shootingDT <- DT[points %in% c(0, 2, 3) & !grepl("free throw", type) & (!is.na(converted_x) & !is.na(converted_y)), 
                     .(player, team, result, type, assist, shot_distance, converted_x, converted_y)]
  } else {
    shootingDT <- DT[points %in% c(0, 2, 3) & !grepl("free throw", type) & (!is.na(converted_x) & !is.na(converted_y)) & Date >= startDate & Date <= endDate, 
                     .(player, team, result, type, assist, shot_distance, converted_x, converted_y)]
  }
  
  # Difference among 2pt shots and 3pt shots
  shootingDT <- shootingDT[, typeShot := ifelse(test = grepl('3pt', type), yes = '3pt Shots', no = "2pt Shots")]
  
  # We modify long shot to display them on a half court
  shootingDT <- shootingDT[, converted_x_corrected := ifelse(test = converted_y > 47 & shot_distance < 48.5, 
                                                             yes = 25+(25-converted_x), 
                                                             no = ifelse(test = converted_y < 47 & shot_distance > 48.5, 
                                                                         yes = 25+(25-converted_x), no = converted_x))]
  
  shootingDT <- shootingDT[, converted_y_corrected := ifelse(test = converted_y > 47 & shot_distance < 48.5, 
                                                             yes = 47+(47-converted_y), 
                                                             no = ifelse(test = converted_y < 47 & shot_distance > 48.5, 
                                                                         yes = 47+(47-converted_y), no = converted_y))]
  
  # First object: Player Shots coord
  playerShootingDT <- shootingDT[player == selectedPlayer, team == selectedTeam, .(result, converted_x_corrected, converted_y_corrected)]
  
  # We filter to get a proper DT to work with
  shootingDT <- shootingDT[, .(player, team, result, assist, typeShot, shot_distance, converted_x_corrected, converted_y_corrected)]
  
  # we add the argument area where we split shots by area with a mapply function
  shootingDT <- shootingDT[typeShot == "3pt Shots" & shot_distance > distLongShot, area := "Long Distance Shot"]
  
  shootingDT <- shootingDT[typeShot == "3pt Shots" & shot_distance <= distLongShot, area := mapply(FUN = function(x,y) {
    if (y <= 14 & x < 25) {
      return("3pt Left Corner")
    } else if (y <= 14 & x > 25) {
      return("3pt Right Corner")
    } else if (x <= 14.3) {
      return("3pt Top Left")
    } else if (x >= 35.7) {
      return("3pt Top Right")
    } else {
      return("3pt Middle")
    }
  }, converted_x_corrected, converted_y_corrected)]
  
  shootingDT <- shootingDT[typeShot == "2pt Shots", area := mapply(FUN = function(x,y) {
    if (y <= 14 & x < 17) {
      return("2pt Left Corner")
    } else if (y <= 14 & x > 33) {
      return("2pt Right Corner")
    } else if (x < 17) {
      return("2pt Top Left")
    } else if (x > 33) {
      return("2pt Top Right")
    } else if (y <= 9.25) {
      return("Under the Circle")
    } else if (y <= 19) {
      return("Short Paint Shot")
    } else {
      return("Long Paint Shot")
    }
  }, converted_x_corrected, converted_y_corrected)]
  
  # We save shooting coord including assists
  playerAssistDT <- shootingDT[player == selectedPlayer & team == selectedTeam & result == "made", .(area, assist)]
  
  # We calculate mean league per area
  nbShotsMadeLeagueDT <- shootingDT[result == "made", .(TotalLeagueMade = .N), by = area]
  nbShotsMissedLeagueDT <- shootingDT[result == "missed", .(TotalLeagueMissed = .N), by = area]
  
  # We gather the two preceding data table to calculate mean
  fgShotsLeagueDT <- merge(nbShotsMadeLeagueDT, nbShotsMissedLeagueDT, by = "area", all = TRUE)
  fgShotsLeagueDT <- fgShotsLeagueDT[, LeagueFG := round(100 * TotalLeagueMade / (TotalLeagueMade + TotalLeagueMissed), 2)]
  
  # We calculate mean league per selected player
  nbShotsMadePlayerDT <- shootingDT[player == selectedPlayer & team == selectedTeam & result == "made", .(TotalPlayerMade = .N), by = area]
  nbShotsMissedPlayerDT <- shootingDT[player == selectedPlayer & team == selectedTeam & result == "missed", .(TotalPlayerMissed = .N), by = area]
  
  # We gather the two preceding data table to calculate mean
  fgShotsPlayerDT <- merge(nbShotsMadePlayerDT, nbShotsMissedPlayerDT, by = "area", all = TRUE)
  
  # We merge to get final DT
  fgLeaguePlayerShotsDT <- merge(fgShotsLeagueDT, fgShotsPlayerDT, by = "area", all.x = TRUE, all.y = FALSE)
  
  # If no shot we force value to zero
  fgLeaguePlayerShotsDT[is.na(fgLeaguePlayerShotsDT)] <- 0
  fgLeaguePlayerShotsDT <- fgLeaguePlayerShotsDT[, PlayerFG := round(100 * TotalPlayerMade / (TotalPlayerMade + TotalPlayerMissed), 2)]
  
  # We calculate difference of FG
  fgLeaguePlayerShotsDT <- fgLeaguePlayerShotsDT[, diffFG := PlayerFG - LeagueFG]
  fgLeaguePlayerShotsDT <- fgLeaguePlayerShotsDT[order(-diffFG)]
  
  return(list(coordsDT = playerShootingDT, leagueDT = fgLeaguePlayerShotsDT, assistsDT = playerAssistDT))
  
}

#-------------------------------------------------------------------------------
getAssistsShotsPlayer <- function(playerShotsAssistsDT, selectedArea = "All") {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - playerShotsAssistsDT : DT Shots of a player including assists
  # - selectedArea : an area to select (default, all area)
  #-------------------------------------------------------------------------------
  # We create a ranking by player with number of assists
  if (selectedArea == "All") {
    assistsShotsDT <- playerShotsAssistsDT[, .(TotalAssists = .N), by = assist]
  } else {
    assistsShotsDT <- playerShotsAssistsDT[area == selectedArea, .(Total = .N), by = assist]
  }
  
  # We indicate if non  and change columns names
  assistsShotsDT[assist == "", "assist"] <- "Non Assisted"
  colnames(assistsShotsDT) <- c("Player", "Total")
  
  # We create the first DT corresponding to Assisted or not
  assistsShotsGlobalDT <- data.table(Type = c("Assisted", "Not Assisted"), 
                                     Total = c(sum(assistsShotsDT[Player != "Non Assisted", Total]), sum(assistsShotsDT[Player == "Non Assisted", Total])))
  assistsShotsGlobalDT <- assistsShotsGlobalDT[, Distribution := round(100 * Total / sum(assistsShotsGlobalDT$Total), 2)]
  
  # We create the second DT corresponding to detailed assists per area
  assistsShotsDetailedDT <- assistsShotsDT[Player != "Non Assisted"]
  assistsShotsDetailedDT <- assistsShotsDetailedDT[, Distribution := round(100 * Total / sum(assistsShotsDT[Player != "Non Assisted", Total]), 2)]
  
  # We rank by players
  assistsShotsDetailedDT <- assistsShotsDetailedDT[order(-Total)]
  
  return(list(Global = assistsShotsGlobalDT, Detailed = assistsShotsDetailedDT))
  
}