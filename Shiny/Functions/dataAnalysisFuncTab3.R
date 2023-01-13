#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------

##############
# CREATE OUTPUTS : 3rd page Shiny - Impact of a player on its team
##############
# Main DT
# getGamesId
# getPlayerDT

# Object 1 & 2
# getHistPlayerStats
# getClassicPlayerStats
# getClassicPlayerStatsChart

# Object 3
# getAdvancedPlayerStats
# getAdvancedPlayerStatsPerDateGraph

# Object 4 & 5
# getPlayerImpact

# Object 6
# getPlayerPointsPerPeriod : Get total points of a player per quarter
# getPlayerMinutesPerPeriod : Get total min played per period for a specified player
# getPlayerPeriodStatsChart : Display bart chart with points per period and minutes per period for a specified player

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
getGamesId <-  function(selectedTeam = NULL, selectedPlayer, startDate = NULL, endDate = NULL, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team the player played for
  # - selectedPlayer : selectedPlayer
  # - DT : data tabke where to find datas (default)
  #-------------------------------------------------------------------------------
  # We filter DT with dates
  if (!is.null(startDate) & !is.null(endDate)) {
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We get game id where selected player played for the team
  gamesIdDT <- DT[(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer),
                  .(game_id, Home, onFloorHome, Away, onFloorAway)]
  
  # We identify the team the player played for 
  gamesIdDT <- gamesIdDT[, team := ifelse(onFloorHome %like% selectedPlayer, Home, Away)]
  
  # We only conserve datas related to selectedTeam, not old team or new team
  if (!is.null(selectedTeam)) {  
    gamesId <- unique(gamesIdDT[team == selectedTeam, game_id])
  } else {
    gamesId <- unique(gamesIdDT[, game_id])
  }
  
  return(gamesId)
  
}

#-------------------------------------------------------------------------------
getPlayerDT <-  function(selectedTeam = NULL, selectedPlayer, startDate = NULL, endDate = NULL, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team the player played for
  # - selectedPlayer : selectedPlayer
  # - DT : data tabke where to find datas (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesIdPlayer <- getGamesId(selectedTeam, selectedPlayer, startDate, endDate, DT)
  
  playerDT <- DT[game_id %in% gamesIdPlayer]
  
  return(playerDT)
  
}

#-------------------------------------------------------------------------------
getHistPlayerStats <- function(selectedTeam, selectedPlayer, startDate = NULL, endDate = NULL, DTcalendar, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - minTotGames : a number of minimum game to rank players (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  t <- Sys.time()
  
  # Return by default if no player selected
  if (selectedPlayer == "Player") {
    histStatsDT <- data.table("Date" = "-", "W/L" = "-", "Opp" = "-", "Pts" = "-", "Asts" = "-", "Rebs" = "-", "Stls" = "-", "Blks" = "-", "TOs" = "-")
    return(histStatsDT)
  }
  
  # We filter DT based on dates
  if (!is.null(startDate) & !is.null(endDate)) {
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We take into account games where player was on the floor
  gamesIds <- unique(DT[(Home == selectedTeam & onFloorHome %like% selectedPlayer) | (Away == selectedTeam & onFloorAway %like% selectedPlayer), game_id])
  
  # Return by default if no game is found
  if (length(gamesIds) == 0) {
    histStatsDT <- data.table("Date" = "-", "W/L" = "-", "Opp" = "-", "Pts" = "-", "Asts" = "-", "Rebs" = "-", "Stls" = "-", "Blks" = "-", "TOs" = "-")
    return(histStatsDT)
  }
  
  # We filter DT
  DT <- DT[game_id %in% gamesIds]
  
  ########
  # POINTS
  ########
  
  # We filter datas to get points
  totPointsDT <- DT[points %in% c(0, 1, 2, 3) & team == selectedTeam & player == selectedPlayer, .(TotalPoints = sum(points)), by = game_id]
  
  ##########
  # ASSISTS
  ##########
  
  # We filter datas to get assists
  totAssistsDT <- DT[assist == selectedPlayer & team == selectedTeam, .(TotalAssists = .N), by = game_id]
  
  ##########
  # REBOUNDS
  ##########
  
  # We filter datas to get rebounds
  totReboundsDT <- DT[event_type == "rebound" & player == selectedPlayer & team == selectedTeam, .(TotalRebounds = .N), by = game_id]
  
  ##########
  # TURNOVER
  ##########
  
  # We filter datas to get TO
  totTODT <- DT[event_type == "turnover" & player == selectedPlayer & team == selectedTeam, .(TotalTO = .N), by = game_id]
  
  # ##########
  # # STEALS
  # ##########
  
  # We arrange datas
  DTsteals <- DT[steal != ""]
  DTsteals <- DTsteals[, team := ifelse(team == Away, Home, Away)]
  
  # We filter datas to get TO
  totStealsDT <- DTsteals[team == selectedTeam & steal == selectedPlayer, .(TotalSteals = .N), by = game_id]
  
  ##########
  # BLOCKS
  ##########
  
  # We arrange datas
  DTblocks <- DT[block != ""]
  DTblocks <- DTblocks[, team := ifelse(team == Away, Home, Away)]
  
  # We filter datas to get TO
  totBlocksDT <- DTblocks[team == selectedTeam & block == selectedPlayer, .(TotalBlocks = .N), by = game_id]
  
  # We merge all datas
  histStatsDT <- Reduce(function(...) merge(..., by = "game_id", all = TRUE), list(totPointsDT, totAssistsDT, totReboundsDT, totStealsDT, totBlocksDT, totTODT))
  
  # We force value to 0 if NA
    histStatsDT[is.na(histStatsDT)] <- 0
  
  # We force all values to 0 if player has zero line stats for a game
  missing_gameId <- setdiff(gamesIds, histStatsDT[, game_id])
  if (length(missing_gameId) > 0) {
    complementDT <- data.table(game_id = missing_gameId, TotalPoints = 0, TotalAssists = 0, TotalRebounds = 0, 
                               TotalSteals = 0, TotalBlocks = 0, TotalTO = 0)
    histStatsDT <- rbind(histStatsDT, complementDT)
  }
  
  # We merge with nba calendar to get wins & losses
  histStatsDT <- merge(histStatsDT, DTcalendar[, .(game_id, Date, Home, Away, Winner, Loser)], by = "game_id", all.x = TRUE, all.y = FALSE)
  
  # We create two variables, home or away and defeat or wins
  histStatsDT <- histStatsDT[, Where := ifelse(Home == selectedTeam, "H", "A")]
  histStatsDT <- histStatsDT[, Result := ifelse(Winner == selectedTeam, "W", "L")]
  histStatsDT <- histStatsDT[, Opponent := ifelse(Winner == selectedTeam, Loser, Winner)]
  
  # We delete variables
  histStatsDT <- histStatsDT[, .(Date, Where, Result, Opponent, TotalPoints, TotalAssists, TotalRebounds, TotalSteals, TotalBlocks, TotalTO)]
  colnames(histStatsDT) <- c("Date", "Place", "W/L", "Vs", "Pts", "Asts", "Rebs", "Stls", "Blks", "TOs")
  histStatsDT <- histStatsDT[, Opp := paste(ifelse(Place == "H", "h", "@"), "-", Vs, sep = "")]
  histStatsDT <- histStatsDT[, .SD, , .SDcols = c("Date", "W/L", "Opp", "Pts", "Asts", "Rebs", "Stls", "Blks", "TOs")]

  return(histStatsDT[order(-Date)])
  
  print(Sys.time()-t)
}

#-------------------------------------------------------------------------------
getClassicPlayerStats <- function(selectedTeam, selectedPlayer, startDate = NULL, endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - minTotGames : a number of minimum game to rank players (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  
  t <- Sys.time()
  
  # We filter DT based on extreme dates values
  if (!is.null(startDate) & !is.null(endDate)) {  
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We find player tot games for the period
  playerTotGames <- nrow(unique(DT[(Home == selectedTeam & onFloorHome %like% selectedPlayer) | (Away == selectedTeam & onFloorAway %like% selectedPlayer), "game_id"]))
  
  ########
  # GAMES
  ########
  
  ########
  # POINTS
  ########
  
  # We filter datas to get points
  totPointsDT <- DT[points %in% c(0, 1, 2, 3) & team == selectedTeam, .(TotalPoints = sum(points)), by = player]
  
  ##########
  # ASSISTS
  ##########
  
  # We filter datas to get assists
  totAssistsDT <- DT[assist != "" & team == selectedTeam, .(TotalAssists = .N), by = assist]
  colnames(totAssistsDT) <- c("player", "TotalAssists")
  
  ##########
  # REBOUNDS
  ##########
  
  # We filter datas to get rebounds
  totReboundsDT <- DT[event_type == "rebound" & player != "" & team == selectedTeam, .(TotalRebounds = .N), by = player]
  
  ##########
  # TURNOVER
  ##########
  
  # We filter datas to get TO
  totTODT <- DT[event_type == "turnover" & player != "" & team == selectedTeam, .(TotalTO = .N), by = player]
  
  # ##########
  # # STEALS
  # ##########
  
  # We arrange datas
  DTsteals <- DT[steal != ""]
  DTsteals <- DTsteals[, team := ifelse(team == Away, Home, Away)]
  
  # We filter datas to get TO
  totStealsDT <- DTsteals[team == selectedTeam, .(TotalSteals = .N), by = steal]
  
  # renamin of columns
  colnames(totStealsDT) <- c("player", "TotalSteals")
  
  ##########
  # BLOCKS
  ##########
  
  # We arrange datas
  DTblocks <- DT[block != ""]
  DTblocks <- DTblocks[, team := ifelse(team == Away, Home, Away)]
  
  # We filter datas to get TO
  totBlocksDT <- DTblocks[team == selectedTeam, .(TotalBlocks = .N), by = block]
  
  # renamin of columns
  colnames(totBlocksDT) <- c("player", "TotalBlocks")
  
  # We merge all datas
  statsDT <- Reduce(function(...) merge(..., by = "player", all.x = TRUE, all.y = FALSE), list(totPointsDT, totAssistsDT, totReboundsDT, totStealsDT, totBlocksDT, totTODT))
  
  # We force value to 0 if NA
  statsDT[is.na(statsDT)] <- 0
  
  # We calculate variables per game
  for (i in c("Points", "Assists", "Rebounds", "TO", "Steals", "Blocks")) {
    statsDT <- statsDT[order(-get(colnames(statsDT)[grepl(paste("Total", i, sep = ""), colnames(statsDT))], statsDT))]
    statsDT <- statsDT[, paste("Rank", i, sep = "") := .I]
    # statsDT <- statsDT[, paste("Mean", i, sep = "") := round(get(colnames(statsDT)[grepl(paste("Total", i, sep = ""), colnames(statsDT))], statsDT) / TotalGames, 2)]
  }
  
  # We keep the number of players
  nbTeamPlayers <- nrow(statsDT)
  
  # We filter on selectedPlayer
  statsDT <- statsDT[player == selectedPlayer]
  
  # we calculate the mean for each stats types
  for (i in c("Points", "Assists", "Rebounds", "TO", "Steals", "Blocks")) {
    statsDT <- statsDT[, paste("Mean", i, sep = "") := round(get(colnames(statsDT)[grepl(paste("Total", i, sep = ""), colnames(statsDT))], statsDT) / playerTotGames, 2)]
  }
    
  return(list(playerDT = statsDT, nbPlayersTeam = nbTeamPlayers, nbTotGames = playerTotGames))
  
  print(Sys.time()-t)
  
}

#-------------------------------------------------------------------------------
getPlayerClassicStatsChart <- function(selectedTeam, selectedPlayer, startDate = NULL ,endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  t <- Sys.time()
  
  # We filter DT based on dates
  if (!is.null(startDate) & !is.null(endDate)) {
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We take into account games where player was on the floor
  if (selectedPlayer != "Player") {
	gamesIds <- unique(DT[(Home == selectedTeam & onFloorHome %like% selectedPlayer) | (Away == selectedTeam & onFloorAway %like% selectedPlayer), game_id])
	continue <- TRUE
  } else {
	continue <- FALSE
  }
  
  # Return by default if no game is found
  if (continue == FALSE) {
    continue <- FALSE
  } else if (continue == TRUE) {
	if (length(gamesIds > 0)) {
	  continue <- TRUE
	} else {
	  continue <- FALSE
	}
  }
  
  # We render the graph or an empty graph
  if (selectedTeam != "Team" & selectedPlayer != "Player" & continue == TRUE) {
    
    # we dl datas from previous function
    listStats <- getClassicPlayerStats(selectedTeam, selectedPlayer, startDate, endDate, DT)
    
    # On vérifie que le nombre de match min est respecté
    playerStatsDT <- listStats[["playerDT"]]
    
    # We download team players stats
    nbTeamPlayers <- listStats[["nbPlayersTeam"]]
    
    # Vector to be displayed
    chartDT <- data.table(Stats = c("Points", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers"),
                          PerGame = c(playerStatsDT[, MeanPoints], 
                                      playerStatsDT[, MeanRebounds], 
                                      playerStatsDT[, MeanAssists], 
                                      playerStatsDT[, MeanSteals], 
                                      playerStatsDT[, MeanBlocks], 
                                      playerStatsDT[, MeanTO]),
                          Total = c(playerStatsDT[, TotalPoints], 
                                    playerStatsDT[, TotalRebounds], 
                                    playerStatsDT[, TotalAssists], 
                                    playerStatsDT[, TotalSteals], 
                                    playerStatsDT[, TotalBlocks], 
                                    playerStatsDT[, TotalTO]),
                          Rank = c(playerStatsDT[, RankPoints], 
                                   playerStatsDT[, RankRebounds], 
                                   playerStatsDT[, RankAssists], 
                                   playerStatsDT[, RankSteals], 
                                   playerStatsDT[, RankBlocks], 
                                   playerStatsDT[, RankTO]),
                          Top = c(nbTeamPlayers + 1 - playerStatsDT[, RankPoints], 
                                  nbTeamPlayers + 1 - playerStatsDT[, RankRebounds], 
                                  nbTeamPlayers + 1 - playerStatsDT[, RankAssists], 
                                  nbTeamPlayers + 1 - playerStatsDT[, RankSteals],
                                  nbTeamPlayers + 1 - playerStatsDT[, RankBlocks], 
                                  nbTeamPlayers + 1 - playerStatsDT[, RankTO]))
    
    # We plot the spider chart
    fig <- plot_ly(data = chartDT, type = "scatterpolar", mode = "markers", 
                   r = ~Top, theta = ~Stats,
                   hovertemplate = ~paste(Stats, "Per Game :", PerGame,
                                          "<br>Total :", Total,
                                          "<br>Total Team Rank :", paste(Rank, "/", nbTeamPlayers, sep = ""), 
                                          "<extra></extra>"),
                   fill = 'toself')
    
    # title and axes format
    fig <- layout(p = fig, 
                  title = paste("Games Played :", listStats[["nbTotGames"]]),
                  polar = list(radialaxis = list(visible = FALSE, range = c(1, nbTeamPlayers))), 
                  showlegend = FALSE, 
                  plot_bgcolor = "rgba(255, 255, 255, 0)", 
                  paper_bgcolor = "rgba(255, 255, 255, 0)")
    
  } else {
    # We plot the spider chart
    fig <- plot_ly(data = data.table(Top = c(0, 0, 0, 0, 0, 0), Stats = c("Points", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers")),
                   type = "scatterpolar", mode = "markers", 
                   r = ~Top, theta = ~Stats,
                   hoverinfo = "none",
                   fill = 'toself')
    
    # title and axes format
    fig <- layout(p = fig, 
                  title = "",
                  polar = list(radialaxis = list(visible = FALSE)), 
                  showlegend = FALSE, 
                  plot_bgcolor = "rgba(255, 255, 255, 0)", 
                  paper_bgcolor = "rgba(255, 255, 255, 0)")
    
  }
  
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  # Final spider chart
  return(fig)
  
  print(Sys.time()-t)
  
}

#-------------------------------------------------------------------------------
getAdvancedPlayerStats <- function(selectedTeam, selectedPlayer, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - minTotGames : a number of minimum game to rank players (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  ########
  # GAMES
  ########
  
  # Player Minutes
  minDT <- getPlayerMinutesPerGame(selectedTeam, selectedPlayer, DTplayer)
  
  ########
  # POINTS
  ########
  
  # We filter datas to get points
  totPointsDT <- DTplayer[points %in% c(0, 1, 2, 3)]
  
  totPlayerPoints <- totPointsDT[player == selectedPlayer, .(sumPlayerPts = sum(points)), by = game_id]
  totTeamPoints <- totPointsDT[team == selectedTeam, .(sumTeamPts = sum(points)), by = game_id]
  totOppPoints <- totPointsDT[team != selectedTeam, .(sumOppPts = sum(points)), by = game_id]
  
  ########
  # FG
  ########
  
  # We also save FGM and FGA
  totPlayerFGA <- totPointsDT[!(type %like% "free throw") & points %in% c(0, 2, 3) & player == selectedPlayer, .(sumPlayerFGA = .N), by = game_id]
  totTeamFGA <- totPointsDT[team == selectedTeam & !(type %like% "free throw") & points %in% c(0, 2, 3), .(sumTeamFGA = .N), by = game_id]
  totOppFGA <- totPointsDT[team != selectedTeam & !(type %like% "free throw") & points %in% c(0, 2, 3), .(sumOppFGA = .N), by = game_id]
  
  totPlayerFGM <- totPointsDT[points %in% c(2, 3) & player == selectedPlayer, .(sumPlayerFGM = .N), by = game_id]
  totTeamFGM <- totPointsDT[team == selectedTeam & points %in% c(2, 3), .(sumTeamFGM = .N), by = game_id]
  totOppFGM <- totPointsDT[team != selectedTeam & points %in% c(2, 3), .(sumOppFGM = .N), by = game_id]
  
  totPlayer3PM <- totPointsDT[points == 3 & player == selectedPlayer, .(sumPlayer3PM = .N), by = game_id]
  totPlayer3PA <- totPointsDT[player == selectedPlayer & type %like% "3pt", .(sumPlayer3PA = .N), by = game_id]
  totTeam3PM <- totPointsDT[points == 3 & team == selectedTeam, .(sumTeam3PM = .N), by = game_id]
  
  totPlayerFTM <- totPointsDT[points == 1 & player == selectedPlayer, .(sumPlayerFTM = .N), by = game_id]
  totTeamFTM <- totPointsDT[team == selectedTeam & points == 1, .(sumTeamFTM = .N), by = game_id]
  totOppFTM <- totPointsDT[team != selectedTeam & points == 1, .(sumOppFTM = .N), by = game_id]
  
  totPlayerFTA <- totPointsDT[type %like% "free throw" & player == selectedPlayer, .(sumPlayerFTA = .N), by = game_id]
  totTeamFTA <- totPointsDT[team == selectedTeam & type %like% "free throw", .(sumTeamFTA = .N), by = game_id]
  totOppFTA <- totPointsDT[team != selectedTeam & type %like% "free throw", .(sumOppFTA = .N), by = game_id]
  
  ##########
  # ASSISTS
  ##########
  
  # We filter datas to get assists
  assistsDT <- DTplayer[assist != ""]
  
  # We gather total assists per player & team
  totPlayerAssist <- assistsDT[assist == selectedPlayer, .(sumPlayerAssists = .N), by = game_id]
  totTeamAssist <- assistsDT[team == selectedTeam, .(sumTeamAssists = .N), by = game_id]
  
  ##########
  # REBOUNDS
  ##########
  
  # We filter datas to get rebounds
  reboundsDefPlayer <- DTplayer[type == "rebound defensive" & player == selectedPlayer, .(sumPlayerDefReb = .N), by = game_id]
  reboundsOffPlayer <- DTplayer[type == "rebound offensive" & player == selectedPlayer, .(sumPlayerOffReb = .N), by = game_id]
  
  reboundsOffTeam <- DTplayer[type == "rebound offensive" & team == selectedTeam & player != "", .(sumTeamOffReb = .N), by = game_id]
  reboundsOffOpp <- DTplayer[type == "rebound offensive" & team != selectedTeam & player != "", .(sumOppOffReb = .N), by = game_id]
  
  reboundsDefTeam <- DTplayer[type == "rebound defensive" & team == selectedTeam & player != "", .(sumTeamDefReb = .N), by = game_id]
  reboundsDefOpp <- DTplayer[type == "rebound defensive" & team != selectedTeam & player != "", .(sumOppDefReb = .N), by = game_id]
  
  ##########
  # TURNOVER
  ##########
  
  # We calculate total team turnover
  totToTeam <- DTplayer[event_type == "turnover" & team == selectedTeam, .(sumTeamTO = .N), by = game_id]
  totToOpp <- DTplayer[event_type == "turnover" & team != selectedTeam, .(sumOppTO = .N), by = game_id]
  totToPlayer <- DTplayer[event_type == "turnover" & player == selectedPlayer, .(sumPlayerTO = .N), by = game_id]
  
  ##########
  # STEALS
  ##########
  
  # We calculate team, opp and player steals
  totStlTeam <- DTplayer[team != selectedTeam & steal != "", .(sumTeamStl = .N), by = game_id]
  totStlPlayer <- DTplayer[steal == selectedPlayer, .(sumPlayerStl = .N), by = game_id]
  
  ##########
  # BLOCKS
  ##########
  
  # We calculate team, opp and player steals
  totBlkTeam <- DTplayer[team != selectedTeam & block != "", .(sumTeamBlk = .N), by = game_id]
  totBlkPlayer <- DTplayer[block == selectedPlayer, .(sumPlayerBlk = .N), by = game_id]
  
  ##########
  # FOULS
  ##########
  
  totFoulsPlayer <- DTplayer[event_type == "foul" & player == selectedPlayer, .(sumPlayerFouls = .N), by = game_id]
  totFoulsTeam <- DTplayer[event_type == "foul" & team == selectedTeam, .(sumTeamFouls = .N), by = game_id]
  
  ########################################
  ############## Off Rating ##############
  ########################################
  
  # We merge all datas before calculating Off & Def ratings
  statsMerged <- Reduce(function(...) merge(..., by = "game_id", all = TRUE),
                           list(totPlayerPoints, totTeamPoints, minDT, totPlayerFGA, totTeamFGA, totOppFGA, totPlayerFGM, totTeamFGM, totOppFGM, totPlayer3PA, 
                                totPlayer3PM, totTeam3PM, totPlayerFTM, totTeamFTM, totPlayerFTA, totTeamFTA, totOppFTA, totPlayerAssist, totTeamAssist, 
                                reboundsOffPlayer, reboundsOffTeam, reboundsOffOpp, reboundsDefTeam, reboundsDefOpp, totToTeam, totToOpp, totToPlayer, 
                                totStlTeam, totStlPlayer, totBlkTeam, totBlkPlayer, totFoulsPlayer, totFoulsTeam, totOppPoints, reboundsDefPlayer, totOppFTM))
  # We force NA values to zero
  statsMerged[is.na(statsMerged)] <- 0
  
  #qAst
  statsMerged <- copy(statsMerged)[, qAst := ((minPlayer / (minTeam / 5)) * (1.14 * ((sumTeamAssists - sumPlayerAssists) / sumTeamFGM))) +
                        ((((sumTeamAssists / minTeam) * minPlayer * 5 - sumPlayerAssists) / ((sumTeamFGM / minTeam) * minPlayer * 5 - sumPlayerFGM)) *
                        (1 - (minPlayer / (minTeam / 5))))]
  
  #FGpart
  statsMerged[, FGpart := sumPlayerFGM * (1 - 0.5 * ((sumPlayerPts - sumPlayerFTM) / (2 * sumPlayerFGA)) * qAst)]
  statsMerged[is.na(FGpart), "FGpart"] <- 0
  
  # ASTpart            
  statsMerged[, ASTpart := 0.5 * (((sumTeamPts - sumTeamFTM) - (sumPlayerPts - sumPlayerFTM)) / (2 * (sumTeamFGA - sumPlayerFGA))) * sumPlayerAssists]          
   
  # FTpart
  statsMerged[, FTpart := (1-(1-(sumPlayerFTM/sumPlayerFTA))^2) * 0.4 * sumPlayerFTA]         
  statsMerged[is.na(FTpart), "FTpart"] <- 0
  
  # Team Scoring Possession
  statsMerged[, Team_Scoring_Poss := sumPlayerFGM + (1 - (1 - (sumTeamFTM / sumTeamFTA))^2) * sumTeamFTA * 0.4]

  # Team ORB per cent
  statsMerged[, Team_ORB_perCent := sumTeamOffReb / (sumTeamOffReb + sumOppDefReb)]       
  
  # Team play per cent
  statsMerged[, Team_Play_perCent := Team_Scoring_Poss / (sumTeamFGA + sumTeamFTA * 0.44 + sumTeamTO)]
  
  # Team ORB weight
  statsMerged[, Team_ORB_Weight := ((1 - Team_ORB_perCent) * Team_Play_perCent) / ((1 - Team_ORB_perCent) * Team_Play_perCent + Team_ORB_perCent * (1 - Team_Play_perCent))]
              
  # Player offensive rebound part
  statsMerged[, ORB_Part := sumPlayerOffReb * Team_ORB_Weight * Team_Play_perCent]
               
  # Scoring Possession
  statsMerged[, ScPoss := (FGpart + ASTpart + FTpart) * (1 - (sumTeamOffReb / Team_Scoring_Poss) * Team_ORB_Weight * Team_Play_perCent) + ORB_Part]
               
  # Missed FG possession
  statsMerged[, missedFGposs := (sumPlayerFGA - sumPlayerFGM) * (1 - 1.07 * Team_ORB_perCent)]
               
  # Missed FT possession
  statsMerged[, missedFTposs := (1 - (sumPlayerFTM / sumPlayerFTA))^2 * 0.4 * sumPlayerFTA]
  statsMerged[is.na(missedFTposs), "missedFTposs"] <- 0
  
  # Total Possession Player
  statsMerged[, totalPlayerPoss := ScPoss + missedFGposs + missedFTposs + sumPlayerTO]
               
  # Points Produced FG
  statsMerged[, producedPointsFG := 2 * (sumPlayerFGM + 0.5 * sumPlayer3PM) * (1 - 0.5 * ((sumPlayerPts - sumPlayerFTM) / (2 * sumPlayerFGA)) * qAst)]
  statsMerged[is.na(producedPointsFG), "producedPointsFG"] <- 0
       
  # Points Produced Assists
  statsMerged[, producedPointsAssists := 2 * ((sumTeamFGM - sumPlayerFGM + 0.5 * (sumTeam3PM - sumPlayer3PM)) / (sumTeamFGM - sumPlayerFGM)) *
                0.5 * (((sumTeamPts - sumTeamFTM) - (sumPlayerPts - sumPlayerFTM)) / (2 * (sumTeamFGA - sumPlayerFGA))) * sumPlayerAssists]
               
  # Points Produced Off Rebounds
  statsMerged[, producedPointsRebOff := sumPlayerOffReb * Team_ORB_Weight * Team_Play_perCent *
                (sumPlayerPts / (sumTeamFGM + (1 - (1 - (sumTeamFTM / sumTeamFTA))^2) * 0.4 * sumTeamFTA))]
  statsMerged[is.na(producedPointsRebOff), "producedPointsRebOff"] <- 0
           
  # Points Produced
  statsMerged[, producedPointsPlayer := (producedPointsFG + producedPointsAssists + sumPlayerFTM) *
                (1 - (sumTeamOffReb / Team_Scoring_Poss) * Team_ORB_Weight * Team_Play_perCent) + producedPointsRebOff]
               
  # Offensive Rating
  statsMerged[, OffRating := round(100 * (producedPointsPlayer / totalPlayerPoss), 2)]
  
  ########################################
  ############## Def Rating ##############
  ########################################
  
  # We add variables to def rating DT
  statsMerged <- copy(statsMerged)[, DOR_perCent := sumOppOffReb / (sumOppOffReb + sumTeamDefReb)]
  
  # Defensive Field Goal
  statsMerged[, DFG_perCent := sumOppFGM / sumOppFGA]
  
  # FM weight
  statsMerged[, FMwt := (DFG_perCent * (1 - DOR_perCent)) / (DFG_perCent * (1 - DOR_perCent) + (1 - DFG_perCent) * DOR_perCent)]
  
  # Stop1
  statsMerged[, Stop1 := sumPlayerStl + sumPlayerBlk * FMwt * (1 - 1.07 * DOR_perCent) + sumPlayerDefReb * (1 - FMwt)]
  
  # Stop2
  statsMerged[, Stop2 := (((sumOppFGA - sumOppFGM - sumTeamBlk) / minTeam) * FMwt * (1 - 1.07 * DOR_perCent) + ((sumOppTO - sumTeamStl) / minTeam)) * minPlayer +
                 (sumPlayerFouls / sumTeamFouls) * 0.4 * sumOppFTA * (1 - (sumOppFTM / sumOppFTA))^2]
  
  # Tot stops
  statsMerged[, totStops := Stop1 + Stop2]
  
  # Team possession
  statsMerged[, teamPoss := 0.5 * ((sumTeamFGA + 0.4 * sumTeamFTA - 1.07 * (sumTeamOffReb / (sumTeamOffReb + sumOppDefReb)) * (sumTeamFGA - sumTeamFGM) + sumTeamTO) + 
                                        (sumOppFGA + 0.4 * sumOppFTA - 1.07 * (sumOppOffReb / (sumOppOffReb + sumTeamDefReb)) * (sumOppFGA - sumOppFGM) + sumOppTO))]
  
  # stop percentage
  statsMerged[, Stop_perCent := totStops * minTeam / (teamPoss * minPlayer)]
  
  # team defensive rating
  statsMerged[, teamDefRat := 100 * (sumOppPts / teamPoss)]
  
  # Defense scoring poss
  statsMerged[, defScPoss := sumOppPts / (sumOppFGM + (1 - (1 - (sumOppFTM / sumOppFTA))^2) * sumOppFTA*0.4)]
  
  # Defense scoring poss
  statsMerged[, DefRating := round(teamDefRat + 0.2 * (100 * defScPoss * (1 - Stop_perCent) - teamDefRat), 2)]
  
  ##########
  # Usage Percent :
  ##########
  
  # statsMerged[, usagePlayer := 100 * ((sumPlayerFGA + 0.44 * sumPlayerFTA + sumPlayerTO) * (minTeam / 5)) / (minPlayer * (sumTeamFGA + 0.44 * sumTeamFTA + sumTeamTO))]
  
  statsMerged <- statsMerged[, .(game_id, OffRating, DefRating)]
  
  return(statsMerged)
  
}

#-------------------------------------------------------------------------------
getAdvancedPlayerStatsPerDateGraph <- function(selectedTeam, selectedPlayer, perMonthWeek = "Month", DTcalendar, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : a team to select
  # - selectedPlayer : a player to select from selected team
  # - perMonthWeek : Results by Month or Week (default)
  # - date1 : Minimum date (null by default, takes all current season)
  # - date2 : Maximum date (null by default, takes all current season)
  # - DT : a data table where datas are extracted (default)
  #-------------------------------------------------------------------------------
  # We DL advanced datas
  playerStatsDT <- getAdvancedPlayerStats(selectedTeam, selectedPlayer, DTplayer)
  
  # We isolate games id to get team results in nba calendar
  gamesId <- unique(playerStatsDT[, game_id])
  
  # We get team calendar for those games IDs
  DTcalendar <- DTcalendar[game_id %in% gamesId, .(game_id, Date, Home, Away, Winner)]
  
  # We merge datas
  playerStatsDT <- merge(playerStatsDT, DTcalendar[, .(game_id, Date)], by = "game_id", all = TRUE)
  
  # We add variable to group stats per ponth or week
  if (perMonthWeek == "Month") {
    playerStatsDT[, MonthOrWeek := lubridate::month(Date)]
    DTcalendar[, MonthOrWeek := lubridate::month(Date)]
  } else if (perMonthWeek == "Week") {
    playerStatsDT[, MonthOrWeek := lubridate::week(Date)]
    DTcalendar[, MonthOrWeek := lubridate::week(Date)]
  } else if (perMonthWeek == "Day") {
    playerStatsDT[, MonthOrWeek := Date]
    DTcalendar[, MonthOrWeek := Date]
  }
  
  # We include Month and week to dates
  if (perMonthWeek == "Month") {
    
    # Conversion DT for factoring period
    convertDT <- data.table(numMonth = seq(1, 12, 1), 
                            nameMonth = c("January", "Februray", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
    
    # We translate number to Month name
    playerStatsDT[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
      convertDT[numMonth == x, nameMonth]
    })]
    DTcalendar[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
      convertDT[numMonth == x, nameMonth]
    })]
    
    # We do a factorization to respect time order of the season
    playerStatsDT$MonthOrWeek <- factor(playerStatsDT$MonthOrWeek, 
                                  levels = c("September", "October", "November", "December", "January", "Februray", "March", "April", "May", "June", "July", "August"), 
                                  ordered = TRUE)
    DTcalendar$MonthOrWeek <- factor(DTcalendar$MonthOrWeek, 
                                        levels = c("September", "October", "November", "December", "January", "Februray", "March", "April", "May", "June", "July", "August"), 
                                        ordered = TRUE)
    
  } else if (perMonthWeek == "Week") {
    
    # Conversion DT for factoring period
    convertDT <- data.table(numWeek = seq(1, 52, 1), 
                            nameWeek = sapply(seq(1, 52, 1), function(x) {paste("W-", x, sep = "")}))
    
    # We translate number to Month name
    playerStatsDT[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
      convertDT[numWeek == x, nameWeek]
    })]
    DTcalendar[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
      convertDT[numWeek == x, nameWeek]
    })]
    
    # We do a factorization to respect time order of the season
    playerStatsDT$MonthOrWeek <- factor(playerStatsDT$MonthOrWeek, 
                                  levels = c(paste("W-", seq(35, 52, 1), sep = ""), paste("W-", seq(1, 34, 1), sep = "")))
    DTcalendar$MonthOrWeek <- factor(DTcalendar$MonthOrWeek, 
                                        levels = c(paste("W-", seq(35, 52, 1), sep = ""), paste("W-", seq(1, 34, 1), sep = "")))
    # end if  
  }
  
  # We calculate mean per month or week
  playerStatsDT <- playerStatsDT[, .(meanOffRating = round(mean(OffRating), 2), meanDefRating = round(mean(DefRating), 2)), by = MonthOrWeek]
  
  # And number of wins and loses
  DTcalendarWins <- DTcalendar[Winner == selectedTeam, .(TotWins = .N), by = MonthOrWeek]
  DTcalendarLoss <- DTcalendar[Winner != selectedTeam, .(TotLoss = .N), by = MonthOrWeek]
  
  # We combined Wins & losses and add 0 if needed
  DTcalendarCombined <- merge(DTcalendarWins, DTcalendarLoss, by = "MonthOrWeek", all = TRUE)
  DTcalendarCombined[is.na(DTcalendarCombined)] <- 0
  
  # 1st bar chart : wins
  fig <- plot_ly(data = DTcalendarCombined, type = "bar", 
                 x = ~MonthOrWeek, y = ~TotWins,
                 name = "Team Wins",
                 texttemplate = ~paste("<b>", get("TotWins", DTcalendarCombined), "W"),
                 marker = list(color = "rgba(255, 99, 71, 0.6)"),
                 hoverinfo = "none",
                 showlegend = TRUE)
  
  # 2nd bar chart : losses
  fig <- add_trace(p = fig, data = DTcalendarCombined, type = "bar", 
                   y = ~TotLoss,
                   name = "Team Losses",
                   texttemplate = ~paste("<b>", TotLoss, "L"),
                   marker = list(color = "rgba(51, 132, 255, 0.6)"),
                   hoverinfo = "none",
                   showlegend = TRUE)
  
  fig <- add_trace(p = fig, data = playerStatsDT, type = "scatter", mode = "markers",
                   x = ~MonthOrWeek, y = ~meanOffRating,
                   yaxis = "y2",
                   name = "Player Off Rating",
                   marker = list(color = "rgba(151, 44, 92, 0.6)", size = 10),
                   hovertemplate = ~paste("Off Rating :<b>", meanOffRating, "<extra></extra>"), 
                   showlegend = TRUE)
  
  fig <- add_trace(p = fig, data = playerStatsDT, type = "scatter", mode = "markers",
                   x = ~MonthOrWeek, y = ~meanDefRating,
                   yaxis = "y2",
                   name = "Player Def Rating",
                   marker = list(color = "rgba(0, 0, 0, 0.6)", size = 10),
                   hovertemplate = ~paste("Def Rating :<b>", meanDefRating, "<extra></extra>"), 
                   showlegend = TRUE)
    
  # Title and disposition
  fig <- layout(p = fig, data = playerStatsDT,
                title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "Wins / Losses Contribution"),
                xaxis = list(title=""),
                yaxis2 = list(overlaying = "y",
                              side = "left",
                              title = "",
                              #range = ~c(0, max(meanStat1)+1),
                              zeroline = TRUE,
                              showline = FALSE,
                              showticklabels = TRUE,
                              showgrid = FALSE),
                yaxis = list(title = "",
                             zeroline = TRUE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = FALSE),
                barmode = "group", 
                plot_bgcolor = "rgba(255, 255, 255, 0)", 
                paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  # No plot bar top right
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getPlayerPointsPerPeriod <- function(selectedTeam, selectedPlayer, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : Team where selected player is part of the roster
  # - selectedPlayer : select a player
  # - DT :  data table where to get datas (default)
  #-------------------------------------------------------------------------------
  
  # We filter datas on the specified player
  playerPointsDT <- DTplayer[!is.na(points) & team == selectedTeam & player == selectedPlayer, .(Total = sum(points)), by = period]
  
  DTplayer <- DTplayer[, periodGame := paste(game_id, period, sep="|")]
  
  # creation of a new variable indicating if the player is a starter at each event or not
  DTplayerOnFloor <- DTplayer[onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer]
  periodGameId <- unique(DTplayerOnFloor[, periodGame])
  
  # We calculate nb of quarters played by the player
  periodPlayerPlayedDT <- DTplayerOnFloor[, .(periodPlayed = uniqueN(game_id)), by = period]
  
  # We group datas per total game played and total team points for period played by player
  periodTeamPlayedDT <- DTplayer[periodGame %in% periodGameId & team == selectedTeam & !is.na(points), .(teamPoints = sum(points)), by = period]
  
  # We group datas to calculate opp points for each quarter played by the player
  periodOppPlayedDT <- DTplayer[periodGame %in% periodGameId & team != selectedTeam & !is.na(points), .(oppPoints = sum(points)), by = period]
  
  # We merge datas
  pointsPerPeriodDT <- Reduce(function(...) merge(..., by = "period", all = TRUE), list(playerPointsDT, periodPlayerPlayedDT, periodTeamPlayedDT, periodOppPlayedDT))
  
  # We calculate diff points 
  pointsPerPeriodDT <- pointsPerPeriodDT[, DiffPoints := teamPoints - oppPoints]
  
  # We add the period
  pointsPerPeriodDT <- pointsPerPeriodDT[, period := ifelse(period <= 4, paste("Q", period, sep = ""), paste("OT", period - 4, sep = ""))]
  
  # We create the mean points per period for a specific player
  pointsPerPeriodDT <- pointsPerPeriodDT[, ":=" (PointsPerPeriod = round(Total / periodPlayed, 1), 
                                                 TeamPointsPerPeriod = round(teamPoints / periodPlayed, 1), 
                                                 DiffPointsPerPeriod = round(DiffPoints / periodPlayed, 1))]
  
  return(pointsPerPeriodDT)
  
}

#-------------------------------------------------------------------------------
getPlayerMinutesPerGame <- function(selectedTeam, selectedPlayer, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : selecte a team
  # - selectedPlayer : select a player
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  # we filter the nbaDatas
  playerTimeDT <- DTplayer[((entered == selectedPlayer | left == selectedPlayer & event_type == "substitution") | 
                              (event_type %in% c("start of period", "timeout", "end of period"))), 
                           .(game_id, period, onFloorHome, onFloorAway, elapsed, entered, left)][order(game_id, period)]
  
  # creation of a new variable indicating if the player is a starter at each event or not
  playerTimeDT <- playerTimeDT[, isStarter := ifelse(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer, TRUE, FALSE)]
  
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
  
  # We add min for the whole team
  gameTimeDT <- playerTimeDT[, .(minTeam = (max(period)-4) * 5 * 5 + 5 * 48), by = game_id]
  
  # We gather time played per period and calculate the mean (per min)
  playerTimeDT <- playerTimeDT[isStarter == TRUE, .(minPlayer = round(sum(playTime) / 60, 2)), by = game_id]
  
  # we merge datas
  playerTimeDT <- merge(playerTimeDT, gameTimeDT, by = "game_id", all.x = TRUE, all.y = FALSE)
  
  return(playerTimeDT)
  
}

#-------------------------------------------------------------------------------
getPlayerMinutesPerPeriod <- function(selectedTeam, selectedPlayer, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : selecte a team
  # - selectedPlayer : select a player
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  # we filter the nbaDatas
  playerTimeDT <- DTplayer[((entered == selectedPlayer | left == selectedPlayer & event_type == "substitution") | 
                              (event_type %in% c("start of period", "timeout", "end of period"))), 
                           .(game_id, period, onFloorHome, onFloorAway, elapsed, entered, left)][order(game_id, period)]
  
  # creation of a new variable indicating if the player is a starter at each event or not
  playerTimeDT <- playerTimeDT[, isStarter := ifelse(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer, TRUE, FALSE)]
  
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
  playerTimeDT <- playerTimeDT[isStarter == TRUE, 
                               .(minPerPeriodTot = sum(playTime) / 60, totalPeriodPlayed = uniqueN(game_id)), 
                               by = .(period)]
  
  playerTimeDT[, minPerPeriodMean := minPerPeriodTot / totalPeriodPlayed]
  playerTimeDT[, minPerPeriodMean := round(floor(minPerPeriodMean) + (minPerPeriodMean - floor(minPerPeriodMean)) * 60 / 100, 1)]
  
  # We rename period variables
  playerTimeDT <- playerTimeDT[, period := ifelse(period <= 4, paste("Q", period, sep = ""), paste("OT", period - 4, sep = ""))]
  
  return(playerTimeDT)
  
}

#-------------------------------------------------------------------------------
getPlayerPeriodStatsChart <- function(selectedTeam, selectedPlayer, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : select a team
  # - selectedPlayer : select a player
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  # Points for the selected player, we transpose matrix and clean attributes
  playerPointsDT <- getPlayerPointsPerPeriod(selectedTeam, selectedPlayer, DTplayer)
  
  # Minutes played for the selected player
  playerMinutesDT <- getPlayerMinutesPerPeriod(selectedTeam, selectedPlayer, DTplayer)
  
  # We merge datas and factor period column for display purposes (OT is after quarters)
  playerPtsMinDT <- merge(playerPointsDT, playerMinutesDT,
                          by = "period", all = TRUE, sort = FALSE)
  
  playerPtsMinDT[, "period"] <- factor(x = playerPtsMinDT[, period], 
                                       levels = playerPtsMinDT[, period], 
                                       ordered = TRUE)
  
  # Creation of the chart
  fig <- plotly::plot_ly(data = playerPtsMinDT, 
                         x = ~period, 
                         y = ~PointsPerPeriod, type = "bar", 
                         name = "Player Points",
                         marker = list(color = "rgba(255, 99, 71, 0.6)"),
                         texttemplate = ~paste("<b>", PointsPerPeriod, "</b><br>ppq"),
                         hovertemplate = ~paste("Team ppq:<b>", TeamPointsPerPeriod, "<extra></extra>"),
                         showlegend = TRUE)
  
  fig <- plotly::add_bars(p = fig, data = playerPtsMinDT,
                          x = ~period,
                          y = ~minPerPeriodMean,
                          name = "Player Minutes",
                          marker = list(color = "rgba(51, 132, 255, 0.6)"),
                          texttemplate = ~paste("<b>", minPerPeriodMean, "</b><br>mpq"),
                          hovertemplate = ~paste("Number of", period, "played:<b>", totalPeriodPlayed, "</b><extra></extra>"),
                          showlegend = TRUE)
  
  fig <- plotly::add_markers(p = fig, data = playerPtsMinDT, type = "scatter",
                          x = ~period,
                          y = ~DiffPointsPerPeriod,
                          yaxis = "y2",
                          name = "Teams Diff Points",
                          marker = list(color = "rgba(0, 0, 0, 0.6)"),
                          hovertemplate = ~paste("Team +/- : ", "<b>", ifelse(DiffPointsPerPeriod > 0, "+", ""), DiffPointsPerPeriod, "</b><extra></extra>", sep = ""),
                          showlegend = TRUE)
  
  fig <- layout(p = fig,
                title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "Average Points and Minutes per period"),
                yaxis = list(zeroline = TRUE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = FALSE,
                             title = ""),
                yaxis2 = list(overlaying = "y", side = "right", 
                              zeroline = TRUE,
                              showline = FALSE,
                              showticklabels = FALSE,
                              showgrid = FALSE,
                              title = ""),
                xaxis = list(title = ""), 
                plot_bgcolor = "rgba(255, 255, 255, 0)", 
                paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  # No plot bar top right
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getPlayerImpact <- function(selectedTeam, selectedPlayer, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : a team to select
  # - selectedPlayer : a player from the selected team
  # - DT : a data table where datas are extracted (default)
  #-------------------------------------------------------------------------------
  ###############
  # 1/ FIELD GOAL
  ###############
  
  # Filtering and variables selection
  shootingDT <- DTplayer[event_type == "shot", 
                           .(game_id, Date, type, result, points, team, onFloorHome, onFloorAway)]
  
  # Creation of a list for on & off court data for specified players
  shootingOnOffList <- list(onFloor = shootingDT[onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer], 
                            offFloor = shootingDT[!(onFloorHome %like% selectedPlayer)&!(onFloorAway %like% selectedPlayer)])
  
  # lapply to calculate FG% in each part of the list (only 2pt Shots and 3pt Shots)
  shootingOnOffList <- lapply(shootingOnOffList, FUN = function(x) {
    
    # We create a new variable to specify shots types
    tmpDT <- x[, ":=" (onFloorHome=NULL, onFloorAway=NULL)]
    tmpDT[, typeSimplified := ifelse(grepl("3pt", type), "3pt Shots", 
                                     ifelse(grepl("free throw", type), "Free Throws", "2pt Shots"))]
    
    # We identify player team shots and opponents team shots
    tmpDT[, TeamOrOpp := ifelse(team == selectedTeam, "Team", "Opponent")]
    
    # Sum of missed and made shots and dcast to provide a better DT
    tmpDT <- tmpDT[, .(Total = .N), by = c("TeamOrOpp", "typeSimplified", "result")]
    tmpDT <- dcast(tmpDT, formula = TeamOrOpp + typeSimplified ~ result, value.var = "Total")
    
    # FG in %
    tmpDT[, FG := round(made / (made + missed) * 100, 2)]
    
  })
  
  # We gather previous datas to calculate FG% for all shots (combination of 2pts and 3pts shots)
  allShotsOnOffList <- lapply(X = shootingOnOffList, FUN = function(x) {
    
    # Calculation of all shots made and missed by team or opponent
    tmpDT <- x[, .(made = sum(made), missed = sum(missed)), by = TeamOrOpp]
    
    # Calculation of FG%
    tmpDT[, FG := round(made / (made + missed) * 100, 2)]
    
    # Identification of all shots
    tmpDT[, typeSimplified := "All Shots"]
    tmpDT <- tmpDT[, .(TeamOrOpp, typeSimplified, made, missed, FG)]
    
  })
  
  # We aggregate all previous list into two data tables
  shootingSeparatedDT <- rbindlist(shootingOnOffList, idcol = "Floor")
  shootingSeparatedDT <- dcast(shootingSeparatedDT, formula = typeSimplified ~ TeamOrOpp + Floor, value.var = "FG")
  shootingCombinedDT <- rbindlist(allShotsOnOffList, idcol = "Floor")
  shootingCombinedDT <- dcast(shootingCombinedDT, formula = typeSimplified ~ TeamOrOpp + Floor, value.var = "FG")
  
  # Final object to be displayed regarding shooting part
  shootingFinalDT <- rbind(shootingSeparatedDT, shootingCombinedDT)
  colnames(shootingFinalDT) <- c("Type", "Opponent FG w/o Player", "Opponent FG w Player", "Team FG w/o Player", "Team FG w Player")
  
  ###########
  # 2/ POINTS
  ###########
  
  # Filtering and variables selection
  pointsDT <- DTplayer[event_type %in% c("shot", "free throw") & result == "made" & points %in% c(1, 2, 3), 
                         .(game_id, team, points, onFloorHome, onFloorAway)]
  
  # Logical variable if player is on the floor
  pointsDT[, onFloor := ifelse(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer, 
                               TRUE, FALSE)]
  
  # Removing of two variables
  pointsDT[, ":=" (onFloorHome=NULL, onFloorAway=NULL)]
  
  # We create the variable where points scored by other team are negative and player team is positive
  pointsDT[, PlusTeam := ifelse(team == selectedTeam, points, -points)]
  
  # Creation of a list to calculate +/- when player is on and off the court
  plusMinusOnOffList <- list(onFloor = pointsDT[onFloor == TRUE],
                             offFloor = pointsDT[onFloor == FALSE])
  
  # We sum +/- and calculate the mean
  plusMinusOnOffList <- lapply(X = plusMinusOnOffList, FUN = function(x) {
    
    tmpDT <- x[, .(PlusMinus = sum(PlusTeam), nbGames = uniqueN(game_id))]
    tmpDT <- tmpDT[, MeanPlusMinus := round(PlusMinus / nbGames, 2)]
    
  })
  
  # We aggregate ^plus minus list to create one DT
  pointsFinalDT <- rbindlist(plusMinusOnOffList, idcol = "Floor")
  colnames(pointsFinalDT) <- c("On-Off Floor", "Total +/-", "Number of Games", "Average +/-")
  
  
  # Final object, a list of two DT
  return(list(ShootImpact = shootingFinalDT,
              PointImpact = pointsFinalDT))
  
}