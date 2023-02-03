#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------

##############
# CREATE OUTPUTS : 2nd page Shiny - Team stats
##############
# Object 1
# getRankingStreakMap (check R script in USmap directory)
# getNBAmap (check R script in USmap directory)

# Object 2
# getRankingStreakTeam

# Object 3
# getTeamStats
# getTeamStatsGraph

# Object 4
# getTeamStatsPerPlayer
# getTeamStatsPerPlayerChart

# Object 5
# getBestGames

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
getRankingStreakMap <- function(listResultTeams) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - listResultTeams : A list of DT with results per team
  #-------------------------------------------------------------------------------
  # We filter ranking streak datas datas previously DL in Shiny
  listResultTeams <- lapply(X = listResultTeams, FUN = function(x) {
    x <- x[Date == max(Date), .(Date, Bilan, Ratio, Streak)]
  })
  
  # if all is selected we aggregate in a simple DT the latest results of each team
  resultsTeamsDT <- rbindlist(listResultTeams, idcol = "Team")[order(-Ratio)]
  return(resultsTeamsDT)
  
}

#-------------------------------------------------------------------------------
getRankingStreakTeam <- function(selectedTeam, listResultTeams) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : Name of a team used to filter listResultTeams
  # - listResultTeams : A list of DT with results per team
  #-------------------------------------------------------------------------------
  
  # We filter ranking streak datas datas previously DL in Shiny
  resultsTeamsDT = listResultTeams[[selectedTeam]]
  
  # We add variables
  resultsTeamsDT <- resultsTeamsDT[, Score := paste(home_score, "-", away_score)]
  resultsTeamsDT <- resultsTeamsDT[, .(Date, Home, Away, Score, Bilan, Ratio, Streak)][order(-Date)]
  
  return(resultsTeamsDT)
  
}

#-------------------------------------------------------------------------------
getTeamStats <- function(selectedTeam, typeStat = "Points", DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : A team to be selected
  # - typeStat : statistics to analyze (default)
  #-------------------------------------------------------------------------------
  # We calculate league mean
  meanLeague <- round(sum(DT[, .SD, .SDcols = typeStat]) / nrow(DT), 2)
  
  # we select team
  teamDT <- DT[Team == selectedTeam]
  
  # We extract team, opp stats
  teamStat <- teamDT[, .SD, .SDcols = typeStat]
  teamStat <- get(typeStat, pos = teamStat)
  oppStat <- teamDT[, .SD, .SDcols = paste("Opp. ", typeStat, sep = "")]
  oppStat <- get(colnames(oppStat), pos = oppStat)
  
  # We extract team, opp ranks
  teamRank <- teamDT[, .SD, .SDcols = paste(typeStat, " - Rank", sep = "")]
  teamRank <- get(colnames(teamRank), pos = teamRank)
  oppRank <- teamDT[, .SD, .SDcols = paste("Opp. ", typeStat, " - Rank", sep = "")]
  oppRank <- get(colnames(oppRank), pos = oppRank)
  
  # Final DT to return
  return(data.table(Type = c("Team", "League", "Opponents"), Value = c(teamStat, meanLeague, oppStat), Rank = c(teamRank, 0, oppRank))) 
  
}

#-------------------------------------------------------------------------------
getTeamStatsGraph <- function(selectedTeam, typeStat = "Points", DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : Vector with a NBA team
  # - typeStat : statistics to analyze (default)
  #-------------------------------------------------------------------------------
  # We use the function getTeamStats to get Datas with a selected team
  teamStatsDT <- getTeamStats(selectedTeam, typeStat, DT)
  
  # We factorize to display X in a good order
  teamStatsDT$Type <- factor(teamStatsDT$Type, levels = c("Team", "League", "Opponents"))
  
  # We focus on the graph design with values from team DT
  fig <- plotly::plot_ly(data = teamStatsDT, type = "bar",
                         x = ~Type,
                         y = ~Value, 
                         texttemplate = ~paste("<b>", Value, "</b>", paste(str_sub(typeStat, 1, 1), "PG", sep = "")),
                         hovertemplate = ~ifelse(Type == "League", "", paste("Rank : <b>", Rank, "<extra></extra>", sep = "")),
                         marker = list(color = brewer.pal(9, "Reds")[c(5, 6, 7)], opacity = 0.6,
                                       line = list(width = 5, color = brewer.pal(9, "Reds")[c(5, 6, 7)], opacity = 1)),
                         showlegend = FALSE)
  
  # We customize the axes
  fig <- plotly::layout(p = fig, 
                        title = paste("<b>", selectedTeam, "</b>", "- Team vs. League", typeStat, "Per Game"),
                        yaxis = list(range = c(min(teamStatsDT$Value)-1, max(teamStatsDT$Value)+1),
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
getTeamStatsPerPlayer <- function(selectedTeam, typeStat = "Points", DTroster, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : Vector of a team name
  # - typeStat : Type of Stat (default)
  # - DTroster : To get nb of games per player
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # We download number of games played by each player for the selected Team
  gamesPlayersDT <- DTroster[team == selectedTeam, .(player, TotalGames)]
  
  # We filter with a smaller DT for team purposes
  teamGamesId <- unique(DT[team == selectedTeam, game_id])
  DTteam <- DT[game_id %in% teamGamesId]
  
  # Points variable
  if (typeStat == "Points") {
    # Tot Team
    totTeam <- sum(DTteam[team == selectedTeam & points %in% c(1, 2, 3), sum(points)])
    
    # Distribution per player
    distrPlayer <- DTteam[team == selectedTeam & points %in% c(1, 2, 3), .(Total=sum(points)), by = player]
    
  } else if (typeStat == "Assists") {
    # Tot Team
    totTeam <- nrow(DTteam[team == selectedTeam & assist != ""])
    
    # Distribution per player
    distrPlayer <- DTteam[team == selectedTeam & assist != "", .(Total=.N), by = assist]
    
  } else if (typeStat == "Rebounds") {
    # Tot Team
    totTeam <- nrow(DTteam[team == selectedTeam & event_type == "rebound" & player != ""])
    
    # Distribution per player
    distrPlayer <- DTteam[team == selectedTeam & event_type == "rebound" & player != "", .(Total=.N), by = player]
    
  } else if (typeStat == "Steals") {
    # Tot Team
    totTeam <- nrow(DTteam[team != selectedTeam & steal != ""])
    
    # Distribution per player
    distrPlayer <- DTteam[team != selectedTeam & steal != "", .(Total=.N), by = steal]
    
  } else if (typeStat == "Blocks") {
    # Tot Team
    totTeam <- nrow(DTteam[team != selectedTeam & block != ""])
    
    # Distribution per player
    distrPlayer <- DTteam[team != selectedTeam & block != "", .(Total=.N), by = block]
    
  } else if (typeStat == "Turnovers") {
    # Tot Team
    totTeam <- nrow(DTteam[team == selectedTeam & event_type == "turnover"])
    
    # Distribution per player
    distrPlayer <- DTteam[team == selectedTeam & event_type == "turnover", .(Total=.N), by = player]
    
  }
  
  # We change colnames
  colnames(distrPlayer) <- c("player", "Total")
  
  # We merge with number of games played
  distrPlayer <- merge(distrPlayer, gamesPlayersDT, by = "player", all.x = TRUE, all.y = FALSE)
  
  # We add variables per cent
  distrPlayer <- distrPlayer[, totDistr := round(100 * Total / totTeam, 2)]
  
  # We add per game for each player
  distrPlayer <- distrPlayer[, perGame := round(Total / TotalGames, 2)]
  distrPlayer <- distrPlayer[order(-totDistr)]
  
  return(distrPlayer)
  
}

#-------------------------------------------------------------------------------
getTeamStatsPerPlayerChart <- function(selectedTeam, typeStat = "Points", nbPlayers = 10, DTroster, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : A vector of a team to select
  # - typeStat : Type of stats to be displayed (default)
  # - nbPlayers : An integer of the number of players to be displayed on the graph (default)
  #-------------------------------------------------------------------------------
  # We load datas with getTeamStatsPerPlayer function
  teamPlayersStatsDT <- getTeamStatsPerPlayer(selectedTeam, typeStat, DTroster, DT)
  
  # We calculate the number of players for graph purpose
  teamNbPlayers <- nrow(teamPlayersStatsDT)
  
  # We filter datas regarding to the number of players selected with nbPlayers variable
  teamPlayersStatsDT <- head(teamPlayersStatsDT, nbPlayers)
  
  # We create the chart pie
  fig <- plotly::plot_ly(data = teamPlayersStatsDT, type = "pie", 
                         values = ~totDistr,
                         labels = ~player,
                         marker = list(colors = colorRampPalette(colors = brewer.pal(9, "Blues"))(min(nbPlayers, teamNbPlayers))),
                         texttemplate = ~paste('<b>', str_extract(player, "(?<=[:space:])[A-z]+"), '<br>', totDistr, '%'),
                         hovertemplate = ~paste('<b>', player, 
                                                '<br></b>', paste("Total", typeStat, ": <b>"), Total, 
                                                '<br></b>', paste(typeStat, "Per Game: <b>"), perGame, 
                                                '<br></b>Total Games: <b>', TotalGames, 
                                                '<extra></extra>'),
                         hole = 0.3, 
                         showlegend = FALSE)
  
  # Chart layout
  fig <- plotly::layout(p = fig,
                        title = list(text = paste("<b>", selectedTeam, "-", "</b>Distribution of Total", typeStat),
                                     yanchor = "bottom",
                                     xanchor = "middle"), 
                        plot_bgcolor = "rgba(255, 255, 255, 0)", 
                        paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  # No display bar
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getBestGames <- function(selectedTeam, nbGames = 10, typeStat = "Points", DTroster, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : a vector with a team name.
  # - nbGames : Number of best games to be displayed (default)
  # - typeStat : Type of statistics (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  # List of roster of the selected team
  teamRoster <- getListRoster(team = selectedTeam, DTroster)[[selectedTeam]]
  
  # Points
  if (typeStat == "Points") {
    
    statsPlayerDT <- DT[player %in% teamRoster & !is.na(points) & team == selectedTeam, 
                        .(Total = sum(points)), 
                        by = .(game_id, Date, Home, Away, player)]
    
    # Assists    
  } else if (typeStat == "Assists") {
    
    statsPlayerDT <- DT[assist %in% teamRoster &  team == selectedTeam, 
                        .(Total = .N), 
                        by = .(game_id, Date, Home, Away, assist)]
    
    # Blocks  
  } else if (typeStat == "Blocks") {
    
    statsPlayerDT <- DT[block %in% teamRoster & (Home == selectedTeam | Away == selectedTeam), 
                        .(Total = .N), 
                        by = .(game_id, Date, Home, Away, block)]
    
    # Steals 
  } else if (typeStat == "Steals") {
    
    statsPlayerDT <- DT[steal %in% teamRoster & (Home == selectedTeam | Away == selectedTeam), 
                        .(Total = .N), 
                        by = .(game_id, Date, Home, Away, steal)]
    
    
    # Rebounds 
  } else if (typeStat == "Rebounds") {
    
    statsPlayerDT <- DT[player %in% teamRoster & team == selectedTeam & event_type == "rebound", 
                        .(Total = .N), 
                        by = .(game_id, Date, Home, Away, player)]
    
  }
  
  # Renaming of column names for harmonization
  colnames(statsPlayerDT) <- c("GameId", "Date", "Home", "Away", "Player", "Total")
  
  # We just take the opposite team
  statsPlayerDT <- statsPlayerDT[, Opponent := ifelse(Home == selectedTeam, paste("h -", Away), paste("@ -", Home))] 
  
  # We filter variables
  statsPlayerDT <- statsPlayerDT[, .(Player, Total, Date, Opponent)]
  
  # we select the n-th first games with most points per player 
  bestStatsDT <- head(statsPlayerDT[order(-Total)], n = nbGames)
  
  return(bestStatsDT)
  
}