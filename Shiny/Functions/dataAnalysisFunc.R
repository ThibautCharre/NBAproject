#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------
##############
# IMPORT DATAS
##############
# cleanDatas : Import datas, filter on type of season (Regular or Playoffs) 

##############
# USEFULL HOMEMADE FUNCTIONS
##############
# isOnFloor : Detect if a player is on the floor based on string vectors. Return a logical response
# getGamesId : Display games_id of games played by a player for a specified team
# getShortTeamName : Connect Long name to Short NBA team name

##############
# CREATE INPUTS
##############
# getNBAcalendar : Add Home Team and Away team to each game_id and their scores
# getNBAplayers : List NBA players and paste name values from 10 variable into 2 variables in main data table
# getTeamPlayer : Get Short Team Name of a specific player
# getNbGames : Calculate nb of games for each player of the league
# gePlayersPosSal : Connect players with their position and salary via a csv file
# getListRoster : Display a team roster

##############
# CREATE OUTPUTS : 1st page Shiny - Team stats
##############
# Object 1
# getRankingStreak : Get historical results if a team is selected, otherwise current ranking of the league

# Object 2
# getTeamStats : Calculate selected stats for a selected team
# getTeamStatsGraph : Bar chart for getTeamStats results

# Object 3
# getTeamStatsPerPlayer : Stats for all players of a specified team
# getTeamStatsPerPlayerChart : Display of a selected stats distribution for a specific team

# Object 4
# getBestGames : Display a team best games for a specified stats category 

##############
# CREATE OUTPUTS : 2nd page Shiny - Impact of a player on its team
##############
# Object 1
# getLeaguePlayerStats : Get stats for all players in the league to rank them
# getPlayerStatsChart : Get spider chart to check weaknesses and players qualities

# Object 2
# getPlayerPointsPerPeriod : Get total points of a player per quarter
# getPlayerMinutesPerPeriod : Get total min played per period for a specified player
# getPlayerPeriodStatsChart : Display bart chart with points per period and minutes per period for a specified player

# Object 3
# getPlayerStatsPerDate : Display player stats and team results between two dates
# getPlayerStatsPerDateGraph : Display preceding results in a graph

# Object 4
# getPlayerImpact : Get team and opponents FG and +/- when a player is on court or off court
# getPlayerImpactGraph : Display preceding result in a graph

# Object 5
# getPlayerClutch : Display FG and ppg for a player in defined clutch games

##############
# CREATE OUTPUTS : 3rd page Shiny - Shooting charts
##############
# Object 1
# getShotsPlayer : Gives two data table (1st : player shots perf vs. league perf per area, 2nd : coordinates of shots)

# Object 2
# getAssistsShotsPlayer : Display assisted/non-assisted shots per area for a player and distribution of assists by teammates
# getAssistsShotsPlayerChart : Gives two pie charts of preceding datas

##############
# CREATE OUTPUTS : 3rd page Shiny - Shooting charts
##############
# Object 1
# getShotsLeagueRanking : A data table with best shooters per area

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
getLogoImage <- function(shortName) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  #-------------------------------------------------------------------------------
  pic <- load.image("Logos/", shortName, ".png", sep = "")
  
  return(pic)
}

#-------------------------------------------------------------------------------
cleanDatas <- function(season, seasonType = "Regular Season", filePattern = "combined-stats", 
                       colDelete = c("date", "remaining_time", "play_length", "play_id", "away", "home", "num", 
                                     "opponent", "outof", "possession", "reason", "original_x", "original_y", "description"), 
					   path = "CombinedGames",
                       saveDicoPlayers = FALSE) {
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
  
  # Return a vector with teams name
  teamDT <- sort(unique(nbaDT[team != "", team]))
  teamDT <- sapply(teamDT, getLongTeamName, USE.NAMES = FALSE)
  
  # return a vector of players names
  playerDT <- nbaDT[, .SD, .SDcols = c("a1", "a2", "a3", "a4", "a5", "h1", "h2", "h3", "h4", "h5")]
  playerDT <- sort(x = 
                     unique(
                       unlist(playerDT, use.names = FALSE)), 
                   decreasing = FALSE)
  
  # We clean nbaDT by subsetting all a & h columns into 2 columns (memory purposes)
  nbaDT[, onFloorHome := paste(h1, h2, h3, h4, h5, sep = "|")]
  nbaDT[, onFloorAway := paste(a1, a2, a3, a4, a5, sep = "|")]
  nbaDT[, ":=" (a1=NULL, a2=NULL, a3=NULL, a4=NULL, a5=NULL, h1=NULL, h2=NULL, h3=NULL, h4=NULL, h5=NULL)]
  
  return(list(nbaTeams = teamDT, nbaPlayers = playerDT, nbaDatas = nbaDT))
  
}

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
getTeamDT <-  function(selectedTeam, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team the player played for
  # - DT : data tabke where to find datas (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesIdTeam <- unique(DT[team == selectedTeam, game_id])
  
  teamDT <- DT[game_id %in% gamesIdTeam]
  
  return(teamDT)
  
}

#-------------------------------------------------------------------------------
getShortTeamName <- function(longName) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - longName : Long name of a team
  #-------------------------------------------------------------------------------
  teamsDT <- data.table(long = c("All", "Team", "Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", 
                                 "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Lakers", 
                                 "Los Angeles Clippers", "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", 
                                 "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", 
                                 "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards"),
                        short = c("All", "Team", "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", 
                                  "DEN", "DET", "GSW", "HOU", "IND", "LAL", 
                                  "LAC", "MEM", "MIA", "MIL", "MIN", "NOP", 
                                  "NYK", "OKC", "ORL", "PHI", "PHX", "POR", 
                                  "SAC", "SAS", "TOR", "UTA", "WAS")
  )
  
  shortName <- teamsDT[long == longName, short]
  
  return(shortName)
  
}

#-------------------------------------------------------------------------------
getLongTeamName <- function(shortName) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - shortName : Long name of a team
  #-------------------------------------------------------------------------------
  teamsDT <- data.table(long = c("All", "Team", "Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", 
                                 "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Lakers", 
                                 "Los Angeles Clippers", "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", 
                                 "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", 
                                 "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards"),
                        short = c("All", "Team", "ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", 
                                  "DEN", "DET", "GSW", "HOU", "IND", "LAL", 
                                  "LAC", "MEM", "MIA", "MIL", "MIN", "NOP", 
                                  "NYK", "OKC", "ORL", "PHI", "PHX", "POR", 
                                  "SAC", "SAS", "TOR", "UTA", "WAS")
  )
  
  longName <- teamsDT[short == shortName, long]
  
  return(longName)
  
}

#-------------------------------------------------------------------------------
getNBAcalendar <- function(season, path = "AllGames", DT = nbaDatas) {
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

#-------------------------------------------------------------------------------
getTeamPlayer <- function(players, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - players : Vector of players names
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  playerTeamDT <- DT[player %in% players & event_type != "jump ball", 
                     .(player, team)][order(player)]
  playerTeamDT <- unique(playerTeamDT)
  
  return(playerTeamDT)
  
}

#-------------------------------------------------------------------------------
getNbGames <-  function(DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : where to get infos (default)
  #-------------------------------------------------------------------------------
  # We find the number of games for each player
  gamesDT <- rbind(DT[, .(game_id, team, entered)], 
                   DT[, .(game_id, team, left)], use.names = FALSE) 
  gamesDT <- unique(gamesDT)
  
  # We aggergate nb of games by player and team
  gamesDT <- gamesDT[entered != "", .(TotalGames = uniqueN(game_id)), by = c("entered", "team")]
  colnames(gamesDT) <- c("player", "team", "TotalGames")
  
  return(gamesDT)
  
}


#-------------------------------------------------------------------------------
gePlayersFich <- function(season, seasonType, path = "Dictionary/") {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : where there is the list of players and their respective teams (default)
  # - path : path for the position.salary per player file (default)
  #-------------------------------------------------------------------------------
  # We download the dictionary pos salary age per player template
  posSalPlayersDT <- fread(paste(path, season, "/", seasonType, "/playersSummary.csv", sep = ""))
  
  return(posSalPlayersDT)
  
}

#-------------------------------------------------------------------------------
getListRoster <- function(team, DT = dicoPlayerMinute) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - team : Vector with teams names (default)
  #-------------------------------------------------------------------------------
  # For shiny purposes
  if (team == "Team") {
    return("Player")
  }
  
  teamsList <- lapply(sort(team), function(x) {
    sort(DT[team == x, player])
  })
  names(teamsList) <- sort(team)
  
  return(teamsList)
  
}

#-------------------------------------------------------------------------------
getRankingStreakLeague <- function(DTcalendar) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # Creation of a list of historical nba teams results in DTs
  teams <- unique(DTcalendar[, Home])
  listResultTeams <- lapply(X = teams, FUN = function(x) {
    
    # x represents each team
    tmpDT <- DTcalendar[(Winner == x | Loser == x), .(game_id, Date, Winner, Loser, Home, Away, home_score, away_score)]
    
    # We add if it is a win (+1) or a loss(-1) for each game
    tmpDT[, WorL := ifelse(Winner == x, 1, -1)]
    tmpDT[, Streak := NA_integer_]
    
    # For loop to calculate streak at each time points
    tmpDT[1, 'Streak'] <- tmpDT[1, WorL]
    for (i in seq(2, nrow(tmpDT))) {
      if (tmpDT[i, WorL] == tmpDT[i - 1, WorL]) {
        tmpDT[i, 'Streak'] <- sum(tmpDT[i - 1, Streak], tmpDT[i - 1, WorL]) 
      } else {
        tmpDT[i, 'Streak'] <- tmpDT[i, WorL]
      }
    }
    
    # We add the cumulated wins & losses for each team at each time points
    tmpDT[, Wins := ifelse(WorL > 0, 1, 0)]
    tmpDT[, Loses := ifelse(WorL < 0, 1, 0)]
    tmpDT[, Bilan := paste(cumsum(Wins), "-", cumsum(Loses))]
    tmpDT[, Ratio := round(cumsum(Wins) / (cumsum(Wins)+cumsum(Loses)) * 100, 2)]
    
  }) # end of lapply
  
  # Re-naming of elements of the list
  names(listResultTeams) <- teams
  
  return(listResultTeams)
  
}

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
  resultsTeamsDT <- resultsTeamsDT[, Home_Score := ifelse(home_score > away_score, home_score, away_score)]
  resultsTeamsDT <- resultsTeamsDT[, Away_Score := ifelse(home_score < away_score, home_score, away_score)]
  resultsTeamsDT <- resultsTeamsDT[, Result := paste(Home_Score, "-", Away_Score)]
  resultsTeamsDT <- resultsTeamsDT[, .(Date, Home, Away, Result, Bilan, Ratio, Streak)][order(-Date)]
  
  return(resultsTeamsDT)
  
}

#-------------------------------------------------------------------------------
getTeamStats <- function(selectedTeam, typeStat = "Points", DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : A team to be selected
  # - typeStat : statistics to analyze (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # Number of games in the league
  nbGamesLeague <- length(unique(DT[, game_id]))
  
  # We filter with a smaller DT for team purposes
  teamGamesId <- unique(DT[team == selectedTeam, game_id])
  DTteam <- DT[game_id %in% teamGamesId]
  
  # We calculate nb of games
  nbGamesTeam <- length(unique(DTteam[, game_id]))
  
  # We calculate mean league
  if (typeStat == "Points") {
    totLeague <- sum(DT[points %in% c(1, 2, 3), points])
    meanLeague <- round(totLeague / (nbGamesLeague * 2), 2) 
    
    totTeam <- sum(DTteam[team == selectedTeam & points %in% c(1, 2, 3), sum(points)])
    meanTeam <- round(totTeam / nbGamesTeam , 2)
    
    totOpp <- sum(DTteam[team != selectedTeam & points %in% c(1, 2, 3), points])
    meanOpp <- round(totOpp / nbGamesTeam, 2)
    
  } else if (typeStat == "Assists") {
    totLeague <- nrow(DT[assist != ""])
    meanLeague <- round(totLeague / (nbGamesLeague * 2), 2)
    
    totTeam <- nrow(DTteam[team == selectedTeam & assist != ""])
    meanTeam <- round(totTeam / nbGamesTeam, 2)
    
    totOpp <- nrow(DTteam[team != selectedTeam & assist != ""])
    meanOpp <- round(totOpp / nbGamesTeam, 2)
    
  } else if (typeStat == "Rebounds") {
    totLeague <- nrow(DT[event_type == "rebound" & player != ""])
    meanLeague <- round(totLeague / (nbGamesLeague * 2), 2)
    
    totTeam <- nrow(DTteam[team == selectedTeam & event_type == "rebound" & player != ""])
    meanTeam <- round(totTeam / nbGamesTeam, 2)
    
    totOpp <- nrow(DTteam[team != selectedTeam & event_type == "rebound" & player != ""])
    meanOpp <- round(totOpp / nbGamesTeam, 2)
    
  } else if (typeStat == "Steals") {
    totLeague <- nrow(DT[steal != ""])
    meanLeague <- round(totLeague / (nbGamesLeague * 2), 2)
    
    totTeam <- nrow(DTteam[team != selectedTeam & event_type == "turnover" & steal != ""])
    meanTeam <- round(totTeam / nbGamesTeam, 2)
    
    totOpp <- nrow(DTteam[team == selectedTeam & event_type == "turnover" & steal != ""])
    meanOpp <- round(totOpp / nbGamesTeam, 2)
    
  } else if (typeStat == "Blocks") {
    totLeague <- nrow(DT[block != ""])
    meanLeague <- round(totLeague / (nbGamesLeague * 2), 2)
    
    totTeam <- nrow(DTteam[team != selectedTeam & block != ""])
    meanTeam <- round(totTeam / nbGamesTeam, 2)
    
    totOpp <- nrow(DTteam[team == selectedTeam & block != ""])
    meanOpp <- round(totOpp / nbGamesTeam, 2)
    
  } else if (typeStat == "Turnovers") {
    totLeague <- nrow(DT[event_type == "turnover"])
    meanLeague <- round(totLeague / (nbGamesLeague * 2), 2)
    
    totTeam <- nrow(DTteam[team == selectedTeam & event_type == "turnover"])
    meanTeam <- round(totTeam / nbGamesTeam, 2)
    
    totOpp <- nrow(DTteam[team != selectedTeam & event_type != "turnover"])
    meanOpp <- round(totOpp / nbGamesTeam, 2)
    
  }
  
  finalDT <- data.table(Type = c("Team", "League", "Opponents"), Value = c(meanTeam, meanLeague, meanOpp))
  
  return(finalDT)
  
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
                         hoverinfo = "none",
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

#-------------------------------------------------------------------------------
getClassicPlayerStats <- function(selectedTeam, selectedPlayer, startDate = NULL, endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - minTotGames : a number of minimum game to rank players (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  
  # We filter DT based on extreme dates values
  if (!is.null(startDate) & !is.null(endDate)) {  
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We find player tot games for the period
  playerTotGames <- nrow(unique(DT[team == selectedTeam & (onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer), "game_id"]))
  
  ########
  # GAMES
  ########
  
  # We find the number of games for each player
  # timeDT <- DTgamesPlayed[team == selectedTeam, .(player, TotalGames, TotMin)]
  
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
  
}

#-------------------------------------------------------------------------------
getPlayerClassicStatsChart <- function(selectedTeam, selectedPlayer, startDate = NULL ,endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  if (selectedTeam != "Team" & selectedPlayer != "Player") {
    
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
  
}

#-------------------------------------------------------------------------------
getHistPlayerStats <- function(selectedTeam, selectedPlayer, startDate = NULL, endDate = NULL, DTcalendar, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - minTotGames : a number of minimum game to rank players (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  # Return by default
  if (selectedTeam == "Team" | selectedPlayer == "Player") {
    histStatsDT <- data.table("Date" = "-", "W/L" = "-", "Opp" = "-", "Pts" = "-", "Asts" = "-", "Rebs" = "-", "Stls" = "-", "Blks" = "-", "TOs" = "-")
    return(histStatsDT)
  }
  
  # We filter DT based on dates
  if (!is.null(startDate) & !is.null(endDate)) {
    DT <- DT[Date >= startDate & Date <= endDate]
  }
  
  # We take into account games where player was on the floor
  gamesIds <- unique(DT[onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer, game_id])
  
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
  
  # We filter datas based on date
  if (!is.null(startDate) & !is.null(endDate)) {
    histStatsDT <- histStatsDT[Date >= startDate & Date <= endDate]
  }

  return(histStatsDT[order(-Date)])
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

# #-------------------------------------------------------------------------------
# getPlayerStatsPerDate <- function(selectedTeam, selectedPlayer, perMonthWeek = "Month", DTcalendar, DTplayer) {
#   #-------------------------------------------------------------------------------
#   # @ variables :
#   # - selectedTeam : a team to select
#   # - selectedPlayer : a player to select from selected team
#   # - perMonthWeek : Results by Month or Week (default)
#   # - date1 : Minimum date (null by default, takes all current season)
#   # - date2 : Maximum date (null by default, takes all current season)
#   # - DT : a data table where datas are extracted (default)
#   #-------------------------------------------------------------------------------
#   ############
#   ##### TEAM # Results by team selected if selectedPlayer
#   ############
#   winsLooseTeamDT <- DTcalendar[Winner %like% selectedTeam|Loser %like% selectedTeam]
#   
#   # We add variable to group stats per ponth or week
#   if (perMonthWeek == "Month") {
#     winsLooseTeamDT[, MonthOrWeek := lubridate::month(Date)]
#   } else if (perMonthWeek == "Week") {
#     winsLooseTeamDT[, MonthOrWeek := lubridate::week(Date)]
#   }
#   
#   # We aggregate wins and loses by selected team and period, if no wins then we force value to zero
#   winsTeamDT <- winsLooseTeamDT[Winner == selectedTeam, .(TotalWins = .N), by = MonthOrWeek]
#   if (nrow(winsTeamDT) == 0) {
#     winsTeamDT <- data.table(MonthOrWeek = unique(winsLooseTeamDT$MonthOrWeek),
#                              TotalWins = 0)
#     # end if
#   }
#   
#   # Same thing for loses
#   lossesTeamDT <- winsLooseTeamDT[Loser == selectedTeam, .(TotalLosses = .N), by = MonthOrWeek]
#   if (nrow(lossesTeamDT) == 0) {
#     lossesTeamDT <- data.table(MonthOrWeek = unique(winsLooseTeamDT$MonthOrWeek),
#                                TotalLosses = 0)
#     # end if
#   }
#   
#   # We merge wins and losses
#   winsLooseTeamDT <- merge(winsTeamDT, lossesTeamDT, by = c("MonthOrWeek"), all = TRUE)
#   
#   # We force NA values to 0
#   winsLooseTeamDT[is.na(winsLooseTeamDT)] <- 0
#   
#   #############################
#   ##### PLAYER/LEAGUE LEADERS #
#   #############################
#   
#   # We filter generic datas used for all stats
#   nbaDatasDT <- DTplayer
#   
#   # if no games we return an empty data.table
#   if (nrow(nbaDatasDT) == 0) {
#     perMonthWeek <- "Week"
#     weekVector <- unique(lubridate::week(seq(from = date1, to = date2, by = 1)))
#     return(data.table(MonthOrWeek = weekVector, TotalWins = 0, TotalLosses = 0))
#     # end if
#   }
#   
#   # We add variable to group stats per month or week
#   if (perMonthWeek == "Month") {
#     nbaDatasDT[, MonthOrWeek := lubridate::month(Date)] 
#   } else if (perMonthWeek == "Week") {
#     nbaDatasDT[, MonthOrWeek := lubridate::week(Date)]
#   }
#   
#   # GAMES - 1
#   nbGamesDT <- nbaDatasDT[, .(TotalGames = uniqueN(game_id)), by = MonthOrWeek]
#   
#   # If no games played by player then we only display wins and losses
#   if (nrow(nbGamesDT) == 0) {
#     return(winsLooseTeamDT)
#   }
#   
#   # POINTS - 2
#   playerPointsDT <- nbaDatasDT[player == selectedPlayer & points %in% c(0, 1, 2, 3), 
#                                .(Date, MonthOrWeek, player, points)]
#   playerPointsDT <- playerPointsDT[, .(TotalPts = sum(points)), by = c("player", "MonthOrWeek")]
#   
#   # FIELD GOAL - 3
#   # We filter on shooting datas and create a variable to include 2pts Shots or 2pts Shots types
#   fieldGoalDT <- nbaDatasDT[result %in% c("made", "missed") & player == selectedPlayer, 
#                             .(MonthOrWeek, player, type, result)]
#   fieldGoalDT[, type := ifelse(grepl("3pt", type), "3pt Shots", 
#                                ifelse(grepl("free throw", type), 
#                                       "Free Throws", "2pt Shots"))]
#   
#   # We aggregate per type of shots and dcast for better results
#   fieldGoalDT <- fieldGoalDT[, .(Total = .N), by = c("player", "MonthOrWeek", "type", "result")]
#   fieldGoalDT <- dcast(fieldGoalDT, formula =  player + MonthOrWeek ~ type + result, value.var = "Total", fill = 0)
#   
#   # We check if all shoot type and results are tgere, if not, we create and force value to zero
#   missingTypeShots <- setdiff(c("player", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed"), 
#                               colnames(fieldGoalDT))
#   if (length(missingTypeShots) > 0) {  
#     for (i in seq(1, length(missingTypeShots))) {
#       fieldGoalDT <- fieldGoalDT[, missingTypeShots[i] := 0]
#     }
#   }
#   
#   # We calculate the % of FG
#   fieldGoalDT[, ":=" (FG2pts = round(get("2pt Shots_made") / (get("2pt Shots_missed") + get("2pt Shots_made"))  * 100, 2), 
#                       FG3pts = round(get("3pt Shots_made") / (get("3pt Shots_missed") + get("3pt Shots_made"))  * 100, 2),
#                       FGft = round(get("Free Throws_made") / (get("Free Throws_missed") + get("Free Throws_made"))  * 100, 2))]
#   
#   # If no FG, then we force to zero
#   fieldGoalDT[is.na(fieldGoalDT)] <- 0
#   fieldGoalDT <- fieldGoalDT[, .(player, MonthOrWeek, FG2pts, FG3pts, FGft)]
#   
#   
#   # ASSISTS - 4
#   assistsPlayerDT <- nbaDatasDT[assist == selectedPlayer, .(Date, MonthOrWeek, assist, team)]
#   assistsPlayerDT <- assistsPlayerDT[, .(TotalAss = .N), by = c("assist", "MonthOrWeek")]
#   
#   # REBOUND - 5
#   reboundsPlayerDT <- nbaDatasDT[event_type == "rebound" & player == selectedPlayer, .(Date, MonthOrWeek, player, team)]
#   reboundsPlayerDT <- reboundsPlayerDT[, .(TotalReb = .N), by = c("player", "MonthOrWeek")]
#   
#   # We aggregate all objects
#   statsPlayerPeriodDT <- merge(playerPointsDT, fieldGoalDT, 
#                                by = c("player", "MonthOrWeek"), all = TRUE)
#   statsPlayerPeriodDT <- merge(statsPlayerPeriodDT, assistsPlayerDT, 
#                                by.x = c("player", "MonthOrWeek"), by.y = c("assist", "MonthOrWeek"), all = TRUE)
#   statsPlayerPeriodDT <- merge(statsPlayerPeriodDT, reboundsPlayerDT, 
#                                by = c("player", "MonthOrWeek"), all = TRUE)
#   statsPlayerPeriodDT <- merge(winsLooseTeamDT, statsPlayerPeriodDT, 
#                                by = c("MonthOrWeek"), all = TRUE)
#   
#   # We add number of games to finally get per game results
#   statsPlayerPeriodDT <- merge(statsPlayerPeriodDT, nbGamesDT, 
#                                by = c("MonthOrWeek"), all = TRUE)
#   
#   # New variables to get mean
#   statsPlayerPeriodDT[, PPG := round(TotalPts / TotalGames, 2)]
#   statsPlayerPeriodDT[, APG := round(TotalAss / TotalGames, 2)]
#   statsPlayerPeriodDT[, RPG := round(TotalReb / TotalGames, 2)]
#   
#   return(statsPlayerPeriodDT)
#   
# }
# 
# #-------------------------------------------------------------------------------
# getPlayerStatsPerDateGraph <- function(selectedTeam, selectedPlayer, perMonthWeek = "Month", date1 = NULL, date2 = NULL, DTcalendar = NBAcalendar, DTplayer) {
#   #-------------------------------------------------------------------------------
#   # @ variables :
#   # - selectedTeam : a team to select
#   # - selectedPlayer : a player to select from selected team
#   # - perMonthWeek : Results by Month or Week (default)
#   # - date1 : Minimum date (null by default, takes all current season)
#   # - date2 : Maximum date (null by default, takes all current season)
#   # - DT : a data table where datas are extracted (default)
#   #-------------------------------------------------------------------------------
#   # We donwnload stats for a specific player
#   playerStatsDT <- getPlayerStatsPerDate(selectedTeam, selectedPlayer, perMonthWeek, date1, date2, DTcalendar, DTplayer)
#   
#   if (perMonthWeek == "Month") {
#     
#     # Conversion DT for factoring period
#     convertDT <- data.table(numMonth = seq(1, 12, 1), 
#                             nameMonth = c("January", "Februray", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
#     
#     # We translate number to Month name
#     playerStatsDT[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
#       convertDT[numMonth == x, nameMonth]
#     })]
#     
#     # We do a factorization to respect time order of the season
#     playerStatsDT$MonthOrWeek <- factor(playerStatsDT$MonthOrWeek, 
#                                         levels = c("September", "October", "November", "December", "January", "Februray", "March", "April", "May", "June", "July", "August"), 
#                                         ordered = TRUE)
#     
#   } else if (perMonthWeek == "Week") {
#     
#     # Conversion DT for factoring period
#     convertDT <- data.table(numWeek = seq(1, 52, 1), 
#                             nameWeek = sapply(seq(1, 52, 1), function(x) {paste("W-", x, sep = "")}))
#     
#     # We translate number to Month name
#     playerStatsDT[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
#       convertDT[numWeek == x, nameWeek]
#     })]
#     
#     # We do a factorization to respect time order of the season
#     playerStatsDT$MonthOrWeek <- factor(playerStatsDT$MonthOrWeek, 
#                                         levels = c(paste("W-", seq(35, 52, 1), sep = ""), paste("W-", seq(1, 34, 1), sep = "")))
#     # end if  
#   }
#   
#   # 1st bar chart : wins
#   fig <- plot_ly(data = playerStatsDT, type = "bar", 
#                  x = ~MonthOrWeek, y = ~TotalWins,
#                  marker = list(color = "rgba(255, 99, 71, 0.5)"),
#                  texttemplate = ~paste("<b>", TotalWins, "W"),
#                  hoverinfo = "none",
#                  showlegend = FALSE)
#   
#   # 2nd bar chart : losses
#   fig <- add_trace(p = fig, type = "bar", 
#                    y = ~TotalLosses,
#                    texttemplate = ~paste("<b>", TotalLosses, "L"),
#                    marker = list(color = "rgba(51, 132, 255, 0.5)"),
#                    hoverinfo = "none",
#                    showlegend = FALSE)
#   
#   # 3rd marks : PPG plus other stats (only available if player played at least 1 game betwwen date1 and date 2)
#   hasPlayerPlayed <- ifelse(ncol(playerStatsDT) == 3, FALSE, TRUE)
#   
#   if (hasPlayerPlayed == TRUE) {
#     fig <- add_trace(p = fig, data = playerStatsDT, type = "scatter", mode = "markers",
#                      x = ~MonthOrWeek, y = ~PPG,
#                      yaxis = "y2",
#                      marker = list(color = "rgba(151, 44, 92, 0.8)", size = 10),
#                      hovertemplate = ~paste(TotalGames, "games",
#                                             "<br>PPG :", PPG,
#                                             "<br>APG :", APG,
#                                             "<br>RPG :", RPG,
#                                             "<br><br>2pts FG :", FG2pts, "%",
#                                             "<br>3pts FG :", FG3pts, "%",
#                                             "<br>FT FG :", FGft, "%",
#                                             "<extra></extra>"), 
#                      showlegend = FALSE)
#     
#     # Title and disposition
#     fig <- layout(p = fig, data = playerStatsDT,
#                   title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "Stats per", perMonthWeek),
#                   xaxis = list(title=""),
#                   yaxis2 = list(overlaying = "y",
#                                 side = "right",
#                                 range = ~c(0, max(PPG)+1),
#                                 zeroline = TRUE,
#                                 showline = FALSE,
#                                 showticklabels = FALSE,
#                                 showgrid = FALSE),
#                   yaxis = list(title = "",
#                                zeroline = TRUE,
#                                showline = FALSE,
#                                showticklabels = FALSE,
#                                showgrid = TRUE),
#                   barmode = "group")
#     # end if
#   } else if (hasPlayerPlayed == FALSE) {
#     # Title and disposition
#     fig <- layout(p = fig, data = playerStatsDT,
#                   title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "</b>Did Not Play"),
#                   xaxis = list(title=""),
#                   yaxis = list(title = "Team Wins/Losses",
#                                zeroline = TRUE,
#                                showline = TRUE,
#                                showticklabels = TRUE,
#                                showgrid = TRUE),
#                   barmode = "group")
#     # enf else if  
#   }
#   
#   # No plot bar top right
#   fig <- plotly::config(p = fig, displayModeBar = FALSE)
#   
#   return(fig)
#   
# }

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

# #-------------------------------------------------------------------------------
# getPlayerImpactGraph <- function(selectedTeam, selectedPlayer, DTplayer) {
#   #-------------------------------------------------------------------------------
#   # @ variables :
#   # - selectedTeam : a team to select
#   # - selectedPlayer : a player from the selected team
#   # - DT : a data table where datas are extracted (default)
#   #-------------------------------------------------------------------------------
#   # We download impact datas
#   playerImpactDT <- getPlayerImpact(selectedTeam, selectedPlayer, DTplayer)
#   
#   # We treat shooting datas to display on a 1st graph
#   shootingImpactDT <- playerImpactDT[["ShootImpact"]]
#   
#   # We treat points datas to display on a 2nd graph
#   pointImpactDT <- playerImpactDT[["PointImpact"]]
#   
#   # We factor to get a correct order
#   pointImpactDT$Floor <- c("With Player", "Without Player")
#   pointImpactDT$Floor <- factor(x = pointImpactDT$Floor, levels = c("With Player", "Without Player"))
#   
#   # We keep nb games to include in a title
#   nbGamesPlayed <- unique(pointImpactDT[, nbGames])
#   
#   # Impact on Shooting datas
#   fig <- plot_ly(data = shootingImpactDT, type = "bar", name = "Team FG with Player", 
#                  x = ~typeSimplified, y = ~Team_onFloor, 
#                  marker = list(color = "rgba(255, 99, 71, 1)"),
#                  hovertemplate = ~paste(Team_onFloor, "%", "<extra></extra>"))
#   
#   fig <- add_bars(p = fig, name = "Team FG without Player", 
#                   y = ~Team_offFloor, 
#                   marker = list(color = "rgba(255, 99, 71, 0.7)"),
#                   hovertemplate = ~paste(Team_offFloor, "%", "<extra></extra>"))
#   
#   fig <- add_bars(p = fig, name = "Opp Team FG with Player", 
#                   y = ~Opponent_onFloor, 
#                   marker = list(color = "rgba(71, 99, 255, 1)"),
#                   hovertemplate = ~paste(Opponent_onFloor, "%", "<extra></extra>"))
#   
#   fig <- add_bars(p = fig, name = "Opp Team FG without Player", 
#                   y = ~Opponent_offFloor, 
#                   marker = list(color = "rgba(71, 99, 255, 0.7)"),
#                   hovertemplate = ~paste(Opponent_offFloor, "%", "<extra></extra>"))
#   
#   fig <- layout(p = fig, 
#                 title = paste("<b>", selectedPlayer, "-", selectedTeam, "<br></b>: FG & +/- Impacts on", nbGamesPlayed, "games played"),
#                 barmode = "group", 
#                 legend = list(x = 100, y = 0.7))
#   
#   # Impact on points graph
#   fig2 <- plot_ly(data = pointImpactDT, type = "bar", 
#                   x = ~Floor, y = ~MeanPlusMinus, 
#                   marker = list(color = c("rgba(180, 190, 95, 0.7)", "rgba(180, 107, 95, 0.7)")),
#                   hovertemplate = ~paste("+/- :", MeanPlusMinus, "pts", "<extra></extra>"), 
#                   showlegend = FALSE)
#   
#   fig2 <- layout(p = fig2,
#                  yaxis = list(title = ""),
#                  xaxis = list(title = ""))
#   
#   # We combine the two preceding graphs
#   fig3 <- subplot(fig, fig2, nrows = 2, heights = c(0.7, 0.3), margin = 0.05)
#   
#   return(fig3)
#   
# }

#-------------------------------------------------------------------------------
getPlayerClutch <- function(selectedTeam, selectedPlayer, timeLeft = 4, diffScore = 5, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : "All" to check league Leaders
  # - timeLeft : at which point time we consider a clutch moment
  # - diffScore : difference between scores to consider game is close
  # - minNbGame : Nb of close game competed by a player to be considered in the league ranking
  # - DT
  #-------------------------------------------------------------------------------
  # We add the minute variable
  DTplayer <- DTplayer[, minute := as.integer(str_sub(elapsed, 3, 4))]
  
  # We filter general datas
  nbaDatasDT <- DTplayer[period >= 4]
  
  # We add the variable minute by translating elapsed time variable
  nbaDatasDT <- nbaDatasDT[, minute := as.integer(str_sub(elapsed, 3, 4))]
  
  # Games where a clutch momentum is played by the player
  gameClutch <- unique(nbaDatasDT[period == 4 & minute == (12-timeLeft) & abs(away_score - home_score) <= diffScore, game_id])
  
  # We filter datas based on clutch games vector
  gameClutchDT <- DT[game_id %in% gameClutch & ((period == 4 & minute >= (12 - timeLeft))|period > 4)]
  
  # If no clutch game found, then we display a data table with zero everywhere
  if (nrow(gameClutchDT) == 0) {
    return(data.table(player = selectedPlayer, 
                      TotalPoints = 0, PPG = 0, 
                      "2pts Attempts" = 0, "2pts Field Goal" = 0, 
                      "3pts Attempts" = 0, "3pts Field Goal" = 0,
                      "Free Throws Attempts" = 0, "Free Throws Field Goal" = 0, 
                      TotalWins = 0, TotalLosses = 0))
    # enf if  
  }
  
  ####################
  # 1/ PLAYER WIN/LOSS
  ####################
  
  # We filter on score for clutch games
  winLossDT <- gameClutchDT[event_type == "end of period" & (home_score != away_score), .(game_id, Home, home_score, Away, away_score)]
  
  # Creation of total win or loss for a player
  winLossDT[, Wins := ifelse((Home == selectedTeam & home_score > away_score) | (Away == selectedTeam & away_score > home_score), 
                             1, 0)]
  winLossDT[, Losses := ifelse(Wins == 1, 0, 1)]
  
  # We sum number of wins & losses
  playerwinLossDT <- data.table(player = selectedPlayer, 
                                TotalWins = sum(winLossDT[, "Wins"]),
                                TotalLosses = sum(winLossDT[, "Losses"]))
  
  ###########
  # 2/ POINTS
  ###########
  
  # We calculate PPG and number of closed games per player
  pointsPlayerDT <- gameClutchDT[player == selectedPlayer & points %in% c(1, 2, 3), .(TotalPoints = sum(points)), by = c("player")]
  
  # If no points detected, then we return a data table with 0 as value for total points
  if (nrow(pointsPlayerDT) == 0) {
    pointsPlayerDT <- data.table(player = selectedPlayer, TotalPoints = 0)
  }
  
  # We add PPG by dividing total points by umber of clutch games
  pointsPlayerDT <- pointsPlayerDT[, PPG := round(TotalPoints / length(gameClutch), 2)]
  
  ############
  # 3/ FIELD GOAL
  ############
  
  # We focus on shots
  shootingPlayerDT <- gameClutchDT[player == selectedPlayer & (event_type == "shot"|event_type == "free throw")]
  
  # Same thing for Field Goal
  shootingPlayerDT <- shootingPlayerDT[, type := ifelse(grepl("3pt", type), "3pt Shots", 
                                                        ifelse(grepl("free throw", type), 
                                                               "Free Throws", "2pt Shots"))]
  
  # We aggregate shots by type and result for the selected player
  shootingPlayerDT <- shootingPlayerDT[, .(Total = .N), by = c("player", "type", "result")]
  
  # dcast to provide a better data table
  shootingPlayerDT <- dcast(shootingPlayerDT, player  ~ type + result, value.var = "Total", fill = 0)
  
  # If NaN or NA then we force value to 0
  shootingPlayerDT[is.na(shootingPlayerDT)] <- 0
  
  # We check if all shoot type and results are tgere, if not, we create and force value to zero
  missingTypeShots <- setdiff(c("player", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "2pt Shots_missed", "Free Throws_made", "Free Throws_missed"), 
                              colnames(shootingPlayerDT))
  if (length(missingTypeShots > 0)) {
    for (i in seq(1, length(missingTypeShots))) {
      shootingPlayerDT <- shootingPlayerDT[, missingTypeShots[i] := 0] 
    }
  }
  
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
  colnames(shootingPlayerDT) <- c("player", "2pts Attempts", "2pts Field Goal", "3pts Attempts", "3pts Field Goal", "Free Throws Attempts", "Free Throws Field Goal")
  shootingPlayerDT[is.na(shootingPlayerDT)] <- 0
  
  #####################
  # POINTS + FIELD GOAL
  #####################
  # We merge both previous datas
  playerScoringDT <- merge(pointsPlayerDT, shootingPlayerDT, 
                           by = c("player"), all.x = TRUE, all.y = FALSE)
  
  #####################
  # FINAL OBJECT (depending on player selection or not)
  #####################
  playerScoringWL <- merge(playerScoringDT, playerwinLossDT, by = "player", all = TRUE)
  
  return(playerScoringWL)
  
}

#-------------------------------------------------------------------------------
getPlayerGlobalShooting <- function(selectedTeam, selectedPlayer, startDate = NULL, endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # Returns object by default
  if (selectedTeam == "Team" | selectedPlayer == "Player") {
    
    shootingCalDT <- data.table(Date = "-", Opp = "-", "2PM" = "-", "2PA" = "-", "3PM" = "-", "3PA" = "-", "FTM" = "-", "FTA" = "-")
    shootingFinalDT <- data.table(Type = c("2pts Shots", "3pts Shots", "Free Throws", "Effective Shots", "True Shooting"), 
                                  FieldGoal = c(0, 0, 0, 0, 0))
    
    return(list(calDT = shootingCalDT, graphDT = shootingFinalDT))
  }
  
  # We download datas
  if (is.null(startDate) | is.null(endDate)) {
    shootingDT <- DT[player == selectedPlayer & team == selectedTeam & event_type %in% c("free throw", "shot"), .(Date, Home, Away, points, type, result)]
    shootingDT <- shootingDT[, oppTeam := ifelse(Home == selectedTeam, Away, Home)]
  } else {
    shootingDT <- DT[player == selectedPlayer & team == selectedTeam & event_type %in% c("free throw", "shot") & Date >= startDate & Date <= endDate, .(Date, Home, Away, points, type, result)]
    shootingDT <- shootingDT[, oppTeam := ifelse(Home == selectedTeam, Away, Home)]
  }
  
  # We isolate total points of the player
  totPoints <- sum(shootingDT[, "points"])
  
  # We create categories
  shootingDT <- shootingDT[, typeShot := ifelse(test = grepl('3pt', type), yes = '3pt Shots', 
                                                no = ifelse(grepl("free throw", type), "Free Throws", "2pt Shots"))]
  
  # We aggregate nb of shots by result and type of shot
  shootingDT <- shootingDT[, .(Total = .N), by = c("Date", "oppTeam", "typeShot", "result")]
  
  # dcast to provide results by columns
  shootingDT <- dcast(data = shootingDT, formula = Date + oppTeam ~ typeShot + result, value.var = "Total")
  
  # We check if all shoot type and results are there, if not, we create and force value to zero
  missingTypeShots <- setdiff(c("Date", "oppTeam", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed"), 
                              colnames(shootingDT))
  
  if (length(missingTypeShots) > 0) { 
    for (i in seq(1, length(missingTypeShots))) {
      shootingDT <- shootingDT[, missingTypeShots[i] := 0]
    }
  }
  
  # We replace NA by zero
  shootingDT[is.na(shootingDT)] <- 0
  
  # We reorganize columns order
  shootingDT <- shootingDT[, .SD, .SDcols = c("Date", "oppTeam", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed")]
  
  # shooting calendar dt
  shootingCalDT <- copy(shootingDT)
  colnames(shootingCalDT) <- c("Date", "Opp", "2PM", "2PA", "3PM", "3PA", "FTM", "FTA")
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
getShotsPlayerCoordinates <- function(selectedPlayer, selectedTeam, startDate = NULL, endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We download datas
  if (is.null(startDate)|is.null(endDate)) {
    shootingDT <- DT[player == selectedPlayer & team == selectedTeam & points %in% c(0, 2, 3) & !grepl("free throw", type) & (!is.na(converted_x) & !is.na(converted_y)), 
                     .(result, type, assist, shot_distance, converted_x, converted_y)]
  } else {
    shootingDT <- DT[player == selectedPlayer & team == selectedTeam & points %in% c(0, 2, 3) & !grepl("free throw", type) & (!is.na(converted_x) & !is.na(converted_y)) & Date >= startDate & Date <= endDate, 
                     .(result, type, assist, shot_distance, converted_x, converted_y)]
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
  
  
  # We save shooting coord for a specific player
  playerShootingCoordDT <- shootingDT[, .(result, shot_distance, converted_x_corrected, converted_y_corrected)]
  
  return(playerShootingCoordDT)
  
}

#-------------------------------------------------------------------------------
getShotsPlayerVsLeague <- function(selectedPlayer, selectedTeam, distLongShot = 27, startDate = NULL, endDate = NULL, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - distLongShot : distance in feet after which a shot is considered a long shot (distance)
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
  
  # We filter to get a proper DT to work with
  shootingDT <- shootingDT[, .(player, team, result, typeShot, shot_distance, converted_x_corrected, converted_y_corrected)]
  
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
  
  # We also return 
  return(playerVsLeagueDT = fgLeaguePlayerShotsDT)
  
}

#-------------------------------------------------------------------------------
getAssistsShotsPlayer <- function(selectedPlayer, selectedTeam, startDate = NULL, endDate = NULL, selectedArea = "All", distLongShot = 27,  DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - selectedArea : an area to select (default, all area)
  # - distLongShot : distance in feet after which a shot is considered a long shot (distance)
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We download datas and filter to made shots
  if (is.null(startDate)|is.null(endDate)) {
    shootingDT <- DT[points %in% c(0, 2, 3) & player == selectedPlayer & team == selectedTeam & !grepl("free throw", type) & result == "made" & (!is.na(converted_x) & !is.na(converted_y)), 
                     .(type, assist, shot_distance, converted_x, converted_y)]
  } else {
    shootingDT <- DT[points %in% c(0, 2, 3) & player == selectedPlayer & team == selectedTeam & Date >= startDate & Date <= endDate & !grepl("free throw", type) & result == "made" & (!is.na(converted_x) & !is.na(converted_y)), 
                     .(type, assist, shot_distance, converted_x, converted_y)]
  }
  
  # Difference among 2pt shots and 3pt shots
  shootingDT <- shootingDT[, typeShot := ifelse(test = grepl("3pt", type), yes = "3pt Shots", no = "2pt Shots")]
  
  # We modify long shot to display them on a half court
  shootingDT <- shootingDT[, converted_x_corrected := ifelse(test = converted_y > 47 & shot_distance < 48.5, 
                                                             yes = 25+(25-converted_x), 
                                                             no = ifelse(test = converted_y < 47 & shot_distance > 48.5, 
                                                                         yes = 25+(25-converted_x), no = converted_x))]
  
  shootingDT <- shootingDT[, converted_y_corrected := ifelse(test = converted_y > 47 & shot_distance < 48.5, 
                                                             yes = 47+(47-converted_y), 
                                                             no = ifelse(test = converted_y < 47 & shot_distance > 48.5, 
                                                                         yes = 47+(47-converted_y), no = converted_y))]
  
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
  
  # We create a ranking by player with number of assists
  if (selectedArea == "All") {
    assistsShotsDT <- shootingDT[, .(TotalAssists = .N), by = assist]
  } else {
    assistsShotsDT <- shootingDT[area == selectedArea, .(Total = .N), by = assist]
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

#-------------------------------------------------------------------------------
getShotsLeagueRanking <- function(selectedArea = "All", minShots = 10, distLongShot = 27, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedArea : Select an area for league shots ranking (defaukt is all)
  # - minShots : Minimum shots taken (default)
  # - distLongShot : Distance from basket where it is considered as a long shot (default)
  # - DT : where to get infos (default)
  #-------------------------------------------------------------------------------
  # We download datas
  shootingDT <- DT[points %in% c(0, 2, 3) & !grepl("free throw", type) & (!is.na(converted_x) & !is.na(converted_y)), 
                   .(player, result, type, assist, shot_distance, converted_x, converted_y)]
  
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
  
  # We filter to get a proper DT to work with
  shootingDT <- shootingDT[, .(player, result, typeShot, shot_distance, converted_x_corrected, converted_y_corrected)]
  
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
  
  # We filter on selected area
  if (selectedArea == "All") {
    shootingAreaDT <- shootingDT
  } else {
    shootingAreaDT <- shootingDT[area %in% selectedArea]
  }
  
  # We aggregate results by players
  shootingAreaMadeDT <- shootingAreaDT[result == "made", .(TotalMade = .N), by = player]
  shootingAreaMissedDT <- shootingAreaDT[result == "missed", .(TotalMissed = .N), by = player]
  
  # We merge datas
  shootingLeagueDT <- merge(shootingAreaMadeDT, shootingAreaMissedDT, by = "player", all = TRUE)
  
  # When NA value, we force value to 0
  shootingLeagueDT[is.na(shootingLeagueDT)] <- 0
  
  # We create the Field Goal variable
  shootingLeagueDT <- shootingLeagueDT[, FieldGoal := round(100 * TotalMade / (TotalMade + TotalMissed), 2)]
  
  # We filter with number of minimum shots taken
  shootingLeagueDT <- shootingLeagueDT[(TotalMissed + TotalMade) >= minShots]
  
  # We order by FG
  shootingLeagueDT <- shootingLeagueDT[order(-FieldGoal)]
  
  return(shootingLeagueDT)
  
}

#-------------------------------------------------------------------------------
getShootingCustom <- function(selectedTeam = "All", shootType = "All Shots", 
                              position = "All", 
                              salaryLow = 0, salaryHigh = 100000000, 
                              experienceLow = 0, experienceHigh = 100,
                              ageLow = 1, ageHigh = 100,
                              nbPlayer = 15, DTplayerPres = dicoPlayerFich, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : data table where to look for the datas
  # - selectedTeam : select a team
  # - shootType : Either all, 2pt or 3pt shots or FT
  # - nbPlayer : Number of players displayed
  #-------------------------------------------------------------------------------
  # First we select players corresponding to criteria
  if ("All" %in% position) {
    playersFilteredDT <- DTplayerPres[Salary >= salaryLow & Salary <= salaryHigh & 
                                        Age >= ageLow & Age <= ageHigh &
                                        Experience >= experienceLow & Experience <= experienceHigh]
  } else {
    playersFilteredDT <- DTplayerPres[Salary >= salaryLow & Salary <= salaryHigh & 
                                        Age >= ageLow & Age <= ageHigh &
                                        Experience >= experienceLow & Experience <= experienceHigh &
                                        Position %in% position]
  }
  
  # We filter taking all shots excluding FT
  if (shootType == "All Shots") {
    shootingDT <- DT[result %in% c("made", "missed") & !grepl("free throw", type), .(Total=.N), by = .(player, team, result)]
    
    # Just 3pt shots
  } else if (shootType == "3pt Shots") {
    shootingDT <- DT[result %in% c("made", "missed") & grepl("3pt", type), .(Total=.N), by = .(player, team, result)]
    
    # Just FT shots    
  } else if (shootType == "Free Throws") {
    shootingDT <- DT[result %in% c("made", "missed") & grepl("free throw", type), .(Total=.N), by = .(player, team, result)]
    # Just 2pt shots  
  } else if (shootType == "2pt Shots") {
    shootingDT <- DT[result %in% c("made", "missed") & !grepl("free throw", type) & !grepl("3pt", type), .(Total=.N), by = .(player, team, result)]
  }
  
  # We filter with selected players
  shootingFilteredDT <- merge(playersFilteredDT, shootingDT, by = "player", all.x = FALSE, all.y = FALSE)
  
  # if a selected team is selected
  if (selectedTeam != "All") {
    shootingFilteredDT <- shootingFilteredDT[team == selectedTeam]
  } else {
    shootingFilteredDT <- shootingFilteredDT[, .(Total=sum(Total)), by = setdiff(names(shootingFilteredDT), c("team", "Total"))]  
  } 
  
  # We separate the made and missed and gather them in a data table
  shootingMadeDT <- shootingFilteredDT[result == "made", .SD, .SDcols = setdiff(names(shootingFilteredDT), "result")]
  colnames(shootingMadeDT)[colnames(shootingMadeDT) == "Total"] <- "made"
  
  shootingMissedDT <- shootingFilteredDT[result == "missed", .SD, .SDcols = setdiff(names(shootingFilteredDT), "result")]
  colnames(shootingMissedDT)[colnames(shootingMissedDT) == "Total"] <- "missed"
  
  # We merge the two previous DT
  shootingFilteredDT <- merge(shootingMadeDT, shootingMissedDT, by = setdiff(names(shootingMissedDT), c("made", "missed")), all = TRUE)
  
  # We force NA values to zero
  shootingFilteredDT[is.na(shootingFilteredDT)] <- 0
  
  # We create new variables to be displayed
  shootingFilteredDT <- shootingFilteredDT[, total := made + missed]
  shootingFilteredDT <- shootingFilteredDT[, FieldGoal := round(made / total * 100, 2)][order(-total)]
  
  # We take the ith first lines
  shootingFilteredDT <- head(shootingFilteredDT, n = min(nbPlayer, nrow(shootingFilteredDT)))
  
  return(shootingFilteredDT)
  
}

#-------------------------------------------------------------------------------
getShootingCustomGraph <- function(shootType, DTstats) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : data table where to look for the datas
  # - selectedTeam : select a team
  # - shootType : Either all, 2pt or 3pt shots or FT
  # - nbPlayer : Number of players displayed
  #-------------------------------------------------------------------------------
  # We download customed datas with preceding function
  shootingDT <- DTstats
  
  # Creation of a plotly graph
  fig <- plot_ly(data = shootingDT, type = "scatter",
                 mode = "markers",
                 x = ~total, y = ~FieldGoal,
                 color = ~Salary,
                 colors = "Spectral",
                 marker = list(sizeref = 0.1, sizemode = "area", opacity = 0.5),
                 text = ~str_extract_all(string = get(x = 'player'), pattern = "(?<=([:blank:]))[A-z]+"),
                 hovertemplate = ~paste("<b>", player, "-", "</b>", Position,
                                        "</b><br>Total Attempted :<b>", total,
                                        "</b><br>Field Goal (%) :<b>", FieldGoal,
                                        "</b><br><br>", Age, "<b>years old",
                                        "</b><br>Salary :<b>", round(Salary/1000000, 3), "M $",
                                        "<extra></extra>"),
                 showlegend = FALSE)
  
  # We add text on top of each bubble
  fig <- add_text(p = fig,
                  textposition = "top")
  
  # Title and axis range
  fig <- layout(p = fig,
                title = "",
                xaxis = list(title = "Total Attempted",
                             zeroline = FALSE),
                yaxis = list(title = paste(shootType, "- Field Goal (%)"),
                             zeroline = FALSE), 
				plot_bgcolor = "rgba(255, 255, 255, 0)",
                paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getStatsCustom <- function(selectedTeam = "All", statType = "Points", 
                           position = "All", 
                           salaryLow = 0, salaryHigh = 100000000, 
                           experienceLow = 0, experienceHigh = 100,
                           ageLow = 1, ageHigh = 100,
                           nbPlayer = 15, DTgamesPlayed = dicoPlayerMinute, DTplayerPres = dicoPlayerFich, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : data table where to look for the datas
  # - selectedTeam : select a team
  # - statType : Points, Assists, Rebounds, Blocks, Steals
  # - nbPlayer : Number of players displayed
  #-------------------------------------------------------------------------------
  # First we select players corresponding to criteria
  if ("All" %in% position) {
    playersFilteredDT <- DTplayerPres[Salary >= salaryLow & Salary <= salaryHigh & 
                                        Age >= ageLow & Age <= ageHigh &
                                        Experience >= experienceLow & Experience <= experienceHigh]
  } else {
    playersFilteredDT <- DTplayerPres[Salary >= salaryLow & Salary <= salaryHigh & 
                                        Age >= ageLow & Age <= ageHigh &
                                        Experience >= experienceLow & Experience <= experienceHigh &
                                        Position %in% position]
  }
  
  # We filter by points
  if (statType == "Points") {
    statDT <- DT[points %in% c(1, 2, 3), .(Total = sum(points)), by = .(player, team)]
    
    # Assists
  } else if (statType == "Assists") {
    nbaDatas <- DT[assist != ""]
    statDT <- nbaDatas[, .(Total=.N), by = .(assist, team)]
    
    # Rebounds    
  } else if (statType == "Rebounds") {
    nbaDatas <- DT[event_type == "rebound"]
    statDT <- nbaDatas[, .(Total=.N), by = .(player, team)]
    
    # Blocks
  } else if (statType == "Blocks") {
    nbaDatas <- copy(DT)[, team := ifelse(Home == team, Away, Home)]
    statDT <- nbaDatas[block != "", .(Total=.N), by = .(block, team)]
    
    # Steals
  } else if(statType == "Steals") {
    nbaDatas <- copy(DT)[, team := ifelse(Home == team, Away, Home)]
    statDT <- nbaDatas[steal != "", .(Total=.N), by = .(steal, team)]
    
  }
  
  # We rename columns
  colnames(statDT) <- c("player", "team", "Total")
  
  # We filter with selected players
  statFilteredDT <- merge(playersFilteredDT, statDT, by = "player", all.x = FALSE, all.y = FALSE)
  
  # We filter with minutes played
  statFilteredDT <- merge(statFilteredDT, DTgamesPlayed[, .(player, team, TotalGames, AvgMin)], by = c("player", "team"), all.x = TRUE, all.y = FALSE)
  
  # if a selected team is selected
  if (selectedTeam != "All") {
    statFilteredDT <- statFilteredDT[team == selectedTeam]
    statFilteredDT <- statFilteredDT[, statMean := round(Total/TotalGames, 2)]
    statFilteredDT <- statFilteredDT[, .SD, .SDcols = setdiff(colnames(statFilteredDT), "team")]
    
  } else {
    statFilteredDT <- statFilteredDT[, 
                                     .(Total=sum(Total), TotalGames = sum(TotalGames), AvgMin=(TotalGames * AvgMin / sum(TotalGames))), 
                                     by = names(playersFilteredDT)]
  
    statFilteredDT <- statFilteredDT[, 
                                     .(AvgMin=sum(AvgMin)), 
                                     by = setdiff(names(statFilteredDT), "AvgMin")]
    
    statFilteredDT <- statFilteredDT[, statMean := round(Total/TotalGames, 2)]
  }
  
  statFilteredDT <- statFilteredDT[order(-statMean)]
  statFilteredDT <- head(statFilteredDT, nbPlayer)
  
  return(statFilteredDT)
  
}

#-------------------------------------------------------------------------------
getStatCustomGraph <- function(statType, DTstats) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : data table where to look for the datas
  # - selectedTeam : select a team
  # - shootType : Either all, 2pt or 3pt shots or FT
  # - nbPlayer : Number of players displayed
  #-------------------------------------------------------------------------------
  # We download customed datas with preceding function
  statDT <- DTstats
  
  # Creation of a plotly graph
  fig <- plot_ly(data = statDT, type = "scatter",
                 mode = "markers",
                 x = ~AvgMin, y = ~statMean,
                 color = ~Salary,
                 colors = "Spectral",
                 marker = list(size = ~TotalGames, sizeref = 1, opacity = 0.6),
                 text = ~str_extract_all(string = get(x = 'player'), pattern = "(?<=([:blank:]))[A-z]+"),
                 hovertemplate = ~paste("<b>", player, " - ", "</b>", Position, 
                                        "</b><br>Games Played : <b>", TotalGames,
                                        "</b><br>Minutes Per Game : <b>", AvgMin,
                                        "</b><br>", statType, " Per Game : <b>", statMean,
                                        "<b><br><br>", Age, " </b>years old",
                                        "</b><br>Salary : <b>", round(Salary/1000000, 3), " M $",
                                        "<extra></extra>", sep = ""),
                 showlegend = FALSE)
  
  # We add text on top of each bubble
  fig <- add_text(p = fig,
                  textposition = "top")
  
  # Title and axis range
  fig <- layout(p = fig,
                title = "",
                xaxis = list(title = "Minutes PG",
                             zeroline = FALSE), 
                yaxis = list(title = paste(statType, "PG"),
                             zeroline = FALSE), 
                plot_bgcolor = "rgba(255, 255, 255, 0)",
                paper_bgcolor = "rgba(255, 255, 255, 0)")
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getTeamPoss <- function(selectedTeam, DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team to be selected
  # - DT : data table where to look for the datas
  #-------------------------------------------------------------------------------
  # We filter datas
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

#-------------------------------------------------------------------------------
getTeamStatsCustom <- function(statType = "Points", DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team to be selected
  # - DT : data table where to look for the datas
  #-------------------------------------------------------------------------------
  # We calculate total stats depending on stats selected
  if (statType == "Points") {
    teamDT <- DT[!is.na(points), .(Total = sum(points), Games = uniqueN(game_id)), by = team]
    teamDT <- teamDT[, Mean := round(Total / Games, 2)]
    teamDT <- teamDT[order(-Mean)]
    teamDT <- teamDT[, Rank := .I]
    
  } else if (statType == "Assists") {
    teamDT <- DT[assist != "", .(Total = .N, Games = uniqueN(game_id)), by = team]
    teamDT <- teamDT[, Mean := round(Total / Games, 2)]
    teamDT <- teamDT[order(-Mean)]
    teamDT <- teamDT[, Rank := .I]
    
  } else if (statType == "Rebounds") {
    teamDT <- DT[event_type == "rebound" & player != "", .(Total = .N, Games = uniqueN(game_id)), by = team]
    teamDT <- teamDT[, Mean := round(Total / Games, 2)]
    teamDT <- teamDT[order(-Mean)]
    teamDT <- teamDT[, Rank := .I]
    
  } else if (statType == "Turnovers") {
    teamDT <- DT[event_type == "turnover", .(Total = .N, Games = uniqueN(game_id)), by = team]
    teamDT <- teamDT[, Mean := round(Total / Games, 2)]
    teamDT <- teamDT[order(-Mean)]
    teamDT <- teamDT[, Rank := .I]
    
  } else if (statType == "Steals") {
    stealDT <- copy(DT)[, team := ifelse(team == Home, Away, Home)]
    teamDT <- stealDT[steal != "", .(Total = .N, Games = uniqueN(game_id)), by = team]
    teamDT <- teamDT[, Mean := round(Total / Games, 2)]
    teamDT <- teamDT[order(-Mean)]
    teamDT <- teamDT[, Rank := .I]
    
  } else if (statType == "Blocks") {
    blockDT <- copy(DT)[, team := ifelse(team == Home, Away, Home)]
    teamDT <- blockDT[block != "", .(Total = .N, Games = uniqueN(game_id)), by = team]
    teamDT <- teamDT[, Mean := round(Total / Games, 2)]
    teamDT <- teamDT[order(-Mean)]
    teamDT <- teamDT[, Rank := .I]
  }
  
  # We extract possessions per team
  teamDT <- teamDT[, teamPoss := sapply(X = teamDT$team, FUN = getTeamPoss, DT)]
  
  # We rank by possession
  teamDT <- teamDT[order(-teamPoss)]
  teamDT <- teamDT[, RankPoss := .I]
  
  return(teamDT[order(Rank)])
  
}

#-------------------------------------------------------------------------------
getTeamStatsCustomGraph <- function(statType = "Points", DT) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team to be selected
  # - DT : data table where to look for the datas
  #-------------------------------------------------------------------------------
  # We DL team stats for graph purposes
  statDT <- getTeamStatsCustom(statType, DT)
  
  # We add images path
  statDT <- statDT[, pathImg := paste("Logos/", team, "-min.png", sep = "")]
  
  # We use gg plot
  fig <- ggplot(data = statDT) +
    aes(x = teamPoss, y = Mean, image = pathImg) +
    geom_image(size = .05) +
    # coord_equal() +
    theme_bw(base_line_size = 0) +
    theme(axis.text = element_blank())
  
    return(fig)
  
}