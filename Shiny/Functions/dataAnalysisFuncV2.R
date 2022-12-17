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
getLogoImage <- function(shortName, path = "Logos/") {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  #-------------------------------------------------------------------------------
  pic <- load.image(paste(path, shortName, ".png", sep = ""))
  
  return(pic)
}

#-------------------------------------------------------------------------------
cleanDatas <- function(season, seasonType = "Regular Season", filePattern = "combined-stats", 
                       colDelete = c("date", "remaining_time", "play_length", "play_id", "away", "home", "num", 
                                     "opponent", "outof", "possession", "reason", "original_x", "original_y", "description"), 
					   saveDicoPlayers = FALSE,
                       path = getwd()) {
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
  nbaDT <- fread(file, sep = ",", dec = ".", quote = "'", header = TRUE, data.table = TRUE, drop = colDelete)
  
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
getPlayerDT <-  function(selectedTeam = NULL, selectedPlayer, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : team the player played for
  # - selectedPlayer : selectedPlayer
  # - DT : data tabke where to find datas (default)
  #-------------------------------------------------------------------------------
  # We get game id where selected player played for the team
  gamesIdPlayer <- getGamesId(selectedTeam, selectedPlayer, DT)
  
  playerDT <- DT[game_id %in% gamesIdPlayer]
  
  return(playerDT)
  
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
getNBAcalendar <- function(season, DT = nbaDatas, path = paste0(getwd(), "/AllGames")) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  # - team : a vector with teams names (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # we list files in the directory 
  filesName <- list.files(paste("AllGames/", season, sep = ""))
  
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
gePlayersFich <- function(season, seasonType, path = "Dictionary/", DT = dicoPlayerMinute) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : where there is the list of players and their respective teams (default)
  # - path : path for the position.salary per player file (default)
  #-------------------------------------------------------------------------------
  # We download the dictionary pos salary age per player template
  posSalPlayersDT <- fread(paste(path, season, "/", seasonType, "/playersSummary.csv", sep = ""))
  
  # sapply for those players
  teamsPlayers <- sapply(unique(DT$player), function(x) {
    paste(DT[player == x, team], collapse = "-")
  })
  
  # We get the data table for players with multiple teams 
  nbaPlayersDT <- data.table(player = attr(teamsPlayers, "names"), team = teamsPlayers)
  
  # We merge datas with positions and salaries to get informations for each players
  nbaPlayersDT <- merge(nbaPlayersDT, posSalPlayersDT, by.x = "player", by.y = "Players", all.x = TRUE, all.y = FALSE)
  
  return(nbaPlayersDT)  
  
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
getTeamStats <- function(selectedTeam, typeStat = "Points", DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : A team to be selected
  # - typeStat : statistics to analyze (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # Data table to translate Shiny variable name to datas variablee name
  translationDT <- data.table(Type = c("Points", "Assists", "Steals", "Blocks", "Turnovers", "Fouls", "Rebounds"), 
                              Variable = c("points", "assist", "steal", "block", "turnover", "foul", "rebound")
  )
  variableName <- translationDT[Type == typeStat, Variable]
  
  # Calculation of opponents teams stats, first a vector with all teams is created
  teams <- unique(DT[, team])
  
  ##############################################################################
  ################################### POINTS ###################################
  ##############################################################################
  if (typeStat == "Points") {
    
    # We filter generic datas
    nbaDT <- DT[!is.na(get(variableName))]
    
    # Calculation of total points made and games played per team
    teamsDT <- nbaDT[, .(Total = sum(get(variableName)), Games = uniqueN(game_id)), by = team][order(team)]
    
    # We create a list with DT for each team with total opponent teams stats
    listOppTeam <- lapply(X = teams, FUN = function(x) {
      tmpDT <- nbaDT[(Home == x | Away == x)]
      tmpDT[, Opponent := ifelse(team == x, "", team)]
      tmpDT <- tmpDT[Opponent != "", .(TotalOpp = sum(get(variableName)), GamesOpp = uniqueN(game_id)), by = .(Opponent)][order(Opponent)]
      
    }) # end lapply
    
    # We name the elements of the list list
    names(listOppTeam) <- teams
    
    # We sum variable Total and Games played
    oppTeamsDT <- rbindlist(l = listOppTeam, idcol = "team")
    oppTeamsDT <- oppTeamsDT[, .(TotalOpp=(sum(TotalOpp)), GamesOpp=(sum(GamesOpp))), by = team]
    
    ##############################################################################
    ################################## ASSISTS ###################################
    ##############################################################################
  } else if (typeStat == "Assists") {
    
    # We filter generic datas
    nbaDT <- DT[get(variableName) != ""]
    
    # Calculation of total assists made and games played per team
    teamsDT <- nbaDT[, .(Total = .N, Games = uniqueN(game_id)), by = .(team)][order(team)]
    
    # We create a list with DT for each team with totel opponent teams stats
    listOppTeam <- lapply(X = teams, FUN = function(x) {
      tmpDT <- nbaDT[(Home == x | Away == x)]
      tmpDT[, Opponent := ifelse(team == x, "", team)]
      tmpDT <- tmpDT[Opponent != "", .(TotalOpp = .N, GamesOpp = uniqueN(game_id)), by = .(Opponent)][order(Opponent)]
    })
    
    # We name the elements of the list list
    names(listOppTeam) <- teams
    
    # We sum variable Total and Games played
    oppTeamsDT <- rbindlist(l = listOppTeam, idcol = "team")
    oppTeamsDT <- oppTeamsDT[, .(TotalOpp=(sum(TotalOpp)), GamesOpp=(sum(GamesOpp))), by = team]
    
    ##############################################################################
    ################################# REBOUNDS ###################################
    ##############################################################################
  } else if (typeStat == "Rebounds") {
    
    # We filter generic datas
    nbaDT <- DT[event_type == variableName & player != ""]
    
    # Calculation of total assists made and games played per team
    teamsDT <- nbaDT[, .(Total = .N, Games = uniqueN(game_id)), by = .(team)][order(team)]
    
    # We create a list with DT for each team with total opponent teams stats
    listOppTeam <- lapply(X = teams, FUN = function(x) {
      tmpDT <- nbaDT[(Home == x | Away == x)]
      tmpDT[, Opponent := ifelse(team == x, "", team)]
      tmpDT <- tmpDT[Opponent != "", .(TotalOpp = .N, GamesOpp = uniqueN(game_id)), by = .(Opponent)][order(Opponent)]
    })
    
    # We name the elements of the list
    names(listOppTeam) <- teams
    
    # We sum variable Total and Games played
    oppTeamsDT <- rbindlist(l = listOppTeam, idcol = "team")
    oppTeamsDT <- oppTeamsDT[, .(TotalOpp=(sum(TotalOpp)), GamesOpp=(sum(GamesOpp))), by = team]
    
    ##############################################################################
    ################################ TO & FOULS ##################################
    ##############################################################################
  } else if (typeStat %in% c("Turnovers", "Fouls")) {
    
    # We filter generic datas
    nbaDT <- DT[event_type == variableName]
    
    # Calculation of total assists made and games played per team
    teamsDT <- nbaDT[, .(Total = .N, Games = uniqueN(game_id)), by = .(team)][order(team)]
    
    # We create a list with DT for each team with totel opponent teams stats
    listOppTeam <- lapply(X = teams, FUN = function(x) {
      tmpDT <- nbaDT[(Home == x | Away == x)]
      tmpDT[, Opponent := ifelse(team == x, "", team)]
      tmpDT <- tmpDT[Opponent != "", .(TotalOpp = .N, GamesOpp = uniqueN(game_id)), by = .(Opponent)][order(Opponent)]
    })
    
    # We name the elements of the list
    names(x = listOppTeam) <- teams
    
    # We sum variable Total and Games played
    oppTeamsDT <- rbindlist(l = listOppTeam, idcol = "team")
    oppTeamsDT <- oppTeamsDT[, .(TotalOpp=(sum(TotalOpp)), GamesOpp=(sum(GamesOpp))), by = team]
    
    ##############################################################################
    ############################### STLS & BLKS ################################## CONSTRUCTION !!!!!!!!
    ##############################################################################
  } else if (typeStat %in% c('Steals', 'Blocks')) {
    
    # We filter generic datas and transform team variable as steals and blocks concern other team for a player
    nbaDT <- DT[get(variableName) != ""]
    nbaDT[, team := ifelse(Home == team, Away, Home)]
    
    # Calculation of total assists made and games played per team
    teamsDT <- nbaDT[, .(Total = .N, Games = uniqueN(game_id)), by = .(team)][order(team)]
    
    # We create a list with DT for each team with totel opponent teams stats
    listOppTeam <- lapply(X = teams, FUN = function(x) {
      tmpDT <- nbaDT[(Home == x | Away == x)]
      tmpDT[, Opponent := ifelse(team == x, "", team)]
      tmpDT <- tmpDT[Opponent != "", .(TotalOpp = .N, GamesOpp = uniqueN(game_id)), by = .(Opponent)][order(Opponent)]
    })
    
    # We name the elements of the list
    names(x = listOppTeam) <- teams
    
    # We sum variable Total and Games played
    oppTeamsDT <- rbindlist(l = listOppTeam, idcol = "team")
    oppTeamsDT <- oppTeamsDT[, .(TotalOpp=(sum(TotalOpp)), GamesOpp=(sum(GamesOpp))), by = team]
    
  } # enf If by typeStat
  
  # We add the team mean per game of the specified stat
  teamsDT[, PerGame := round(Total / Games, 2)]
  
  if (typeStat %in% c("Turnovers", "Fouls")) {
    teamsDT <- teamsDT[order(PerGame)]
  } else {
    teamsDT <- teamsDT[order(-PerGame)]
  }
  
  teamsDT[, Rank := .I]
  
  # First we calculate the mean of the stat for all teams
  leagueMean <- round(mean(teamsDT[, PerGame]), 2)
  
  # Before we filter the data table to selected team stats
  teamsDT <- teamsDT[team == selectedTeam]
  
  # We add the opponent team mean per game of the specified stat
  oppTeamsDT[, PerGameOpp := round(TotalOpp / GamesOpp, 2)]
  
  if (typeStat %in% c("Turnovers", "Fouls")) {
    oppTeamsDT <- oppTeamsDT[order(-PerGameOpp)]
  } else {
    oppTeamsDT <- oppTeamsDT[order(PerGameOpp)]
  }
  
  oppTeamsDT <- oppTeamsDT[, RankOpp := .I]
  oppTeamsDT <- oppTeamsDT[team == selectedTeam]
  
  # We merge team stats and opponents team stats
  teamStatDT <- merge(teamsDT, oppTeamsDT, by = "team", all = TRUE)
  
  # List grouping league mean stat and team stat  
  listTeamStats <- list(leagueMean = leagueMean, 
                        teamStatDT = teamStatDT) 
  
  return(listTeamStats)
  
}

#-------------------------------------------------------------------------------
getTeamStatsGraph <- function(selectedTeam, typeStat = "Points", DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : Vector with a NBA team
  # - typeStat : statistics to analyze (default)
  #-------------------------------------------------------------------------------
  # We use the function getTeamStats to get Datas with a selected team
  listTeamStats <- getTeamStats(selectedTeam, typeStat, DT)
  
  # We create a DT with values regarding a specific team to be displayed on the graph
  teamStatsDT <- data.table(type = c("Team", "Mean", "Opponents"),
                            value = c(listTeamStats[["teamStatDT"]][, PerGame], 
                                      listTeamStats[["leagueMean"]], 
                                      listTeamStats[["teamStatDT"]][, PerGameOpp]),
                            rank = c(listTeamStats[["teamStatDT"]][, Rank], 
                                     "", 
                                     listTeamStats[["teamStatDT"]][, RankOpp]))
  
  # We create a factor to display the variables Team, Mean, Opponent by order
  teamStatsDT$type <- factor(teamStatsDT$type, 
                             c("Team", "Mean", "Opponents"))
  
  # We focus on the graph design with values from team DT
  fig <- plotly::plot_ly(data = teamStatsDT, type = "bar",
                         x = ~type,
                         y = ~value, 
                         texttemplate = ~paste("<b>", value, "</b>", paste(str_sub(typeStat, 1, 1), "PG", sep = "")),
                         hovertemplate = ~paste(ifelse(type == "Mean", "League Mean", paste("League Rank :", rank)), "<extra></extra>"),
                         marker = list(color = brewer.pal(9, "Reds")[c(5, 6, 7)], opacity = 0.6,
                                       line = list(width = 5, color = brewer.pal(9, "Reds")[c(5, 6, 7)], opacity = 1)),
                         showlegend = FALSE)
  
  # We customize the axes
  fig <- plotly::layout(p = fig, 
                        title = paste("<b>", selectedTeam, "</b>", "- Comparison of Team, Mean and Opponents", typeStat, "Per Game"),
                        yaxis = list(range = c(min(teamStatsDT$value)-1, max(teamStatsDT$value)+1),
                                     zeroline = TRUE,
                                     showline = FALSE,
                                     showticklabels = FALSE,
                                     showgrid = FALSE,
                                     title = ""),
                        xaxis = list(title = ""))
  
  # No plot bar top right
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getTeamStatsPerPlayer <- function(selectedTeam, typeStat = "Points", DTroster = dicoPlayerMinute, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : Vector of a team name
  # - typeStat : Type of Stat (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # Data table to translate Shiny variable name to datas variablee name
  translationDT <- data.table(Type = c("Points", "Assists", "Steals", "Blocks", "Turnovers", "Fouls", "Rebounds"), 
                              Variable = c("points", "assist", "steal", "block", "turnover", "foul", "rebound")
  )
  variableName <- translationDT[Type == typeStat, Variable]
  
  # Roster selection
  Roster <- getListRoster(team = selectedTeam, DTroster)[[selectedTeam]]
  
  # Total games for each player
  gamesPerPlayerDT <- data.table(player = Roster, 
                                 Games = sapply(Roster, function(x) {
                                   DTroster[player == x & team == selectedTeam, TotalGames]}))
  
  # Use of another function to get total stats scored by the team and split it among players
  listTeamStats <- getTeamStats(selectedTeam, typeStat, DT)
  teamStats <- listTeamStats[["teamStatDT"]][, Total]
  teamGames <- listTeamStats[["teamStatDT"]][, Games]
  
  # For each stat type we create a DT filtered on those stats
  if (typeStat == "Points") {
    statsPlayerDT <- DT[player %in% Roster & !is.na(points) & team == selectedTeam, 
                        .(Total = sum(get(variableName))), 
                        by = .(player)]
    
  } else if (typeStat == c("Assists")) {
    statsPlayerDT <- DT[get(variableName) %in% Roster & team == selectedTeam, 
                        .(Total = .N), 
                        by = .(get(variableName))]
    
    # We re-name player variable
    colnames(statsPlayerDT) <- c("player", "Total")
    
  } else if (typeStat %in% c("Blocks", "Steals")) {
    statsPlayerDT <- DT[get(variableName) %in% Roster & 
                          (Home == selectedTeam | Away == selectedTeam), 
                        .(Total = .N), 
                        by = .(get(variableName))]
    
    # We re-name player variable
    colnames(statsPlayerDT) <- c("player", "Total")
    
    # Rebounds, Turnovers or Fouls  
  } else if (typeStat %in% c("Rebounds", "Turnovers", "Fouls")) {
    
    statsPlayerDT <- DT[player %in% Roster & team == selectedTeam 
                        & event_type == variableName, 
                        .(Total = .N), 
                        by = .(player)]
    
  } # end of If loop by typeStat
  
  # Merge with number of games per player and we delete players with zero stats (NA)
  statsPlayerDT <- merge(statsPlayerDT, gamesPerPlayerDT, by = "player", all = TRUE)
  statsPlayerDT <- statsPlayerDT[!is.na(Total)]
  
  # Add of statistics per game and team contribution
  statsPlayerDT <- statsPlayerDT[, PerGame := round((Total / Games), 2)]
  statsPlayerDT <- statsPlayerDT[, TeamContribution := round((Total / teamStats * 100), 2)]
  
  # We treat cases where stats are not associated with players, for turnovers mainly
  if (sum(statsPlayerDT[, Total]) < teamStats) {
    
    missingStats <- teamStats - sum(statsPlayerDT[, Total])
    
    # Creation of a new data table for missing values
    missingStatsDT <- data.table(player = "Team",
                                 Total = missingStats,
                                 Games = teamGames)
    
    # Add of statistics per game and team contribution
    missingStatsDT <- missingStatsDT[, ':=' (PerGame = round(Total / Games, 2), 
                                             TeamContribution = round((Total / TotalDataTeam * 100), 2))]
    
    # We add missing stats to players stats
    statsPlayerDT <- rbind(statsPlayerDT, missingStatsDT)
    
  } # end of If missing stats loop
  
  statsPlayerDT <- statsPlayerDT[order(-PerGame)]
  return(statsPlayerDT)
  
}

#-------------------------------------------------------------------------------
getTeamStatsPerPlayerChart <- function(selectedTeam, typeStat = "Points", nbPlayers = 10, DTroster = dicoPlayerMinute, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : A vector of a team to select
  # - typeStat : Type of stats to be displayed (default)
  # - nbPlayers : An integer of the number of players to be displayed on the graph (default)
  #-------------------------------------------------------------------------------
  # We load datas with getTeamStatsPerPlayer function
  teamPlayersStatsDT <- getTeamStatsPerPlayer(selectedTeam, typeStat, DTroster, DT)
  
  # We re-name variables
  colnames(teamPlayersStatsDT) <- c("Player Name", 
                                    paste("Total", typeStat), 
                                    "Total Games", 
                                    paste(typeStat, "Per Game"), 
                                    "Team Contribution (%)") 
  
  # We calculate the number of players for graph purpose
  teamNbPlayers <- nrow(teamPlayersStatsDT)
  
  # We filter datas regarding to the number of players selected with nbPlayers variable
  teamPlayersStatsDT <- head(teamPlayersStatsDT, nbPlayers)
  
  # We create the chart pie
  fig <- plotly::plot_ly(data = teamPlayersStatsDT, type = "pie", 
                         values = ~get(paste("Total", typeStat), teamPlayersStatsDT),
                         labels = ~get("Player Name", teamPlayersStatsDT),
                         marker = list(colors = colorRampPalette(colors = brewer.pal(9, "Blues"))(min(nbPlayers, teamNbPlayers))),
                         texttemplate = ~paste('<b>', str_extract(get("Player Name", teamPlayersStatsDT), "(?<=[:space:])[A-z]+"), 
                                               '<br>', get("Team Contribution (%)", teamPlayersStatsDT), '%'),
                         hovertemplate = ~paste('<b>', get("Player Name", teamPlayersStatsDT), 
                                                '<br></b>', paste("Total", typeStat, ": <b>"), get(paste("Total", typeStat), teamPlayersStatsDT), 
                                                '<br></b>', paste(typeStat, "Per Game: <b>"), get(paste(typeStat, "Per Game"), teamPlayersStatsDT), 
                                                '<br></b>Total Games: <b>', get("Total Games", teamPlayersStatsDT), 
                                                '<extra></extra>'),
                         hole = 0.3, 
                         showlegend = FALSE)
  
  # Chart layout
  fig <- plotly::layout(p = fig,
                        title = list(text = paste("<b>", selectedTeam, "-", "</b>Distribution of Total", typeStat),
                                     yanchor = "bottom",
                                     xanchor = "middle"))
  
  # No display bar
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getBestGames <- function(selectedTeam, nbGames = 10, typeStat = "Points", DTroster = dicoPlayerMinute, DT = nbaDatas) {
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
getLeaguePlayerStats <- function(selectedTeam, selectedPlayer, DTgamesPlayed = dicoPlayerMinute, DT = nbaDatas) { 
  #-------------------------------------------------------------------------------
  # @ variables :
  # - minTotGames : a number of minimum game to rank players (default)
  # - DT : data table where to look for the datas (default)
  #-------------------------------------------------------------------------------
  ########
  # GAMES
  ########
  
  # We select games played by the player 
  gamesDT <- getPlayerDT(selectedTeam, selectedPlayer, nbaDatas)
  
  # We find the number of games for each player
  timeDT <- DTgamesPlayed[player == selectedPlayer & team == selectedTeam, .(TotalGames, TotMin)]
  
  ########
  # POINTS
  ########
  
  # We filter datas to get points
  totPointsDT <- gamesDT[points %in% c(0, 1, 2, 3)]
  totPlayerPoints <- sum(totPointsDT[player == selectedPlayer, points])
  totTeamPoints <- sum(totPointsDT[team == selectedTeam, points])
  
  ########
  # FG
  ########
  
  # We also save FGM and FGA
  totPlayerFGA <- nrow(totPointsDT[!(type %like% "free throw") & points %in% c(0, 2, 3) & player == selectedPlayer])
  totTeamFGA <- nrow(totPointsDT[team == selectedTeam & !(type %like% "free throw") & points %in% c(0, 2, 3)])
  
  totPlayerFGM <- nrow(totPointsDT[points %in% c(2, 3) & player == selectedPlayer])
  totTeamFGM <- nrow(totPointsDT[team == selectedTeam & points %in% c(2, 3)])
  
  totPlayer3PM <- nrow(totPointsDT[points == 3 & player == selectedPlayer])
  totTeam3PM <- nrow(totPointsDT[points == 3 & team == selectedTeam])
  
  totPlayerFTM <- nrow(totPointsDT[points == 1 & player == selectedPlayer])
  totTeamFTM <- nrow(totPointsDT[team == selectedTeam & points == 1])
  
  totPlayerFTA <- nrow(totPointsDT[type %like% "free throw" & player == selectedPlayer])
  totTeamFTA <- nrow(totPointsDT[team == selectedTeam & type %like% "free throw"])
  
  ########
  # TIME
  ########
  
  # Minutes played by player
  totPlayerMin <- timeDT[, TotMin]
  
  totTeamMinDT <- gamesDT[, .(PeriodMax = max(period)), by = game_id]
  totTeamMin <- nrow(totTeamMinDT[PeriodMax == 4]) * 240 + nrow(totTeamMinDT[PeriodMax == 5]) * 265 + nrow(totTeamMinDT[PeriodMax == 6]) * 290
  
  ##########
  # ASSISTS
  ##########
  
  # We filter datas to get assists
  assistsDT <- gamesDT[assist != ""]
  
  # We gather total assists per player
  totPlayerAssist <- nrow(assistsDT[assist == selectedPlayer])
  totTeamAssist <- nrow(assistsDT[team == selectedTeam])
  
  ##########
  # REBOUNDS
  ##########
  
  # We filter datas to get rebounds
  reboundsPlayer <- nrow(gamesDT[event_type == "rebound" & player == selectedPlayer])
  reboundsOffPlayer <- nrow(gamesDT[type == "rebound offensive" & player == selectedPlayer])
  
  reboundsOffTeam <- nrow(gamesDT[type == "rebound offensive" & team == selectedTeam & player != ""])
  reboundsDefOpp <- nrow(gamesDT[type == "rebound defensive" & team != selectedTeam & player != ""])
  
  ##########
  # TURNOVER
  ##########
  
  # We calculate total team turnover
  totToTeam <- nrow(gamesDT[event_type == "turnover" & team == selectedTeam])
  totToPlayer <- nrow(gamesDT[event_type == "turnover" & player == selectedPlayer])
  
  ##########
  # STEALS
  ##########

  # We filter datas to get steals
  stealsDT <- DT[steal != "", .(game_id, Home, Away, team, steal)]

  # We change team name by the other
  stealsDT <- stealsDT[, team := ifelse(team == Home, Away, Home)]

  # We gather total steals per Player
  stealsDT <- stealsDT[, .(TotalSteals = .N), by = c("steal", "team")]

  ##########
  # BLOCKS
  ##########

  # We filter datas to get blocks
  blocksDT <- DT[block != "", .(game_id, Home, Away, team, block)]

  # We change team name by the other
  blocksDT <- blocksDT[, team := ifelse(team == Home, Away, Home)]

  # We gather total steals per Player
  blocksDT <- blocksDT[, .(TotalBlocks = .N), by = c("block", "team")]
  
}  

#-------------------------------------------------------------------------------
getPlayerStatsChart <- function(selectedTeam, selectedPlayer, minTotGames = 20, DTgamesPlayed = dicoPlayerMinute, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # On vérifie que le nombre de match min est respecté
  leagueStatsList <- getLeaguePlayerStats(minTotGames, DTgamesPlayed, DT)
  gamesPlayersDT <- leagueStatsList[["nbGamesDT_svg"]]
  
  if (gamesPlayersDT[player == selectedPlayer & team == selectedTeam, TotalGames] >= minTotGames) {
    
    # We download league players stats
    leagueStatsDT <- leagueStatsList[["playerStatsDT"]]
    nbLeaguePlayer <- nrow(leagueStatsDT)
    
    # We filter with selected team and selected player
    playerStatsDT <- leagueStatsDT[player == selectedPlayer & team == selectedTeam]
    
    # Vector to be displayed
    chartDT <- data.table(Stats = c("Points", "Rebounds", "Assists", "Steals", "Blocks"),
                          PerGame = c(playerStatsDT[, PPG], 
                                      playerStatsDT[, RPG], 
                                      playerStatsDT[, APG], 
                                      playerStatsDT[, SPG], 
                                      playerStatsDT[, BPG]),
                          Rank = c(playerStatsDT[, RankPoints], 
                                   playerStatsDT[, RankRebounds], 
                                   playerStatsDT[, RankAssists], 
                                   playerStatsDT[, RankSteals], 
                                   playerStatsDT[, RankBlocks]),
                          Top = c(round(1 - playerStatsDT[, RankPoints] / nrow(leagueStatsDT), 2), 
                                  round(1 - playerStatsDT[, RankRebounds] / nrow(leagueStatsDT), 2), 
                                  round(1 - playerStatsDT[, RankAssists] / nrow(leagueStatsDT), 2), 
                                  round(1 - playerStatsDT[, RankSteals] / nrow(leagueStatsDT), 2),
                                  round(1 - playerStatsDT[, RankBlocks] / nrow(leagueStatsDT), 2)))
    
    # We plot the spider chart
    fig <- plot_ly(data = chartDT, type = "scatterpolar", mode = "markers", 
                   r = ~Top, theta = ~Stats,
                   hovertemplate = ~paste(Stats, "Per Game :", PerGame, 
                                          "<br>League Rank :", paste(Rank, "/", nbLeaguePlayer, sep = ""), 
                                          "<extra></extra>"),
                   fill = 'toself')
    
    # title and axes format
    fig <- layout(p = fig, 
                  title = paste("Games Played :", playerStatsDT[, TotalGames]),
                  polar = list(radialaxis = list(visible = FALSE, range = c(0,1))), 
                  showlegend = FALSE)
    
    fig <- plotly::config(p = fig, displayModeBar = FALSE)
    
    # Final spider chart
    return(fig)
    
  } else {
    # We create the data table to be displayed in a chart bart
    chartDT <- data.table(Games = c("Played", "Required"), 
                          nbGames = c(gamesPlayersDT[player == selectedPlayer & team == selectedTeam, TotalGames], 
                                      minTotGames))
    
    fig <- plot_ly(data = chartDT, type = "bar", 
                   x = ~Games, y = ~nbGames, 
                   marker = list(color = c("rgba(51, 132, 255, 0.5)", "rgba(255, 99, 71, 0.5)")))
    
    fig <- layout(p = fig, 
                  title = paste("<b>WARNING</b> : Not enough games played for ranking"), 
                  yaxis = list(zeroline = TRUE,
                               showline = FALSE,
                               showticklabels = FALSE,
                               showgrid = FALSE,
                               title = ""))
    # No plot bar top right
    fig <- plotly::config(p = fig, displayModeBar = FALSE)
    
    # Final bart chart
    return(fig)
    
  }
  
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
  playerPointsDT <- DTplayer[!is.na(points) & team == selectedTeam & player == selectedPlayer, .(Total = sum(points)), by = .(period, player)]
  
  # creation of a new variable indicating if the player is a starter at each event or not
  periodPlayedDT <- DTplayer[(onFloorHome %like% selectedPlayer|onFloorAway %like% selectedPlayer), .(game_id, period, Home, onFloorHome, Away, onFloorAway)]
  
  # We identify the team the player played for 
  periodPlayedDT <- periodPlayedDT[, team := ifelse(grepl(selectedPlayer, onFloorHome), Home, Away)]
  
  # We only conserve datas related to selectedTeam, not old team or new team
  periodPlayedDT <- periodPlayedDT[team == selectedTeam, .(game_id, period)]
  
  # We group datas per total game played
  periodPlayedDT <- periodPlayedDT[, .(TotalPlayed = uniqueN(game_id)), by = .(period)]
  
  # We merge datas with total points per period and number of periods played
  pointsPerPeriodDT <- merge(playerPointsDT, periodPlayedDT, 
                             by = "period", all = TRUE)
  
  # We add the period
  pointsPerPeriodDT <- pointsPerPeriodDT[, period := ifelse(period <= 4, 
                                                            paste("Q", period, sep = ""), 
                                                            paste("OT", period - 4, sep = ""))]
  
  # We create the mean points per period for a specific player
  pointsPerPeriodDT <- pointsPerPeriodDT[, PointsPerPeriod := round(Total / TotalPlayed, 2)]
  
  
  return(pointsPerPeriodDT)
  
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
  playerTimeDT[, minPerPeriodMean := round(floor(minPerPeriodMean) + (minPerPeriodMean - floor(minPerPeriodMean)) * 60 / 100, 2)]
  
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
  
  # Customization of chart colors
  nbBarsChart <- seq(from = 0.2, to = 1, length.out = nrow(playerPtsMinDT))
  colorCustomized <- sapply(nbBarsChart, FUN = function(x) {paste("rgba(255, 99, 71, ", x, ")", sep = "")})
  colorCustomized2 <- sapply(nbBarsChart, FUN = function(x) {paste("rgba(68, 149, 191, ", x, ")", sep = "")})
  
  # Creation of the chart
  fig <- plotly::plot_ly(data = playerPtsMinDT, 
                         x = ~period, 
                         y = ~PointsPerPeriod, type = "bar", 
                         marker = list(color = "rgba(255, 99, 71, 0.5)"),
                         texttemplate = ~paste("<b>", PointsPerPeriod, "</b><br>Points"),
                         hovertemplate = ~paste("<b>Number of", period, "played:</b>", totalPeriodPlayed, "<extra></extra>"),
                         showlegend = FALSE)
  
  fig <- plotly::add_bars(p = fig, data = playerPtsMinDT,
                          x = ~period,
                          y = ~minPerPeriodMean,
                          marker = list(color = "rgba(51, 132, 255, 0.5)"),
                          texttemplate = ~paste("<b>", minPerPeriodMean, "</b><br>Mins"),
                          hovertemplate = ~paste("<b>Number of", period, "played:</b>", totalPeriodPlayed, "<extra></extra>"),
                          showlegend = FALSE)
  
  fig <- layout(p = fig, 
                title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "Average Points and Minutes per period"),
                yaxis = list(zeroline = TRUE,
                             showline = FALSE,
                             showticklabels = FALSE,
                             showgrid = TRUE,
                             title = ""), 
                xaxis = list(title = ""))
  
  # No plot bar top right
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getPlayerStatsPerDate <- function(selectedTeam, selectedPlayer, perMonthWeek = "Month", date1 = NULL, date2 = NULL, DTcalendar = NBAcalendar, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : a team to select
  # - selectedPlayer : a player to select from selected team
  # - perMonthWeek : Results by Month or Week (default)
  # - date1 : Minimum date (null by default, takes all current season)
  # - date2 : Maximum date (null by default, takes all current season)
  # - DT : a data table where datas are extracted (default)
  #-------------------------------------------------------------------------------
  ############
  ##### TEAM # Results by team selected if selectedPlayer
  ############
  
  winsLooseTeamDT <- DTcalendar[Date >= max(min(Date), date1) & Date <= min(max(Date), date2) & 
                                  (grepl(selectedTeam, Winner)|grepl(selectedTeam, Loser))]
  
  # We add variable to group stats per ponth or week
  if (perMonthWeek == "Month") {
    winsLooseTeamDT[, MonthOrWeek := lubridate::month(Date)]
  } else if (perMonthWeek == "Week") {
    winsLooseTeamDT[, MonthOrWeek := lubridate::week(Date)]
  }
  
  # We aggregate wins and loses by selected team and period, if no wins then we force value to zero
  winsTeamDT <- winsLooseTeamDT[Winner == selectedTeam, .(TotalWins = .N), by = MonthOrWeek]
  if (nrow(winsTeamDT) == 0) {
    winsTeamDT <- data.table(MonthOrWeek = unique(winsLooseTeamDT$MonthOrWeek),
                             TotalWins = 0)
    # end if
  }
  
  # Same thing for loses
  lossesTeamDT <- winsLooseTeamDT[Loser == selectedTeam, .(TotalLosses = .N), by = MonthOrWeek]
  if (nrow(lossesTeamDT) == 0) {
    lossesTeamDT <- data.table(MonthOrWeek = unique(winsLooseTeamDT$MonthOrWeek),
                               TotalLosses = 0)
    # end if
  }
  
  # We merge wins and losses
  winsLooseTeamDT <- merge(winsTeamDT, lossesTeamDT, by = c("MonthOrWeek"), all = TRUE)
  
  # We force NA values to 0
  winsLooseTeamDT[is.na(winsLooseTeamDT)] <- 0
  
  #############################
  ##### PLAYER/LEAGUE LEADERS #
  #############################
  
  # We filter generic datas used for all stats
  nbaDatasDT <- DTplayer[Date >= max(min(Date), date1) & Date <= min(max(Date), date2)]
  
  # if no games we return an empty data.table
  if (nrow(nbaDatasDT) == 0) {
    perMonthWeek <- "Week"
    weekVector <- unique(lubridate::week(seq(from = date1, to = date2, by = 1)))
    return(data.table(MonthOrWeek = weekVector, TotalWins = 0, TotalLosses = 0))
    # end if
  }
  
  # We add variable to group stats per month or week
  if (perMonthWeek == "Month") {
    nbaDatasDT[, MonthOrWeek := lubridate::month(Date)] 
  } else if (perMonthWeek == "Week") {
    nbaDatasDT[, MonthOrWeek := lubridate::week(Date)]
  }
  
  # GAMES - 1
  nbGamesDT <- nbaDatasDT[, .(TotalGames = uniqueN(game_id)), by = MonthOrWeek]
  
  # If no games played by player then we only display wins and losses
  if (nrow(nbGamesDT) == 0) {
    return(winsLooseTeamDT)
  }
  
  # POINTS - 2
  playerPointsDT <- nbaDatasDT[player == selectedPlayer & points %in% c(0, 1, 2, 3), 
                               .(Date, MonthOrWeek, player, points)]
  playerPointsDT <- playerPointsDT[, .(TotalPts = sum(points)), by = c("player", "MonthOrWeek")]
  
  # FIELD GOAL - 3
  # We filter on shooting datas and create a variable to include 2pts Shots or 2pts Shots types
  fieldGoalDT <- nbaDatasDT[result %in% c("made", "missed") & player == selectedPlayer, 
                            .(MonthOrWeek, player, type, result)]
  fieldGoalDT[, type := ifelse(grepl("3pt", type), "3pt Shots", 
                               ifelse(grepl("free throw", type), 
                                      "Free Throws", "2pt Shots"))]
  
  # We aggregate per type of shots and dcast for better results
  fieldGoalDT <- fieldGoalDT[, .(Total = .N), by = c("player", "MonthOrWeek", "type", "result")]
  fieldGoalDT <- dcast(fieldGoalDT, formula =  player + MonthOrWeek ~ type + result, value.var = "Total", fill = 0)
  
  # We check if all shoot type and results are tgere, if not, we create and force value to zero
  missingTypeShots <- setdiff(c("player", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed"), 
                              colnames(fieldGoalDT))
  if (length(missingTypeShots) > 0) {  
    for (i in seq(1, length(missingTypeShots))) {
      fieldGoalDT <- fieldGoalDT[, missingTypeShots[i] := 0]
    }
  }
  
  # We calculate the % of FG
  fieldGoalDT[, ":=" (FG2pts = round(get("2pt Shots_made") / (get("2pt Shots_missed") + get("2pt Shots_made"))  * 100, 2), 
                      FG3pts = round(get("3pt Shots_made") / (get("3pt Shots_missed") + get("3pt Shots_made"))  * 100, 2),
                      FGft = round(get("Free Throws_made") / (get("Free Throws_missed") + get("Free Throws_made"))  * 100, 2))]
  
  # If no FG, then we force to zero
  fieldGoalDT[is.na(fieldGoalDT)] <- 0
  fieldGoalDT <- fieldGoalDT[, .(player, MonthOrWeek, FG2pts, FG3pts, FGft)]
  
  
  # ASSISTS - 4
  assistsPlayerDT <- nbaDatasDT[assist == selectedPlayer, .(Date, MonthOrWeek, assist, team)]
  assistsPlayerDT <- assistsPlayerDT[, .(TotalAss = .N), by = c("assist", "MonthOrWeek")]
  
  # REBOUND - 5
  reboundsPlayerDT <- nbaDatasDT[event_type == "rebound" & player == selectedPlayer, .(Date, MonthOrWeek, player, team)]
  reboundsPlayerDT <- reboundsPlayerDT[, .(TotalReb = .N), by = c("player", "MonthOrWeek")]
  
  # We aggregate all objects
  statsPlayerPeriodDT <- merge(playerPointsDT, fieldGoalDT, 
                               by = c("player", "MonthOrWeek"), all = TRUE)
  statsPlayerPeriodDT <- merge(statsPlayerPeriodDT, assistsPlayerDT, 
                               by.x = c("player", "MonthOrWeek"), by.y = c("assist", "MonthOrWeek"), all = TRUE)
  statsPlayerPeriodDT <- merge(statsPlayerPeriodDT, reboundsPlayerDT, 
                               by = c("player", "MonthOrWeek"), all = TRUE)
  statsPlayerPeriodDT <- merge(winsLooseTeamDT, statsPlayerPeriodDT, 
                               by = c("MonthOrWeek"), all = TRUE)
  
  # We add number of games to finally get per game results
  statsPlayerPeriodDT <- merge(statsPlayerPeriodDT, nbGamesDT, 
                               by = c("MonthOrWeek"), all = TRUE)
  
  # New variables to get mean
  statsPlayerPeriodDT[, PPG := round(TotalPts / TotalGames, 2)]
  statsPlayerPeriodDT[, APG := round(TotalAss / TotalGames, 2)]
  statsPlayerPeriodDT[, RPG := round(TotalReb / TotalGames, 2)]
  
  return(statsPlayerPeriodDT)
  
}

#-------------------------------------------------------------------------------
getPlayerStatsPerDateGraph <- function(selectedTeam, selectedPlayer, perMonthWeek = "Month", date1 = NULL, date2 = NULL, DTcalendar = NBAcalendar, DTplayer) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : a team to select
  # - selectedPlayer : a player to select from selected team
  # - perMonthWeek : Results by Month or Week (default)
  # - date1 : Minimum date (null by default, takes all current season)
  # - date2 : Maximum date (null by default, takes all current season)
  # - DT : a data table where datas are extracted (default)
  #-------------------------------------------------------------------------------
  # We donwnload stats for a specific player
  playerStatsDT <- getPlayerStatsPerDate(selectedTeam, selectedPlayer, perMonthWeek, date1, date2, DTcalendar, DTplayer)
  
  if (perMonthWeek == "Month") {
    
    # Conversion DT for factoring period
    convertDT <- data.table(numMonth = seq(1, 12, 1), 
                            nameMonth = c("January", "Februray", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
    
    # We translate number to Month name
    playerStatsDT[, MonthOrWeek := sapply(X = MonthOrWeek, FUN = function(x) {
      convertDT[numMonth == x, nameMonth]
    })]
    
    # We do a factorization to respect time order of the season
    playerStatsDT$MonthOrWeek <- factor(playerStatsDT$MonthOrWeek, 
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
    
    # We do a factorization to respect time order of the season
    playerStatsDT$MonthOrWeek <- factor(playerStatsDT$MonthOrWeek, 
                                        levels = c(paste("W-", seq(35, 52, 1), sep = ""), paste("W-", seq(1, 34, 1), sep = "")))
    # end if  
  }
  
  # 1st bar chart : wins
  fig <- plot_ly(data = playerStatsDT, type = "bar", 
                 x = ~MonthOrWeek, y = ~TotalWins,
                 marker = list(color = "rgba(255, 99, 71, 0.5)"),
                 texttemplate = ~paste("<b>", TotalWins, "W"),
                 hoverinfo = "none",
                 showlegend = FALSE)
  
  # 2nd bar chart : losses
  fig <- add_trace(p = fig, type = "bar", 
                   y = ~TotalLosses,
                   texttemplate = ~paste("<b>", TotalLosses, "L"),
                   marker = list(color = "rgba(51, 132, 255, 0.5)"),
                   hoverinfo = "none",
                   showlegend = FALSE)
  
  # 3rd marks : PPG plus other stats (only available if player played at least 1 game betwwen date1 and date 2)
  hasPlayerPlayed <- ifelse(ncol(playerStatsDT) == 3, FALSE, TRUE)
  
  if (hasPlayerPlayed == TRUE) {
    fig <- add_trace(p = fig, data = playerStatsDT, type = "scatter", mode = "markers",
                     x = ~MonthOrWeek, y = ~PPG,
                     yaxis = "y2",
                     marker = list(color = "rgba(151, 44, 92, 0.8)", size = 10),
                     hovertemplate = ~paste(TotalGames, "games",
                                            "<br>PPG :", PPG,
                                            "<br>APG :", APG,
                                            "<br>RPG :", RPG,
                                            "<br><br>2pts FG :", FG2pts, "%",
                                            "<br>3pts FG :", FG3pts, "%",
                                            "<br>FT FG :", FGft, "%",
                                            "<extra></extra>"), 
                     showlegend = FALSE)
    
    # Title and disposition
    fig <- layout(p = fig, data = playerStatsDT,
                  title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "Stats per", perMonthWeek),
                  xaxis = list(title=""),
                  yaxis2 = list(overlaying = "y",
                                side = "right",
                                range = ~c(0, max(PPG)+1),
                                zeroline = TRUE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE),
                  yaxis = list(title = "",
                               zeroline = TRUE,
                               showline = FALSE,
                               showticklabels = FALSE,
                               showgrid = TRUE),
                  barmode = "group")
    # end if
  } else if (hasPlayerPlayed == FALSE) {
    # Title and disposition
    fig <- layout(p = fig, data = playerStatsDT,
                  title = paste("<b>", str_extract_all(selectedPlayer, "(?<=[:space:]).*"), "</b>-", "</b>Did Not Play"),
                  xaxis = list(title=""),
                  yaxis = list(title = "Team Wins/Losses",
                               zeroline = TRUE,
                               showline = TRUE,
                               showticklabels = TRUE,
                               showgrid = TRUE),
                  barmode = "group")
    # enf else if  
  }
  
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
  # We filter original datas based on id games 
  nbaDatasDT <- DTplayer
  
  ###############
  # 1/ FIELD GOAL
  ###############
  
  # Filtering and variables selection
  shootingDT <- nbaDatasDT[event_type == "shot", 
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
  pointsDT <- nbaDatasDT[event_type %in% c("shot", "free throw") & result == "made" & points %in% c(1, 2, 3), 
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

#-------------------------------------------------------------------------------
getPlayerImpactGraph <- function(selectedTeam, selectedPlayer, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedTeam : a team to select
  # - selectedPlayer : a player from the selected team
  # - DT : a data table where datas are extracted (default)
  #-------------------------------------------------------------------------------
  # We download impact datas
  playerImpactDT <- getPlayerImpact(selectedTeam, selectedPlayer, DT)
  
  # We treat shooting datas to display on a 1st graph
  shootingImpactDT <- playerImpactDT[["ShootImpact"]]
  
  # We treat points datas to display on a 2nd graph
  pointImpactDT <- playerImpactDT[["PointImpact"]]
  
  # We factor to get a correct order
  pointImpactDT$Floor <- c("With Player", "Without Player")
  pointImpactDT$Floor <- factor(x = pointImpactDT$Floor, levels = c("With Player", "Without Player"))
  
  # We keep nb games to include in a title
  nbGamesPlayed <- unique(pointImpactDT[, nbGames])
  
  # Impact on Shooting datas
  fig <- plot_ly(data = shootingImpactDT, type = "bar", name = "Team FG with Player", 
                 x = ~typeSimplified, y = ~Team_onFloor, 
                 marker = list(color = "rgba(255, 99, 71, 1)"),
                 hovertemplate = ~paste(Team_onFloor, "%", "<extra></extra>"))
  
  fig <- add_bars(p = fig, name = "Team FG without Player", 
                  y = ~Team_offFloor, 
                  marker = list(color = "rgba(255, 99, 71, 0.7)"),
                  hovertemplate = ~paste(Team_offFloor, "%", "<extra></extra>"))
  
  fig <- add_bars(p = fig, name = "Opp Team FG with Player", 
                  y = ~Opponent_onFloor, 
                  marker = list(color = "rgba(71, 99, 255, 1)"),
                  hovertemplate = ~paste(Opponent_onFloor, "%", "<extra></extra>"))
  
  fig <- add_bars(p = fig, name = "Opp Team FG without Player", 
                  y = ~Opponent_offFloor, 
                  marker = list(color = "rgba(71, 99, 255, 0.7)"),
                  hovertemplate = ~paste(Opponent_offFloor, "%", "<extra></extra>"))
  
  fig <- layout(p = fig, 
                title = paste("<b>", selectedPlayer, "-", selectedTeam, "<br></b>: FG & +/- Impacts on", nbGamesPlayed, "games played"),
                barmode = "group", 
                legend = list(x = 100, y = 0.7))
  
  # Impact on points graph
  fig2 <- plot_ly(data = pointImpactDT, type = "bar", 
                  x = ~Floor, y = ~MeanPlusMinus, 
                  marker = list(color = c("rgba(180, 190, 95, 0.7)", "rgba(180, 107, 95, 0.7)")),
                  hovertemplate = ~paste("+/- :", MeanPlusMinus, "pts", "<extra></extra>"), 
                  showlegend = FALSE)
  
  fig2 <- layout(p = fig2,
                 yaxis = list(title = ""),
                 xaxis = list(title = ""))
  
  # We combine the two preceding graphs
  fig3 <- subplot(fig, fig2, nrows = 2, heights = c(0.7, 0.3), margin = 0.05)
  
  return(fig3)
  
}

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
getPlayerGlobalShooting <- function(selectedPlayer, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We download datas
  shootingDT <- DT[player == selectedPlayer & event_type %in% c("free throw", "shot"), .(player, points, type, result)]
  
  # We isolate total points of the player
  totPoints <- sum(shootingDT[, "points"])
  
  # We create categories
  shootingDT <- shootingDT[, typeShot := ifelse(test = grepl('3pt', type), yes = '3pt Shots', 
                                                no = ifelse(grepl("free throw", type), "Free Throws", "2pt Shots"))]
  
  # We aggregate nb of shots by result and type of shot
  shootingDT <- shootingDT[, .(Total = .N), by = c("player", "typeShot", "result")]
  
  # dcast to provide results by columns
  shootingDT <- dcast(data = shootingDT, formula = player ~ typeShot + result, value.var = "Total")
  
  # We check if all shoot type and results are tgere, if not, we create and force value to zero
  missingTypeShots <- setdiff(c("player", "2pt Shots_made", "2pt Shots_missed", "3pt Shots_made", "3pt Shots_missed", "Free Throws_made", "Free Throws_missed"), 
                              colnames(shootingDT))
  
  if (length(missingTypeShots) > 0) {  
    for (i in seq(1, length(missingTypeShots))) {
      shootingDT <- shootingDT[, missingTypeShots[i] := 0]
    }
  }
  
  # We create the variables FG as percent
  shootingDT <- shootingDT[, ":=" (twoFG = round(100 * get("2pt Shots_made") / (get("2pt Shots_made") + get("2pt Shots_missed")), 2),
                                   threeFG = round(100 * get("3pt Shots_made") / (get("3pt Shots_made") + get("3pt Shots_missed")), 2),
                                   FtFG = round(100 * get("Free Throws_made") / (get("Free Throws_made") + get("Free Throws_missed")), 2))]
  
  
  # Creation of the variable efficient field goal
  shootingDT <- shootingDT[, eFG := round(100 * (get("2pt Shots_made") + get("3pt Shots_made") + 0.5 * get("3pt Shots_made")) / (get("2pt Shots_made") + get("3pt Shots_made") + 
                                                                                                                                   get("2pt Shots_missed") + get("3pt Shots_missed")), 2)]
  
  # Creation of the variable true shooting per cent
  shootingDT <- shootingDT[, tFG := round(100 * totPoints / (2 * (get("2pt Shots_made") + get("2pt Shots_missed") + get("3pt Shots_made") + get("3pt Shots_missed") + 
                                                                    0.44*get("Free Throws_made") + 0.44*get("Free Throws_missed"))), 2)]
  
  # we filter datas
  shootingFilteredDT <- shootingDT[, .(twoFG, threeFG, FtFG, eFG, tFG)]
  
  shootingFinalDT <- data.table(Type = c("2pts Shots", "3pts Shots", "Free Throws", "Effective Shots", "True Shooting"), 
                                FieldGoal = c(shootingFilteredDT$twoFG, shootingFilteredDT$threeFG, shootingFilteredDT$FtFG, shootingFilteredDT$eFG, shootingFilteredDT$tFG))
  
  return(shootingFinalDT)
  
}

#-------------------------------------------------------------------------------
getPlayerGlobalShootingGraph <- function(selectedPlayer, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We dl datas
  shootingDT <- getPlayerGlobalShooting(selectedPlayer, DT)
  
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
                             showgrid = FALSE))
  
  fig <- config(p = fig, displayModeBar = FALSE)
  
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getShotsPlayerCoordinates <- function(selectedPlayer, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - DT : where to get datas (default)
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
  
  
  # We save shooting coord for a specific player
  playerShootingCoordDT <- shootingDT[player == selectedPlayer, .(result, shot_distance, converted_x_corrected, converted_y_corrected)]
  
  return(playerShootingCoordDT)
  
}

#-------------------------------------------------------------------------------
getShotsPlayerVsLeague <- function(selectedPlayer, distLongShot = 27, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - distLongShot : distance in feet after which a shot is considered a long shot (distance)
  # - DT : where to get datas (default)
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
  
  # We calculate mean league per area
  nbShotsMadeLeagueDT <- shootingDT[result == "made", .(TotalLeagueMade = .N), by = area]
  nbShotsMissedLeagueDT <- shootingDT[result == "missed", .(TotalLeagueMissed = .N), by = area]
  
  # We gather the two preceding data table to calculate mean
  fgShotsLeagueDT <- merge(nbShotsMadeLeagueDT, nbShotsMissedLeagueDT, by = "area", all = TRUE)
  fgShotsLeagueDT <- fgShotsLeagueDT[, LeagueFG := round(100 * TotalLeagueMade / (TotalLeagueMade + TotalLeagueMissed), 2)]
  
  # We calculate mean league per selected player
  nbShotsMadePlayerDT <- shootingDT[player == selectedPlayer & result == "made", .(TotalPlayerMade = .N), by = area]
  nbShotsMissedPlayerDT <- shootingDT[player == selectedPlayer & result == "missed", .(TotalPlayerMissed = .N), by = area]
  
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
getAssistsShotsPlayer <- function(selectedPlayer, selectedArea = "All", distLongShot = 27,  DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - selectedPlayer : a player to select
  # - selectedArea : an area to select (default, all area)
  # - distLongShot : distance in feet after which a shot is considered a long shot (distance)
  # - DT : where to get datas (default)
  #-------------------------------------------------------------------------------
  # We download datas and filter to made shots
  shootingDT <- DT[points %in% c(0, 2, 3) & player == selectedPlayer & !grepl("free throw", type) & result == "made" & (!is.na(converted_x) & !is.na(converted_y)), 
                   .(type, assist, shot_distance, converted_x, converted_y)]
  
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
                              experienceLow = 1, experienceHigh = 100,
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
                                        Experience >= experienceLow & Experience <= experienceHigh, 
                                      .(player, Position, Salary, Age, Experience)]
  } else {
    playersFilteredDT <- DTplayerPres[Salary >= salaryLow & Salary <= salaryHigh & 
                                        Age >= ageLow & Age <= ageHigh &
                                        Experience >= experienceLow & Experience <= experienceHigh &
                                        Position %in% position, 
                                      .(player, Position, Salary, Age, Experience)]
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
    shootingFilteredDT <- shootingFilteredDT[, .(Total=sum(Total)), by = .(player, Position, Age, Experience, Salary, result)]
    
  } 
  
  # We dcast values to get missed and made in variables (columns)
  shootingFilteredDT <- dcast(data = shootingFilteredDT, formula = player + Position + Salary + Age + Experience ~ result, value.var = "Total")
  
  # We create new variables to be displayed
  shootingFilteredDT <- shootingFilteredDT[, total := made+missed]
  shootingFilteredDT <- shootingFilteredDT[, FieldGoal := round(made / total * 100, 2)][order(-total)]
  
  # We take the ith first lines
  shootingFilteredDT <- head(shootingFilteredDT, n = min(nbPlayer, nrow(shootingFilteredDT)))
  
  return(shootingFilteredDT)
  
}

#-------------------------------------------------------------------------------
getShootingCustomGraph <- function(selectedTeam = "All", shootType = "All Shots", 
                                   position = "All", 
                                   salaryLow = 0, salaryHigh = 100000000, 
                                   experienceLow = 1, experienceHigh = 100,
                                   ageLow = 1, ageHigh = 100,
                                   nbPlayer = 15, DTplayerPres = dicoPlayerFich, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : data table where to look for the datas
  # - selectedTeam : select a team
  # - shootType : Either all, 2pt or 3pt shots or FT
  # - nbPlayer : Number of players displayed
  #-------------------------------------------------------------------------------
  # We download customed datas with preceding function
  shootingDT <- getShootingCustom(selectedTeam, shootType, position, salaryLow, salaryHigh, 
                                  experienceLow, experienceHigh, ageLow, ageHigh, nbPlayer, DTplayerPres, DT)
  
  # Creation of a plotly graph
  fig <- plot_ly(data = shootingDT, type = "scatter",
                 mode = "markers",
                 x = ~total, y = ~FieldGoal,
                 color = ~Salary,
                 colors = "Spectral",
                 marker = list(size = ~Age, sizeref = 0.6, opacity = 0.5),
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
                title = paste(shootType, "Shooting Elite -", selectedTeam),
                xaxis = list(title = "Total Attempted",
                             zeroline = FALSE),
                yaxis = list(title = paste(shootType, "- Field Goal (%)"),
                             zeroline = FALSE))
  
  return(fig)
  
}

#-------------------------------------------------------------------------------
getStatsCustom <- function(selectedTeam = "All", statType = "Points", 
                           position = "All", 
                           salaryLow = 0, salaryHigh = 100000000, 
                           experienceLow = 1, experienceHigh = 100,
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
                                        Experience >= experienceLow & Experience <= experienceHigh, 
                                      .(player, Position, Salary, Age, Experience)]
  } else {
    playersFilteredDT <- DTplayerPres[Salary >= salaryLow & Salary <= salaryHigh & 
                                        Age >= ageLow & Age <= ageHigh &
                                        Experience >= experienceLow & Experience <= experienceHigh &
                                        Position %in% position, 
                                      .(player, Position, Salary, Age, Experience)]
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
  statFilteredDT <- merge(playersFilteredDT[, .(player, Position, Experience, Salary, Age)], statDT, by = "player", all.x = FALSE, all.y = FALSE)
  
  # We filter with minutes played
  statFilteredDT <- merge(statFilteredDT, DTgamesPlayed[, .(player, team, TotalGames, AvgMin)], by = c("player", "team"), all.x = TRUE, all.y = FALSE)
  
  # if a selected team is selected
  if (selectedTeam != "All") {
    statFilteredDT <- statFilteredDT[team == selectedTeam]
    statFilteredDT <- statFilteredDT[, statMean := round(Total/TotalGames, 2)]
    
  } else {
    statFilteredDT <- statFilteredDT[, .(Total=sum(Total), TotalGames = sum(TotalGames), AvgMin=(TotalGames * AvgMin / sum(TotalGames))), by = .(player, Position, Age, Experience, 
                                                                                                                                                 Salary)]
    statFilteredDT <- statFilteredDT[, .(AvgMin=sum(AvgMin)), by = .(player, Position, Age, Experience, Salary, Total, TotalGames)]
    statFilteredDT <- statFilteredDT[, statMean := round(Total/TotalGames, 2)]
  } 
  
  statFilteredDT <- statFilteredDT[order(-statMean)]
  statFilteredDT <- head(statFilteredDT, nbPlayer)
  
  return(statFilteredDT)
  
}

#-------------------------------------------------------------------------------
getStatCustomGraph <- function(selectedTeam = "All", statType = "Points", 
                               position = "All", 
                               salaryLow = 0, salaryHigh = 100000000, 
                               experienceLow = 1, experienceHigh = 100,
                               ageLow = 1, ageHigh = 100,
                               nbPlayer = 15, DTgamesPlayed = dicoPlayerMinute, DTplayerPres = dicoPlayerFich, DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - DT : data table where to look for the datas
  # - selectedTeam : select a team
  # - shootType : Either all, 2pt or 3pt shots or FT
  # - nbPlayer : Number of players displayed
  #-------------------------------------------------------------------------------
  # We download customed datas with preceding function
  statDT <- getStatsCustom(selectedTeam, statType, position, salaryLow, salaryHigh, 
                           experienceLow, experienceHigh, ageLow, ageHigh, nbPlayer, 
                           DTgamesPlayed, DTplayerPres, DT)
  
  # Creation of a plotly graph
  fig <- plot_ly(data = statDT, type = "scatter",
                 mode = "markers",
                 x = ~AvgMin, y = ~statMean,
                 color = ~Salary,
                 colors = "Spectral",
                 marker = list(size = ~Age, sizeref = 0.6, opacity = 0.5),
                 text = ~str_extract_all(string = get(x = 'player'), pattern = "(?<=([:blank:]))[A-z]+"),
                 hovertemplate = ~paste("<b>", player, " - ", "</b>", Position, 
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
                title = paste(statType, "Elite -", selectedTeam),
                xaxis = list(title = "Minutes PG",
                             zeroline = FALSE),
                yaxis = list(title = paste(statType, "PG"),
                             zeroline = FALSE))
  
  
  return(fig)
  
}