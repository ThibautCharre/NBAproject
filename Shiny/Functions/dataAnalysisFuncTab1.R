#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------

##############
# USEFULL HOMEMADE FUNCTIONS
##############
# getShortTeamName
# getLongTeamName
# getListRoster

##############
# CREATE INPUTS
##############
# cleanDatas
# getNBAcalendar
# getRankingStreakLeague (for us map to be ready when tab 2 is active)

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

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
cleanDatas <- function(season, seasonType = "Regular Season", filePattern = "combined-stats", 
                       colDelete = c("date", "play_length", "play_id", "away", "home", "num", 
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
getNBAcalendar <- function(season, path = "AllGames", fileGames = "games.csv", DT = nbaDatas) {
  #-------------------------------------------------------------------------------
  # @ variables :
  # - path : Path where to find all files of each NBA games (default)
  # - team : a vector with teams names (default)
  # - DT : Generic data table (default)
  #-------------------------------------------------------------------------------
  # we list files in the directory 
  gamesVect <- fread(paste(path, "/", season, "/", fileGames, sep = ""), header = FALSE)
  
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