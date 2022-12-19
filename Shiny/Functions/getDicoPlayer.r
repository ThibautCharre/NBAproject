library(stringr)
library(data.table)

#-------------------------------------------------------------------------------
getDicoPlayers <- function(season, seasonType = "Regular Season", filePattern = "combined-stats", 
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
  
  # We create a file with nba players, teams and nb games IF saveDicoPlayers TRUE
   dicoPlayers <- nbaDT[team != "" & player != "" & event_type != "jump ball"]
   dicoPlayers<- dicoPlayers[, .(TotalGames = uniqueN(game_id)), by = .(player, team)]
   dicoPlayers <- dicoPlayers[order(player, team)]
   fwrite(dicoPlayers, file = paste("Shiny/Dictionary/", season, "/", seasonType, "/dicoPlayers.csv", sep="")) 
}

season <- "2021-2022"
seasonType <- "Playoffs"
getDicoPlayers(season = season, seasonType = seasonType)

