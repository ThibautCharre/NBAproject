#-------------------------------------------------------------------------------
# Functions table of contens
#-------------------------------------------------------------------------------

##############
# CREATE OUTPUTS : 5th page Shiny - Bubble charts
##############
# Object
# getShootingCustom
# getShootingCustomGraph
# getStatsCustom
# getStatsCustomGraph

#-------------------------------------------------------------------------------
# Functions 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
getShootingCustom <- function(selectedTeam = "All", shootType = "All Shots", 
                              position = "All", 
                              salaryLow = 0, salaryHigh = 100000000, 
                              experienceLow = 0, experienceHigh = 100,
                              ageLow = 1, ageHigh = 100,
                              nbPlayer = 15, DTgamesPlayed = dicoPlayerMinute, DTplayerPres = dicoPlayerFich, DT = nbaDatas) {
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
  
  # We filter with minutes played
  shootingFilteredDT <- merge(shootingFilteredDT, DTgamesPlayed[, .(player, team, TotalGames)], by = c("player", "team"), all.x = TRUE, all.y = FALSE)
  
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
                 marker = list(size = ~TotalGames, sizeref = 1, opacity = 0.6),
                 text = ~str_extract_all(string = get(x = 'player'), pattern = "(?<=([:blank:]))[A-z]+"),
                 hovertemplate = ~paste("<b>", player, "-", "</b>", Position,
                                        "</b><br>Games Played : <b>", TotalGames,
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
  
  # No plot bar top right
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
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
  
  # No plot bar top right
  fig <- plotly::config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}