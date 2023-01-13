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