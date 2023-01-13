library(data.table)
library(stringr)

source("Shiny/Functions/dataAnalysisFuncTab1.R")
source("Shiny/Functions/dataAnalysisFuncTab2.R")
source("Shiny/Functions/dataAnalysisFuncTab3.R")
source("Shiny/Functions/dataAnalysisFuncTab4.R")
source("Shiny/Functions/dataAnalysisFuncTab5.R")
source("Shiny/BasketCourt/court.R")

seasonSelected1 <- "2020-2021"
seasonTypeSelected1 <- "Regular Season"

teamStats <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/teamStatSummary.csv", sep = ""))
dicoPlayerFich <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/playersSummary.csv", sep = ""))
dicoPlayerMinute <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/minutesSummary.csv", sep = ""))
listDatas <- cleanDatas(season = seasonSelected1, seasonType = seasonTypeSelected1, path = "Shiny/CombinedGames")
nbaDatas <- listDatas[["nbaDatas"]]

NBAcalendar <- getNBAcalendar(season = seasonSelected1, DT = nbaDatas, path = "Shiny/AllGames")
nbaDatas <- merge(nbaDatas, NBAcalendar[, .(game_id, Date, Home, Away)], by = "game_id")



# Script to calculate bench points
subteam <- "GSW"

ratio <- round(100 * sumBenchPts/(sumBenchPts+sumTituPts), 2)

getBenchDistrib <- function(selectedTeam, DT) {
  
  # We filter datas
  DT <- DT[Home == selectedTeam|Away == selectedTeam]
  
  # We calculate nb of games
  nbGames <- length(unique(DT$game_id))
  
  # We define type of shots
  DT <- DT[, type := ifelse(type %like% "3pt", "3pt Shots", ifelse(type %like% "free throw", "FT Shots", "2pt Shots"))]
  
  # We flag players starting a game
  beginGameDT <- DT[period == 1 & event_type == "start of period", .(game_id, team, onFloorHome, Home, onFloorAway, Away)]
  beginGameDT <- beginGameDT[, onFloorTeam := ifelse(Away == selectedTeam, onFloorAway, onFloorHome)]
  beginGameDT <- beginGameDT[, .(game_id, onFloorTeam)]
  
  # We identify if the player scoring is a bench player or not
  DT <- merge(DT, beginGameDT, by = "game_id", all.x = TRUE, all.y = FALSE)
  DT <- DT[points %in% c(0, 1, 2, 3) & team == selectedTeam]
  DT <- DT[, isStarterScoring := mapply(FUN = function(x, y) {
    if (y %like% x) {
      "Starter"
    } else {
      "Bench"
    }
  }, DT$player, DT$onFloorTeam)]
  
  # We aggregate shots for bench
  pointsDT <- DT[, .(Tot = .N), by = c("type", "isStarterScoring", "result")]
  
  # we use dcast to spare result
  pointsDT <- dcast(data = pointsDT, formula = type ~ isStarterScoring + result, value.var = "Tot")
  
  # We save total excluding shot type 
  totStarterMade = sum(pointsDT$Starter_made * c(2, 3, 1))
  totBenchMade = sum(pointsDT$Bench_made * c(2, 3, 1))
  
  totalDT <- data.table(Type = "Total",
                        Starter = round(totStarterMade / nbGames, 2), 
                        Bench = round(totBenchMade / nbGames, 2), 
                        Total = round((totStarterMade + totBenchMade) / nbGames, 2))
  
  # We calculate percentage for shots
  pointsDT <- pointsDT[, ":=" (benchEfficiency = round(100 * Bench_made / (Bench_made + Bench_missed), 2), 
                               starterEfficiency = round(100 * Starter_made / (Starter_made + Starter_missed), 2), 
                               totalEfficiency = round(100 * (Starter_made + Bench_made) / (Starter_made + Starter_missed + Bench_made + Bench_missed), 2))]
  
  # We add points per game
  pointsDT <- pointsDT[, ":=" (benPPG = round(Bench_made * ifelse(type == "2pt Shots", 2, ifelse(type == "3pt Shots", 3, 1)) / nbGames, 2), 
                               starterPPG = round(Starter_made * ifelse(type == "2pt Shots", 2, ifelse(type == "3pt Shots", 3, 1)) / nbGames, 2), 
                               totalPPG = round((Starter_made + Bench_made) * ifelse(type == "2pt Shots", 2, ifelse(type == "3pt Shots", 3, 1)) / nbGames, 2))]
  
  # Final DT to be displayed
  pointsDT <- data.table(Type = pointsDT$type, 
                         Starter = paste(pointsDT$starterPPG, " (", pointsDT$starterEfficiency, "%)", sep = ""), 
                         Bench = paste(pointsDT$benPPG, " (", pointsDT$benchEfficiency, "%)", sep = ""), 
                         Total = paste(pointsDT$totalPPG, " (", pointsDT$totalEfficiency, "%)", sep = ""))
  
  # We bind DT
  teamDistrDT <- rbind(pointsDT, totalDT)
  
  return(teamDistrDT)
  
}


