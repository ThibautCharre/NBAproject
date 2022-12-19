library(data.table)
library(stringr)

seasonSelected1 <- "2021-2022"
seasonTypeSelected1 <- "Regular Season"

dicoPlayerFich <- gePlayersFich(season = seasonSelected1, seasonType = seasonTypeSelected1, path = "Shiny/Dictionary/")
dicoPlayerMinute <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/minutesSummary.csv", sep = ""))
listDatas <- cleanDatas(season = seasonSelected1, seasonType = seasonTypeSelected1, path = "Shiny/CombinedGames")
nbaDatas <- listDatas[["nbaDatas"]]

NBAcalendar <- getNBAcalendar(season = seasonSelected1, DT = nbaDatas, path = "Shiny/AllGames")
nbaDatas <- merge(nbaDatas, NBAcalendar[, .(game_id, Date, Home, Away)], by = "game_id")

datas <- getShootingCustom(DT = nbaDatas, DTplayerPres = dicoPlayerFich)
datas



















# Script to calculate bench points

subteam <- "GSW"
DTteam <- nbaDatas[Home == subteam|Away == subteam]
DTstarting <- DTteam[period == 1 & event_type == "start of period", .(game_id, team, onFloorHome, Home, onFloorAway, Away)]
DTstarting <- DTstarting[, onFloorTeam := ifelse(Away == subteam, onFloorAway, onFloorHome)]
DTstarting <- DTstarting[, .(game_id, onFloorTeam)]

DTteam <- merge(DTteam, DTstarting, by = "game_id", all.x = TRUE, all.y = FALSE)
benchDT <- DTteam[points %in% c(1, 2, 3) & team == subteam]
benchDT <- benchDT[, isStarterScoring := mapply(FUN = function(x, y) {
  if (y %like% x) {
    TRUE
  } else {
    FALSE
  }
}, benchDT$player, benchDT$onFloorTeam)]

benchDT <- benchDT[isStarterScoring == FALSE]
benchScoreDT <- benchDT[, .(benchPts = sum(points)), by = player]

tituDT <- benchDT[isStarterScoring == TRUE]
tituScoreDT <- tituDT[, .(benchPts = sum(points)), by = player]
