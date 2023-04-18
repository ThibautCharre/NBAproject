library(data.table)
library(stringr)

source("Shiny/Functions/dataAnalysisFuncTab1.R")
source("Shiny/Functions/dataAnalysisFuncTab2.R")
source("Shiny/Functions/dataAnalysisFuncTab3.R")
source("Shiny/Functions/dataAnalysisFuncTab4.R")
source("Shiny/Functions/dataAnalysisFuncTab5.R")
source("Shiny/Functions/dataAnalysisFuncTab6.R")
source("Shiny/BasketCourt/court.R")

seasonSelected1 <- "2022-2023"
seasonTypeSelected1 <- "Regular Season"

teamStats <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/teamStatSummary.csv", sep = ""))
dicoPlayerFich <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/playersSummary.csv", sep = ""))
dicoPlayerMinute <- fread(file = paste("Shiny/Dictionary/", seasonSelected1, "/", seasonTypeSelected1, "/minutesSummary.csv", sep = ""))
listDatas <- cleanDatas(season = seasonSelected1, seasonType = seasonTypeSelected1, path = "Shiny/CombinedGames")
nbaDatas <- listDatas[["nbaDatas"]]

NBAcalendar <- getNBAcalendar(season = seasonSelected1, DT = nbaDatas, path = "Shiny/AllGames")
nbaDatas <- merge(nbaDatas, NBAcalendar[, .(game_id, Date, Home, Away)], by = c("game_id"))









