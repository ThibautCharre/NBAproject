library(stringr)
library(data.table)
library(rvest)

# 1 - INPUTS
season <- "2021-2022"
seasonType <- "Regular Season"

# 2 - Read the dico player file where all players are mentionned
#playersDT <- fread(file = paste("Shiny/Dictionary/", season, "/", seasonType, "/dicoPlayers.csv", sep = ""))
#playersVect <- unique(playersDT$player)

playersVect <- readClipboard()

# 3 - We format nba players names for webscrapping
firstLetterName <- str_to_lower(string = str_extract_all(string = playersVect, pattern = "(?<=[:space:])[A-z]{1}"))

fiveLetterName <- str_to_lower(string = str_extract_all(string = playersVect, pattern = "(?<=[:space:])[A-z]+"))
fiveLetterName <- ifelse(nchar(fiveLetterName) > 5, str_sub(string = fiveLetterName, start = 1, end = 5), fiveLetterName)

twoLetterFirstName <- str_to_lower(str_extract_all(string = playersVect, pattern = "^[A-z]{2}"))

# 4 - We get the complete http website adress for each players
playersWebDT <- data.table(firstPart = firstLetterName, secondPart = fiveLetterName, thirdPart = twoLetterFirstName)
playersWebDT[, completeAdress := paste("https://www.basketball-reference.com/players/", firstPart, "/", secondPart, thirdPart, "01.html", sep = "")]

# 5 - We use rverst package for webscrapping
getBirth <- function(webLink) {
  html_struct <- read_html(x = webLink)
  node <- html_elements(x = html_struct, css = "#necro-birth")
  birth <- html_attr(x = node, name = "data-birth")
  
  return(birth)
}

getXP <- function(webLink) {
  download.file(webLink, destfile = "Shiny/whatever.html")
  html_struct <- read_html("Shiny/whatever.html")
  XP <- html_elements(x = html_struct, xpath = "//*[@id='meta']/div[2]/p[12]/text()")
  XP <- str_extract(string = XP[2], pattern = "[0-9]")
  
  return(XP)
}

playersWebVect <- playersWebDT[, completeAdress]

XPVect <- sapply(X = playersWebVect, function(x) {
  tryCatch({
    getXP(x)
  }, error = function(e) {
    "ERROR"
  }, warning = function(w) {
    "WARNING"
  })
})



# 6 - We apply functions to the whole players
playersWebDT <- playersWebDT[, Birth := sapply(X = completeAdress, function(x) {
    getBirth(x)
  })]

playersWebDT <- playersWebDT[, Exp := getXP(completeAdress)]



