getNBAmap <- function(coordFile = "USmap/nbateamsgeo.csv", listResultTeams) {  

  # We add markers corresponding to stadium coordinates  
  teamCoord <- fread(file = coordFile)
  
  # We create a map with US states
  mapStates <- map("state", fill = TRUE, plot = FALSE)
  
  # We connect states and their division and we create a color panel to fill states according to their state
  division <- c("", "Pacific", "",
                "Pacific", "North West", "",
                "", "", "South East",
                "South East", "", "Central",
                "Central", "", "",
                "", "South West", "",
                "", "Atlantic", "Atlantic",
                "Atlantic", "Central", "Central",
                "North West", "", "",
                "", "", "",
                "", "", "",
                "Atlantic", "Atlantic", "Atlantic",
                "Atlantic", "South East", "South East",
                "South East", "", "Central", 
                "North West", "North West", "Atlantic", 
                "Atlantic", "", "",
                "South West", "South West", "North West",
                "", "South East", "South East", 
                "South East", "North West", "North West",
                "North West", "North West", "North West",
                "", "Central", "")
  mapStates[['division']] <- factor(division)
  
  pal <- colorFactor(palette = c("white", brewer.pal(n=9, name = "Blues")[c(5, 6, 7)], brewer.pal(n=9, name = "Reds")[c(5, 6, 7)]), 
                     levels = c("", "Atlantic", "South East", "Central", "South West", "Pacific", "North West")
                     )
                     
  # We customize the map and states according to their coordinates and add legend
  usMap <- leaflet(data = mapStates, options = leafletOptions(minZoom = 4, maxZoom = 4))
  usMap <- setView(map = usMap, lng = -97.515, lat = 39.749, zoom = 4)
  usMap <- addPolygons(map = usMap,
                   popup = ~paste("State:<b>", names),
                   opacity = 2, fillOpacity = 0.8, 
                   fillColor = ~pal(division),
                   color = "black", weight =  1, 
                   stroke = TRUE)
  usMap <- addLegend(map = usMap, position = "bottomright", 
                 pal = pal, 
                 values = ~division, title = "Division League"
                )
  
  # We download NBAdatas
  teamDatas <- getRankingStreakMap(listResultTeams)
  
  # We merge coordinates and datas
  allDatas <- merge(teamCoord, teamDatas, 
                    by = "Team", allx = FALSE, all.y = TRUE)
  
  # we customize the labels and popup for teams
  nbaMap <- addAwesomeMarkers(map = usMap, data = allDatas, 
                       lng = ~Longitude, lat = ~Latitude,
                       layerId = ~Name,
                       label = ~Name,
                       popup = ~paste("Record :<b>", Bilan, "</b><br>Streak :<b>", Streak),
                       icon = awesomeIcons(icon = sapply(X = allDatas$Streak, function(x) {
                         if (x < 0 & x > -6) {
                           "minus"
                         } else if (x > 0 & x < 6) {
                           "plus"
                         } else if (x >= 6) {
                           "fire"
                         } else {
                           "tint"
                         }
                        }), 
                        markerColor = sapply(X = allDatas$Streak, function(x) {
                          if (x < 0) {
                            "black"
                          } else {
                            "darkgreen"
                          }
                        })
                        )
                       )
  
  return(nbaMap)
  
}
