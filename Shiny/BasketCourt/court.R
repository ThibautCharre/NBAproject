#-------------------------------------------------------------------------------
# function to draw a basket court and integrate datas depending on type of datas the user specifies
#-------------------------------------------------------------------------------
# @ variables :
# - RankingOrShots : Shot chart or Ranking chart
# - selectedPlayer : Can be null in ranking system
# - onlyMade : Display only made shots ?
#-------------------------------------------------------------------------------
getShotChart <- function(selectedPlayer, selectedTeam, shootingDatas = "EmptyChart", playerShotsDT) {
  
  if (shootingDatas == "PlayerChart") {
    
    # We include datas in a chart
    fig <- plot_ly(data = playerShotsDT, type = "scatter", mode="markers",
                   x = ~converted_x_corrected, y = ~converted_y_corrected,
                   color = ~result,
                   colors = brewer.pal(9, "Set1")[c(2, 7)],
                   hoverinfo = "none",
                   marker = list(size = 15, opacity = 0.6))
    
  # End if   
    
  } else if (shootingDatas == "EfficientChart") {
    
    # We download shooting datas
    playerShotsDT <- playerShotsDT
    
    # We remove datas with NA as players did not shoot from there
    playerShotsDT <- playerShotsDT[!is.na(diffFG)]
    
    # We create a variable for attempted shots we will display on sizes of bubbles
    playerShotsDT <- playerShotsDT[, TotalPlayerAttempted := TotalPlayerMissed + TotalPlayerMade]
    
    # We create a variable under FG League or above we will display in a color
    playerShotsDT <- playerShotsDT[, vsLeague := sapply(diffFG, function(x) {
      
      if (x < -10) {
        "Diff FG < -10%"
      } else if (-10 <= x & x < -5) {
        "-10% <= Diff FG < -5%"
      } else if (-5 <= x & x < 0) {
        "-5% <= Diff FG <- 0%"
      } else if (0 <= x & x < 5) {
        "0% <= Diff FG <- +5%"
      } else if (5 <= x & x < 10) {
        "+5% <= Diff FG <- +10%"
      } else if (x >= 10) {
        "Diff FG >= +10%"
      }
      
    })]
    
    playerShotsDT$vsLeague <- factor(x = playerShotsDT$vsLeague, 
                                  levels = c("Diff FG < -10%", "-10% <= Diff FG < -5%", "-5% <= Diff FG <- 0%", 
                                             "0% <= Diff FG <- +5%", "+5% <= Diff FG <- +10%", "Diff FG >= +10%"))
    
    # We pre define a figure with only markers for hoverinfo
    playerShotsDT <- playerShotsDT[, xMarker := sapply(area, function(x) {
      
      dataShots <- data.table(xsi=c(1.5, 48.5, 10, 40, 10, 40, 25, 25, 25, 25, 6, 44, 25), 
                              ysi=c(7, 7, 7, 7, 19, 19, 5.25, 14.125, 24, 30.5, 22.5, 22.5, 37),
                              area = c("3pt Left Corner", "3pt Right Corner", "2pt Left Corner", "2pt Right Corner",
                                       "2pt Top Left", "2pt Top Right", "Under the Circle", "Short Paint Shot",
                                       "Long Paint Shot", "3pt Middle", "3pt Top Left", "3pt Top Right", 
                                       "Long Distance Shot"))
      
      dataShots[area == x, xsi]
      
      # end of sapply
    })]
    
    # We pre define a figure with only markers for hoverinfo
    playerShotsDT <- playerShotsDT[, yMarker := sapply(area, function(x) {
      
      dataShots <- data.table(xsi=c(1.5, 48.5, 10, 40, 10, 40, 25, 25, 25, 25, 6, 44, 25), 
                              ysi=c(7, 7, 7, 7, 19, 19, 5.25, 14.125, 24, 30.5, 22.5, 22.5, 37),
                              area = c("3pt Left Corner", "3pt Right Corner", "2pt Left Corner", "2pt Right Corner",
                                       "2pt Top Left", "2pt Top Right", "Under the Circle", "Short Paint Shot",
                                       "Long Paint Shot", "3pt Middle", "3pt Top Left", "3pt Top Right", 
                                       "Long Distance Shot"))
      
      dataShots[area == x, ysi]
      
      # end of sapply
    })]
    
    fig <- plot_ly(data = playerShotsDT, type = "scatter", mode = "markers",
                   x = ~xMarker, y = ~yMarker,
                   color = ~vsLeague,
                   colors = "Reds",
                   size = ~TotalPlayerAttempted,
                   fill = ~"",
                   marker = list(sizeref = 0.005, sizemode = "area"),
                   hovertemplate = ~paste("Area :<b>", area, 
                                          "<br></b>Total Attempted :<b>", TotalPlayerAttempted,
                                          "</b><br>", "Player Field Goal :<b>", PlayerFG, "%", 
                                          "</b><br>", "League Field Goal :<b>", LeagueFG, "%",
                                          "<extra></extra>"))
  
  # end if    
  } else if (shootingDatas == "EmptyChart") {
    
    fig <- plot_ly(type = "scatter", mode = "markers")
    
  }
  
  # We build shapes on the graph  
  fig <- layout(p = fig, 
                title = ifelse(!is.null(selectedPlayer), paste("<b>", selectedPlayer, "-", selectedTeam, "</b>Shooting Chart"), ""),
                xaxis = list(
                  # type = "linear",
                  range = c(0, 50),
                  showgrid = FALSE,
                  zeroline = FALSE,
                  visible = FALSE),
                yaxis = list(
                  # type = "linear",
                  range = c(0, 47),
                  showgrid = FALSE,
                  zeroline = FALSE,
                  visible = FALSE),
                legend = list(x = 0.01, y = 0.99),
                plot_bgcolor = "rgba(255, 255, 255, 0)", 
                paper_bgcolor = "rgba(255, 255, 255, 0)",
                shapes = list(
                  # # All basket court
                  list(type = "rect",
                       x0 = 0, x1 = 50, y0 = 0, y1 = 47, 
                       line = list(width = 4)
                  ),
                  # Outside paint
                  list(type = "rect", 
                       x0 = 17, x1 = 33, y0 = 0, y1 = 19
                  ),
                  # Inside paint
                  list(type = "rect", 
                       x0 = 19, x1 = 31, y0 = 0, y1 = 19
                  ),
                  # FT circle
                  list(type = "circle", 
                       x0 = 19, x1 = 31, y0 = 13, y1 = 25
                  ),
                  # Board line
                  list(type = "line", 
                       x0 = 22, x1 = 28, y0 = 4, y1 = 4
                  ),
                  # Basketball circle
                  list(type = "circle", 
                       x0 = 23.75, x1 = 26.25, y0 = 4, y1 = 6.5
                  ),
                  # Half court semi circle
                  list(type = "path",
                       path = "M 19 47 C 19 39, 31 39, 31 47"
                  ),
                  # Under the circle 
                  list(type = "rect",
                       x0 = 17, x1 = 33, y0 = 0, y1 = 9.25
                  ),
                  # 2pt near circle until FT line
                  list(type = "rect",
                       x0 = 17, x1 = 33, y0 = 9.25, y1 = 19
                  ),
                  # Long paint
                  list(type = "path",
                       path = "M 17 27.67 C 22.5 29.43, 27.5 29.43, 33 27.67 L 33 19 L 17 19 L 17 27.67"
                  ),
                  # 2pt left corner shot
                  list(type = "rect", 
                       x0 = 3, x1 = 17, y0 = 0, y1 = 14
                  ),
                  # 2pt right corner shot
                  list(type = "rect", 
                       x0 = 33, x1 = 47, y0 = 0, y1 = 14
                  ),
                  # 2pt top left shot
                  list(type = "path",
                       path = "M 3 14 C 5.25 20.5, 10.5 25.42, 17 27.67 L 17 14 L 3 14"
                  ),
                  # 2pt top right shot
                  list(type = "path",
                       path = "M 47 14 C 44.75 20.5, 39.5 25.42, 33 27.67 L 33 14 L 47 14"
                  ),
                  # two Lines 3pt line
                  list(type = "line", 
                       x0 = 3, x1 = 3, y0 = 0, y1 = 14
                  ),
                  list(type = "line", 
                       x0 = 47, x1 = 47, y0 = 0, y1 = 14
                  ),
                  # 3pt left corner shot
                  list(type = "rect", 
                       x0 = 0, x1 = 3, y0 = 0, y1 = 14
                  ),
                  # 3pt top left shot
                  list(type = "path",
                       path = "M 0 15 C 2.3 22, 7.3 27.3, 14.3 30 L 14.3 26.55 C 8.3 23.5 4.8 19 3 14 L 0 14 L 0 15"
                  ),
                  # 3pt right corner shot
                  list(type = "rect", 
                       x0 = 47, x1 = 50, y0 = 0, y1 = 14
                  ),
                  # 3pt top right shot
                  list(type = "path",
                       path = "M 50 15 C 47.7 22, 42.7 27.3, 35.7 30 L 35.7 26.55 C 41.7 23.5 45.2 19 47 14 L 50 14 L 50 15"
                  ),
                  # Enf of very long distance shot
                  list(type = "path",
                       path = "M 14.3 30 C 20.95 32.65, 29.05 32.65, 35.7 30 L 35.7 26.55 C 29 29.8, 21 29.8, 14.3 26.55"
                  )
                )
  )
  
  fig <- config(p = fig, displayModeBar = FALSE)
  
  return(fig)
  
}