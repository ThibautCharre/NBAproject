###
# Packages
###
# Website
library(shiny)
# Data Treatment
library(data.table)
library(DT)
library(stringr)
library(magrittr)
# Graph
library(plotly)
library(RColorBrewer)
# Map
library(leaflet)
library(maps)

###
# ShortName path
###

###
# Functions to create objects displayed
###
source("Functions/dataAnalysisFuncTab1.R")
source("Functions/dataAnalysisFuncTab2.R")
source("Functions/dataAnalysisFuncTab3.R")
source("Functions/dataAnalysisFuncTab4.R")
source("Functions/dataAnalysisFuncTab5.R")
source("Functions/dataAnalysisFuncTab6.R")
source("BasketCourt/court.R")

############################################
############################ INPUTS ########
############################################
#----------
# We create a list of areas on the court
#----------
area <- c("All", "Under the Circle", "Short Paint Shot", "Long Paint Shot",  
          "2pt Left Corner", "2pt Right Corner", "2pt Top Left", "2pt Top Right",
          "3pt Left Corner", "3pt Right Corner", "3pt Top Left", "3pt Top Right", 
          "3pt Middle", "Long Distance Shot")

################################################################################
###################################### UI ######################################
################################################################################
ui <- 
  
  tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
    tags$head(tags$script(src = "effect.js"))
    ,
    
    navbarPage(title = "Stat-IB",
               
               tabPanel("New Comers", value = "welcome",
                        
                        ############################################################################
                        ################################### TAB 1 ##################################
                        ############################################################################
                        
                        h2("First Words"),
                        tags$p(class = "introText", 
                               HTML("Welcome ! 
                        <br>Stat-IB is a data analysis project created in 2022.
                        <br>Its main objective is to display NBA statistics in graphical and interactive ways.
                        <br>Statistics are calculated in-live and are based on a specific part of a specific season (playoffs vs. regular season).
                        <br>So before going further, let's select the season to be exploited.")),
                        h2("Data Selection"),
                        tags$div(class = "container", 
                                 selectInput(inputId = "seasonSelected1", label = "SEASON", choices = c("2020-2021", "2021-2022"), selected = c("2020-2021")),
                                 selectInput(inputId = "seasonTypeSelected1", label = "TYPE", choices = c("Regular Season", "Playoffs"), selected = c("Regular Season")),
                                 tags$div(class = "basketballGo",
                                          tags$div(class = "lineVert"),
                                          tags$div(class = "lineCurvLeftGo"),
                                          tags$div(class = "lineCurvRightGo"),
                                          actionButton(inputId = "goButton1", label = "GO!", onclick = "$('.nav.navbar-nav>li>a').parent().removeClass('disabled'); document.getElementById('resetButton').disabled = false;document.getElementById('goButton1').disabled = true;")
                                 ),
                                 tags$div(class = "basketball",
                                          tags$div(class = "lineVert"),
                                          tags$div(class = "lineCurvLeft"),
                                          tags$div(class = "lineCurvRight"),
                                          actionButton(inputId = "resetButton", label = "RESET!")
                                 )
                        ),
                        verbatimTextOutput(outputId = "confirmationText1"),
                        h2("League Calendar"),
                        tags$div(class = "RowDT",
                                 DT::dataTableOutput(outputId = "calendarDT1", width = "40%")
                        )
               ),
               
               
               
               tabPanel("Team Building", value = "teamDash",
                        
                        ############################################################################
                        ################################### TAB 2 ##################################
                        ############################################################################
                        
                        h2("Team Selection"),
                        leafletOutput(outputId = "usmap2", height = "400px", width = "700px"),
                        verbatimTextOutput(outputId = "selectedTeamText2"),
                        h2("Results and schedule"),
                        tags$div(class = "RowDT",
                                 DT::dataTableOutput(outputId = "teamRank2", width = "40%"),
                        ),
                        h2("Collective Stats"),
                        tags$div(class = "Row66",
                                 tags$div(class = "BlockParamDTchart",
                                          uiOutput(outputId = "statSelected2"),
                                          plotlyOutput(outputId = "teamBarChart2", width = "35vw", height = "20vw")
                                 ),
                                 tags$div(class = "BlockParamDTchart",
                                          uiOutput(outputId = "nbPlayerChart2"),       
                                          plotlyOutput(outputId = "teamPieChart2", width = "35vw", height = "20vw")
                                 )
                        ),
                        tags$div(class = "RowDT",
                                 DT::dataTableOutput(outputId = "bestGames2", width = "40%")
                        )
               ),
               
               tabPanel("Business Card", value = "teamPlayer",
                        
                        ############################################################################
                        ################################### TAB 3 ##################################
                        ############################################################################ 
                        
                        h2("Parameters"),
                        tags$div(class = "param34container",
                                 selectInput(inputId = "selectedTeam3", label = "TEAM", choices = "Team", selected = "Team"), 
                                 selectInput(inputId = "selectedPlayer3", label = "PLAYER", choices = "Player", selected = "Player"),
                                 dateRangeInput(inputId = "dateRange3", label = "SCHEDULE", 
                                                start = Sys.Date(), end = Sys.Date(), min =  Sys.Date(), max = Sys.Date(), 
                                                weekstart = 1, autoclose = FALSE)
                        ),
                        h2("Summary"),
                        tags$div(class = "RowDT",
                                 DT::dataTableOutput("sumGamesDT3", width = "40vw")
                                 ),
                        tags$div(class = "RowDT",
                                 tags$div(class = "BlockParamDTchart",
                                          selectInput(inputId = "statSelected3", 
                                                      label = "STATS", 
                                                      choices = c("Points", "Assists", "Rebounds", "Blocks", "Steals"), width = "150px"),
                                          plotlyOutput(outputId = "sumGraph3", width = "60vw", height = "25vw")
                                 )
                        ),
                        tags$div(class = "basketball",
                                 tags$div(class = "lineVert"),
                                 tags$div(class = "lineCurvLeft"),
                                 tags$div(class = "lineCurvRight"),
                                 actionButton(inputId = "goButton3", label = "GO!")
                        ),
                        h2("Efficiency/Impact"),
                        uiOutput(outputId = "allItems3")
               ),
               
               
               
               tabPanel("Pros vs. Cons", value = "kingCourt", 
                        
                        ############################################################################
                        ################################### TAB 4 ##################################
                        ############################################################################
                        
                        h2("Parameters"),
                        tags$div(class = "param34container",
                                 selectInput(inputId = "selectedTeam4", label = "TEAM", choices = "Team", selected = "Team"), 
                                 selectInput(inputId = "selectedPlayer4", label = "PLAYER", choices = "Player", selected = "Player"),
                                 dateRangeInput(inputId = "dateRange4", label = "SCHEDULE", 
                                                start = Sys.Date(), end = Sys.Date(), min =  Sys.Date(), max = Sys.Date(), 
                                                weekstart = 1, autoclose = FALSE)
                        ),
                        h2("Summary"),
                        tags$div(class = "Row66",
                                 DT::dataTableOutput(outputId = "playerShotsDT4", width = "40vw"),
                                 plotlyOutput(outputId = "playerShotsSum4", width = "35vw", height = "20vw")
                        ),
                        tags$div(class = "basketball",
                                 tags$div(class = "lineVert"),
                                 tags$div(class = "lineCurvLeft"),
                                 tags$div(class = "lineCurvRight"),
                                 actionButton(inputId = "goButton4", label = "GO!")
                        ),
                        h2("Shooting Spots"),
                        uiOutput(outputId = "allItems4")
               ), 
               
               
               tabPanel("Best Employee", value = "moneyball",
                        
                        ############################################################################
                        ################################### TAB 5 ##################################
                        ############################################################################
                        
                        h2("Floor is Yours"),
                        tags$div(class = "param5container",
                                 selectInput(inputId = "categorySelected5", label = "CATEGORY", choices = c("Classic", "Shooting"), selected = "Classic"),
                                 selectInput(inputId = "statSelected5", label = "STATS", choices = c("Points", "Assists", "Rebounds", "Blocks", "Steals"), selected = "Points"),
                                 selectInput(inputId = "selectedTeam5", label = "TEAM", choices = "All", selected = "All"),
                                 selectInput(inputId = "positionSelected5", label = "POSITION", choices = c("All", "PG", "SG", "SF", "PF", "C"), selected = "All", multiple = TRUE),
                                 sliderInput(inputId = "salarySelected5", label = "SALARY (M$)", min = 0, max = 100, value = c(0, 100), step = 1), 
                                 sliderInput(inputId = "ageSelected5", label = "AGE", min = 18, max = 50, value = c(18, 50), step = 1), 
                        ),
                        tags$div(class = "basketballGo",
                                 tags$div(class = "lineVert"),
                                 tags$div(class = "lineCurvLeftGo"),
                                 tags$div(class = "lineCurvRightGo"),
                                 actionButton(inputId = "goButton5", label = "GO!")
                        ),
                        h2("Bubbles ..."),
                        numericInput(inputId = "nbPlayerSelected5", label = "NB PLAYERS", value = 15, min = 10, max = 50, step = 1, width = "200px"),
                        plotlyOutput(outputId = "customGraph5", width = "65vw", height = "50vw"),
                        h2("Player Card"),
                        tags$div(class = "RowDT",
                                 DT::dataTableOutput(outputId = "playerCarDT5", width = "40%"),
                        ),
                        h2("Player Salary"),
                        tags$div(class = "RowDT",
                                 DT::dataTableOutput(outputId = "playerSalDT5", width = "40%")
                        )
               ), 
               
               
               tabPanel("Bonus", value = "bonus",
                        
                        ############################################################################
                        ################################### TAB 6 ##################################
                        ############################################################################
                        
                        h2("Filtering"), 
                        tags$div(class = "param6container",
                          radioButtons(inputId = "teamOpp6", label = "WHO", choices = c("Team", "Opp", "Both"), selected = "Team", inline = TRUE), 
                          selectInput(inputId = "typeStat6", label = "STATS", choices = c("Possession", "Points", "Assists", "Rebounds", "Steals", "Blocks", "Turnovers", "All FGM", "All FGA", "All FG%", "3FGM", "3FGA", "3FG%"), selected = c("Points"), multiple = TRUE)
                        ),
                        tags$div(class = "RowDT",
                          DT::dataTableOutput(outputId = "teamStatDT6")
                        )
               ),
                        
                        
                        
    ),
    
    tags$div(class = "busy", img(src = "Loading/circleWait.gif")),
    tags$body(style = "background-image: url(BckPics/tab2Basket.jpg);"),
    tags$nav(tags$div(class = "hamb", img(src = "hamb.png", width = "75px", height = "75px")))
  )

################################################################################
################################ SERVER ########################################
################################################################################
server <- function(input, output, session) {
  
  ############################################################################
  ################################### TAB 1 ##################################
  ############################################################################
  
  # RESET - BUTTON
  observeEvent(input$resetButton, {
    session$reload()
  })
  
  # - INTRO TEXT
  output$introText1 <- renderText({
    "Welcome ! 
    \nStat-IB is a data analysis project created in 2022.
    \nIts main objective is to display NBA statistics in graphical and interactive ways.
    \nStatistics are calculated in-live and are based on a specific part of a specific season (playoffs vs. regular season).
    \nSo before going further, let's select the season to be exploited."
  }) 
  
  # - SEASON VARIABLE
  seasonSelected1 <- eventReactive(input$goButton1, {
    input$seasonSelected1
  })
  
  # - TYPE SEASON VARIABLE
  seasonTypeSelected1 <- eventReactive(input$goButton1, {
    input$seasonTypeSelected1
  })
  
  # --------------------------- DATAS DOWNLOAD --------------------------------- 
  # - DATAS DOWNLOADED : DT PLAYER MINUTE
  teamStatSum <- reactive({
    fread(file = paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/teamStatSummary.csv", sep = ""))
  })
  
  dicoPlayerMinute <- reactive({
    fread(file = paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/minutesSummary.csv", sep = ""))
  })
  
  # - DATAS DOWNLOADED : Vector of NBA teams
  nbaTeams <- reactive({
    sort(sapply(dicoPlayerMinute()$team, FUN = getLongTeamName, USE.NAMES = FALSE))
  })
  
  # - DATAS DOWNLOADED : Vector of NBA players
  nbaPlayers <- reactive({
    dicoPlayerMinute()$player
  })
  
  # - DATAS DOWNLOADED : DT with players characteristics (position, salary, age)
  dicoPlayerFich <- reactive({
    fread(paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/playersSummary.csv", sep = ""))
  })
  
  # - DATAS DOWNLOADED : LIST of vector players and nbaDatas DT
  nbaDatasDT <- reactive({
    readRDS(paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/nbaDatas.rds", sep = ""))
  })
  
  # - DATAS DOWNLOADED : DT NBA Calendar
  NBAcalendar <- reactive({
    readRDS(paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/nbaCalendar.rds", sep = ""))
  })
  
  # - DATAS DOWNLOADED : DT merge nbaDatasDT & NBAcalendar
  nbaDatasDTmerged <- reactive({
    merge(nbaDatasDT(), NBAcalendar()[, .(game_id, Date, Home, Away)], by = "game_id")
  })
  
  # - We download the ranking streak for all league
  resultsByTeamList <- reactive({
    getRankingStreakLeague(NBAcalendar())
  })
  
  # ------------------------- END DATAS DOWNLOAD -------------------------------
  
  # - CONFIRMATION DL TEXT
  output$confirmationText1 <- renderText({
    if (input$goButton1 == 0) {
      "No Datas Selected\n!! Click on Go Button !!"
    } else {
      if (length(resultsByTeamList()) > 0) {
        paste("Datas Selected : OK !!\nSeason : ", seasonSelected1(), "\nPeriod : ", seasonTypeSelected1(), sep = "")
      }
    }
  })
  
  output$calendarDT1 <- DT::renderDataTable({
    if (input$goButton1 != 0) {
      tmpCal <- NBAcalendar()[, .(Date, Home, home_score, Away, away_score, Winner)]
      tmpCal <- tmpCal[, Score := paste(home_score, "-", away_score, sep = "")]
      tmpCal[, .(Date, Home, Away, Score, Winner)][order(-Date)]
    }
  }, options = list(pageLength = 10, dom = "tp"))
  
  ############################################################################
  ################################### TAB 2 ##################################
  ############################################################################
  
  # - US MAP LEAFLET
  output$usmap2 <- renderLeaflet({
    source("USmap/usgeo.R")
    getNBAmap(listResultTeams = resultsByTeamList())
  })
  
  # - LONG NAME TEAM VARIABLE
  longNameSelected2 <- reactive({
    input$usmap2_marker_click$id
  })
  
  # - REACTIVE VALUE TO CHANGE TEXT
  reactTextSelectedTeam1 <- reactiveValues(value2 = FALSE)
  
  observeEvent(input$goButton1, {
    reactTextSelectedTeam1$value2 = FALSE
  })
  
  observeEvent(input$usmap2_marker_click, {
    reactTextSelectedTeam1$value2 = TRUE
  })
  
  # - SELECTED TEAM TEXT
  output$selectedTeamText2 <- renderText({
    if (reactTextSelectedTeam1$value2 == FALSE) {
      "Select a Team on the Map"
    } else {
      paste("Selected: ", longNameSelected2(), sep = "")
    }
  })
  
  # - SHORT NAME TEAM VARIABLE
  shortNameSelected2 <- reactive({
    req(input$usmap2_marker_click)
    getShortTeamName(longName = input$usmap2_marker_click$id)
  })
  
  # - TEAM LOGO
  #output$logo2 <- renderImage(list(src = paste("Logos/", shortNameSelected2(), "-min", ".png", sep = ""), contentType = "image/png", height = "250px", width = "auto"), deleteFile = FALSE)
  
  # - TEAM RANKING DT
  output$teamRank2 <- DT::renderDataTable({
    req(shortNameSelected2())
    getRankingStreakTeam(selectedTeam = shortNameSelected2(), listResultTeam = resultsByTeamList())
  }, options = list(pageLength = 5, dom = "tp"))
  
  # - STAT SELECTION RENDER UI
  output$statSelected2 <- renderUI({
    req(shortNameSelected2())
    selectInput(inputId = "statSelected2", 
                label = "STATS", 
                choices = c("Points", "Assists", "Rebounds", "Blocks", "Steals"), width = "150px")
  })
  
  # - STAT SELECTED VARIABLE
  statSelected2 <- reactive({input$statSelected2})
  
  # - NUMBER PLAYER GRAPH RENDER UI
  output$nbPlayerChart2 <- renderUI({
    req(shortNameSelected2())
    numericInput(inputId = "nbPlayerChart2",
                 label = "NB PLAYERS",
                 value = 10, min = 5, max = 25, step = 1,
                 width = "150px")
  })
  
  # - NUMBER PLAYER GRAPH VARIABLE
  nbPlayerChart2 <- reactive({input$nbPlayerChart2})
  
  # - TEAM BAR CHART GRAPH
  output$teamBarChart2 <- renderPlotly({
    req(statSelected2(), shortNameSelected2())
    getTeamStatsGraph(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), DT = teamStatSum())
  })
  
  # - TEAM PIE CHART GRAPH
  output$teamPieChart2 <- renderPlotly({
    req(statSelected2(), req(nbPlayerChart2()), shortNameSelected2())
    getTeamStatsPerPlayerChart(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), nbPlayers = nbPlayerChart2(), DTroster = dicoPlayerMinute(), DT = nbaDatasDTmerged())
  })
  
  # - BEST GAMES DT
  output$bestGames2 <- DT::renderDataTable({
    req(statSelected2(), shortNameSelected2())
    getBestGames(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), nbGames = 20, DTroster = dicoPlayerMinute(), DT = nbaDatasDTmerged())
  }, options = list(pageLength = 5, dom = "tp"))
  
  ############################################################################
  ################################### TAB 3 ##################################
  ############################################################################
  
  # UPDATE OF SELECTINPUT BOX TEAM AND ROSTER
  observe({
    updateSelectInput(inputId = "selectedTeam3", label = "TEAM", choices = c("Team", nbaTeams()), selected = "Team")
  })
  
  roster3 <- reactive({getListRoster(team = getShortTeamName(input$selectedTeam3), DT = dicoPlayerMinute())})
  
  observe({
    updateSelectInput(inputId = "selectedPlayer3", label = "PLAYER", 
                      choices = c("Player", roster3()), 
                      selected = "Player")
  })
  
  # - SHORT NAME TEAM VARIABLE
  shortNameSelected3 <- reactive({
    getShortTeamName(longName = input$selectedTeam3)
  })
  
  # - PLAYER NAME VARIABLE
  playerSelected3 <- reactive({
    input$selectedPlayer3
  })
  
  # - UPDATE DATE RANGE OUTPUT
  observe({
    updateDateRangeInput(inputId = "dateRange3", start = min(nbaDatasDTmerged()$Date), end = max(nbaDatasDTmerged()$Date), min =  min(nbaDatasDTmerged()$Date), max = max(nbaDatasDTmerged()$Date))
  })
  
  # - CALENDAR RANGE VARIABLES
  dateRange3Min <- reactive({
    input$dateRange3[1]
  })
  dateRange3Max <- reactive({
    input$dateRange3[2]
  })
  
  # - PLAYER GAMES LIST
  playerGamesList <- reactive({
    getHistPlayerStats(selectedTeam = shortNameSelected3(), selectedPlayer = playerSelected3(), startDate = dateRange3Min(), endDate = dateRange3Max(), DTcalendar = NBAcalendar(), DT = nbaDatasDTmerged())
  })
    
  # - SUMMARY GAMES DT
  output$sumGamesDT3 <- DT::renderDataTable({
    tryCatch({
      playerGamesList()[["historicalDT"]]
    }, warning = function(w) {
      NULL
    }, error = function(e) {
      NULL
    })
  }, options = list(pageLength = 5, dom = "tp"))
  
  # # - COMPARISON GAMES DT
  # output$sumGraph3 <- renderPlotly({
  #   tryCatch({
  #     getPlayerClassicStatsChart(selectedStat = "Points", histDT = playerGamesList[["historicalDT"]], histMeanDT = playerGamesList[["historicalMeanDT"]])
  #   }, warning = function(w) {
  #     NULL
  #   }, error = function(e) {
  #     NULL
  #   })
  # })
  
  # - STAT SELECTED VARIABLE
  statSelected3 <- reactive({
    input$statSelected3
  })
  
  # - COMPARISON GAMES DT
  output$sumGraph3 <- renderPlotly({
    #req(playerSelected3() != "Player", statSelected3())
    getPlayerClassicStatsChart(selectedStat = statSelected3(), histDT = playerGamesList()[["historicalDT"]], histMeanDT = playerGamesList()[["historicalMeanDT"]])
  })
  
  # - SHORT NAME TEAM FIXED VARIABLE
  shortNameSelected3Graph <- eventReactive(input$goButton3, {
    getShortTeamName(longName = input$selectedTeam3)
  })
  
  # - PLAYER NAME FIXED VARIABLE
  playerSelected3Graph <- eventReactive(input$goButton3, {
    input$selectedPlayer3
  })
  
  # - CALENDAR RANGE FIXED VARIABLES
  dateRange3MinGraph <- eventReactive(input$goButton3, {
    input$dateRange3[1]
  })
  dateRange3MaxGraph <- eventReactive(input$goButton3, {
    input$dateRange3[2]
  })
  
  # - nbaDatasDT PLAYER
  playerDT3 <- eventReactive(input$goButton3, {
    req(playerSelected3Graph() != "Player", shortNameSelected3Graph() != "Team", dateRange3MinGraph(), dateRange3MaxGraph())
    getPlayerDT(shortNameSelected3Graph(), playerSelected3Graph(), startDate = dateRange3MinGraph(), endDate = dateRange3MaxGraph(), DT = nbaDatasDTmerged())
  })
  
  
  # - FREQ CHART RENDER UI
  output$selectedFreq3 <- renderUI({
    if (nrow(playerDT3()) > 0) {
      radioButtons(inputId = "selectedFreq3In", label = "RESULTS BY", choices = c("Month", "Week"), selected = "Month", inline = TRUE)
    }
  })
  
  # - FREQ CHART VARIABLE
  freqSelected3 <- reactive({input$selectedFreq3In})
  
  # - PLAYER DATE BAR CHART
  output$dateBarChart3 <- renderPlotly({
    req(freqSelected3(), playerDT3())
    if (nrow(playerDT3()) > 0) {
      getAdvancedPlayerStatsPerDateGraph(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), perMonthWeek = freqSelected3(), 
                                         DTcalendar = NBAcalendar(), DTplayer =  playerDT3())
    }
  })
  
  # - PLAYER PERIOD BAR CHART
  output$periodBarChart3 <- renderPlotly({
    req(playerDT3())
    if (nrow(playerDT3()) > 0) {
      getPlayerPeriodStatsChart(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), DTplayer = playerDT3())
    }
  })
  
  # - IMPACT PLAYER DT
  listImpactDT3 <- reactive({
    req(playerDT3())
    if (nrow(playerDT3()) > 0) {
      getPlayerImpact(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), playerDT3())
    }
  })
  
  output$impactFG3 <- renderDataTable({
    req(listImpactDT3())
    listImpactDT3()[["ShootImpact"]]
  }, options = list(dom = "t"))
  
  output$impactPluMin3 <- renderDataTable({
    req(listImpactDT3())
    listImpactDT3()[["PointImpact"]]
  }, options = list(dom = "t"))
  
  # - REACTIVE VALUE TO SHOW OR HIDE ITEMS
  hideOrShowItems3 <- reactiveValues(value = FALSE)
  
  observeEvent(input$goButton3, {
    hideOrShowItems3$value <- TRUE
  })
  
  observe({
    if (playerSelected3Graph() != playerSelected3() | dateRange3MinGraph() != dateRange3Min() | dateRange3MaxGraph() != dateRange3Max()) {
      hideOrShowItems3$value <- FALSE
    }
  })
  
  # ALL ITEMS RENDER UI
  output$allItems3 <- renderUI({
    
    req(hideOrShowItems3$value == TRUE)
    
    tagList(
      tags$div(class = "Row66",
               tags$div(class = "BlockParamDTchart", 
                        uiOutput("selectedFreq3"),       
                        plotlyOutput(outputId = "dateBarChart3", width = "35vw", height = "20vw")
               ), 
               DT::dataTableOutput("impactFG3", width = "40vw")
      ),
      tags$div(class = "Row66",
               DT::dataTableOutput("impactPluMin3", width = "40vw"),
               plotlyOutput(outputId = "periodBarChart3", width = "35vw", height = "20vw")
      )
    )
  })
  
  ############################################################################
  ################################### TAB 4 ##################################
  ############################################################################
  
  # UPDATE OF SELECTINPUT BOX TEAM AND ROSTER
  observe({
    updateSelectInput(inputId = "selectedTeam4", label = "TEAM", choices = c("Team", nbaTeams()), selected = "Team")
  })
  
  roster4 <- reactive({getListRoster(team = getShortTeamName(input$selectedTeam4), DT = dicoPlayerMinute())})
  
  observe({
    updateSelectInput(inputId = "selectedPlayer4", label = "PLAYER", 
                      choices = c("Player", roster4()), 
                      selected = "Player")
  })
  
  # - UPDATE DATE RANGE OUTPUT
  observe({
    updateDateRangeInput(inputId = "dateRange4", start = min(nbaDatasDTmerged()$Date), end = max(nbaDatasDTmerged()$Date), min =  min(nbaDatasDTmerged()$Date), max = max(nbaDatasDTmerged()$Date))
  })
  
  # - SHORT NAME TEAM VARIABLE
  shortNameSelected4 <- reactive({
    getShortTeamName(longName = input$selectedTeam4)
  })
  
  # - PLAYER SELECTED VARIABLE
  playerSelected4 <- reactive({
    input$selectedPlayer4
  })
  
  # - CALENDAR RANGE VARIABLES
  dateRange4Min <- reactive({
    input$dateRange4[1]
  })
  dateRange4Max <- reactive({
    input$dateRange4[2]
  })
  
  # - PLAYER SHOOTING LIST
  shootingList4 <- reactive({
    req(shortNameSelected4(), playerSelected4(), dateRange4Min(), dateRange4Max())
    getPlayerGlobalShooting(shortNameSelected4(), playerSelected4(), dateRange4Min(), dateRange4Max(), NBAcalendar(), nbaDatasDTmerged())
  })
  
  # - REACTIVE VALUE TO DISPLAY ALL ITEMS
  continueCalc <- reactiveValues(value = TRUE)
  
  observe({
    if (shootingList4()[["calDT"]][1, "Opp"] == "-") {
      continueCalc$value <- FALSE
    } else {
      continueCalc$value <- TRUE
    }
  })
  
  # - PLAYER SHOOTING DT
  output$playerShotsDT4 <- DT::renderDataTable({
    tryCatch({
      shootingList4()[["calDT"]]
    }, warning = function(w) {
      NULL
    }, error = function(e) {
      NULL
    })
  }, options = list(pageLength = 5, dom = "tp"))
  
  # - PLAYER SHOOTING SUMMARY GRAPH
  output$playerShotsSum4 <- renderPlotly({
    tryCatch({
      getPlayerGlobalShootingGraph(playerSelected4(), startDate = dateRange4Min(), endDate = dateRange4Max(), DT = shootingList4()[["graphDT"]])
    }, warning = function(w) {
      NULL
    }, error = function(e) {
      NULL
    })
  })
  
  # - GRAPH VARIABLES
  playerSelected4Graph <- eventReactive(input$goButton4, {
    input$selectedPlayer4
  })
  shortNameSelected4Graph <- eventReactive(input$goButton4, {
    getShortTeamName(longName = input$selectedTeam4)
  })
  dateRange4MinGraph <- eventReactive(input$goButton4, {
    input$dateRange4[1]
  })
  dateRange4MaxGraph <- eventReactive(input$goButton4, {
    input$dateRange4[2]
  })
  
  # - TYPE CHART RENDER UI
  output$selectedChart4 <- renderUI({
    req(playerSelected4Graph()!="Player", shortNameSelected4Graph()!="Team", continueCalc$value == TRUE)
    radioButtons(inputId = "selectedChart4In", label = "CHART", choices = c("Spots", "Efficiency"), selected = "Spots", inline = TRUE)
  })
  
  # - SELECT CHART VARIABLE
  chartType4 <- reactive({
    input$selectedChart4In
  })
  
  # - PLAYER SHOOTING LIST CHARTS
  listPlayerShots4 <- reactive({
    req(playerSelected4Graph() != "Player", shortNameSelected4Graph() != "Team", dateRange4MinGraph(), dateRange4MaxGraph(), continueCalc$value == TRUE)
    getPlayerShotDT(selectedPlayer = playerSelected4Graph(), selectedTeam = shortNameSelected4Graph(), startDate = dateRange4MinGraph(), endDate = dateRange4MaxGraph(), DT = nbaDatasDTmerged())
  })
  
  # - PLAYER CHARTS
  output$playerShotsChart4 <- renderPlotly({
    req(listPlayerShots4(), chartType4())
    if (chartType4() == "Spots"){
      getShotChart(playerSelected4Graph(), shortNameSelected4Graph(), "PlayerChart", listPlayerShots4()[["coordsDT"]])
    } else if (chartType4() == "Efficiency") {
      getShotChart(playerSelected4Graph(), shortNameSelected4Graph(), "EfficientChart", listPlayerShots4()[["leagueDT"]])
    }
  })
  
  # - AREA SELECTED RENDER UI
  output$areaSelected4 <- renderUI({
    req(playerSelected4Graph() != "Player", shortNameSelected4Graph() != "Team", continueCalc$value == TRUE)
    selectInput(inputId = "areaSelected4", label = "AREA", choices = area, selected = area[1])
  })
  
  # - AREA SELECTED VARIABLE
  areaSelected4 <- reactive({
    input$areaSelected4
  })
  
  # - ASSISTS DISTRIBUTION LIST
  listAssists4 <- reactive({
    req(listPlayerShots4(), areaSelected4())
    getAssistsShotsPlayer(listPlayerShots4()[["assistsDT"]], selectedArea = areaSelected4())
  })
  
  output$globalAssist4 <- DT::renderDataTable({
    req(listAssists4())
    listAssists4()[["Global"]]
  }, options = list(pageLength = 5, dom = "tp"))
  
  output$detailedAssist4 <- DT::renderDataTable({
    req(listAssists4())
    listAssists4()[["Detailed"]]
  }, options = list(pageLength = 5, dom = "tp"))
  
  # - REACTIVE VALUE TO SHOW OR HIDE ITEMS
  hideOrShowItems4 <- reactiveValues(value = FALSE)
  
  observeEvent(input$goButton4, {
    hideOrShowItems4$value <- TRUE
  })
  
  observe({
    if (playerSelected4Graph() != playerSelected4() | dateRange4MinGraph() != dateRange4Min() | dateRange4MaxGraph() != dateRange4Max()) {
      hideOrShowItems4$value <- FALSE
    }
  })
  
  # - RENDER UI ALLITEMS4
  output$allItems4 <- renderUI({
    
    req(hideOrShowItems4$value == TRUE)
    
    tagList(
      tags$div(class = "Row66",
               tags$div(class = "BlockParamDTchart",
                        uiOutput(outputId = "selectedChart4"),
                        plotlyOutput(outputId = "playerShotsChart4", width = "45.9vw", height = "47vw")
               ),
               tags$div(class = "BlockParamDTchart",
                        uiOutput(outputId = "areaSelected4"),
                        DT::dataTableOutput(outputId = "globalAssist4", width = "30vw"),
                        DT::dataTableOutput(outputId = "detailedAssist4", width = "30vw")
               )
      )
    )
  })
  
  ############################################################################
  ################################### TAB 5 ##################################
  ############################################################################
  
  # - CATEGORY SELECTED VARIABLE
  categorySelected5 <- reactive({input$categorySelected5})
  
  # - CATEGORY SELECTED VARIABLE FOR GRAPH
  categorySelected5Graph <- eventReactive(input$goButton5, {
    input$categorySelected5
  })
  
  # - STATS CHOICES VECTORS
  choicesVec <- reactive({
    if (categorySelected5() == "Classic") {
      c("Points", "Assists", "Rebounds", "Blocks", "Steals")
    } else if (categorySelected5() == "Shooting") {
      c("All Shots", "2pt Shots", "3pt Shots", "Free Throws")
    }
  })
  
  # - UPDATE STAT SELECTED
  observe({
    updateSelectInput(inputId = "statSelected5", label = "STATS", choices = choicesVec(), selected = choicesVec()[1])
  })
  
  # - STAT SELECTED VARIABLE
  statSelected5 <- eventReactive(input$goButton5,{
    input$statSelected5
  })
  
  # UPDATE SELECTED TEAM
  observe({
    updateSelectInput(inputId = "selectedTeam5", label = "TEAM", choices = c("All", nbaTeams()), selected = "All")
  })
  
  # - TEAM SELECTED VARIABLE
  teamSelected5 <- eventReactive(input$goButton5,{
    getShortTeamName(input$selectedTeam5)
  })
  
  # - POSITION SELECTED VARIABLE
  positionSelected5 <- eventReactive(input$goButton5,{
    input$positionSelected5
  })
  
  minSliderSalary <- eventReactive(input$goButton5, {
    floor(min(dicoPlayerFich()$Salary/1000000))
  })
  
  maxSliderSalary <- eventReactive(input$goButton5, {
    ceiling(max(dicoPlayerFich()$Salary/1000000))+1
  })
  
  observe({
    updateSliderInput(inputId = "salarySelected5", label = "SALARY (M$)", min = minSliderSalary(), max = maxSliderSalary(), value = c(minSliderSalary(), maxSliderSalary()))
  })
  
  # - SALARY SELECTED VARIABLE
  salaryLow5 <- eventReactive(input$goButton5, {
    input$salarySelected5[1]
  })
  salaryHigh5 <- eventReactive(input$goButton5, {
    input$salarySelected5[2]
  })
  
  minSliderAge <- eventReactive(input$goButton5, {
    min(dicoPlayerFich()$Age)
  })
  
  maxSliderAge <- eventReactive(input$goButton5, {
    max(dicoPlayerFich()$Age)
  })
  
  observe({
    updateSliderInput(inputId = "ageSelected5", label = "AGE", min = minSliderAge(), max = maxSliderAge(), value = c(minSliderAge(), maxSliderAge()))
  })
  
  # - AGE SELECTED VARIABLE
  ageLow5 <- eventReactive(input$goButton5, {
    input$ageSelected5[1]
  })
  ageHigh5 <- eventReactive(input$goButton5, {
    input$ageSelected5[2]
  })
  
  # - NB PLAYER GRAPH VARIABLE
  nbPlayerSelected5 <- reactive({
    req(input$goButton5)
    input$nbPlayerSelected5
  })
  
  # - STATS DT
  statsDT5 <- reactive({
    req(categorySelected5Graph(), teamSelected5(), statSelected5(), positionSelected5(), salaryLow5(), salaryHigh5(), ageLow5(), ageHigh5(), nbPlayerSelected5())
    if (categorySelected5Graph() == "Classic") {
      getStatsCustom(selectedTeam = teamSelected5(), statType = statSelected5(), position = positionSelected5(), 
                     salaryLow = salaryLow5() * 1000000, salaryHigh = salaryHigh5() * 1000000, 
                     ageLow = ageLow5(), ageHigh = ageHigh5(), 
                     nbPlayer = nbPlayerSelected5(), 
                     DTgamesPlayed = dicoPlayerMinute(), DTplayerPres = dicoPlayerFich(), DT = nbaDatasDTmerged())
    } else if (categorySelected5Graph() == "Shooting") {
      getShootingCustom(selectedTeam = teamSelected5(), shootType = statSelected5(), position = positionSelected5(), 
                        salaryLow = salaryLow5() * 1000000, salaryHigh = salaryHigh5() * 1000000, 
                        ageLow = ageLow5(), ageHigh = ageHigh5(), 
                        nbPlayer = nbPlayerSelected5(), 
                        DTgamesPlayed = dicoPlayerMinute(), DTplayerPres = dicoPlayerFich(), DT = nbaDatasDTmerged())
    }
  })
  
  # - CUSTOM GRAPH
  output$customGraph5 <- renderPlotly({
    input$goButton5
    req(statsDT5())
    if (categorySelected5Graph() == "Classic") {
      getStatCustomGraph(statSelected5(), statsDT5())
    } else if (categorySelected5Graph() == "Shooting") {
      getShootingCustomGraph(statSelected5(), statsDT5())
    }
  })
  
  # - DATA TABLE CLICK EVENT
  dataPlayer5 <- reactive({event_data("plotly_click")})
  
  # - DATA TABLE PLAYER SUMMARY
  output$playerCarDT5 <- DT::renderDataTable({
    req(statsDT5())
    statsDT5()[dataPlayer5()$pointNumber + 1, .SD, .SDcols = c("player", "team", "Position", "Birth", "Age", "Experience")]
  }, options = list(pageLength = 5, dom = "t"))
  
  # - DATA TABLE PLAYER SALARY
  output$playerSalDT5 <- DT::renderDataTable({
    req(statsDT5())
    
    tmpDT <- DT::datatable(statsDT5()[dataPlayer5()$pointNumber + 1, .SD, 
                                      .SDcols = setdiff(colnames(statsDT5()), c("player", "Position", "Birth", "Age", "Experience", "Total", "team", "TotalGames", "AvgMin", "statMean", "made", "missed", "total", "FieldGoal"))
    ], options = list(dom = "t"))
    
    if (seasonSelected1() == "2022-2023") {
      tmpDT <- formatCurrency(tmpDT, 1:6, digits = 0)
    } else {
      tmpDT <- formatCurrency(tmpDT, 1, digits = 0)
    }
  })
  
  ############################################################################
  ################################### TAB 6 ##################################
  ############################################################################
  
  teamOpp6 <- reactive({
    input$teamOpp6
  })
  
  statSelected6 <- reactive({
    input$typeStat6
  })
  
  output$teamStatDT6 <- DT::renderDataTable({
    req(teamOpp6(), statSelected6())
    getTeamCustomDT(typeStat = statSelected6(), teamOpp = teamOpp6(), DT = teamStatSum())
  }, options = list(pageLength = 30, dom = "t"))
  
}

shinyApp(ui, server, options = list(launch.browser = T))
#shinyApp(ui, server)