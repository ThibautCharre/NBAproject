###
# Packages
###
# Website
library(shiny)
library(shinycssloaders)
library(DT)
library(profvis)
# Icon
library(fontawesome)
# Data Treatment
library(data.table)
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
source("Functions/dataAnalysisFunc.R")
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
               # tags$div(
               #   class = "busy",
               #   img(src = "https://i.gifer.com/XOsX.gif")
               # ),
               
               tabPanel("New Comers", value = "welcome",
                        
                        #tags$header(style = "background-image: url(BckPics/tab1Basket.jpg);"), 
                        
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
                                 selectInput(inputId = "seasonSelected1", label = "Season", choices = c("2020-2021", "2021-2022"), selected = c("2020-2021")),
                                 selectInput(inputId = "seasonTypeSelected1", label = "Period", choices = c("Regular Season", "Playoffs"), selected = c("Regular Season")),
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
                        verbatimTextOutput(outputId = "confirmationText1")
               ),
               
               
               
               tabPanel("Team Building", value = "teamDash",
                        
                        #tags$header(style = "background-image: url(BckPics/tab2Basket.jpg);"),
                        
                        ############################################################################
                        ################################### TAB 2 ##################################
                        ############################################################################
                        
                        h2("NBA League"),
                        leafletOutput(outputId = "usmap2", height = "500px", width = "100%"),
                        verbatimTextOutput(outputId = "selectedTeamText2"),
                        h2("Results and schedule"),
                        fluidRow(
                          imageOutput(outputId = "logo2", width = "auto", height = "auto"),
                          DT::dataTableOutput(outputId = "teamRank2")
                        ),
                        h2("Statistics"),
                        tags$div(class = "paramCharts", 
                                 uiOutput(outputId = "statSelected2"),
                                 uiOutput(outputId = "nbPlayerChart2")
                        ),
                        fluidRow(
                          column(plotlyOutput(outputId = "teamBarChart2"), width = 6),
                          column(plotlyOutput(outputId = "teamPieChart2"), width = 6)
                        ),
                        h2("Best Performances"),
                        fluidRow(
                          DT::dataTableOutput(outputId = "bestGames2")
                        )
               ),
               
               
               
               tabPanel("Business Card", value = "teamPlayer",
                        
                        ############################################################################
                        ################################### TAB 3 ##################################
                        ############################################################################ 
                        
                        h2("Season Stats"),
                        tags$div(class = "param3container",
                                 selectInput(inputId = "selectedTeam3", label = "TEAM", choices = "Team", selected = "Team"), 
                                 selectInput(inputId = "selectedPlayer3", label = "PLAYER", choices = "Player", selected = "Player"),
                                 dateRangeInput(inputId = "dateRange3", label = "SCHEDULE", 
                                                start = Sys.Date(), end = Sys.Date(), min =  Sys.Date(), max = Sys.Date(), 
                                                weekstart = 1, autoclose = FALSE)
                        ),
                        fluidRow(
                          column(DT::dataTableOutput("sumGamesDT3", width = "100%"), width = 6),
                          column(plotlyOutput(outputId = "spiderChart3", width = "650px"), width = 6)
                        ),
                        tags$div(class = "basketball",
                                 tags$div(class = "lineVert"),
                                 tags$div(class = "lineCurvLeft"),
                                 tags$div(class = "lineCurvRight"),
                                 actionButton(inputId = "goButton3", label = "GO!")
                        ),
                        h2("Player Efficiency"),
                        uiOutput(outputId = "allItems3")
               ),
               
               
               
               tabPanel("Pros vs. Cons", value = "kingCourt", 
                        
                        ############################################################################
                        ################################### TAB 4 ##################################
                        ############################################################################
                        
                        h2("General Shooting"),
                        tags$div(class = "param4container",
                                 selectInput(inputId = "selectedTeam4", label = "TEAM", choices = "Team", selected = "Team"), 
                                 selectInput(inputId = "selectedPlayer4", label = "PLAYER", choices = "Player", selected = "Player"),
                                 dateRangeInput(inputId = "dateRange4", label = "SCHEDULE", 
                                                start = Sys.Date(), end = Sys.Date(), min =  Sys.Date(), max = Sys.Date(), 
                                                weekstart = 1, autoclose = FALSE)
                        ),
                        fluidRow(
                          column(DT::dataTableOutput(outputId = "playerShotsDT4", width = "100%"), width = 6),
                          column(plotlyOutput(outputId = "playerShotsSum4", width = "100%"), width = 6)
                        ),
                        tags$div(class = "basketball",
                                 tags$div(class = "lineVert"),
                                 tags$div(class = "lineCurvLeft"),
                                 tags$div(class = "lineCurvRight"),
                                 actionButton(inputId = "goButton4", label = "GO!")
                        ),
                        h2("Shooting Spots & Assists"),
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
                                 sliderInput(inputId = "salarySelected5", label = "SQLQRY (M$)", min = 0, max = 100, value = c(0, 100), step = 1), 
                                 sliderInput(inputId = "ageSelected5", label = "AGE", min = 18, max = 50, value = c(18, 50), step = 1)
                        ),
                        h2("Bubble Visualization"),
                        numericInput(inputId = "nbPlayerSelected5", label = "NB PLAYERS", value = 15, min = 10, max = 50, step = 1, width = "200px"),
                        plotlyOutput(outputId = "customGraph5", width = "75%", height = "800px"),
                        h2("Player Card"),
                        fluidRow(
                          DT::dataTableOutput(outputId = "playerCarDT5")
                        ),
                        h2("Player Salary"),
                        fluidRow(
                          DT::dataTableOutput(outputId = "playerSalDT5")
                        )
               )
    ),
    
    tags$body(style = "background-image: url(BckPics/tab2Basket.jpg);"),
    tags$nav(tags$div(class = "hamb", img(src = "hamb.png", width = "100px")))
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
  dicoPlayerMinute <- reactive({
    fread(file = paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/minutesSummary.csv", sep = ""))
  })
  
  # - DATAS DOWNLOADED : DT with players characteristics (position, salary, age)
  dicoPlayerFich <- reactive({
    gePlayersFich(seasonSelected1(), seasonTypeSelected1())
  })
  
  # - DATAS DOWNLOADED : LIST of vector players and nbaDatas DT
  seasonDatasList <- reactive({
    cleanDatas(seasonSelected1(), seasonTypeSelected1())
  })
  
  # - DATAS DOWNLOADED : Vector of NBA teams
  nbaTeams <- reactive({
    seasonDatasList()[["nbaTeams"]]
  })
  
  # - DATAS DOWNLOADED : Vector of NBA players
  nbaPlayers <- reactive({
    seasonDatasList()[["nbaPlayers"]]
  })
  
  # - DATAS DOWNLOADED : DT Reference for other functions
  nbaDatasDT <- reactive({
    seasonDatasList()[["nbaDatas"]]
  })
  
  # - DATAS DOWNLOADED : DT NBA Calendar
  NBAcalendar <- reactive({
    getNBAcalendar(seasonSelected1(), DT = nbaDatasDT())
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
  output$logo2 <- renderImage(list(src = paste("Logos/", shortNameSelected2(), "-min", ".png", sep = ""), contentType = "image/png", height = "250px", width = "auto"), deleteFile = FALSE)
  
  # - TEAM RANKING DT
  output$teamRank2 <- DT::renderDataTable({
    req(shortNameSelected2())
    getRankingStreakTeam(selectedTeam = shortNameSelected2(), listResultTeam = resultsByTeamList())
  }, options = list(pageLength = 9, dom = "tp"))
  
  # - STAT SELECTION RENDER UI
  output$statSelected2 <- renderUI({
    req(shortNameSelected2())
    selectInput(inputId = "statSelected2", 
                label = "Category", 
                choices = c("Points", "Assists", "Rebounds", "Blocks", "Steals"), width = "150px")
  })
  
  # - STAT SELECTED VARIABLE
  statSelected2 <- reactive({input$statSelected2})
  
  # - NUMBER PLAYER GRAPH RENDER UI
  output$nbPlayerChart2 <- renderUI({
    req(shortNameSelected2())
    numericInput(inputId = "nbPlayerChart2",
                 label = "Nb Players",
                 value = 10, min = 5, max = 25, step = 1,
                 width = "150px")
  })
  
  # - NUMBER PLAYER GRAPH VARIABLE
  nbPlayerChart2 <- reactive({input$nbPlayerChart2})
  
  # - TEAM BAR CHART GRAPH
  output$teamBarChart2 <- renderPlotly({
    req(statSelected2(), shortNameSelected2())
    getTeamStatsGraph(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), DT = nbaDatasDTmerged())
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
  
  # - SPIDER CHART GRAPH
  output$spiderChart3 <- renderPlotly({
    #req(playerSelected3() != "Player")
    tryCatch({
      getPlayerClassicStatsChart(selectedTeam = shortNameSelected3(), selectedPlayer = playerSelected3(), startDate = dateRange3Min(), endDate = dateRange3Max(), DT = nbaDatasDTmerged())
    }, warning = function(w) {
      NULL
    }, error = function(e) {
      NULL
    })
  })
  
  # - SUMMARY GAMES DT
  output$sumGamesDT3 <- DT::renderDataTable({
    #req(playerSelected3() != "Player")
    tryCatch({
      getHistPlayerStats(selectedTeam = shortNameSelected3(), selectedPlayer = playerSelected3(), startDate = dateRange3Min(), endDate = dateRange3Max(), DTcalendar = NBAcalendar(), DT = nbaDatasDTmerged())
    }, warning = function(w) {
      NULL
    }, error = function(e) {
      NULL
    })
  }, options = list(pageLength = 5, dom = "tp"))
  
  # - FREQ CHART RENDER UI
  observeEvent(input$goButton3, {
    output$selectedFreq3 <- renderUI({
      radioButtons(inputId = "selectedFreq3", label = "Datas Frequency", choices = c("Month", "Week"), selected = "Month", inline = TRUE, width = "auto")
    })
  })
  
  # - FREQ CHART VARIABLE
  freqSelected3 <- reactive({input$selectedFreq3})
  
  # - SHORT NAME TEAM FIXED VARIABLE
  shortNameSelected3Graph <- eventReactive(input$goButton3, {
    getShortTeamName(longName = input$selectedTeam3)
  })
  
  # - PLAYER NAME FIXED VARIABLE
  playerSelected3Graph <- eventReactive(input$goButton3, {
    input$selectedPlayer3
  })
  
  # - nbaDatasDT PLAYER
  playerDT3 <- eventReactive(input$goButton3, {
    req(playerSelected3Graph() != "Player", shortNameSelected3Graph() != "Team", dateRange3Min(), dateRange3Max())
    getPlayerDT(shortNameSelected3Graph(), playerSelected3Graph(), startDate = dateRange3Min(), endDate = dateRange3Max(), DT = nbaDatasDTmerged())
  })
  
  # - PLAYER DATE BAR CHART
  output$dateBarChart3 <- renderPlotly({
    req(freqSelected3(), playerDT3())
    getAdvancedPlayerStatsPerDateGraph(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), perMonthWeek = freqSelected3(), 
                                       DTcalendar = NBAcalendar(), DTplayer =  playerDT3()) 
  })
  
  # - PLAYER PERIOD BAR CHART
  output$periodBarChart3 <- renderPlotly({
    req(playerDT3())
    getPlayerPeriodStatsChart(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), DTplayer = playerDT3())
  })
  
  # - IMPACT PLAYER DT
  listImpactDT3 <- reactive({
    req(playerDT3())
    getPlayerImpact(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), playerDT3())
  })
  
  output$impactFG3 <- renderDataTable({
    req(listImpactDT3())
    listImpactDT3()[["ShootImpact"]]
  }, options = list(dom = "t"))
  
  output$impactPluMin3 <- renderDataTable({
    req(listImpactDT3())
    listImpactDT3()[["PointImpact"]]
  }, options = list(dom = "t"))
  
  # ALL ITEMS RENDER UI
  output$allItems3 <- renderUI({
    
    input$goButton3
    
    tagList(
      uiOutput("selectedFreq3"),
      fluidRow(
        column(plotlyOutput(outputId = "dateBarChart3", width = "100%"), width = 6),
        column(DT::dataTableOutput("impactFG3", width = "100%"), width = 6)
      ),
      h2("Scoring Impact"),
      fluidRow(
        column(DT::dataTableOutput("impactPluMin3", width = "100%"), width = 6),
        column(plotlyOutput(outputId = "periodBarChart3", width = "100%"), width = 6)
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
    getPlayerGlobalShooting(shortNameSelected4(), playerSelected4(), dateRange4Min(), dateRange4Max(), nbaDatasDTmerged())
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
  
  # - TYPE CHART RENDER UI
  output$selectedChart4 <- renderUI({
    req(playerSelected4()!="Player", shortNameSelected4()!="Team")
    input$goButton4
    radioButtons(inputId = "selectedChart4", label = "Select a Chart", choices = c("Spots", "Efficiency"), selected = "Spots", inline = TRUE)
  })
  
  # - SELECT CHART VARIABLE
  chartType4 <- reactive({
    input$selectedChart4
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
  
  # - PLAYER SHOOTING LIST CHARTS
  listCharts4 <- reactive({
    req(playerSelected4Graph() != "Player", shortNameSelected4Graph() != "Team", dateRange4MinGraph(), dateRange4MaxGraph())
    list(ShotChart = getShotChart(selectedPlayer = playerSelected4Graph(), selectedTeam = shortNameSelected4Graph(), shootingDatas = "PlayerChart", startDate = dateRange4MinGraph(), endDate = dateRange4MaxGraph(), DT = nbaDatasDTmerged()),
         EfficientChart = getShotChart(selectedPlayer = playerSelected4Graph(), selectedTeam = shortNameSelected4Graph(), shootingDatas = "EfficientChart", startDate = dateRange4MinGraph(), endDate = dateRange4MaxGraph(), DT = nbaDatasDTmerged()))
  })
  
  # - PLAYER CHARTS
  output$playerShotsChart4 <- renderPlotly({
    req(listCharts4())
    if (chartType4() == "Spots"){
      listCharts4()[["ShotChart"]]
    } else if (chartType4() == "Efficiency") {
      listCharts4()[["EfficientChart"]]
    }
  })
  
  # - AREA SELECTED RENDER UI
  output$areaSelected4 <- renderUI({
    input$goButton4
    req(playerSelected4Graph() != "Player", shortNameSelected4Graph() != "Team")
    selectInput(inputId = "areaSelected4", label = "AREA", choices = area, selected = area[1])
  })
  
  # - AREA SELECTED VARIABLE
  areaSelected4 <- reactive({
    input$areaSelected4
  })
  
  # - ASSISTS DISTRIBUTION LIST
  listAssists4 <- reactive({
    req(playerSelected4Graph() != "Player", shortNameSelected4Graph() != "Team",dateRange4MinGraph(), dateRange4MaxGraph())
    getAssistsShotsPlayer(selectedPlayer = playerSelected4Graph(), selectedTeam = shortNameSelected4Graph(), startDate = dateRange4MinGraph(), endDate = dateRange4MaxGraph(), selectedArea = areaSelected4(), DT = nbaDatasDTmerged())
  })
  
  output$globalAssist4 <- DT::renderDataTable({
    req(listAssists4())
    listAssists4()[["Global"]]
  }, options = list(pageLength = 5, dom = "tp"))
  
  output$detailedAssist4 <- DT::renderDataTable({
    req(listAssists4())
    listAssists4()[["Detailed"]]
  }, options = list(pageLength = 5, dom = "tp"))
  
  # - RENDER UI ALLITEMS4
  output$allItems4 <- renderUI({
    
    input$goButton4
    
    tagList(
      fluidRow(
        column(
          uiOutput(outputId = "selectedChart4"),
          plotlyOutput(outputId = "playerShotsChart4", width = "830px", height = "850px")
          , width = 6),
        column(
          uiOutput(outputId = "areaSelected4"),
          DT::dataTableOutput(outputId = "globalAssist4", width = "100%"),
          DT::dataTableOutput(outputId = "detailedAssist4", width = "100%")
          , width = 6)
      )
    )
  })
  
  ############################################################################
  ################################### TAB 5 ##################################
  ############################################################################
  
  # - CATEGORY SELECTED VARIABLE
  categorySelected5 <- reactive({input$categorySelected5})
  
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
  statSelected5 <- reactive({input$statSelected5})
  
  # UPDATE SELECTED TEAM
  observe({
    updateSelectInput(inputId = "selectedTeam5", label = "TEAM", choices = c("All", nbaTeams()), selected = "All")
  })
  
  # - TEAM SELECTED VARIABLE
  teamSelected5 <- reactive({getShortTeamName(input$selectedTeam5)})
  
  # - POSITION SELECTED VARIABLE
  positionSelected5 <- reactive({input$positionSelected5})
  
  minSliderSalary <- reactive({
    floor(min(dicoPlayerFich()$Salary/1000000))
  })
  
  maxSliderSalary <- reactive({
    ceiling(max(dicoPlayerFich()$Salary/1000000))+1
  })
  
  observe({
    updateSliderInput(inputId = "salarySelected5", label = "SALARY (M$)", min = minSliderSalary(), max = maxSliderSalary(), value = c(minSliderSalary(), maxSliderSalary()))
  })
  
  # - SALARY SELECTED VARIABLE
  salaryLow5 <- reactive({input$salarySelected5[1]})
  salaryHigh5 <- reactive({input$salarySelected5[2]})
  
  minSliderAge <- reactive({
    min(dicoPlayerFich()$Age)
  })
  
  maxSliderAge <- reactive({
    max(dicoPlayerFich()$Age)
  })
  
  observe({
    updateSliderInput(inputId = "ageSelected5", label = "AGE", min = minSliderAge(), max = maxSliderAge(), value = c(minSliderAge(), maxSliderAge()))
  })
  
  # - AGE SELECTED VARIABLE
  ageLow5 <- reactive({input$ageSelected5[1]})
  ageHigh5 <- reactive({input$ageSelected5[2]})
  
  # - NB PLAYER GRAPH VARIABLE
  nbPlayerSelected5 <- reactive({input$nbPlayerSelected5})
  
  # - STATS DT
  statsDT5 <- reactive({
    if (categorySelected5() == "Classic") {
      getStatsCustom(selectedTeam = teamSelected5(), statType = statSelected5(), position = positionSelected5(), 
                     salaryLow = salaryLow5() * 1000000, salaryHigh = salaryHigh5() * 1000000, 
                     ageLow = ageLow5(), ageHigh = ageHigh5(), 
                     nbPlayer = nbPlayerSelected5(), 
                     DTgamesPlayed = dicoPlayerMinute(), DTplayerPres = dicoPlayerFich(), DT = nbaDatasDTmerged())
    } else if (categorySelected5() == "Shooting") {
      getShootingCustom(selectedTeam = teamSelected5(), shootType = statSelected5(), position = positionSelected5(), 
                        salaryLow = salaryLow5() * 1000000, salaryHigh = salaryHigh5() * 1000000, 
                        ageLow = ageLow5(), ageHigh = ageHigh5(), 
                        nbPlayer = nbPlayerSelected5(), 
                        DTplayerPres = dicoPlayerFich(), DT = nbaDatasDTmerged())
    }
  })
  
  # - CUSTOM GRAPH
  output$customGraph5 <- renderPlotly({
    if (categorySelected5() == "Classic") {
      tryCatch({
        getStatCustomGraph(statSelected5(), statsDT5())
      }, error = function(e) {
        NULL
      })
    } else if (categorySelected5() == "Shooting") {
      tryCatch({
        getShootingCustomGraph(statSelected5(), statsDT5())
      }, error = function(e) {
        NULL
      })
    }
  })
  
  # - DATA TABLE CLICK EVENT
  dataPlayer5 <- reactive({event_data("plotly_click")})
  
  # - DATA TABLE PLAYER SUMMARY
  output$playerCarDT5 <- DT::renderDataTable({
    statsDT5()[dataPlayer5()$pointNumber + 1, .SD, .SDcols = c("player", "Position", "Birth", "Age", "Experience")]
  }, options = list(pageLength = 5, dom = "t"))
  
  # - DATA TABLE PLAYER SALARY
  output$playerSalDT5 <- DT::renderDataTable({
    tmpDT <- DT::datatable(statsDT5()[dataPlayer5()$pointNumber + 1, .SD, .SDcols = setdiff(colnames(statsDT5()), c("player", "Position", "Birth", "Age", "Experience", "Total", "Team", "TotalGames", "AvgMin", "statMean"))])
    if (seasonSelected1() == "2022-2023") {
      tmpDT <- formatCurrency(tmpDT, 1:6, digits = 0)
    } else {
      tmpDT <- formatCurrency(tmpDT, 1, digits = 0)
    }
  }, options = list(dom = "t"))
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
#shinyApp(ui, server)