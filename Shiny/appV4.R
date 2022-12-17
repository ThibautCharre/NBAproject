###
# Packages
###
# Website
library(shiny)
library(shinycssloaders)
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
ui <- navbarPage(
  
  title = "Stat-IB",
  
  # include css file
  includeCSS(path = "Shiny/www/stylesheet.css"),
  includeScript(path = "Shiny/www/effect.js"),
  div(
    class = "busy",
    img(src = "https://i.gifer.com/XOsX.gif")
  ),
  img(src = "Shiny/www/tab1Basket.jpg"),
  # div(
  #   class = "busy2",
  #   img(src = "https://i.gifer.com/VqrR.gif")
  # ),
  
  tabPanel("welcome",
           
           ############################################################################
           ################################### TAB 1 ##################################
           ############################################################################
           
           h1("Stat-IB : Intelligent Basketball"),
           h2("First Words"),
           tags$p(class = "introText", 
                  HTML("Welcome ! 
                      <br>Stat-IB is a data analysis project created in 2022.
                      <br>Its main objective is to display NBA statistics in graphical and interactive ways.
                      <br>Statistics are calculated in-live and are based on a specific part of a specific season (playoffs vs. regular season).
                      <br>So before going further, let's select the season to be exploited.")),
           #verbatimTextOutput(outputId = "introText1"),
           h2("Data Selection"),
           fluidRow(
             selectInput(inputId = "seasonSelected1", label = "Season", choices = c("2020-2021"), selected = c("2020-2021"), width = "12.5%"),
             selectInput(inputId = "seasonTypeSelected1", label = "Period", choices = c("Regular Season", "Playoffs"), selected = c("Regular Season"), width = "12.5%"),
             actionButton(inputId = "goButton1", label = "GO!", width = "12.5%", onclick = "$(tab).removeClass('disabled'); document.getElementById('resetButton').disabled = false;document.getElementById('goButton1').disabled = true;"),
             actionButton(inputId = "resetButton", label = "RESET!", width = "12.5%")
           ),
           fluidRow(
             verbatimTextOutput(outputId = "confirmationText1")
           )
           
  ),
  
  
  
  tabPanel("teamDash",
           
           ############################################################################
           ################################### TAB 2 ##################################
           ############################################################################
           
           h1("Teams Performances & Players Contributions"),
           h2("NBA League"),
           fluidRow(
             column(withSpinner(leafletOutput(outputId = "usmap2", height = 500), type = 5), width = 7),
             column(
               verticalLayout(
                 verbatimTextOutput(outputId = "selectedTeamText2"), 
                 uiOutput(outputId = "goButton2")
               ), width = 5)
           ),
           uiOutput(outputId = "allItems2"),
           h2("End !!")
  ),
  
  
  
  tabPanel("playerDash",
           
           ############################################################################
           ################################### TAB 3 ##################################
           ############################################################################ 
           
           h1("Impact of a Players"),
           h2("Season Stats"),
           fluidRow(
             column(
               selectInput(inputId = "selectedTeam3", label = "Team", choices = "Team", selected = "Team", width = "30%"), 
               selectInput(inputId = "selectedPlayer3", label = "Player", choices = "Player", selected = "Player", width = "30%"),
               verbatimTextOutput("selectedPlayerPres3")
               , width = 4),
             column(withSpinner(plotlyOutput(outputId = "spiderChart3"), type = 5)
                    , width = 4),
             column(
               #verbatimTextOutput(outputId = "selectedPlayerText3"),
               dateRangeInput(inputId = "dateRange3", label = "Select a Range of Two Dates", 
                              start = Sys.Date(), end = Sys.Date(), min =  Sys.Date(), max = Sys.Date(), 
                              weekstart = 1, autoclose = FALSE, width = "50%"),
               actionButton(inputId = "goButton3", label = "GO !", width = "40%")
               , width = 4)
           ),
           uiOutput(outputId = "allItems3"),
           h2("End !!")
  ),
  
  
  
  tabPanel("kingCourt", 
           
           ############################################################################
           ################################### TAB 4 ##################################
           ############################################################################
           
           h1("Who is the Best Shooter ?"),
           h2("Shooting Positions"),
           fluidRow(
             column(
               verticalLayout(
                 selectInput(inputId = "selectedTeam4", label = "Team", choices = "Team", selected = "Team", width = "50%"), 
                 selectInput(inputId = "selectedPlayer4", label = "Player", choices = "Player", selected = "Player", width = "50%"),
                 verbatimTextOutput(outputId = "selectedPlayerText4"),
                 actionButton(inputId = "goButton4", label = "GO !", width = "30%"), 
                 plotlyOutput(outputId = "playerShotsSum4"))
               , width = 4),
             column(
               tabsetPanel(
                 tabPanel("Coordinates", withSpinner(plotlyOutput(outputId = "playerShotsChart4", width = "823px", height = "850px"), type = 5)), 
                 tabPanel("Efficiency", withSpinner(plotlyOutput(outputId = "playerEfficiencyChart4", width = "823px", height = "850px"), type = 5)))
               , width = 8)
           ), 
           h2("Assists Distribution"), 
           uiOutput(outputId = "allItems4"),
           h2("End !!")
           
  ), 
  
  
  tabPanel("moneyball",
           
           ############################################################################
           ################################### TAB 5 ##################################
           ############################################################################
           
           h1("NBA Leaders"),
           h2("Floor is Yours"),
           fluidRow(
             column(
               verticalLayout(
                 selectInput(inputId = "categorySelected5", label = "Category", choices = c("Classic", "Shooting"), selected = "Classic", width = "50%"),
                 selectInput(inputId = "statSelected5", label = "Stat", choices = "Stats", selected = "Stats", width = "50%"),
                 selectInput(inputId = "selectedTeam5", label = "Team", choices = "All", selected = "All", width = "50%"),
                 selectInput(inputId = "positionSelected5", label = "Position", choices = c("All", "PG", "SG", "SF", "PF", "C"), selected = "All", multiple = TRUE,  width = "50%"),
                 sliderInput(inputId = "salarySelected5", label = "Salary (million $)", min = 0, max = 100, value = c(0, 100), step = 1, width = "50%"), 
                 sliderInput(inputId = "ageSelected5", label = "Age", min = 18, max = 50, value = c(18, 50), step = 1, width = "50%"),
                 numericInput(inputId = "nbPlayerSelected5", label = "Nb Players", value = 15, min = 10, max = 50, step = 1, width = "50%"))
               , width = 4),
             column(plotlyOutput(outputId = "customGraph5", width = "auto", height = "800px"), width = 8)
           ), 
           h2("End !!")
  )
  
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
    gePlayersFich(seasonSelected1(), seasonTypeSelected1(), DT = dicoPlayerMinute())
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
      "Select a Team on the Map \n\nTips : Check your knowledge by clicking on a State"
    } else {
      paste("Selected: ", longNameSelected2(), "\n\nContinue ?", sep = "")
    }
  })
  
  # - RENDERUI ACTIONBUTTON
  output$goButton2 <- renderUI({
    if (reactTextSelectedTeam1$value2 == TRUE) {
      actionButton(inputId = "goButton2", label = "GO!", width = "20%")
    }
  })
  
  # - SHORT NAME TEAM VARIABLE
  shortNameSelected2 <- eventReactive(input$goButton2, {
    getShortTeamName(longName = input$usmap2_marker_click$id)
  })
  
  # - TEAM LOGO
  output$logo2 <- renderImage(list(src = paste("Logos/", shortNameSelected2(), ".png", sep = ""), contentType = "image/png", width = "40%", height = "auto"), deleteFile = FALSE)
  
  # - TEAM RANKING DT
  output$teamRank2 <- renderDataTable({
    getRankingStreakTeam(selectedTeam = shortNameSelected2(), listResultTeam = resultsByTeamList())
  }, options = list(pageLength = 9, dom = "tp"))
  
  # - STAT SELECTION RENDER UI
  observeEvent(input$goButton2, {
    output$statSelected2 <- renderUI({
      selectInput(inputId = "statSelected2", 
                  label = "Category", 
                  choices = c("Points", "Assists", "Rebounds", "Blocks", "Steals"), width = "20%")
    })
  })
  
  # - STAT SELECTED VARIABLE
  statSelected2 <- reactive({input$statSelected2})
  
  # - NUMBER PLAYER GRAPH RENDER UI
  observeEvent(input$goButton2, {
    output$nbPlayerChart2 <- renderUI({
      numericInput(inputId = "nbPlayerChart2",
                   label = "Nb Players",
                   value = 10, min = 5, max = 25, step = 1,
                   width = "20%")
    })
  })
  
  # - NUMBER PLAYER GRAPH VARIABLE
  nbPlayerChart2 <- reactive({input$nbPlayerChart2})
  
  # - TEAM BAR CHART GRAPH
  output$teamBarChart2 <- renderPlotly({
    req(statSelected2())
    getTeamStatsGraph(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), DT = nbaDatasDTmerged())
  })
  
  # - TEAM PIE CHART GRAPH
  output$teamPieChart2 <- renderPlotly({
    req(statSelected2(), req(nbPlayerChart2()))
    getTeamStatsPerPlayerChart(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), nbPlayers = nbPlayerChart2(), DTroster = dicoPlayerMinute(), DT = nbaDatasDTmerged())
  })
  
  # - BEST GAMES DT
  output$bestGames2 <- renderDataTable({
    req(statSelected2())
    getBestGames(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), nbGames = 20, DTroster = dicoPlayerMinute(), DT = nbaDatasDTmerged())
  }, options = list(pageLength = 5, dom = "tp"))
  
  # - ALL ITEMS RENDER UI
  output$allItems2 <- renderUI({
    
    input$goButton2
    
    tagList(
      h2("Team results and schedule"),
      fluidRow(
        column(imageOutput(outputId = "logo2"), width = 5),
        column(dataTableOutput(outputId = "teamRank2"), width = 7)
      ),
      
      # Fluid row to select type of stats
      h2("Team Statistics"),
      fluidRow(
        column(
          wellPanel(
            plotlyOutput(outputId = "teamBarChart2"),
            uiOutput(outputId = "statSelected2", inline = TRUE)
          )
          , width = 6),
        column(
          wellPanel(
            plotlyOutput(outputId = "teamPieChart2"),
            uiOutput(outputId = "nbPlayerChart2")
          )
          , width = 6)
      ),
      h2("Best Performances"),
      fluidRow(
        column(dataTableOutput(outputId = "bestGames2"), width = 6, offset = 3)
      )
    )
  })
  
  ############################################################################
  ################################### TAB 3 ##################################
  ############################################################################
  
  # UPDATE OF SELECTINPUT BOX TEAM AND ROSTER
  observe({
    updateSelectInput(inputId = "selectedTeam3", label = "Team", choices = c("Team", nbaTeams()), selected = "Team")
  })
  
  roster3 <- reactive({getListRoster(team = getShortTeamName(input$selectedTeam3), DT = dicoPlayerMinute())})
  
  observe({
    updateSelectInput(inputId = "selectedPlayer3", label = "Player", 
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
  
  # - SHORT NAME TEAM FIXED VARIABLE
  shortNameSelected3Graph <- eventReactive(input$goButton3, {
    getShortTeamName(longName = input$selectedTeam3)
  })
  
  # - PLAYER NAME FIXED VARIABLE
  playerSelected3Graph <- eventReactive(input$goButton3, {
    input$selectedPlayer3
  })
  
  # - PLAYER PRESENTATION TEXT
  output$selectedPlayerPres3 <- renderText({
    if (playerSelected3() != "Player" | is.null(playerSelected3())) {
      paste("Age: ", dicoPlayerFich()[player == playerSelected3(), Age], 
            "\nExperience: ", dicoPlayerFich()[player == playerSelected3(), Experience], "years", 
            "\nPosition: ", dicoPlayerFich()[player == playerSelected3(), Position],
            "\nSalary: ", round(dicoPlayerFich()[player == playerSelected3(), Salary]/1000000, 3), "M $")
    } else {
      paste("Age: ",  
            "\nExperience: ",  
            "\nPosition: ", 
            "\nSalary: ")
    }
  })
  
  # - PLAYER SELECTED TEXT
  output$selectedPlayerText3 <- renderText({
    if (playerSelected3() == "Player" | is.null(playerSelected3())) {
      "Select a Player"
    } else {
      paste("Selected: ", playerSelected3(), "\n\nContinue ?", sep = "")
    }
  })
  
  # - SPIDER CHART GRAPH
  output$spiderChart3 <- renderPlotly({
    tryCatch({
      getPlayerClassicStatsChart(selectedTeam = shortNameSelected3(), selectedPlayer = playerSelected3(), DTgamesPlayed = dicoPlayerMinute(), DT = nbaDatasDTmerged())
    }, warning = function(w) {
      NULL
    })
  })
  
  # - UPDATE DATE RANGE OUTPUT
  observe({
    updateDateRangeInput(inputId = "dateRange3", start = min(nbaDatasDTmerged()$Date), end = max(nbaDatasDTmerged()$Date), min =  min(nbaDatasDTmerged()$Date), max = max(nbaDatasDTmerged()$Date))
  })
  
  # - CALENDAR RANGE VARIABLES
  dateRange3Min <- eventReactive(input$goButton3, {
    input$dateRange3[1]
  })
  dateRange3Max <- eventReactive(input$goButton3, {
    input$dateRange3[2]
  })
  
  # - FREQ CHART RENDER UI
  observeEvent(input$goButton3, {
    output$selectedFreq3 <- renderUI({
      radioButtons(inputId = "selectedFreq3", label = "Select a Player", choices = c("Month", "Week"), selected = "Month", inline = TRUE, width = "100%")
    })
  })
  
  # - FREQ CHART VARIABLE
  freqSelected3 <- reactive({input$selectedFreq3})
  
  # - nbaDatasDT PLAYER
  playerDT3 <- reactive({
    req(playerSelected3Graph(), dateRange3Min(), dateRange3Max())
    getPlayerDT(shortNameSelected3Graph(), playerSelected3Graph(), startDate = dateRange3Min(), endDate = dateRange3Max(), DT = nbaDatasDTmerged())
  })
  
  # - PLAYER DATE BAR CHART
  output$dateBarChart3 <- renderPlotly({
    req(input$selectedFreq3)
    getAdvancedPlayerStatsPerDateGraph(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), perMonthWeek = freqSelected3(), 
                                       DTcalendar = NBAcalendar(), DTplayer =  playerDT3()) 
  })
  
  # - PLAYER PERIOD BAR CHART
  output$periodBarChart3 <- renderPlotly({
    getPlayerPeriodStatsChart(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), DTplayer = playerDT3())
  })
  
  # - IMPACT PLAYER DT
  listImpactDT3 <- reactive({
    getPlayerImpact(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), playerDT3())
  })
  
  #req(listImpactDT3())
  output$impactFG3 <- renderDataTable({
    listImpactDT3()[["ShootImpact"]]
  }, options = list(dom = "t"))
  
  output$impactPluMin3 <- renderDataTable({
    listImpactDT3()[["PointImpact"]]
  }, options = list(dom = "t"))
  
  # ALL ITEMS RENDER UI
  output$allItems3 <- renderUI({
    
    input$goButton3
    
    tagList(
      h2("Stats per Period & per Quarters"),
      fluidRow(
        column(
          wellPanel(
            plotlyOutput(outputId = "dateBarChart3", width = "100%", height = "auto"),
            uiOutput("selectedFreq3"), 
            uiOutput("dateRange3")
          ), width = 6),
        column(plotlyOutput(outputId = "periodBarChart3", width = "100%", height = "auto"), width = 6)
      ),
      h2("Efficiency on Team and vs. Opponents"),
      fluidRow(
        column(dataTableOutput("impactFG3"), width = 6), 
        column(dataTableOutput("impactPluMin3"), width = 6)
      )
    )
  })
  
  ############################################################################
  ################################### TAB 4 ##################################
  ############################################################################
  
  # UPDATE OF SELECTINPUT BOX TEAM AND ROSTER
  observe({
    updateSelectInput(inputId = "selectedTeam4", label = "Team", choices = c("Team", nbaTeams()), selected = "Team")
  })
  
  roster4 <- reactive({getListRoster(team = getShortTeamName(input$selectedTeam4), DT = dicoPlayerMinute())})
  
  observe({
    updateSelectInput(inputId = "selectedPlayer4", label = "Player", 
                      choices = c("Player", roster4()), 
                      selected = "Player")
  })
  
  # - SHORT NAME TEAM VARIABLE
  shortNameSelected4 <- reactive({
    getShortTeamName(longName = input$selectedTeam4)
  })
  
  # - PLAYER SELECTED VARIABLE
  playerSelected4 <- reactive({
    input$selectedPlayer4
  })
  
  # - PLAYER SELECTED FIXED VARIABLE
  playerSelected4Graph <- eventReactive(input$goButton4, {
    input$selectedPlayer4
  })
  
  # - PLAYER SELECTED TEXT
  output$selectedPlayerText4 <- renderText({
    if (is.null(playerSelected4()) | playerSelected4() == "Player") {
      "Select a Player"
    } else {
      paste("Selected: ", playerSelected4(), "\n\nContinue ?", sep = "")
    }
  })
  
  # - PLAYER SHOOTING SUMMARY GRAPH
  output$playerShotsSum4 <- renderPlotly({
    tryCatch({
      getPlayerGlobalShootingGraph(playerSelected4(), DT = nbaDatasDTmerged())
    }, warning = function(w) {
      NULL
    }, error = function(e) {
      NULL
    })
  })
  
  # - PLAYER SHOOTING CHARTS
  listCharts4 <- eventReactive(input$goButton4, {
    list(ShotChart = getShotChart(selectedPlayer = playerSelected4Graph(), shootingDatas = "PlayerChart", DT = nbaDatasDTmerged()), 
         EfficientChart = getShotChart(selectedPlayer = playerSelected4Graph(), shootingDatas = "EfficientChart", DT = nbaDatasDTmerged()))
  })
  
  # - COORDINATES CHART
  output$playerShotsChart4 <- renderPlotly({
    if (input$goButton4 == 0) {
      getShotChart()
    } else {
      listCharts4()[["ShotChart"]]
    }
  })
  
  # - EFFICIENCY CHART
  output$playerEfficiencyChart4 <- renderPlotly({
    listCharts4()[["EfficientChart"]]
  })
  
  # - AREA SELECTED RENDER UI
  observeEvent(input$goButton4, {
    output$areaSelected4 <- renderUI({
      selectInput(inputId = "areaSelected4", label = "Area", choices = area, selected = area[1])
    })
  })
  
  # - AREA SELECTED VARIABLE
  areaSelected4 <- reactive({
    input$areaSelected4
  })
  
  # - ASSISTS DISTRIBUTION LIST
  listAssists4 <- reactive({
    req(areaSelected4())
    getAssistsShotsPlayer(selectedPlayer = playerSelected4Graph(), selectedArea = areaSelected4(), DT = nbaDatasDTmerged())
  })
  
  output$globalAssist4 <- renderDataTable({
    listAssists4()[["Global"]]
  }, options = list(pageLength = 5, dom = "tp"))
  
  output$detailedAssist4 <- renderDataTable({
    listAssists4()[["Detailed"]]
  }, options = list(pageLength = 5, dom = "tp"))
  
  # ALL ITEMS RENDER UI
  output$allItems4 <- renderUI({
    
    input$goButton4
    
    tagList(
      fluidRow(uiOutput(outputId = "areaSelected4")),
      fluidRow(
        column(dataTableOutput(outputId = "globalAssist4"), width = 6), 
        column(dataTableOutput(outputId = "detailedAssist4"), width = 6) 
      )
    )
  })
  
  ############################################################################
  ################################### TAB 5 ##################################
  ############################################################################
  
  # UPDATE SELECTED TEAM
  observe({
    updateSelectInput(inputId = "selectedTeam5", label = "Team", choices = c("All", nbaTeams()), selected = "All")
  })
  
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
    updateSelectInput(inputId = "statSelected5", label = "Stat", choices = choicesVec(), selected = choicesVec()[1])
  })
  
  # - STAT SELECTED VARIABLE
  statSelected5 <- reactive({input$statSelected5})
  
  # - TEAM SELECTED VARIABLE
  teamSelected5 <- reactive({getShortTeamName(input$selectedTeam5)})
  
  # - POSITION SELECTED VARIABLE
  positionSelected5 <- reactive({input$positionSelected5})
  
  # - UPDATE SALARY SLIDER INPUT
  minSliderSalary <- reactive({
    if (teamSelected5() == "All") {
      floor(min(dicoPlayerFich()$Salary/1000000))
    } else {
      floor(min(dicoPlayerFich()[team == teamSelected5()]$Salary/1000000))
    }
  })
  
  maxSliderSalary <- reactive({
    if (teamSelected5() == "All") {
      ceiling(max(dicoPlayerFich()$Salary/1000000))
    } else {
      ceiling(max(dicoPlayerFich()[team == teamSelected5()]$Salary/1000000))
    }
  })
  
  observe({
    updateSliderInput(inputId = "salarySelected5", label = "Salary (million $)", min = minSliderSalary(), max = maxSliderSalary(), value = c(minSliderSalary(), maxSliderSalary()))
  })
  
  # - SALARY SELECTED VARIABLE
  salaryLow5 <- reactive({input$salarySelected5[1]})
  salaryHigh5 <- reactive({input$salarySelected5[2]})
  
  # - UPDATE AGE SLIDER INPUT
  minSliderAge <- reactive({
    if (teamSelected5() == "All") {
      min(dicoPlayerFich()$Age)
    } else {
      min(dicoPlayerFich()[team == teamSelected5()]$Age)
    }
  })
  
  maxSliderAge <- reactive({
    if (teamSelected5() == "All") {
      max(dicoPlayerFich()$Age)
    } else {
      max(dicoPlayerFich()[team == teamSelected5()]$Age)
    }
  })
  
  observe({
    updateSliderInput(inputId = "ageSelected5", label = "Salary (million $)", min = minSliderAge(), max = maxSliderAge(), value = c(minSliderAge(), maxSliderAge()))
  })
  
  # - AGE SELECTED VARIABLE
  ageLow5 <- reactive({input$ageSelected5[1]})
  ageHigh5 <- reactive({input$ageSelected5[2]})
  
  # - NB PLAYER GRAPH VARIABLE
  nbPlayerSelected5 <- reactive({input$nbPlayerSelected5})
  
  # - CUSTOM GRAPH
  output$customGraph5 <- renderPlotly({
    tryCatch({
      
      if (categorySelected5() == "Classic") {
        
        getStatCustomGraph(selectedTeam = teamSelected5(), statType = statSelected5(), position = positionSelected5(), 
                           salaryLow = salaryLow5() * 1000000, salaryHigh = salaryHigh5() * 1000000, 
                           ageLow = ageLow5(), ageHigh = ageHigh5(), 
                           nbPlayer = nbPlayerSelected5(), 
                           DTgamesPlayed = dicoPlayerMinute(), DTplayerPres = dicoPlayerFich(), DT = nbaDatasDTmerged())
        
      } else if (categorySelected5() == "Shooting") {
        
        getShootingCustomGraph(selectedTeam = teamSelected5(), shootType = statSelected5(), position = positionSelected5(), 
                               salaryLow = salaryLow5() * 1000000, salaryHigh = salaryHigh5() * 1000000, 
                               ageLow = ageLow5(), ageHigh = ageHigh5(), 
                               nbPlayer = nbPlayerSelected5(), DTplayerPres = dicoPlayerFich(), DT = nbaDatasDTmerged())
        
      } 
    }, error = function(e) {
      NULL
    }, warning = function(w) {
      NULL
    })
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))