###
# Packages
###
# Website
library(shiny)
library(shinydashboard)
library(shinycssloaders)
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
ui <- dashboardPage(
  
  # Header
  dashboardHeader(title = "Stat-IB"), 
  
  # Side Bar
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "You are Welcomed", tabName = "welcome"),
      menuItem(text = "The Other Dream Team", tabName = "teamDash"),
      menuItem(text = "Catch Me If You Can ?", tabName = "playerDash"), 
      menuItem(text = "King of the Court", tabName = "kingCourt"),
      menuItem(text = "MoneyBall", tabName = "moneyball")
    )
  ),
  
  # Dash Body  
  dashboardBody(
    # include css file
    includeCSS(path = "Shiny/www/stylesheet.css"),
    includeScript(path = "Shiny/www/effect.js"),
    div(
      class = "busy",
      img(src = "https://i.gifer.com/XOsX.gif")
    ),
    
    tabItems(
      
      ############################################################################
      ################################### TAB 1 ##################################
      ############################################################################
      
      tabItem(tabName = "welcome", 
              
              h1("Stat-IB : Intelligent Basketball"),
              h2("First Words"),
              verbatimTextOutput(outputId = "introText1"),
              h2("Data Selection"),
              fluidRow(
                column(selectInput(inputId = "seasonSelected1", label = "Select Season", choices = c("2020-2021"), selected = c("2020-2021"), width = "50%"), width = 4),
                column(selectInput(inputId = "seasonTypeSelected1", label = "Select Season Period", choices = c("Regular Season", "Playoffs"), selected = c("Regular Season"), width = "50%"), width = 4),
                column(actionButton(inputId = "goButton1", label = "GO!", width = "50%", onclick = "$(tab).removeClass('disabled')"), width = 4)
              ),
              fluidRow(
                verbatimTextOutput(outputId = "confirmationText1"),
                verbatimTextOutput(outputId = "test")
              )
              
      ),
      
      ############################################################################
      ################################### TAB 2 ##################################
      ############################################################################
      
      tabItem(tabName = "teamDash",
              h1("Teams Performances & Players Contributions"),
              h2("NBA League"),
              fluidRow(
                column(withSpinner(leafletOutput(outputId = "usmap2", height = 500), type = 5), width = 7),
                column(
                  verticalLayout(
                    verbatimTextOutput(outputId = "selectedTeamText2"), 
                    actionButton(inputId = "goButton2", label = "GO!", width = "20%")
                  ), width = 5)
              ),
              uiOutput(outputId = "allItems2"),
              h2("End !!")
      ),
      
      ############################################################################
      ################################### TAB 3 ##################################
      ############################################################################
      
      tabItem(tabName = "playerDash",
              h1("Impact of a Players"),
              h2("Season Stats"),
              fluidRow(
                column(
                  uiOutput("teamList3"), 
                  uiOutput("selectedPlayer3", inline = TRUE),
                  verbatimTextOutput("selectedPlayerPres3")
                  , width = 5),
                column(
                  verbatimTextOutput(outputId = "selectedPlayerText3"),
                  actionButton(inputId = "goButton3", label = "GO !", width = "40%") 
                  , width = 2),
                column(plotlyOutput(outputId = "spiderChart3")
                       , width = 5)
              ),
              uiOutput(outputId = "allItems3"),
              h2("End !!")
      ),
      
      ############################################################################
      ################################### TAB 4 ##################################
      ############################################################################
      
      tabItem(tabName = "kingCourt", 
              h1("Who is the Best Shooter ?"),
              h2("Shooting Positions"),
              fluidRow(
                column(
                  verticalLayout(
                    uiOutput(outputId = "teamList4"),
                    uiOutput(outputId = "selectedPlayer4"),
                    verbatimTextOutput(outputId = "selectedPlayerText4"),
                    actionButton(inputId = "goButton4", label = "GO !", width = "30%"), 
                    plotlyOutput(outputId = "playerShotsSum4"))
                  , width = 4),
                column(
                  tabsetPanel(
                    tabPanel("Coordinates", withSpinner(ui_element = plotlyOutput(outputId = "playerShotsChart4", width = "823px", height = "850"), image = "https://i.gifer.com/XOsX.gif")), 
                    tabPanel("Efficiency", withSpinner(ui_element = plotlyOutput(outputId = "playerEfficiencyChart4", width = "823px", height = "850px"), image = "https://i.gifer.com/XOsX.gif")))
                  , width = 8)
              ), 
              h2("Assists Distribution"), 
              uiOutput(outputId = "allItems4"),
              h2("End !!")
              
      ), 
      
      ############################################################################
      ################################### TAB 5 ##################################
      ############################################################################
      
      tabItem(tabName = "moneyball",
              h1("NBA Leaders"),
              h2("Floor is Yours"),
              fluidRow(
                column(
                  verticalLayout(
                    selectInput(inputId = "categorySelected5", label = "Category", choices = c("Classic", "Shooting"), selected = "Classic", width = "50%"),
                    uiOutput(outputId = "statSelected5"),
                    uiOutput(outputId = "teamList5"),
                    selectInput(inputId = "positionSelected5", label = "Position", choices = c("All", "PG", "SG", "SF", "PF", "C"), selected = "All", multiple = TRUE,  width = "50%"),
                    uiOutput(outputId = "salarySelected5"), 
                    uiOutput(outputId = "ageSelected5"),
                    numericInput(inputId = "nbPlayerSelected5", label = "Nb Players", value = 15, min = 10, max = 50, step = 1, width = "50%"))
                  , width = 4),
                column(plotlyOutput(outputId = "customGraph5", width = "auto", height = "800px"), width = 8)
              ), 
              h2("End !!")
      )
      
    )
  )
)

################################################################################
################################ SERVER ########################################
################################################################################
server <- function(input, output) {
  
  ############################################################################
  ################################### TAB 1 ##################################
  ############################################################################
  
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
  # observe({
  #   
  #   input$goButton1
  #   
  #   withProgress(message = "DownLoading Datas", value = 0, {
  
  # - DATAS DOWNLOADED : DT PLAYER MINUTE
  dicoPlayerMinute <- reactive({fread(file = paste("Dictionary/", seasonSelected1(), "/", seasonTypeSelected1(), "/minutesSummary.csv", sep = ""))})
  # incProgress(1/9, detail = "Calculating Mins Per Players")
  
  # - DATAS DOWNLOADED : DT with players characteristics (position, salary, age)
  dicoPlayerFich <- reactive({gePlayersFich(seasonSelected1(), seasonTypeSelected1(), DT = dicoPlayerMinute())})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - DATAS DOWNLOADED : LIST of vector players and nbaDatas DT
  seasonDatasList <- reactive({cleanDatas(seasonSelected1(), seasonTypeSelected1())})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - DATAS DOWNLOADED : Vector of NBA teams
  nbaTeams <- reactive({seasonDatasList()[["nbaTeams"]]})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - DATAS DOWNLOADED : Vector of NBA players
  nbaPlayers <- reactive({seasonDatasList()[["nbaPlayers"]]})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - DATAS DOWNLOADED : DT Reference for other functions
  nbaDatasDT <- reactive({seasonDatasList()[["nbaDatas"]]})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - DATAS DOWNLOADED : DT NBA Calendar
  NBAcalendar <- reactive({getNBAcalendar(seasonSelected1(), DT = nbaDatasDT())})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - DATAS DOWNLOADED : DT merge nbaDatasDT & NBAcalendar
  nbaDatasDTmerged <- reactive({merge(nbaDatasDT(), NBAcalendar()[, .(game_id, Date, Home, Away)], by = "game_id")})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # - We download the ranking streak for all league
  resultsByTeamList <- reactive({getRankingStreakLeague(NBAcalendar())})
  # incProgress(1/9, detail = "DownLoading Players Characteristics")
  
  # })
  
  # })
  
  # ------------------------- END DATAS DOWNLOAD -------------------------------
  
  # - CONFIRMATION DL TEXT
  output$confirmationText1 <- renderText({
    if (input$goButton1 > 0 & length(resultsByTeamList()) > 0) {
      paste("Datas Selected : OK !!\nSeason : ", seasonSelected1(), "\nPeriod : ", seasonTypeSelected1(), sep = "")
    } else {
      "No Datas Selected\n!! Click on Go Button !!"
    }
  })
  
  appearing <- reactiveValues(value2 = FALSE, value3 = FALSE)
  
  observeEvent(input$goButton1, {
    appearing$value2 <- FALSE
    appearing$value3 <- FALSE
  })
  
  observeEvent(input$usmap2_marker_click, {
    appearing$value2 <- TRUE
  })
  
  observeEvent(input$goButton3, {
    appearing$value3 <- TRUE
  })
  
  observe({
    
    if (input$goButton1 > 0) {
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
        if (appearing$value2 == TRUE) {
          input$usmap2_marker_click$id
        } else {
          NULL
        }
      })
      
      # - SELECTED TEAM TEXT
      output$selectedTeamText2 <- renderText({
        if (is.null(longNameSelected2())) {
          "Select a Team on the Map \n\nTips : Check your knowledge by clicking on a State"
        } else {
          paste("Selected: ", longNameSelected2(), "\n\nContinue ?", sep = "")
        }
      })
      
      # - SHORT NAME TEAM VARIABLE
      shortNameSelected2 <- eventReactive(input$goButton2, {
        if (appearing$value2 == TRUE) {
          getShortTeamName(longName = input$usmap2_marker_click$id)
        } else {
          NULL
        }
      })
      
      # - TEAM LOGO 
      output$logo2 <- renderImage(list(src = paste("Logos/", shortNameSelected2(), ".png", sep = ""), contentType = "image/png", width = "40%", height = "auto"), deleteFile = FALSE)
      
      output$teamRank2 <- renderDataTable({
        if (!is.null(shortNameSelected2())) {
          getRankingStreakTeam(selectedTeam = shortNameSelected2(), listResultTeam = resultsByTeamList())
        }
      }, options = list(pageLength = 9, dom = "tp"))
      
      # - STAT SELECTION RENDER UI
      output$statSelected2 <- renderUI({
        if (!is.null(shortNameSelected2())) {
          selectInput(inputId = "statSelected2", 
                      label = "Category", 
                      choices = c("Points", "Assists", "Rebounds", "Blocks", "Steals"), width = "20%")
        }
      })
      
      # - STAT SELECTED VARIABLE
      statSelected2 <- reactive({input$statSelected2})
      
      # - NUMBER PLAYER GRAPH RENDER UI
      output$nbPlayerChart2 <- renderUI({
        if (!is.null(shortNameSelected2())) {
          numericInput(inputId = "nbPlayerChart2",
                       label = "Nb Players",
                       value = 10, min = 5, max = 25, step = 1,
                       width = "20%")
        }
      })
      
      # - NUMBER PLAYER GRAPH VARIABLE
      nbPlayerChart2 <- reactive({input$nbPlayerChart2})
      
      # - TEAM BAR CHART GRAPH
      output$teamBarChart2 <- renderPlotly({
        req(input$statSelected2)
        if (!is.null(shortNameSelected2())) {
          getTeamStatsGraph(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), DT = nbaDatasDTmerged())
        }
      })
      
      # - TEAM PIE CHART GRAPH
      output$teamPieChart2 <- renderPlotly({
        req(input$statSelected2, req(input$nbPlayerChart2))
        if (!is.null(shortNameSelected2())) {
          getTeamStatsPerPlayerChart(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), nbPlayers = nbPlayerChart2(), DTroster = dicoPlayerMinute(), DT = nbaDatasDTmerged())
        }
      })
      
      # - BEST GAMES DT
      output$bestGames2 <- renderDataTable({
        req(input$statSelected2)
        if (!is.null(shortNameSelected2())) {
          getBestGames(selectedTeam = shortNameSelected2(), typeStat = statSelected2(), nbGames = 20, DTroster = dicoPlayerMinute(), DT = nbaDatasDTmerged())
        }
      }, options = list(pageLength = 5, dom = "tp"))
      
      # - ALL ITEMS RENDER UI
      output$allItems2 <- renderUI({
        
        if (appearing$value2 == TRUE) {
          
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
        }
      })
      
      ############################################################################
      ################################### TAB 3 ##################################
      ############################################################################
      
      # - TEAM LIST RENDER UI
      output$teamList3 <- renderUI({
        selectInput(inputId = "selectedTeam3", label = "Team", choices = c("Team", nbaTeams()), selected = "Team", width = "30%")
      })
      
      # - SHORT NAME TEAM VARIABLE
      shortNameSelected3 <- reactive({
        getShortTeamName(longName = input$selectedTeam3)
      })
      
      # - SHORT NAME TEAM FIXED VARIABLE
      shortNameSelected3Graph <- eventReactive(input$goButton3, {
        if (appearing$value3 == TRUE) {
          getShortTeamName(longName = input$selectedTeam3)
        } else {
          NULL
        }
      })
      
      # - ROSTER LIST
      listRoster3 <- reactive({
        req(input$selectedTeam3)
        getListRoster(shortNameSelected3(), DT = dicoPlayerMinute())
      })
      
      output$selectedPlayer3 <- renderUI({
        req(input$selectedTeam3)
        selectInput(inputId = "selectedPlayer3", 
                    label = "Player", 
                    choices = listRoster3(),
                    width = "30%")
      })
      
      # - PLAYER NAME VARIABLE
      playerSelected3 <- reactive({input$selectedPlayer3})
      
      # - PLAYER NAME FIXED VARIABLE
      playerSelected3Graph <- eventReactive(input$goButton3, {
        if (appearing$value3 == TRUE) {
          input$selectedPlayer3
        } else {
          NULL
        }
      })
      
      # - PLAYER PRESENTATION TEXT
      output$selectedPlayerPres3 <- renderText({
        req(playerSelected3())
        if (playerSelected3() != "Player") {
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
        if (is.null(playerSelected3())) {
          "Select a Player"
        } else {
          paste("Selected: ", playerSelected3(), "\n\nContinue ?", sep = "")
        }
      })
      
      # - SPIDER CHART GRAPH
      output$spiderChart3 <- renderPlotly({
        # We use a trycatch as selected player comes later after selected team creating an error when filtering DT
        tryCatch({
          getPlayerStatsChart(selectedTeam = shortNameSelected3(), selectedPlayer = playerSelected3(), DTgamesPlayed = dicoPlayerMinute(), DT = nbaDatasDTmerged())
        }, warning = function(w) {
          NULL
        }, error = function(e) {
          NULL
        })
      })
      
      # - FREQ CHART RENDER UI
      output$selectedFreq3 <- renderUI({
        #if (!is.null(playerSelected3Graph())& playerSelected3Graph() != "Player") {
          radioButtons(inputId = "selectedFreq3", label = "Select a Player", choices = c("Month", "Week"), selected = "Month", inline = TRUE, width = "100%")
        #}
      })
      
      # - FREQ CHART VARIABLE
      freqSelected3 <- reactive({input$selectedFreq3})
      
      # - CALENDAR RANGE RENDER UI
      output$dateRange3 <- renderUI({
        #if (!is.null(playerSelected3Graph())& playerSelected3Graph() != "Player") {
          dateRangeInput(inputId = "dateRange3", label = "Select a Range of Two Dates", 
                         start = min(nbaDatasDTmerged()$Date), end = max(nbaDatasDTmerged()$Date), min =  min(nbaDatasDTmerged()$Date), max = max(nbaDatasDTmerged()$Date), 
                         weekstart = 1, autoclose = FALSE, width = "50%")
        #}
      })
      
      # - CALENDAR RANGE VARIABLES
      dateRange3Min <- reactive({input$dateRange3[1]})
      dateRange3Max <- reactive({input$dateRange3[2]})
      
      
      # - nbaDatasDT PLAYER
      playerDT3 <- eventReactive(input$goButton3, {
        if (!is.null(playerSelected3Graph())) {
          getPlayerDT(shortNameSelected3Graph(), playerSelected3Graph(), DT = nbaDatasDTmerged())
        }
      })
      
      # - PLAYER DATE BAR CHART
      output$dateBarChart3 <- renderPlotly({
        req(input$selectedFreq3, input$dateRange3)
        #if (!is.null(playerSelected3Graph()) & playerSelected3Graph() != "Player" & !is.null(shortNameSelected3Graph())) {
          getPlayerStatsPerDateGraph(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), perMonthWeek = freqSelected3(), 
                                     date1 = dateRange3Min(), date2 = dateRange3Max(), DTcalendar = NBAcalendar(), DTplayer = playerDT3())
        #}
      })
      
      # - PLAYER PERIOD BAR CHART
      output$periodBarChart3 <- renderPlotly({
        #if (!is.null(playerSelected3Graph()) & playerSelected3Graph() != "Player") {
          getPlayerPeriodStatsChart(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), DTplayer = playerDT3())
        #}
      })
      
      # - IMPACT PLAYER DT
      listImpactDT3 <- reactive({
        if (!is.null(playerSelected3Graph())) {
          getPlayerImpact(selectedTeam = shortNameSelected3Graph(), selectedPlayer = playerSelected3Graph(), playerDT3())
        }
      })
      
      output$impactFG3 <- renderDataTable({
        #if (!is.null(playerSelected3Graph()) & playerSelected3Graph() != "Player") {
          listImpactDT3()[["ShootImpact"]]
        #}
      }, options = list(dom = "t"))
      
      output$impactPluMin3 <- renderDataTable({
        #if (!is.null(playerSelected3Graph()) & playerSelected3Graph() != "Player") {
          listImpactDT3()[["PointImpact"]]
        #}
      }, options = list(dom = "t"))
      
      # ALL ITEMS RENDER UI
      output$allItems3 <- renderUI({
        
        if (appearing$value3 == TRUE) {
          
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
        }
      })
      
      ############################################################################
      ################################### TAB 4 ##################################
      ############################################################################
      
      # - TEAM LIST RENDER UI
      output$teamList4 <- renderUI({
        selectInput(inputId = "selectedTeam4", label = "Team", choices = c("Team", nbaTeams()), selected = "Team", width = "50%")
      })
      
      # - SHORT NAME TEAM VARIABLE
      shortNameSelected4 <- reactive({getShortTeamName(longName = input$selectedTeam4)})
      
      # - ROSTER LIST RENDER UI
      listRoster4 <- reactive({getListRoster(shortNameSelected4(), DT = dicoPlayerMinute())})
      
      output$selectedPlayer4 <- renderUI({
        
        req(input$selectedTeam4)
        selectInput(inputId = "selectedPlayer4", 
                    label = "Player", 
                    choices = listRoster4(),
                    width = "50%")
      })
      
      # - PLAYER SELECTED VARIABLE
      playerSelected4 <- reactive({input$selectedPlayer4})
      
      # - PLAYER SELECTED FIXED VARIABLE
      playerSelected4Graph <- eventReactive(
        input$goButton4, {input$selectedPlayer4}
      )
      
      # - PLAYER SELECTED TEXT
      output$selectedPlayerText4 <- renderText({
        if (is.null(playerSelected4())) {
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
      output$areaSelected4 <- renderUI({
        req(input$goButton4)
        selectInput(inputId = "areaSelected4", label = "Area", choices = area, selected = area[1])
      })
      
      # - AREA SELECTED VARIABLE
      areaSelected4 <- reactive({input$areaSelected4})
      
      # - ASSISTS DISTRIBUTION LIST
      listAssists4 <- reactive({
        req(input$areaSelected4)
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
      
      # - TEAM LIST RENDER UI
      output$teamList5 <- renderUI({
        selectInput(inputId = "selectedTeam5", label = "Team", choices = c("All", nbaTeams()), selected = "All", width = "50%")
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
      
      # - STAT SELECTED RENDER UI
      output$statSelected5 <- renderUI({
        selectInput(inputId = "statSelected5", label = "Stat", choices = choicesVec(), selected = choicesVec()[1], width = "50%")
      })
      
      # - STAT SELECTED VARIABLE
      statSelected5 <- reactive({input$statSelected5})
      
      # - TEAM SELECTED VARIABLE
      teamSelected5 <- reactive({getShortTeamName(input$selectedTeam5)})
      
      # - POSITION SELECTED VARIABLE
      positionSelected5 <- reactive({input$positionSelected5})
      
      # - SALARY SELECTION RENDER UI
      output$salarySelected5 <- renderUI({
        sliderInput(inputId = "salarySelected5", label = "Salary (million $)", min = 0, max = ceiling(max(dicoPlayerFich()$Salary/1000000)), 
                    value = c(0, ceiling(max(dicoPlayerFich()$Salary/1000000))), step = 0.5, width = "50%")
      })
      
      # - SALARY SELECTED VARIABLE
      salaryLow5 <- reactive({input$salarySelected5[1]})
      salaryHigh5 <- reactive({input$salarySelected5[2]})
      
      # - AGE SELECTION RENDER UI
      output$ageSelected5 <- renderUI({
        sliderInput(inputId = "ageSelected5", label = "Age", min = min(dicoPlayerFich()$Age), max = max(dicoPlayerFich()$Age), 
                    value = c(min(dicoPlayerFich()$Age), max(dicoPlayerFich()$Age)), step = 1, width = "50%")
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
      
      # ENF IF OBSERVE  
    }
  })
}

profvis(runApp(shinyApp(ui, server)))