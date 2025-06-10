#setwd("/Users/ianbach/Desktop/MoellerPitcher2025")

# rsconnect::setAccountInfo(name='idbach16',
#                           token='BED119D07E9E27D75E57270C58C3BCAA',
#                          secret='hy3L7mJXFYUiTB2eWM1TIlReC7QdVWebvY1rLKxU')


#library(rsconnect)
#rsconnect::deployApp()

#setwd("C:/Users/ibach/OneDrive - Terillium/Pictures/Moller Misc/Moeller_2025/Varsity")
# Load necessary libraries
# Load necessary libraries
library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(xgboost)
library(purrr)
library(tidyverse)
library(stringr)
#library(emojifont)
library(janitor)
library(tidyr)
library(ggplot2)
library(tools)
library(plotly)
library(readr)
library(rmarkdown)
library(dplyr)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(xgboost)
#source("success_model_moeller.test.R")

# Load data
OldBigMoe <- read.csv('Moeller_2025_Final_Season.csv')
MoeStuff <- read.csv('MoeStuff2025.csv')


# Data manipulation and cleaning
BigMoe <- OldBigMoe %>%
  mutate(PitchType = recode(PitchType,
                            "Fast Ball" = "FastBall",
                            "Breaking Ball" = "BreakingBall",
                            "Two Seam Fast Ball" = "TwoSeamFastBall",
                            "Change Up" = "ChangeUp")) %>%
  mutate(PitchResult = recode(PitchResult,
                              "Strike Looking" = "StrikeLooking",
                              "Strike In Play" = "StrikeInPlay",
                              "Strike Swing and Miss" = "Strike_Swing_Miss",
                              "Strike Foul" = "StrikeFoul")) %>%
  mutate(AtBatResult = recode(AtBatResult,
                              "Ground Out" = "GroundOut",
                              "Fly Out" = "FlyOut",
                              "Strike Out" = "StrikeOut",
                              "Line Out" = "LineOut",
                              "Double Play" = "DoublePlay",
                              "Infield Fly" = "InfieldFly",
                              "Fielders Choice" = "FieldersChoice"))

# Change data types
BigMoe <- BigMoe %>%
  mutate(PitchNo = as.integer(PitchNo),
         Date = as.Date(Date, format = "%m/%d/%Y"),
         Time = as.character(Time),
         PAofInning = as.integer(PAofInning),
         PAofInning.1 = as.integer(PAofInning.1),
         Pitcher = as.factor(Pitcher),
         PitcherHand = as.factor(PitcherHand),
         PitcherTeam = as.factor(PitcherTeam),
         Batter = as.factor(Batter),
         Batter.Hand = as.factor(Batter.Hand),
         BatterTeam = as.factor(BatterTeam),
         Inning = as.integer(Inning),
         Top.Bottom = as.factor(Top.Bottom),
         Outs = as.integer(Outs),
         Balls = as.integer(Balls),
         Strikes = as.integer(Strikes),
         PitchType = as.factor(PitchType),
         PitchResult = as.factor(PitchResult),
         AtBatResult = as.factor(AtBatResult),
         PitchVelo = as.numeric(PitchVelo),
         Location = as.factor(Location),
         AttackZone = as.factor(AttackZone))

# Sample dataframe for the new plots
df <- data.frame(
  Balls = sample(0:3, 1000, replace = TRUE),
  Strikes = sample(0:2, 1000, replace = TRUE),
  BatterTeam = sample(c('Moeller', 'Other'), 1000, replace = TRUE),
  PitchResult = sample(c('Strike Looking', 'Strike Swing and Miss', 'Strike Foul', 'Strike In Play', 'Ball'), 1000, replace = TRUE),
  AtBatResult = sample(c('1B', '2B', '3B', 'HR', 'Out'), 1000, replace = TRUE)
)

# Define the conditions for filtering
conditions <- list(
  list(Balls = 0, Strikes = 0),
  list(Balls = 0, Strikes = 1),
  list(Balls = 0, Strikes = 2),
  list(Balls = 1, Strikes = 0),
  list(Balls = 2, Strikes = 0),
  list(Balls = 3, Strikes = 0),
  list(Balls = 1, Strikes = 1),
  list(Balls = 1, Strikes = 2),
  list(Balls = 2, Strikes = 1),
  list(Balls = 2, Strikes = 2),
  list(Balls = 3, Strikes = 1),
  list(Balls = 3, Strikes = 2)
)

# Define custom colors
custom_colors <- c("Strike Looking" = "navy", "Strike Swing and Miss" = "yellow", "Strike Foul" = "navy", "Strike In Play" = "yellow", "Ball" = "navy")

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  # Background and text styles
  tags$style(HTML("
    body {
      background-image: url('https://raw.githubusercontent.com/IDBach16/MLB-Pitcher-Analysis/main/archbishop-moeller-baseball-toby-hueber-jeff-harwell.jpg');
      background-attachment: fixed;
      background-size: cover;
    }
    .tab-content .active h3, .tab-content .active p {
        color: #E0E0E0;
    }
    .tab-content .active {
        background-color: rgba(0, 0, 0, 0.7);
        padding: 15px;
        border-radius: 5px;
    }
  ")),
  titlePanel("Moeller Pitcher Analysis 2025"),
  navbarPage(
    title = "Pitcher Dashboard",
    
    tabPanel("Introduction",
             h3("Overview of Dashboard (6/10/2025)"),
             p("Powered by AWRE data, the Moeller Pitcher Analysis Dashboard offers deep insights into pitcher performance.
               This advanced tool is divided into key sections, each designed to present specific data aspects clearly and intuitively. 
               Users can explore detailed game summaries, analyze pitch usage, compare performance against right- and left-handed hitters, 
               and scout opponent pitchers. The dashboard also includes dynamic visualizations of pitch outcomes and batted ball types, 
               providing a comprehensive view of pitching strategies and results."),
             
             h3("Headers/Tabs"),
             
             h3(tags$u("Game Summary")),
             p("Purpose: The Game Summary tab allows users to select a specific pitcher team and date to view a summary of game statistics and pitch usage."),
             p("Details:"),
             p("- Select Pitcher Team: Dropdown menu to select the pitcher team."),
             p("- Select Date and Pitcher: Dynamic UI elements to filter and select specific game dates and pitchers."),
             p("- Game Summary Table: Displays summarized game data, including pitch types, velocity, and usage percentages."),
             p("- Usage vs RHH/LHH Tables: Separate tables showing pitch usage and statistics against right-handed and left-handed hitters."),
             p("- Velocity by Inning and Date Plot: A dynamic plot showing pitch velocity by inning and date."),
             
             h3(tags$u("Pitch Usage (Moeller Only)")),
             p("Purpose: This section focuses exclusively on pitches thrown by Moeller pitchers throughout the season, offering detailed statistics and visualizations."),
             p("Details:"),
             p("- Select Pitcher Team: Defaulted to Moeller."),
             p("- Select Pitcher and Pitch Type: Dropdowns to filter data by specific pitchers and pitch types."),
             p("- Pitch Usage Table: Displays pitch usage statistics."),
             p("- Outcome Percentage Plots: Series of plots showing outcome percentages based on different ball and strike counts."),
             
             h3(tags$u("Pitch Usage (Opponent Only)")),
             p("Purpose: This section provides analysis of pitches thrown by opposing teams' pitchers against Moeller, offering insights into opponents' strategies and performance."),
             p("Details:"),
             p("- Select Pitcher Team: Dropdown to select opposing pitcher teams."),
             p("- Select Pitcher and Pitch Type: Dynamic dropdowns to filter data by specific pitchers and pitch types."),
             p("- Pitch Usage Table: Displays pitch usage statistics for opponents."),
             p("- Outcome Percentage Plots: Series of plots showing outcome percentages for different ball and strike counts when Moeller is batting."),
             
             h3(tags$u("Righty vs Lefty")),
             p("Purpose: The Righty vs Lefty section compares the performance of pitchers against right-handed and left-handed hitters, providing detailed statistics and visualizations."),
             p("Details:"),
             p("- Select Pitcher Team and Pitcher: Dropdowns to filter data by pitcher team and specific pitchers."),
             p("- Usage vs Right-Handed Hitters Table: Shows pitch usage and statistics against right-handed hitters."),
             p("- Stats vs Right-Handed Hitters Table: Summarizes key performance metrics against right-handed hitters."),
             p("- Usage vs Left-Handed Hitters Table: Shows pitch usage and statistics against left-handed hitters"),
             p("- Stats vs Left-Handed Hitters Table: Summarizes key performance metrics against left-handed hitters."),
             
             
             
             h3(tags$u("Batted Ball - Season")),
             p("Purpose: The Batted Ball section analyzes the outcomes of batted balls, such as ground outs, fly outs, and extra-base hits, for different pitch types."),
             p("Details:"),
             p("- Select Pitcher Team and Pitcher: Dropdowns to filter data by pitcher team and specific pitchers."),
             p("- Batted Ball Table: Summarizes the number of batted balls and extra-base hits for each pitch type."),
             p("- Batted Ball Plots: Interactive plots that visualize batted ball types by pitch type, pitch velocity, and count situations."),
             
             #tableOutput("dataDictTable")
    ),
    
    tabPanel("Data Dictonary (Analytics Team)",
             h3(tags$u("Purpose:")),
             p("The data dictionary is a structured summary of the variables present in the dataset used for the Moeller Pitcher Analysis Dashboard.
             It provides details such as variable names, their corresponding data types in R, and descriptions explaining the purpose of each variable.
             The dictionary includes key variables like PitchNo (Pitch Number), Date (Date of the game), PitchType (Type of pitch thrown), and 
             PitchResult (Result of the pitch), among others. However, while the data dictionary is essential for understanding the dataset's 
               structure and the meaning of each variable, it may not directly assist users in analyzing the data or interpreting the results 
               presented in the dashboard. The primary purpose of the data dictionary is to aid developers and data analysts in comprehending 
               the dataset's composition, ensuring accurate data manipulation and analysis. Users interested in insights and strategic decisions 
               will find the interactive elements and visualizations in the dashboard more beneficial for their needs."),
             
             tableOutput("dataDictTable")),
    
    
    #######Game SummarY#############
    tabPanel("Game Summary",
             sidebarPanel(
               selectInput("pitcherTeam1", "Select Pitcher Team:", choices = unique(BigMoe$PitcherTeam)),
               uiOutput("dateUI"),
               uiOutput("pitcherUI")
             ),
             mainPanel(
               h3("Pitch Summary by Team & Date"),
               tableOutput("TeamgameSummaryTable"),
               tags$h3("Game Summary"),
               tableOutput("gameSummaryTable"),
               h3("Pitch Usage"),
               DTOutput("pitchUsageByCountTable"),
               tags$h3("Usage vs RHH"),
               tableOutput("usageRHPTable"),
               tags$h3("Stats vs RHH"),
               tableOutput("statsRHPTable"),
               tags$h3("Usage vs LHH"),
               tableOutput("usageLHPTable"),
               tags$h3("Stats vs LHH"),
               tableOutput("statsLHPTable"),
               tags$h3("Velocity by Inning and Date"),
               plotlyOutput("veloPlot")
             )
    ),
    #########Pitch Usage##########
    tabPanel("(Moeller Only) Pitch Count vs Pitch Type - Season",
             fluidRow(
               column(12,
                      sidebarPanel(
                        selectInput("pitcherTeam2", "Select Pitcher Team:", choices = "Moeller"),
                        selectInput("pitcher2", "Select Pitcher:", choices = unique(BigMoe$Pitcher)),
                        selectInput("pitchType", "Select Pitch Type:", choices = unique(BigMoe$PitchType))
                      )
               )
             ),
             fluidRow(
               column(4,
                      tableOutput("pitchUsageTable")
               ),
               column(10,
                      uiOutput("plotOutputs")
               )
             )
    ),
    ########Pitch Usage Opponenet############
    tabPanel("(Opponent Only) Pitch Count vs Pitch Type - Season",
             fluidRow(
               column(12,
                      sidebarPanel(
                        selectInput("pitcherTeamOpponent", "Select Pitcher Team:", choices = unique(BigMoe %>% filter(PitcherTeam != 'Moeller') %>% pull(PitcherTeam))),
                        selectInput("pitcherOpponent", "Select Pitcher:", choices = NULL),
                        selectInput("pitchTypeOpponent", "Select Pitch Type:", choices = unique(BigMoe$PitchType))
                      )
               )
             ),
             fluidRow(
               column(4,
                      tableOutput("pitchUsageTableOpponent")
               ),
               column(10,
                      uiOutput("plotOutputsOpponent")
               )
             )
    ),
    ############
    tabPanel("Season Stats",
             sidebarPanel(
               selectInput("pitcherTeam3", "Select Pitcher Team:", choices = unique(BigMoe$PitcherTeam)),
               selectInput("pitcher3", "Select Pitcher:", choices = unique(BigMoe$Pitcher)),
             ),
             mainPanel(
               h4("Usage Season"),
               tableOutput("usageTableW"),
               h4("Stats Hitters"),
               tableOutput("statsVsTable2"),
               h4("Usage vs Right-Handed Hitters"),
               tableOutput("usageRTable"),
               h4("Stats vs Right-Handed Hitters"),
               tableOutput("statsVsRTable1"),
               tableOutput("statsVsRTable2"),
               h4("Usage vs Left-Handed Hitters"),
               tableOutput("usageLTable"),
               h4("Stats vs Left-Handed Hitters"),
               tableOutput("statsVsLTable1"),
               tableOutput("statsVsLTable2"),
             )
    ),
    
    tabPanel("Batted Ball - Season",
             sidebarPanel(
               selectInput("pitcherTeam4", "Select Pitcher Team:", choices = unique(BigMoe$PitcherTeam)),
               selectInput("pitcher4", "Select Pitcher:", choices = NULL),
               selectInput("atBatResult", "Select Batted Ball Type:", choices = unique(BigMoe$AtBatResult[!BigMoe$AtBatResult %in% c("StrikeOut", "BB")])),
               selectInput("pitchType", "Select Pitch Type:", choices = unique(BigMoe$PitchType)) # New dropdown for Pitch Type
             ),
             mainPanel(
               plotlyOutput("battedBallPlot1"),
               plotlyOutput("battedBallPlot2"),
               plotlyOutput("battedBallPlot4")
             )
    ),
    
    tabPanel("Moeller Pitcher Success Visualization (Updated! 4/9)",
             
             sidebarPanel(
               selectInput("selected_pitcher", "Select Pitcher:", choices = NULL),
               selectInput("selected_pitchtype", "Select Pitch Type:", choices = NULL)
               #selectInput("selected_Count", "Select Count:", choices = NULL)
             ),
             
             mainPanel(
               plotOutput("success_plot")
             )
    ),
    
    tabPanel("MoeStuff+",
             h3(tags$u("MoeStuff+")),
             p("The MoeStuff+ model is a custom pitch scoring metric built using pitch result, at-bat outcome,
             and pitch location. Each pitch result—such as a called strike, foul, or whiff—is assigned a base
             point value. For example, a whiff earns +2.0 points, while a ball is penalized with -0.50 points. 
               If the pitch is put in play, the at-bat result further adjusts the score—outs like a groundout
               or double play earn up to +1.0, while hits like home runs subtract up to -1.0. Additional bonuses
               are applied for certain strikeout types (e.g., +2.0 for a swinging strikeout).
               Pitch location also modifies the score: pitches in the Chase zone get a +3.0 modifier,
               while those in the Waste zone only add +0.5. The final pitch score is the sum of the base, outcome, and location points.
               Scores are averaged by pitcher and pitch type, then scaled so that 100 represents league average. MoeStuff+ rewards pitchers
               who generate positive outcomes in effective zones, offering an easy-to-understand snapshot of pitch execution quality."),
             
             # Dropdown filter
             selectInput("team_filter", "Select Team:", 
                         choices = c("All", unique(MoeStuff$PitcherTeam123)), 
                         selected = "All"),
             
             # Table output
             mainPanel(
               tabsetPanel(
                 tabPanel("Stuff+ by Pitch Type", DTOutput("score_table")),
                 tabPanel("Average Stuff+ by Pitcher", DTOutput("pitcher_avg_table"))
               )
             )
             
    ),
    
    tabPanel("ML Pitcher Success",
             sidebarLayout(
               sidebarPanel(
                 h4("This model runs automatically when the app starts.")
               ),
               mainPanel(
                 p("This model uses past game data to predict how successful each pitcher is likely to be. ",
                   "It considers factors like the pitch type (fastball, breaking ball, changeup, etc.), ",
                   "the count on the batter (balls and strikes), the inning and game situation (early or late), ",
                   "whether the batter is left- or right-handed, the pitch’s location and result (swing and miss, ",
                   "called strike, foul, or ball), and the outcome of the at-bat (out, hit, walk, home run). ",
                   "The model runs thousands of simulations (cross-validation) to find the most reliable patterns ",
                   "in the data. We then rank pitchers individually and also simulate the best 2- and 3-pitcher combinations ",
                   "for team success. At the end, we report the model’s RMSE (Root Mean Square Error) — a score that tells us ",
                   "how accurate the model’s predictions are compared to actual outcomes (lower is better). ",
                   "This tool is meant to help guide lineup decisions by showing which combinations or individuals ",
                   "are currently projected to perform best based on the available data."
                 ),
                 tabsetPanel(
                   tabPanel("Top 3-Pitcher Combos",  
                            h4("Top 3-Pitcher Combos"),  
                            textOutput("rmseText"), 
                            DTOutput("comboTable")  
                   ),
                   tabPanel("Top 2-Relief Pitcher Pairs",  
                            h4("Top 2-Pitcher Pairs"),  
                            textOutput("rmseText"),  
                            DTOutput("pairTable")  
                   ),
                   tabPanel("Individual Pitcher Scores",  
                            h4("Individual Pitcher Scores"),  
                            DTOutput("individualTable")
                   ),
                   tabPanel("Starting Pitcher Pairs",  
                            h4("Starting Pitcher Pairs:"),
                            DTOutput("excludedPlusOneTable")
                   )
                 )
             )
    )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Update the pitcher dropdown based on the selected team
  observe({
    updateSelectInput(session, "pitcher1", choices = unique(BigMoe %>% filter(PitcherTeam == input$pitcherTeam1) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcher2", choices = unique(BigMoe %>% filter(PitcherTeam == input$pitcherTeam2) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcher3", choices = unique(BigMoe %>% filter(PitcherTeam == input$pitcherTeam3) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcher4", choices = unique(BigMoe %>% filter(PitcherTeam == input$pitcherTeam4) %>% pull(Pitcher)))
    updateSelectInput(session, "pitcherOpponent", choices = unique(BigMoe %>% filter(PitcherTeam == input$pitcherTeamOpponent) %>% pull(Pitcher)))
  })
  
  filteredData <- reactive({
    BigMoe %>%
      dplyr::filter(PitcherTeam %in% input$pitcherTeam1,
                    Pitcher %in% input$pitcher1,
                    Date >= input$dateRange[1],
                    Date <= input$dateRange[2])
  })
  
  # Define the data dictionary
  data_dictionary <- data.frame(
    variable_name = c("PitchNo", "Date", "Time", "PAofInning", "PAofInning.1", "Pitcher", 
                      "PitcherHand", "PitcherTeam", "Batter", "Batter Hand", "Top/Bottom", 
                      "Outs", "Balls", "Strikes", "PitchType", "PitchResult", "AtBatResult", 
                      "PitchVelo", "Location", "AttackZone"),
    R_data_type = c("numeric", "Date", "Time", "numeric", "numeric", "factor", 
                    "factor", "factor", "factor", "factor", "factor", 
                    "numeric", "numeric", "numeric", "factor", "factor", "factor", 
                    "numeric", "numeric", "factor"),
    description = c("Pitch Number", "Date of the game", "Time of the pitch", "Plate appearance of inning", 
                    "Duplicated plate appearance of inning", "Pitcher name", "Pitcher hand (R/L)", 
                    "Pitcher team name", "Batter name", "Batter hand (R/L)", "Top or Bottom of inning", 
                    "Number of outs", "Number of balls", "Number of strikes", "Type of pitch thrown", 
                    "Result of the pitch", "Result of the at-bat", "Velocity of the pitch", "Pitch location", 
                    "Zone of the attack")
  )
  
  # Render the data dictionary as a table
  output$dataDictTable <- renderTable({
    data_dictionary
  })
  ###########Game Summmary Creation################ 
  # Reactive expression to filter data based on selected team
  teamData <- reactive({
    BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam1)
  })
  
  # Update date choices based on selected team
  output$dateUI <- renderUI({
    selectInput("date", "Select Date:", choices = unique(teamData()$Date))
  })
  
  # Update pitcher choices based on selected team and date
  output$pitcherUI <- renderUI({
    req(input$date)  # Ensure date is selected before updating pitcher choices
    selectInput("pitcher", "Select Pitcher:", choices = unique(teamData() %>% filter(Date == input$date) %>% pull(Pitcher)))
  })
  
  filteredData <- reactive({
    req(input$date, input$pitcher)  # Ensure date and pitcher are selected before filtering data
    teamData() %>%
      filter(Date == input$date, Pitcher == input$pitcher)
  })
  
  summaryData <- reactive({
    filteredData() %>%
      mutate(PitchType = recode(PitchType,
                                "FastBall" = "FB",
                                "BreakingBall" = "BRB",
                                "Slider" = "SL",
                                "TwoSeamFastBall" = "2FB",
                                "ChangeUp" = "CH",
                                "Curve" = "CB",
                                "Splitter" = "SPL")) %>%
      group_by(PitchType) %>%
      dplyr::summarize(
        No. = n(),
        Velo = round(mean(PitchVelo, na.rm = TRUE), 1),
        VeloMax = ifelse(sum(!is.na(PitchVelo)) > 0, round(max(PitchVelo, na.rm = TRUE), 1), NA),
        Strikes = sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE),
        Balls = sum(PitchResult == "Ball", na.rm = TRUE),
        `Whiff%` = round(sum(PitchResult %in% c("Strike_Swing_Miss"), na.rm = TRUE) / 
                           sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) * 100, 3)
      ) %>%
      mutate(
        `Usage %` = round(No. / sum(No., na.rm = TRUE) * 100, 3),
        `Total` = Strikes + Balls,
        `Strike %` = ifelse(Total > 0, round((Strikes / Total) * 100, 1), NA),
        `Ball %` = ifelse(Total > 0, round((Balls / Total) * 100, 1), NA))
  })
  
  
  output$gameSummaryTable <- renderTable({
    summaryData()
  })
  
  usageRHPData <- reactive({
    filteredData() %>%
      filter(Batter.Hand == 'R') %>%
      mutate(PitchType = recode(PitchType, 
                                "FastBall" = "FB", 
                                "BreakingBall" = 'BRB', 
                                "Slider" = 'SL',
                                "TwoSeamFastBall" = '2FB', 
                                "ChangeUp" = 'CH', 
                                "Splitter" = 'SPL')) %>%
      group_by(PitchType) %>%
      summarize(No. = n(),
                `Usage %` = round(n() / nrow(filteredData()) * 100, 3),
                `2K` = sum(Strikes == 2),
                `2K%` = round(sum(Strikes == 2) / n() * 100, 3),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
                `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
                `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
                `Whiff%` = round(Whiff / Swings * 100, 3))
  })
  ###Pitcher Usage
  #############################
  
  output$pitchUsageByCountTable <- renderDT({
    data2 <- filteredData() %>%
      mutate(PitchType = as.character(PitchType)) %>%  # Ensure PitchType is character
      group_by(Count) %>%
      mutate(TotalPitchesInCount = n()) %>%  
      group_by(Count, PitchType) %>%
      summarize(
        Pitches = n(),
        `Usage %` = round((Pitches / first(TotalPitchesInCount)) * 100, 2),
        `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
        `Whiff%` = round(sum(PitchResult %in% c("Strike_Swing_Miss"), na.rm = TRUE) / 
                           sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) * 100, 3)
      ) %>%
      ungroup() %>% 
      arrange(Count, desc(Pitches))  
    
    # ✅ Debug: Print unique pitch types (for verification)
    print(unique(data2$PitchType))
    
    # ✅ Convert to a datatable with better formatting
    datatable(data2, 
              options = list(
                pageLength = 10,    
                autoWidth = TRUE,   
                order = list(list(0, 'asc')),  
                columnDefs = list(list(className = 'dt-center', targets = "_all"))  
              ),
              rownames = FALSE) %>%
      formatStyle(
        'Usage %', 
        backgroundColor = styleInterval(c(20, 40, 60, 80), 
                                        c('#ff9999', '#ffcc99', '#ffff99', '#ccff99', '#99ff99')),
        color = 'black'  # ✅ Ensures text is always visible
      )
  })
  
  
  
  ###############################################
  
  #################################################
  #TEST for the Pitcher team data####
  ######################################
  
  # Filter data based on team and date (WITHOUT batter filtering)
  filteredData2 <- reactive({
    req(input$date)  # Ensure date is selected before filtering
    teamData() %>%
      filter(Date == input$date)
  })
  
  # Summarize filtered data for team on the selected date
  teamSummaryData <- reactive({
    filteredData2() %>%
      mutate(PitchType = recode(PitchType,
                                "FastBall" = "FB",
                                "BreakingBall" = "BRB",
                                "Slider" = "SL",
                                "TwoSeamFastBall" = "2FB",
                                "ChangeUp" = "CH",
                                "Curve" = "CB",
                                "Splitter" = "SPL")) %>%
      group_by(PitchType) %>%
      dplyr::summarize(
        No. = n(),
        Velo = round(mean(PitchVelo, na.rm = TRUE), 1),
        VeloMax = ifelse(sum(!is.na(PitchVelo)) > 0, round(max(PitchVelo, na.rm = TRUE), 1), NA),
        Strikes = sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE),
        Balls = sum(PitchResult == "Ball", na.rm = TRUE),
        `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
        `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
        `Whiff%` = round(Whiff / Swings * 100, 3)
      ) %>%
      mutate(
        `Usage %` = round(No. / sum(No., na.rm = TRUE) * 100, 3),
        `Total` = Strikes + Balls,
        `Strike %` = ifelse(Total > 0, round((Strikes / Total) * 100, 1), NA),
        `Ball %` = ifelse(Total > 0, round((Balls / Total) * 100, 1), NA)
      )
  })
  
  # Render the summary table
  output$TeamgameSummaryTable <- renderTable({
    teamSummaryData()
  })
  
  ###RHP
  ###################################
  
  output$usageRHPTable <- renderTable({
    usageRHPData()
  })
  
  statsRHPData <- reactive({
    filteredData() %>%
      filter(Batter.Hand == 'R') %>%
      summarize(BF = n_distinct(Inning, Batter),
                K = sum(AtBatResult == "StrikeOut"),
                DP = sum(AtBatResult == "DoublePlay"),
                FO = sum(AtBatResult == "FlyOut"),
                GB = sum(AtBatResult == "GroundOut"),
                LO = sum(AtBatResult == "LineOut"),
                Walks = sum(AtBatResult == "BB"),
                H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),
                XBH = sum(AtBatResult %in% c('2B', '3B', 'HR')))
  })
  
  output$statsRHPTable <- renderTable({
    statsRHPData()
  })
  
  #########Addded pitch usage
  #################################
  
  ###############################################
  ######################################
  
  usageLHPData <- reactive({
    filteredData() %>%
      filter(Batter.Hand == 'L') %>%
      mutate(PitchType = recode(PitchType, 
                                "FastBall" = "FB", 
                                "BreakingBall" = 'BRB', 
                                "Slider" = 'SL',
                                "TwoSeamFastBall" = '2FB', 
                                "ChangeUp" = 'CH', 
                                "Splitter" = 'SPL')) %>%
      group_by(PitchType) %>%
      summarize(No. = n(),
                `Usage %` = round(n() / nrow(filteredData()) * 100, 3),
                `2K` = sum(Strikes == 2),
                `2K%` = round(sum(Strikes == 2) / n() * 100, 3),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
                `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
                `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
                `Whiff%` = round(Whiff / Swings * 100, 3))
  })
  
  output$usageLHPTable <- renderTable({
    usageLHPData()
  })
  
  statsLHPData <- reactive({
    filteredData() %>%
      filter(Batter.Hand == 'L') %>%
      summarize(BF = n_distinct(Inning, Batter),
                K = sum(AtBatResult == "StrikeOut"),
                DP = sum(AtBatResult == "DoublePlay"),
                FO = sum(AtBatResult == "FlyOut"),
                GB = sum(AtBatResult == "GroundOut"),
                LO = sum(AtBatResult == "LineOut"),
                Walks = sum(AtBatResult == "BB"),
                H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),
                XBH = sum(AtBatResult %in% c('2B', '3B', 'HR')))
  })
  
  output$statsLHPTable <- renderTable({
    statsLHPData()
  })
  
  # Dynamically generate color palette and linetypes for pitch types
  output$veloPlot <- renderPlotly({
    data <- filteredData()
    unique_pitch_types <- unique(data$PitchType)
    num_colors <- length(unique_pitch_types)
    pitch_colors <- setNames(colorRampPalette(brewer.pal(9, "Set1"))(num_colors), unique_pitch_types)
    linetypes <- setNames(rep_len(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), num_colors), unique_pitch_types)
    
    pvp_game_plot <- ggplot(data = data, aes(x = Inning, y = PitchVelo, color = PitchType, linetype = PitchType)) +
      geom_boxplot(aes(group = Inning), outlier.shape = NA, width = 0.5) +
      geom_jitter(aes(group = Inning), width = 0.2, alpha = 0.5) +
      geom_smooth(se = FALSE, method = "loess", span = 0.3) +
      facet_wrap(~ Date, scales = "free_x") +
      scale_color_manual(values = pitch_colors) +
      scale_linetype_manual(values = linetypes) +
      labs(
        title = "Velo by Inning and Date",
        x = "Inning",
        y = "Pitch Velocity (MPH)",
        color = "Pitch Type",
        linetype = "Pitch Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold")
      )
    
    ggplotly(pvp_game_plot)
  })
  #########################################$
  output$plotOutputs <- renderUI({
    plots <- lapply(conditions, function(condition) {
      plotname <- paste0("plot_", condition$Balls, "_", condition$Strikes)
      plotOutput(plotname)
    })
    do.call(tagList, plots)
  })
  ################Table is starting##############
  observe({
    for (condition in conditions) {
      local({
        my_condition <- condition
        plotname <- paste0("plot_", my_condition$Balls, "_", my_condition$Strikes)
        #########filterdf#############
        output[[plotname]] <- renderPlot({
          filtered_df <- BigMoe %>%
            filter(PitcherTeam == input$pitcherTeam2 & Pitcher == input$pitcher2 & PitchType == input$pitchType) %>% 
            filter(Balls == my_condition$Balls, Strikes == my_condition$Strikes, BatterTeam != 'Moeller')
          
          strike_looking_count <- sum(filtered_df$PitchResult == 'StrikeLooking', na.rm = TRUE)
          total_count <- nrow(filtered_df)
          strike_swinging_count <- sum(filtered_df$PitchResult == 'Strike_Swing_Miss', na.rm = TRUE)
          strike_foul_count <- sum(filtered_df$PitchResult == 'StrikeFoul', na.rm = TRUE)
          strike_inplay_count <- sum(filtered_df$PitchResult == 'StrikeInPlay', na.rm = TRUE)
          ball_count <- sum(filtered_df$PitchResult == 'Ball', na.rm = TRUE)
          ##################if else##############
          if (total_count > 0) {
            percentage_strike_looking <- (strike_looking_count / total_count) * 100
            percentage_strike_swinging <- (strike_swinging_count / total_count) * 100
            percentage_strike_foul <- (strike_foul_count / total_count) * 100
            percentage_strike_inplay <- (strike_inplay_count / total_count) * 100
            percentage_ball <- (ball_count / total_count) * 100
          } else {
            percentage_strike_looking <- 0
            percentage_strike_swinging <- 0
            percentage_strike_foul <- 0
            percentage_strike_inplay <- 0
            percentage_ball <- 0
          }
          ########Dataframe#############
          condition_results <- data.frame(
            Category = c("Strike Looking", "Strike Swing and Miss", "Strike Foul", "Strike In Play", "Ball"),
            Count = c(percentage_strike_looking, percentage_strike_swinging, percentage_strike_foul, percentage_strike_inplay, percentage_ball)
          )
          ########ggplot###############
          ggplot(condition_results, aes(x = Category, y = Count, fill = Category)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = custom_colors) +
            labs(
              title = paste("Outcome Percentages for Balls =", my_condition$Balls, "and Strikes =", my_condition$Strikes),
              x = "Outcome",
              y = "Percentage"
            ) +
            geom_text(aes(label = sprintf("%.1f%%", Count)), vjust = -0.5, size = 3.5) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              axis.text.y = element_text(size = 10),
              plot.title = element_text(size = 14, face = "bold"),
              legend.position = "none"
            )
        })
      })
    }
  })
  ###############
  ###Usage Whole####
  
  # Usage vs Both table
  output$usageTableW <- renderTable({
    filteredData <- BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & Pitcher == input$pitcher3) %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', TwoSeamFastBall = '2FB', ChangeUp = 'CH'))
    filteredData %>%
      group_by(Pitch = PitchType) %>%
      summarize(No = n(),
                `Usage %` = round(n() / nrow(filteredData) * 100, 3),
                `1K` = sum(Strikes == 1),
                `1K%` = round(sum(Strikes == 1) / n() * 100, 3),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
                `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
                `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
                `Whiff%` = round(Whiff / Swings * 100, 3))
    
  })
  
  # Stats
  output$statsVsTable2 <- renderTable({
    filteredData <- BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & 
               Pitcher == input$pitcher3)
    
    # Calculate components for wOBA
    walks <- sum(filteredData$AtBatResult == "BB", na.rm = TRUE)
    hit_by_pitch <- sum(filteredData$AtBatResult == "HBP", na.rm = TRUE)
    singles <- sum(filteredData$AtBatResult == "1B", na.rm = TRUE)
    doubles <- sum(filteredData$AtBatResult == "2B", na.rm = TRUE)
    triples <- sum(filteredData$AtBatResult == "3B", na.rm = TRUE)
    home_runs <- sum(filteredData$AtBatResult == "HR", na.rm = TRUE)
    
    # Calculate the numerator for wOBA
    woba_numerator <- (0.69 * walks) + (0.72 * hit_by_pitch) + (0.89 * singles) + 
      (1.27 * doubles) + (1.62 * triples) + (2.10 * home_runs)
    
    # Calculate the denominator for wOBA
    woba_denominator <- walks + hit_by_pitch + singles + doubles + triples + home_runs +
      sum(filteredData$AtBatResult %in% c("FieldOut", "StrikeOut", "GroundOut", "FlyOut", "LineOut", "DoublePlay"), na.rm = TRUE)
    
    # Calculate wOBA
    woba <- round(woba_numerator / woba_denominator, 3)
    
    # Summary table
    summaryTable2 <- filteredData %>%
      summarize(
        BF = n_distinct(Inning, Batter),
        K = sum(AtBatResult == "StrikeOut", na.rm = TRUE),
        DP = sum(AtBatResult == "DoublePlay", na.rm = TRUE),
        FO = sum(AtBatResult == "FlyOut", na.rm = TRUE),
        GB = sum(AtBatResult == "GroundOut", na.rm = TRUE),
        LO = sum(AtBatResult == "LineOut", na.rm = TRUE),
        Walks = walks,
        wOBA = sprintf("%.3f", woba)
      )
    
    summaryTable2
  })
  
  
  ##############################
  
  # Usage vs RHH table
  output$usageRTable <- renderTable({
    filteredData <- BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & Pitcher == input$pitcher3 & Batter.Hand == 'R') %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', TwoSeamFastBall = '2FB', ChangeUp = 'CH'))
    filteredData %>%
      group_by(Pitch = PitchType) %>%
      summarize(No = n(),
                `Usage %` = round(n() / nrow(filteredData) * 100, 3),
                `1K` = sum(Strikes == 1),
                `1K%` = round(sum(Strikes == 1) / n() * 100, 3),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
                `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
                `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
                `Whiff%` = round(Whiff / Swings * 100, 3))
    
  })
  
  # Stats vs RHH table
  output$statsVsRTable1 <- renderTable({
    filteredData <- BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & 
               Pitcher == input$pitcher3 & 
               Batter.Hand == 'R')
    
    # Calculate components for wOBA
    walks <- sum(filteredData$AtBatResult == "BB", na.rm = TRUE)
    hit_by_pitch <- sum(filteredData$AtBatResult == "HBP", na.rm = TRUE)
    singles <- sum(filteredData$AtBatResult == "1B", na.rm = TRUE)
    doubles <- sum(filteredData$AtBatResult == "2B", na.rm = TRUE)
    triples <- sum(filteredData$AtBatResult == "3B", na.rm = TRUE)
    home_runs <- sum(filteredData$AtBatResult == "HR", na.rm = TRUE)
    
    # Calculate the numerator for wOBA
    woba_numerator <- (0.69 * walks) + (0.72 * hit_by_pitch) + (0.89 * singles) + 
      (1.27 * doubles) + (1.62 * triples) + (2.10 * home_runs)
    
    # Calculate the denominator for wOBA
    woba_denominator <- walks + hit_by_pitch + singles + doubles + triples + home_runs +
      sum(filteredData$AtBatResult %in% c("FieldOut", "StrikeOut", "GroundOut", "FlyOut", "LineOut", "DoublePlay"), na.rm = TRUE)
    
    # Calculate wOBA
    woba <- round(woba_numerator / woba_denominator, 3)
    
    # Summary table
    summaryTable <- filteredData %>%
      summarize(
        BF = n_distinct(Inning, Batter),
        K = sum(AtBatResult == "StrikeOut", na.rm = TRUE),
        DP = sum(AtBatResult == "DoublePlay", na.rm = TRUE),
        FO = sum(AtBatResult == "FlyOut", na.rm = TRUE),
        GB = sum(AtBatResult == "GroundOut", na.rm = TRUE),
        LO = sum(AtBatResult == "LineOut", na.rm = TRUE),
        Walks = walks,
        wOBA = sprintf("%.3f", woba)
      )
    
    summaryTable
  })
  
  
  output$statsVsRTable2 <- renderTable({
    BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & Pitcher == input$pitcher3 & Batter.Hand == 'R') %>%
      summarize(
        H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),
        XBH = sum(AtBatResult %in% c('2B', '3B', 'HR')),
        HBP = sum(AtBatResult == "HBP"),  # Hit By Pitch
        SF = sum(AtBatResult == "SacFly"),  # Sacrifice Fly
        SH = sum(AtBatResult == "SacBunt"),  # Sacrifice Bunt
        Runs = sum(AtBatResult == "Run"),  # Runs scored
        RBI = sum(AtBatResult == "RBI"),  # Runs Batted In
        Hits = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),  # Hits
        Errors = sum(AtBatResult == "Error"),  # Errors
        FO_to_GB_Percent = ifelse((sum(AtBatResult == "FlyOut") + sum(AtBatResult == "GroundOut")) > 0, 
                                  sum(AtBatResult == "FlyOut") / (sum(AtBatResult == "FlyOut") + sum(AtBatResult == "GroundOut")) * 100, NA)  # FO to GB percentage
      )
  })
  
  # Usage vs LHH table
  output$usageLTable <- renderTable({
    filteredData <- BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & Pitcher == input$pitcher3 & Batter.Hand == 'L') %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', TwoSeamFastBall = '2FB', ChangeUp = 'CH'))
    filteredData %>%
      group_by(Pitch = PitchType) %>%
      summarize(No = n(),
                `Usage %` = round(n() / nrow(filteredData) * 100, 3),
                `1K` = sum(Strikes == 1),
                `1K%` = round(sum(Strikes == 1) / n() * 100, 3),
                `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
                `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
                `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
                `Whiff%` = round(Whiff / Swings * 100, 3))
  })
  
  # Stats vs LHH table
  output$statsVsLTable1 <- renderTable({
    filteredData <- BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & 
               Pitcher == input$pitcher3 & 
               Batter.Hand == 'L')
    
    # Calculate components for wOBA
    walks <- sum(filteredData$AtBatResult == "BB", na.rm = TRUE)
    hit_by_pitch <- sum(filteredData$AtBatResult == "HBP", na.rm = TRUE)
    singles <- sum(filteredData$AtBatResult == "1B", na.rm = TRUE)
    doubles <- sum(filteredData$AtBatResult == "2B", na.rm = TRUE)
    triples <- sum(filteredData$AtBatResult == "3B", na.rm = TRUE)
    home_runs <- sum(filteredData$AtBatResult == "HR", na.rm = TRUE)
    
    # Calculate the numerator for wOBA
    woba_numerator <- (0.69 * walks) + (0.72 * hit_by_pitch) + (0.89 * singles) + 
      (1.27 * doubles) + (1.62 * triples) + (2.10 * home_runs)
    
    # Calculate the denominator for wOBA
    woba_denominator <- walks + hit_by_pitch + singles + doubles + triples + home_runs +
      sum(filteredData$AtBatResult %in% c("FieldOut", "StrikeOut", "GroundOut", "FlyOut", "LineOut", "DoublePlay"), na.rm = TRUE)
    
    # Calculate wOBA
    woba <- round(woba_numerator / woba_denominator, 3)
    
    # Summary table
    summaryTable <- filteredData %>%
      summarize(
        BF = n_distinct(Inning, Batter),
        K = sum(AtBatResult == "StrikeOut", na.rm = TRUE),
        DP = sum(AtBatResult == "DoublePlay", na.rm = TRUE),
        FO = sum(AtBatResult == "FlyOut", na.rm = TRUE),
        GB = sum(AtBatResult == "GroundOut", na.rm = TRUE),
        LO = sum(AtBatResult == "LineOut", na.rm = TRUE),
        Walks = walks,
        wOBA = sprintf("%.3f", woba)
      )
    
    summaryTable
  })
  
  
  output$statsVsLTable2 <- renderTable({
    BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam3 & Pitcher == input$pitcher3 & Batter.Hand == 'L') %>%
      summarize(
        H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),
        XBH = sum(AtBatResult %in% c('2B', '3B', 'HR')),
        HBP = sum(AtBatResult == "HBP"),  # Hit By Pitch
        SF = sum(AtBatResult == "SacFly"),  # Sacrifice Fly
        SH = sum(AtBatResult == "SacBunt"),  # Sacrifice Bunt
        Runs = sum(AtBatResult == "Run"),  # Runs scored
        RBI = sum(AtBatResult == "RBI"),  # Runs Batted In
        Hits = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR')),  # Hits
        Errors = sum(AtBatResult == "Error"),  # Errors
        FO_to_GB_Percent = ifelse((sum(AtBatResult == "FlyOut") + sum(AtBatResult == "GroundOut")) > 0, 
                                  sum(AtBatResult == "FlyOut") / (sum(AtBatResult == "FlyOut") + sum(AtBatResult == "GroundOut")) * 100, NA)  # FO to GB percentage
      )
  })
  
  # Batted ball table
  output$battedBallTable <- renderTable({
    BigMoe %>%
      filter(PitcherTeam == input$pitcherTeam4 & Pitcher == input$pitcher4) %>%
      mutate(PitchType = recode(PitchType,
                                "FastBall" = "FB",
                                "BreakingBall" = "BRB",
                                "Slider" = "SL",
                                "TwoSeamFastBall" = "2FB",
                                "Changeup" = "CH",
                                "Splitter" = "SPL")) %>%
      group_by(PitchType) %>%
      summarize(
        No = n(),
        BIP = sum(AtBatResult %in% c('GroundOut', 'FlyOut', 'LineOut')),
        XBH = sum(AtBatResult %in% c("2B", "3B", "HR"))
      )
  })
  
  # Define data for the plots
  filtered_data2 <- BigMoe %>%
    filter(!is.na(AtBatResult) & AtBatResult != "")
  
  grouped_data <- filtered_data2 %>%
    group_by(PitcherTeam, Pitcher, PitchType, AtBatResult) %>%
    summarize(count = n()) %>%
    ungroup()
  
  count_grouped_data <- filtered_data2 %>%
    mutate(CountSituation = paste(Balls, Strikes, sep = "-")) %>%
    group_by(PitcherTeam, Pitcher, CountSituation, AtBatResult) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    filter(count < 20) 
  
  # Plot 1: Batted Ball Types by Team for Each Pitch Type
  output$battedBallPlot1 <- renderPlotly({
    req(input$pitcherTeam4, input$pitcher4, input$pitchType)
    
    filtered1_data1_velo <- grouped_data %>%
      filter(PitcherTeam == input$pitcherTeam4, Pitcher == input$pitcher4, PitchType == input$pitchType)
    
    p1 <- ggplot(filtered1_data1_velo, aes(x = PitchType, y = count, fill = AtBatResult)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Batted Ball Types by Team for Each Pitch Type",
           x = "Pitch Type",
           y = "Count",
           fill = "Batted Ball Type") +
      theme_minimal()
    
    ggplotly(p1) %>%
      layout(showlegend = TRUE)
  })
  
  # Plot 2: Pitch Velocity and Batted Ball Types
  output$battedBallPlot2 <- renderPlotly({
    req(input$pitcherTeam4, input$pitcher4)
    
    filtered_data2_velo <- filtered_data2 %>%
      filter(PitcherTeam == input$pitcherTeam4, Pitcher == input$pitcher4) %>% 
      filter(PitchVelo > 60)
    
    p2 <- ggplot(filtered_data2_velo, aes(x = PitchVelo, y = AtBatResult, color = PitchType)) +
      geom_jitter(alpha = 0.5) +
      labs(title = "Pitch Velocity and Batted Ball Types",
           x = "Pitch Velocity (mph)",
           y = "Batted Ball Type",
           color = "Pitch Type") +
      theme_minimal()
    
    ggplotly(p2) %>%
      layout(showlegend = FALSE)
  })
  
  output$battedBallPlot4 <- renderPlotly({
    req(input$pitcherTeam4, input$pitcher4, input$atBatResult)
    
    filtered3_data1_velo <- count_grouped_data %>%
      filter(PitcherTeam == input$pitcherTeam4, Pitcher == input$pitcher4, AtBatResult == input$atBatResult)
    
    print(filtered3_data1_velo)  # Debugging: Check if the data is as expected
    
    if (nrow(filtered3_data1_velo) == 0) {
      return(NULL)  # No data to plot
    }
    
    max_y <- max(filtered3_data1_velo$count, na.rm = TRUE)
    p4 <- ggplot(filtered3_data1_velo, aes(x = CountSituation, y = count, fill = AtBatResult)) +
      geom_bar(stat = "identity", position = "dodge", width = 1.0) +
      labs(title = "Batted Ball Types in Different Count Situations",
           x = "Count Situation",
           y = "Count",
           fill = "Batted Ball Type") +
      scale_fill_brewer(palette = "Dark2") +  # Using Dark2 palette for better colors
      ylim(0, max(5, max_y)) +  # Dynamically set y-axis limit
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom"
      )
    
    ggplotly(p4) %>%
      layout(showlegend = TRUE)
  })
  
  
  
  output$plotOutputsOpponent <- renderUI({
    plots <- lapply(conditions, function(condition) {
      plotname <- paste0("plotOpponent_", condition$Balls, "_", condition$Strikes)
      plotOutput(plotname)
    })
    do.call(tagList, plots)
  })
  
  observe({
    for (condition in conditions) {
      local({
        my_condition <- condition
        plotname <- paste0("plotOpponent_", my_condition$Balls, "_", my_condition$Strikes)
        output[[plotname]] <- renderPlot({
          filtered_df <- BigMoe %>%
            filter(PitcherTeam == input$pitcherTeamOpponent & Pitcher == input$pitcherOpponent & PitchType == input$pitchTypeOpponent) %>% 
            filter(Balls == my_condition$Balls, Strikes == my_condition$Strikes)
          
          strike_looking_count <- sum(filtered_df$PitchResult == 'StrikeLooking', na.rm = TRUE)
          total_count <- nrow(filtered_df)
          strike_swinging_count <- sum(filtered_df$PitchResult == 'Strike_Swing_Miss', na.rm = TRUE)
          strike_foul_count <- sum(filtered_df$PitchResult == 'StrikeFoul', na.rm = TRUE)
          strike_inplay_count <- sum(filtered_df$PitchResult == 'StrikeInPlay', na.rm = TRUE)
          ball_count <- sum(filtered_df$PitchResult == 'Ball', na.rm = TRUE)
          
          if (total_count > 0) {
            percentage_strike_looking <- (strike_looking_count / total_count) * 100
            percentage_strike_swinging <- (strike_swinging_count / total_count) * 100
            percentage_strike_foul <- (strike_foul_count / total_count) * 100
            percentage_strike_inplay <- (strike_inplay_count / total_count) * 100
            percentage_ball <- (ball_count / total_count) * 100
          } else {
            percentage_strike_looking <- 0
            percentage_strike_swinging <- 0
            percentage_strike_foul <- 0
            percentage_strike_inplay <- 0
            percentage_ball <- 0
          }
          
          condition_results <- data.frame(
            Category = c("Strike Looking", "Strike Swing and Miss", "Strike Foul", "Strike In Play", "Ball"),
            Count = c(percentage_strike_looking, percentage_strike_swinging, percentage_strike_foul, percentage_strike_inplay, percentage_ball)
          )
          
          ggplot(condition_results, aes(x = Category, y = Count, fill = Category)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual(values = custom_colors) +
            labs(
              title = paste("Outcome Percentages for Balls =", my_condition$Balls, "and Strikes =", my_condition$Strikes),
              x = "Outcome",
              y = "Percentage"
            ) +
            geom_text(aes(label = sprintf("%.1f%%", Count)), vjust = -0.5, size = 3.5) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              axis.text.y = element_text(size = 10),
              plot.title = element_text(size = 14, face = "bold"),
              legend.position = "none"
            )
        })
      })
    }
  })
  ###Model#####
  ##################################
  
  df <- read.csv("Moeller_2025_Final_Season.csv")
  
  df <- df %>% 
    mutate(pitcher_success = case_when(
      PitchResult == "Strike Looking" ~ 1,
      PitchResult == "Strike Foul" & Strikes < 2 ~ 1,
      PitchResult == "Strike Swing and Miss" ~ 1,
      AtBatResult %in% c("Ground Out", "Line Out", "Fly Out", "Fielders Choice", 
                         "Strike Out", "Double Play", "Infield Fly") ~ 1,
      TRUE ~ 0),
      pitch_name_no = case_when(
        PitchType %in% c("Fast Ball","Two Seam Fast Ball") ~ 1,
        PitchType %in% c("Breaking Ball","Slider","Curve") ~ 2,
        PitchType %in% c("Splitter","Change Up") ~ 3,
        TRUE ~ 4),
      PitchVelo = as.numeric(PitchVelo),
      Location = as.numeric(Location)
    ) %>%
    filter(!is.na(Location), !is.na(PitchVelo))
  
  # Pitcher Model
  pitcher_mod <- lm(pitcher_success ~ pitch_name_no + Strikes + PitchVelo + Location, data = df)
  
  # Add predictions to the dataframe
  df <- df %>% 
    mutate(pred_pitcher_success_rate = predict(pitcher_mod),
           pred_pitcher_success_rate = case_when(
             pred_pitcher_success_rate < 0 ~ 0,
             pred_pitcher_success_rate > 1 ~ 1,
             TRUE ~ pred_pitcher_success_rate))
  
  # Simple Grouping
  grouped_df <- df %>% 
    filter(PitcherTeam == "Moeller") %>%
    #mutate(Count = paste0(Balls, "-", Strikes)) %>%
    group_by(Pitcher, PitchType, AttackZone, Count) %>%
    summarise(Pitches = n(),
              SuccessRate = mean(pred_pitcher_success_rate, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Pitches >= 5)
  
  # Update the selectInput choices
  updateSelectInput(session, "selected_pitcher", choices = unique(grouped_df$Pitcher))
  updateSelectInput(session, "selected_pitchtype", choices = unique(df$PitchType))
  updateSelectInput(session, "selected_Count", choices = unique(grouped_df$Count))
  
  # Function to create the strike zone tile plot
  moeller_strike_zone_tile <- function(df, option = "AVG") {
    
    ab_events <- c("Ground Out", "Line Out", "Fly Out", "1B", "2B", "Fielders Choice", "3B", "Double Play", "Error", "Infield Fly", "HR", "Strike Out")
    pa_events <- c(ab_events, "Sacrifice", "BB", "HBP")
    
    combined_df <- df %>% 
      rename(zone = Location) %>%
      mutate(
        swing = ifelse(PitchResult %in% c("Strike Swing and Miss", "Strike Foul", "Strike In Play"), 1, 0),
        whiff = ifelse(PitchResult %in% c("Strike Swing and Miss"), 1, 0),
        AB = ifelse(AtBatResult %in% ab_events, 1, 0),
        PA = ifelse(AtBatResult %in% pa_events, 1, 0),
        hit = ifelse(AtBatResult %in% c("1B", "2B", "3B", "HR"), 1, 0),
        BB = ifelse(AtBatResult == "BB", 1, 0),
        HBP = ifelse(AtBatResult == "HBP", 1, 0),
        Single = ifelse(AtBatResult == "1B", 1, 0),
        Double = ifelse(AtBatResult == "2B", 1, 0),
        Triple = ifelse(AtBatResult == "3B", 1, 0),
        HomeRun = ifelse(AtBatResult == "HR", 1, 0))
    
    data <- combined_df %>% 
      mutate(swing = ifelse(PitchResult %in% c("Strike Swing and Miss", "Strike Foul", "Strike In Play"), 1, 0),
             whiff = ifelse(PitchResult %in% c("Strike Swing and Miss"), 1, 0)) %>%
      group_by(zone) %>%
      summarise(N = n(),
                total = nrow(df),
                swing = sum(as.numeric(swing), na.rm = TRUE),
                whiff = sum(as.numeric(whiff), na.rm = TRUE),
                PA = sum(PA),
                AB = sum(AB),
                hit = sum(hit),
                OB = sum(hit) + sum(BB) + sum(HBP),
                TB = sum(Single) + (2 * sum(Double)) + (3 * sum(Triple)) + (4 * sum(HomeRun)),
                success = sum(pitcher_success)) %>% 
      mutate(successper = success / N,
             swingper = swing / N,
             whiffper = whiff / swing,
             pitchper = N / total,
             AVG = hit / AB,
             OBP = OB / PA,
             SLG = TB / AB,
             OBP = ifelse(is.na(OBP) & !is.na(SLG), 0, OBP),
             SLG = ifelse(is.na(SLG) & !is.na(OBP), 0, SLG),
             OPS = OBP + SLG) %>% 
      mutate(text = case_when(
        option == "swing" ~ ifelse(is.na(swingper), "0%", paste0((round(swingper, digits = 2) * 100), "%")),
        option == "whiff" ~ ifelse(is.na(whiffper), "0%", paste0((round(whiffper, digits = 2) * 100), "%")),
        option == "pitch" ~ ifelse(is.na(pitchper), "0%", paste0((round(pitchper, digits = 2) * 100), "%")),
        option == "success" ~ ifelse(is.na(successper), "0%", paste0((round(successper, digits = 2) * 100), "%")),
        option == "AVG" ~ ifelse(is.na(AVG), " ", sub("^0", "", sprintf("%.3f", AVG))),
        option == "OPS" ~ ifelse(is.na(OPS), " ", sub("^0", "", sprintf("%.3f", OPS)))
      )) %>% 
      mutate(color = case_when(
        option == "swing" ~ (ifelse(is.na(swingper), "gray", ifelse(swingper == 0.5, "white", ifelse(swingper > 0.5, "red", "blue")))),
        option == "whiff" ~ (ifelse(is.na(whiffper), "gray", ifelse(whiffper == 0.5, "white", ifelse(whiffper > 0.5, "red", "blue")))),
        option == "pitch" ~ (ifelse(is.na(pitchper), "gray", ifelse(pitchper == 0.075, "white", ifelse(pitchper > 0.075, "red", "blue")))),
        option == "success" ~ (ifelse(is.na(successper), "gray", ifelse(successper == 0.5, "white", ifelse(successper > 0.5, "red", "blue")))),
        option == "AVG" ~ (ifelse(is.na(AVG), "gray", ifelse(AVG == 0.27, "white", ifelse(AVG > 0.27, "red", "blue")))),
        option == "OPS" ~ (ifelse(is.na(OPS), "gray", ifelse(OPS == 0.7, "white", ifelse(OPS > 0.7, "red", "blue"))))
      )) %>% 
      mutate(alpha = 0.4 * case_when(
        option == "swing" ~ ifelse(is.na(swingper), 0, ifelse(swingper == 0.5, 0.5, ifelse(swingper > 0.5, swingper, 1 - swingper))),
        option == "whiff" ~ ifelse(is.na(whiffper), 0, ifelse(whiffper == 0.5, 0.5, ifelse(whiffper > 0.5, whiffper, 1 - whiffper))),
        option == "pitch" ~ ifelse(is.na(pitchper), 0, ifelse(pitchper == 0.075, 0.5, ifelse(pitchper > 0.075, 10 * pitchper, 10 * pitchper))),
        option == "success" ~ ifelse(is.na(successper), 0, ifelse(successper == 0.5, 0.5, ifelse(successper > 0.5, successper, 1 - successper))),
        option == "AVG" ~ ifelse(is.na(AVG), 0, ifelse(AVG == 0.27, 0.27, ifelse(AVG > 0.27, AVG, 0.5 - AVG))),
        option == "OPS" ~ ifelse(is.na(OPS), 0, ifelse(OPS == 0.7, 0.7, ifelse(OPS > 0.7, OPS, 0.7 - OPS)))
      )) %>% 
      arrange(zone)
    
    all_zones <- data.frame(zone = c(1:9, 11:14, 16:19, 21, 23, 27, 29))
    existing_zones <- unique(data$zone)
    missing_zones <- setdiff(all_zones$zone, existing_zones)
    missing_rows <- data.frame(zone = missing_zones, 
                               xwoba = rep(NA, length(missing_zones)),
                               alpha = rep(0, length(missing_zones)),
                               color = rep("gray", length(missing_zones)))
    
    data <- bind_rows(data, missing_rows) %>%
      arrange(zone)
    
    color_plot <- data %>%
      ggplot() +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[7]],
               fill = c(data$color[[7]])) + 
      geom_text(x = -0.6315, y = 1.85, label = (data$text[[7]][1]), color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[8]],
               fill = c(data$color[[8]])) + 
      geom_text(x = 0, y = 1.85, label = data$text[[8]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 1.5,
               ymax = 2.2, 
               color = "black", 
               alpha = data$alpha[[9]],
               fill = c(data$color[[9]])) + 
      geom_text(x = 0.6315, y = 1.85, label = data$text[[9]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.2,
               ymax = 2.9, 
               color = "black", 
               alpha = data$alpha[[4]],
               fill = c(data$color[[4]])) + 
      geom_text(x = -0.6315, y = 2.55, label = data$text[[4]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[5]],
               fill = c(data$color[[5]])) + 
      geom_text(x = 0, y = 2.55, label = data$text[[5]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.2,
               ymax = 2.9,
               color = "black", 
               alpha = data$alpha[[6]],
               fill = c(data$color[[6]])) + 
      geom_text(x = 0.6315, y = 2.55, label = data$text[[6]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = -0.947,
               xmax = -0.316,
               ymin = 2.9,
               ymax = 3.6,
               color = "black", 
               alpha = data$alpha[[1]],
               fill = c(data$color[[1]])) + 
      geom_text(x = -0.6315, y = 3.25, label = data$text[[1]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = -0.316,
               xmax = 0.316,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[2]],
               fill = c(data$color[[2]])) + 
      geom_text(x = 0, y = 3.25, label = data$text[[2]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = 0.316,
               xmax = 0.947,
               ymin = 2.9,
               ymax = 3.6, 
               color = "black", 
               alpha = data$alpha[[3]],
               fill = c(data$color[[3]])) + 
      geom_text(x = 0.6315, y = 3.25, label = data$text[[3]], color = "black", size = 5) +
      annotate(geom = "rect",
               xmin = 0.947,
               xmax = 1.347,
               ymin = 2.067,
               ymax = 3.034,
               color = "black",
               alpha = data$alpha[[14]],
               fill = c(data$color[[14]])) + 
      geom_text(x = 1.147, y = 2.55, label = data$text[[14]], color = "black", size = 4.5) +
      annotate(geom = "rect",
               xmin = -1.347,
               xmax = -0.947,
               ymin = 2.067,
               ymax = 3.034,
               color = "black", 
               alpha = data$alpha[[13]],
               fill = c(data$color[[13]])) + 
      geom_text(x = -1.147, y = 2.55, label = data$text[[13]], color = "black", size = 4.5) +
      annotate(geom = "rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 3.6,
               ymax = 4, 
               color = "black", 
               alpha = data$alpha[[11]],
               fill = c(data$color[[11]])) + 
      geom_text(x = 0 , y = 3.8, label = data$text[[11]], color = "black", size = 4.5) +
      annotate(geom = "rect",
               xmin = -0.449,
               xmax = 0.449,
               ymin = 1.1,
               ymax = 1.5,
               color = "black",
               alpha = data$alpha[[16]],
               fill = c(data$color[[16]])) + 
      geom_text(x = 0, y = 1.3, label = data$text[[16]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347), 
               y = c(4, 4, 3.6, 3.6, 3.034, 3.034),
               color = "black",
               alpha = data$alpha[[10]],
               fill = c(data$color[[10]])) + 
      geom_text(x = -1.147, y = 3.8, label = data$text[[10]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(0.449, 1.347, 1.347, 0.947, 0.947, 0.449), 
               y = c(4, 4, 3.034, 3.034, 3.6, 3.6               ),
               color = "black",
               alpha = data$alpha[[12]],
               fill = c(data$color[[12]])) + 
      geom_text(x = 1.147, y = 3.8, label = data$text[[12]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(-1.347, -0.449, -0.449, -0.947, -0.947, -1.347),
               y = c(1.1, 1.1, 1.5, 1.5, 2.067, 2.067),
               color = "black",
               alpha = data$alpha[[15]],
               fill = c(data$color[[15]])) + 
      geom_text(x = -1.147, y = 1.3, label = data$text[[15]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(0.449, 1.347, 1.347, 0.947, 0.947, 0.449),
               y = c(1.1, 1.1, 2.067, 2.067, 1.5, 1.5),
               color = "black",
               alpha = data$alpha[[17]],
               fill = c(data$color[[17]])) + 
      geom_text(x = 1.147, y = 1.3, label = data$text[[17]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(-2, 0, 0, -1.347, -1.347, -2),
               y = c(4.6, 4.6, 4, 4, 2.5, 2.5),
               color = "black",
               alpha = data$alpha[[18]],
               fill = c(data$color[[18]])) + 
      geom_text(x = -1.674, y = 4.3, label = data$text[[18]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(2, 0, 0, 1.347, 1.347, 2),
               y = c(4.6, 4.6, 4, 4, 2.5, 2.5),
               color = "black",
               alpha = data$alpha[[19]],
               fill = c(data$color[[19]])) + 
      geom_text(x = 1.674, y = 4.3, label = data$text[[19]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(-2, -1.347, -1.347, 0, 0, -2),
               y = c(2.5, 2.5, 1.1, 1.1, 0.5, 0.5),
               color = "black",
               alpha = data$alpha[[20]],
               fill = c(data$color[[20]])) + 
      geom_text(x = -1.674, y = 0.8, label = data$text[[20]], color = "black", size = 4.5) +
      annotate(geom = "polygon",
               x = c(2, 1.347, 1.347, 0, 0, 2),
               y = c(2.5, 2.5, 1.1, 1.1, 0.5, 0.5),
               color = "black",
               alpha = data$alpha[[21]],
               fill = c(data$color[[21]])) + 
      geom_text(x = 1.674, y = 0.8, label = data$text[[21]], color = "black", size = 5) +
      coord_equal() +
      scale_x_continuous("",
                         limits = c(-2, 2)) +
      scale_y_continuous("",
                         limits = c(0.5, 4.6)) + 
      labs(title = paste0("Moeller Pitching ", tools::toTitleCase(option), " on Pitches"),
           subtitle = "Catcher's POV") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            panel.background = element_blank())
    
    color_plot
  }
  
  # Filtered data based on user input
  filtered_data <- reactive({
    df %>%
      filter(Pitcher == input$selected_pitcher, 
             PitchType == input$selected_pitchtype) 
    #Count == input$selected_Count)
  })
  
  # Render the success plot
  output$success_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      moeller_strike_zone_tile(data, option = "success")
    } else {
      ggplot() +
        labs(title = "No Data Available for Selected Inputs") +
        theme_minimal()
    }
  }) 
  ########################
  ######End of Model###########
  
  
  #######################################
  ########### MoeStuff ######
  base_points <- c(
    "Ball" = -0.50,
    "Ball in Play" = 0,
    "Called Strike" = 1.0,
    "Foul" = 0.25,
    "Strike in Play" = 0,
    "Strike Swing and Miss" = 2.0,
    "Swinging Strike" = 2.0,
    "Whiff" = 2.0
  )
  
  location_modifiers <- c(
    "Chase" = 3.0,
    "Shadow" = 2.0,
    "Heart" = 1.0,
    "Waste" = 0.5
  )
  
  in_play_points <- c(
    "Out" = 0.25,
    "Single" = -0.5,
    "1B" = -0.50,
    "2B" = -0.75,
    "3B" = -0.75,
    "Double" = -0.50,
    "Triple" = -0.75,
    "HomeRun" = -1.0,
    "Fielders Choice" = 0.5,
    "FieldersChoice" = 0.5,
    "Infield Fly" = 0.5,
    "Double Play" = 1.0,
    "GroundOut" = 1.0,
    "FlyOut" = 1.0,
    "Line Out" = 0.50,
    "Sacrifice" = 0.25,
    "Error" = 0.0
  )
  
  # 🔁 Main summary
  summary_df <- reactive({
    filtered_data4 <- MoeStuff
    if (input$team_filter != "All") {
      filtered_data4 <- filtered_data4 %>% filter(PitcherTeam123 == input$team_filter)
    }
    
    filtered_data4 %>%
      mutate(
        BaseScore = base_points[PitchResult],
        BaseScore = case_when(
          PitchResult %in% c("Strike in Play", "Ball in Play") & AtBatResult %in% names(in_play_points) ~ in_play_points[AtBatResult],
          TRUE ~ BaseScore
        ),
        BaseScore = case_when(
          PitchResult %in% c("Strike Swing and Miss", "Swinging Strike", "Whiff") & AtBatResult == "Swinging Strikeout" ~ BaseScore + 2.0,
          PitchResult %in% c("Strike Swing and Miss", "Swinging Strike", "Whiff") & AtBatResult == "Strike Out" ~ BaseScore + 1.0,
          PitchResult == "Called Strike" & AtBatResult == "Called Strikeout" ~ BaseScore + 1.5,
          TRUE ~ BaseScore
        ),
        LocationScore = location_modifiers[AttackZone],
        TotalScore = BaseScore + LocationScore + in_play_points
      ) %>%
      filter(!is.na(TotalScore)) %>%
      group_by(Pitcher, PitcherTeam123, PitchType) %>%
      summarise(
        TotalPitches = n(),
        AvgScore = mean(TotalScore),
        TotalScore = sum(TotalScore),
        .groups = "drop"
      ) %>%
      filter(TotalPitches >= 25) %>%
      mutate(
        MoeStuffPlus = round((AvgScore / mean(AvgScore, na.rm = TRUE)) * 100, 1)
      ) %>%
      arrange(desc(MoeStuffPlus))
  })
  
  # 🔁 New: overall average per pitcher
  pitcher_avg_df <- reactive({
    summary_df() %>%
      group_by(Pitcher, PitcherTeam123) %>%
      summarise(
        TotalPitches = sum(TotalPitches),
        AvgMoeStuffPlus = round(mean(MoeStuffPlus, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(AvgMoeStuffPlus))
  })
  
  # Table 1: Pitch Type Level
  output$score_table <- renderDT({
    datatable(
      summary_df() %>%
        select(Pitcher, PitcherTeam123, PitchType, TotalPitches, MoeStuffPlus),
      options = list(pageLength = 20)
    )
  })
  
  # 🔁 Table 2: Pitcher Average
  output$pitcher_avg_table <- renderDT({
    datatable(
      pitcher_avg_df(),
      options = list(pageLength = 20)
    )
  })
  
  ########XGBooost Model#############
  # ✅ Load pre-trained xgboost model
  xgb_model <- readRDS("xgb_model_moeller.rds")
  
  # ✅ Load your input data (same as OldBigMoe, already prepared)
  df3A <- OldBigMoe
  
  location_modifiersML <- c(
    "Chase" = 1.2, "Shadow" = 1.0, "Heart" = 0.8,
    "Waste" = 0.5, "Unknown" = 0.3
  )
  
  # ✅ Prepare features (same as original training pipeline)
  df3A <- df3A %>%
    filter(stringr::str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE))) %>%
    mutate(
      pitch_level = case_when(
        PitchResult == "Strike Swing and Miss" ~ 1.2,
        PitchResult == "Strike Looking" ~ 1.0,
        PitchResult == "Strike Foul" & Strikes < 2 ~ 1.0,
        PitchResult == "Ball" ~ -1.0,
        TRUE ~ 0
      ),
      Counts = paste0(Balls, "-", Strikes),
      Counts_value = case_when(
        Counts == "0-0" ~ 1.0, Counts == "0-1" ~ 1.1, Counts == "0-2" ~ 1.2,
        Counts == "1-0" ~ 1.0, Counts == "1-1" ~ 1.0, Counts == "1-2" ~ 1.0,
        Counts == "2-0" ~ 0.7, Counts == "2-1" ~ 0.9, Counts == "2-2" ~ 1.2,
        Counts == "3-0" ~ 0.9, Counts == "3-1" ~ 0.9, Counts == "3-2" ~ 1.0,
        TRUE ~ 1.0
      ),
      atbat_level = case_when(
        AtBatResult == "Strike Out" ~ 1.2,
        AtBatResult == "Double Play" ~ 2.0,
        AtBatResult == "Infield Fly" ~ 1.0,
        AtBatResult == "Ground Out" ~ 1.0,
        AtBatResult == "Line Out" ~ 1.0,
        AtBatResult == "Fly Out" ~ 1.0,
        AtBatResult == "Fielders Choice" ~ 0.9,
        AtBatResult == "Walk" ~ -1.1,
        AtBatResult == "Single" ~ -1.1,
        AtBatResult == "Double" ~ -1.2,
        AtBatResult == "Triple" ~ -1.3,
        AtBatResult == "Home Run" ~ -1.5,
        TRUE ~ 0.5
      ),
      pitcher_success = (pitch_level + atbat_level + Counts_value) / 2,
      pitch_name_no = case_when(
        PitchType %in% c("Fast Ball", "Two Seam Fast Ball") ~ 1.0,
        PitchType %in% c("Breaking Ball", "Slider", "Curve") ~ 2.0,
        PitchType %in% c("Splitter", "Change Up") ~ 3.0,
        TRUE ~ 4.0
      ),
      Location = as.numeric(Location),
      Location_scaled = scale(Location)[, 1],
      Balls = as.numeric(Balls),
      Strikes = as.numeric(Strikes),
      Outs = as.numeric(Outs),
      Inning = as.numeric(Inning),
      BatterHand_num = ifelse(`Batter.Hand` == "R", 0, 1),
      AttackZone = ifelse(is.na(AttackZone), "Unknown", AttackZone),
      LocationMod = location_modifiersML[AttackZone],
      IsTwoStrikeCount = ifelse(Strikes == 2, 1, 0),
      IsFullCount = ifelse(Balls == 3 & Strikes == 2, 1, 0),
      IsLateInning = ifelse(Inning >= 7, 1, 0),
      AheadInCount = ifelse(Strikes > Balls, 1, 0),
      IsChaseZone = ifelse(AttackZone == "Chase", 1, 0),
      IsLeftyBatter = ifelse(BatterHand_num == 1, 1, 0),
      Outs_2 = ifelse(Outs == 2, 1, 0)
    ) %>%
    filter(!is.na(Location_scaled))
  
  req(nrow(df3A) > 0)
  
  df3A <- df3A %>%
    filter(!(Pitcher %in% c("Nathan McDowell", "Noah Goettke"))) %>%
    filter(!is.na(Location_scaled))
  
  # ✅ Prepare matrix for prediction
  X <- df3A %>%
    select(
      pitch_name_no, Strikes, Balls, Outs, Location_scaled,
      BatterHand_num, Inning, LocationMod, IsTwoStrikeCount,
      IsFullCount, IsLateInning, AheadInCount, IsChaseZone,
      IsLeftyBatter, Outs_2
    ) %>%
    as.matrix()
  
  # ✅ Predict using pre-trained model
  df3A$predicted_success <- predict(xgb_model, newdata = X)
  
  # ✅ Generate pitcher combo summaries
  excluded_pitchers <- c("Zion Theophilus", "Connor Fuhrer")
  eligible_pitchers <- setdiff(unique(df3A$Pitcher), excluded_pitchers)
  
  valid_combos <- list()
  for (starter in excluded_pitchers) {
    combos <- combn(eligible_pitchers, 2, simplify = FALSE)
    for (combo in combos) {
      valid_combos[[length(valid_combos) + 1]] <- c(starter, combo)
    }
  }
  
  combo_scores <- purrr::map_df(valid_combos, function(p) {
    df3A %>%
      filter(Pitcher %in% p) %>%
      summarise(
        Pitcher_1 = p[1],
        Pitcher_2 = p[2],
        Pitcher_3 = p[3],
        Avg_Team_Success = round(mean(predicted_success, na.rm = TRUE), 3)
      )
  }) %>%
    arrange(desc(Avg_Team_Success))
  
  valid_combos_2 <- combn(eligible_pitchers, 2, simplify = FALSE)
  
  pair_scores <- purrr::map_df(valid_combos_2, function(p) {
    df3A %>%
      filter(Pitcher %in% p) %>%
      summarise(
        Pitcher_1 = p[1],
        Pitcher_2 = p[2],
        Avg_Team_Success = round(mean(predicted_success, na.rm = TRUE), 3)
      )
  }) %>%
    arrange(desc(Avg_Team_Success))
  
  individual_pitcher_scores <- df3A %>%
    filter(!(Pitcher %in% excluded_pitchers)) %>%
    group_by(Pitcher) %>%
    summarise(
      Avg_Predicted_Success = round(mean(predicted_success, na.rm = TRUE), 3),
      Total_Pitches = n()
    ) %>%
    arrange(desc(Avg_Predicted_Success))
  
  # ✅ Render outputs
  output$comboTable <- renderDT({
    datatable(combo_scores, options = list(pageLength = 10))
  })
  
  output$pairTable <- renderDT({
    datatable(pair_scores, options = list(pageLength = 10))
  })
  
  output$individualTable <- renderDT({
    datatable(individual_pitcher_scores, options = list(pageLength = 10))
  })
  
  # ✅ Show static RMSE (you should compute this offline and hardcode it if needed)
  output$rmseText <- renderText({
    paste("Test RMSE (pre-trained model):")  # Replace with your offline RMSE
  })
  
  # ✅ Generate excluded + one eligible pitcher pairs (only two-pitcher combos)
  excluded_plus_one_scores <- purrr::map_df(excluded_pitchers, function(excluded_p) {
    purrr::map_df(eligible_pitchers, function(eligible_p) {
      df3A %>%
        filter(Pitcher %in% c(excluded_p, eligible_p)) %>%
        summarise(
          Pitcher_1 = excluded_p,
          Pitcher_2 = eligible_p,
          Avg_Team_Success = round(mean(predicted_success, na.rm = TRUE), 3)
        )
    })
  }) %>%
    arrange(desc(Avg_Team_Success))
  
  output$excludedPlusOneTable <- renderDT({
    datatable(excluded_plus_one_scores, options = list(pageLength = 10))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
