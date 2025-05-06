# Load required libraries
library(shiny)
library(dplyr)
library(DT)

# Set working directory (Ensure this path is correct for your system)
#setwd("C:/Users/ibach/OneDrive - Terillium/Pictures/Moller Misc/Moeller_2025")

# Load dataset
OldBigMoe <- read.csv("Moeller_2024_2025_Final_Season.csv", stringsAsFactors = FALSE)

# Standardizing column names (removing spaces)
names(OldBigMoe) <- gsub(" ", "", names(OldBigMoe))

# Ensure PitchVelo is numeric and remove invalid entries
OldBigMoe$PitchVelo <- as.numeric(OldBigMoe$PitchVelo)

# Data Cleaning & Recoding
BigMoe <- OldBigMoe %>% filter(PitcherTeam != "Moeller") %>%
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
                              "Fielders Choice" = "FieldersChoice")) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Define UI
ui <- fluidPage(
  
  # Custom CSS to improve readability
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://raw.githubusercontent.com/IDBach16/MLB-Pitcher-Analysis/main/Keith__Jeff.jpg');
        background-attachment: fixed;
        background-size: cover;
        font-family: Arial, sans-serif;
      }
      
      .content-box {
        background-color: rgba(255, 255, 255, 0.9);
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.3);
      }
      
      .table-box {
        background-color: rgba(255, 255, 255, 0.95);
        padding: 15px;
        border-radius: 6px;
        overflow: auto;
        max-height: 400px;
      }

      h3 {
        color: #333;
        font-weight: bold;
        text-align: center;
      }

      table {
        width: 100%;
        border-collapse: collapse;
      }

      table th {
        background-color: #0073e6;
        color: white;
        padding: 10px;
        text-align: left;
      }

      table td {
        padding: 8px;
        border-bottom: 1px solid #ddd;
      }

      .sidebar {
        background-color: rgba(255, 255, 255, 0.9);
        padding: 15px;
        border-radius: 8px;
      }

      select {
        font-size: 16px;
        padding: 5px;
      }
    "))
  ),
  
  # Custom CSS for white title
  tags$head(
    tags$style(HTML("
      h2 {
        color: white !important;  /* Makes title text white */
        text-align: center;
        font-size: 28px; /* Adjust size if needed */
        font-weight: bold;
      }
    "))
  ),
  
  titlePanel("Moeller Hitters (2024-2025) - Opponent Pitcher Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("opponentTeam", "Select Opponent Team:", 
                  choices = unique(BigMoe$PitcherTeam[BigMoe$PitcherTeam != "Moeller"])),  
      uiOutput("dateUI"),
      uiOutput("pitchTypeUI")
    ),
    
    mainPanel(
      
      div(class = "content-box",
          h3("2024 & 2025 Seasons Summary Table"),  # ✅ NEW SEASON SUMMARY TABLE
          div(class = "table-box", tableOutput("seasonSummaryTable"))
      ),
      
      div(class = "content-box",
          h3("Pitch Usage by Count Table By Season"),
          div(class = "table-box", DTOutput("pitchUsageByCountTable"))
      ),
      
      div(class = "content-box",
          h3("Pitchers Who Threw to Moeller Hitters By Game"),
          div(class = "table-box", tableOutput("pitcherTable"))
      ),
      
      div(class = "content-box",
          h3("Game Summary Table"),
          div(class = "table-box", tableOutput("gameSummaryTable"))
      ),
      
      div(class = "content-box",
          h3("Pitch Usage - RHP (Game)"),
          div(class = "table-box", tableOutput("usageRHPTable"))
      ),
      
      div(class = "content-box",
          h3("Pitcher Stats - RHP (Game)"),
          div(class = "table-box", tableOutput("statsRHPTable"))
      ),
      
      div(class = "content-box",
          h3("Pitch Usage - LHP"),
          div(class = "table-box", tableOutput("usageLHPTable"))
      ),
      
      div(class = "content-box",
          h3("Pitcher Stats - LHP (Game)"),
          div(class = "table-box", tableOutput("statsLHPTable"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  ########### Filtered Dataset for Selected Opponent ###########
  seasonData <- reactive({
    validate(need(input$opponentTeam, "Please select an opponent team."))
    
    BigMoe %>%
      filter(PitcherTeam == input$opponentTeam)  # ✅ Filter by selected Opponent Team
  })
  
  
  ########### Data Filtering (By Opponent Team) ########### 
  teamData <- reactive({
    validate(need(input$opponentTeam, "Please select an opponent team."))
    
    BigMoe %>%
      filter(BatterTeam == "Moeller",  
             PitcherTeam == input$opponentTeam)  
  })
  
  ########### Generate Dynamic Date Selector ###########
  output$dateUI <- renderUI({
    req(teamData())  # Ensure data is available before creating UI
    selectInput("date", "Select Date:", choices = unique(teamData()$Date))
  })
  
  ########### Filtered Data (By Selected Date) ###########
  filteredData <- reactive({
    req(input$date)
    
    data <- teamData() %>% filter(Date == input$date)  
    
    validate(need(nrow(data) > 0, "No data available for this selection."))
    
    return(data)
  })
  
  ########### Generate Pitch Type Dropdown Dynamically ###########
  output$pitchTypeUI <- renderUI({
    req(seasonData())  # Ensure data is available before creating UI
    selectInput("pitchType", "Select Pitch Type:", 
                choices = c("All", unique(seasonData()$PitchType)), selected = "All")
  })
  
  ########### Season Summary Table ###########
  output$seasonSummaryTable <- renderTable({
    seasonData() %>%
      group_by(PitchType) %>%
      summarize(
        No. = n(),  
        Velo = round(mean(PitchVelo, na.rm = TRUE), 1),
        VeloMax = ifelse(all(is.na(PitchVelo)), NA, round(max(PitchVelo, na.rm = TRUE), 1)),
        Strikes = sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE),
        Balls = sum(PitchResult == "Ball", na.rm = TRUE)
      ) %>%
      mutate(
        `Usage %` = round(No. / sum(No., na.rm = TRUE) * 100, 3),
        Total = Strikes + Balls,
        `Strike %` = ifelse(Total > 0, round((Strikes / Total) * 100, 1), NA),
        `Ball %` = ifelse(Total > 0, round((Balls / Total) * 100, 1), NA)
      )
  })
  
  ########### ✅ FIXED: Interactive Pitch Usage by Count Table ###########
  output$pitchUsageByCountTable <- renderDT({
    data <- seasonData() %>%
      group_by(Count) %>%
      mutate(TotalPitchesInCount = n()) %>%  # ✅ Total pitches thrown in this count
      group_by(Count, PitchType) %>%
      summarize(
        Pitches = n(),
        `Usage %` = round((Pitches / first(TotalPitchesInCount)) * 100, 2)  # ✅ Corrected Usage %
      ) %>%
      arrange(Count, desc(Pitches))  # ✅ Sort by count and most used pitches
    
    # ✅ Filter by selected pitch type (if not "All")
    if (input$pitchType != "All") {
      data <- data %>% filter(PitchType == input$pitchType)
    }
    
    # ✅ Convert to a datatable with better formatting
    datatable(data, 
              options = list(
                pageLength = 10,    # Show 10 rows per page
                autoWidth = TRUE,   # Auto-adjust column width
                order = list(list(0, 'asc')),  # Sort by Count (ascending)
                columnDefs = list(list(className = 'dt-center', targets = "_all"))  # Center text in all columns
              ),
              rownames = FALSE) %>%
      formatStyle('Usage %', 
                  backgroundColor = styleInterval(c(20, 40, 60, 80), 
                                                  c('#ff9999', '#ffcc99', '#ffff99', '#ccff99', '#99ff99')))  # ✅ Color gradient for usage %
  })
  
  
  ########### Pitchers Who Faced Moeller ########### 
  output$pitcherTable <- renderTable({
    filteredData() %>%
 group_by(Pitcher, PitcherHand, PitchType) %>%
  summarize(
    Pitches_Thrown = n(),
    Avg_Velocity = round(mean(PitchVelo, na.rm = TRUE), 1),
    Max_Velocity = round(max(PitchVelo, na.rm = TRUE), 1),
    Strikes = sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE),
    Balls = sum(PitchResult == "Ball", na.rm = TRUE),
    .groups = "drop_last"  # We'll keep grouping by Pitcher for next step
  ) %>%
  mutate(
    Total = Strikes + Balls,
    `Strike %` = ifelse(Total > 0, round((Strikes / Total) * 100, 1), NA),
    `Ball %` = ifelse(Total > 0, round((Balls / Total) * 100, 1), NA)
  ) %>%
  group_by(Pitcher, PitcherHand) %>%
  mutate(
    `Usage %` = round(Pitches_Thrown / sum(Pitches_Thrown) * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(Pitcher, PitchType)
  })
  
  ########### Game Summary Table ########### 
  output$gameSummaryTable <- renderTable({
    filteredData() %>%
      group_by(PitchType) %>%
      summarize(
        No. = n(),  
        Velo = round(mean(PitchVelo, na.rm = TRUE), 1),
        VeloMax = ifelse(all(is.na(PitchVelo)), NA, round(max(PitchVelo, na.rm = TRUE), 1)),
        Strikes = sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE),
        Balls = sum(PitchResult == "Ball", na.rm = TRUE)
      ) %>%
      mutate(
        `Usage %` = round(No. / sum(No., na.rm = TRUE) * 100, 3),
        Total = Strikes + Balls,
        `Strike %` = ifelse(Total > 0, round((Strikes / Total) * 100, 1), NA),
        `Ball %` = ifelse(Total > 0, round((Balls / Total) * 100, 1), NA)
      )
  })
  
  ########### NEW SEASON SUMMARY TABLE (Filtered by Opponent Team) ########### 
  seasonSummaryData <- reactive({
    validate(need(input$opponentTeam, "Please select an opponent team."))
    
    BigMoe %>%
      filter(PitcherTeam == input$opponentTeam) %>%  # ✅ Filter by selected opponent team
      group_by(PitchType) %>%
      summarize(
        No. = n(),  
        Velo = round(mean(PitchVelo, na.rm = TRUE), 1),
        VeloMax = ifelse(all(is.na(PitchVelo)), NA, round(max(PitchVelo, na.rm = TRUE), 1)),
        Strikes = sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE),
        Balls = sum(PitchResult == "Ball", na.rm = TRUE),
        `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
        `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
        `Whiff%` = round(Whiff / Swings * 100, 3)
      ) %>%
      mutate(
        `Usage %` = round(No. / sum(No., na.rm = TRUE) * 100, 3),
        Total = Strikes + Balls,
        `Strike %` = ifelse(Total > 0, round((Strikes / Total) * 100, 1), NA),
        `Ball %` = ifelse(Total > 0, round((Balls / Total) * 100, 1), NA)
      )
  })
  
  output$seasonSummaryTable <- renderTable({
    seasonSummaryData()
  })
  
  ########### Usage Stats for RHP ########### 
  output$usageRHPTable <- renderTable({
    # Get total pitches thrown by Right-Handed Pitchers
    total_pitches_rhp <- filteredData() %>%
      filter(PitcherHand == 'R') %>%
      nrow()
    
    # Compute pitch usage data
    filteredData() %>%
      filter(PitcherHand == 'R') %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', Slider = 'SL',
                                TwoSeamFastBall = '2FB', ChangeUp = 'CH', Splitter = 'SPL')) %>%
      group_by(Pitch = PitchType) %>%
      summarize(
        No. = n(),  
        `Usage %` = ifelse(total_pitches_rhp > 0, round(n() / total_pitches_rhp * 100, 3), NA),  # ✅ Usage % based on total pitches thrown by RHP
        `2K` = sum(Strikes == 2),
        `2K%` = round(sum(Strikes == 2) / n() * 100, 3),
        `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
        `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
        `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
        `Whiff%` = round(Whiff / Swings * 100, 3)
      )
  })
  
  
  ########### Usage Stats for LHP ########### 
  
  output$usageLHPTable <- renderTable({
    total_pitches_lhp <- filteredData() %>%
      filter(PitcherHand == 'L') %>%
      nrow()
    
    # ✅ Compute pitch usage data
    filteredData() %>%
      filter(PitcherHand == 'L') %>%
      mutate(PitchType = recode(PitchType, FastBall = "FB", BreakingBall = 'BRB', Slider = 'SL',
                                TwoSeamFastBall = '2FB', ChangeUp = 'CH', Splitter = 'SPL')) %>%
      group_by(Pitch = PitchType) %>%
      summarize(
        No. = n(),  
        `Usage %` = ifelse(total_pitches_lhp > 0, round(n() / total_pitches_lhp * 100, 3), NA),  # ✅ Usage % based on total pitches thrown by LHP
        `2K` = sum(Strikes == 2),
        `2K%` = round(sum(Strikes == 2) / n() * 100, 3),
        `Strk%` = round(sum(PitchResult %in% c("StrikeLooking", "StrikeInPlay", "Strike_Swing_Miss", "StrikeFoul"), na.rm = TRUE) / n() * 100, 3),
        `Whiff` = sum(PitchResult == "Strike_Swing_Miss"),
        `Swings` = sum(PitchResult %in% c("Strike_Swing_Miss", "StrikeFoul")),
        `Whiff%` = round(Whiff / Swings * 100, 3)
      )
  })
  
  
  ########### Pitcher Stats for RHP ########### 
  statsRHPData <- reactive({
    req(filteredData())
    
    filteredData() %>%
      filter(PitcherHand == 'R') %>%
      summarize(BF = n_distinct(paste(Inning, Pitcher)),
                K = sum(AtBatResult == "StrikeOut", na.rm = TRUE),
                Walks = sum(AtBatResult == "BB", na.rm = TRUE),
                H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR'), na.rm = TRUE))
  })
  
  output$statsRHPTable <- renderTable({
    statsRHPData()
  })
  
  ########### Pitcher Stats for LHP ########### 
  statsLHPData <- reactive({
    req(filteredData())
    
    filteredData() %>%
      filter(PitcherHand == 'L') %>%
      summarize(BF = n_distinct(paste(Inning, Pitcher)),
                K = sum(AtBatResult == "StrikeOut", na.rm = TRUE),
                Walks = sum(AtBatResult == "BB", na.rm = TRUE),
                H = sum(AtBatResult %in% c('1B', '2B', '3B', 'HR'), na.rm = TRUE))
  })
  
  output$statsLHPTable <- renderTable({
    statsLHPData()
  })
}

# Run App
shinyApp(ui = ui, server = server)
