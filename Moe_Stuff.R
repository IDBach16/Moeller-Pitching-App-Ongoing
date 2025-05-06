#setwd("C:/Users/ibach/OneDrive - Terillium/Pictures/Moller Misc/Moeller_2025/Varsity")



# Load required libraries
library(dplyr)
library(readr)

MoeStuff <- read.csv('MoeStuff2025.csv')


# Define scoring systems
base_points <- c(
  "Ball" = -1.0,
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

# Apply scoring
df <- df %>%
  mutate(
    BaseScore = base_points[PitchResult],
    LocationScore = location_modifiers[AttackZone],
    TotalScore = BaseScore + LocationScore
  ) %>%
  filter(!is.na(TotalScore))  # Filter out rows with NA scoring

# Summarize and normalize
summary_df <- df %>%
  group_by(Pitcher, PitchType) %>%
  summarise(
    TotalPitches = n(),
    AvgScore = mean(TotalScore),
    TotalScore = sum(TotalScore),
    .groups = "drop"
  ) %>%
  filter(TotalPitches >= 20) %>%  # Minimum 5 pitches
  mutate(
    ScorePlus = round((AvgScore / mean(AvgScore, na.rm = TRUE)) * 100, 1)
  ) %>%
  arrange(desc(ScorePlus))  # Sort highest to lowest

# View result
print(summary_df)

#write_csv(summary_df, "pitch_score_summary.csv")
