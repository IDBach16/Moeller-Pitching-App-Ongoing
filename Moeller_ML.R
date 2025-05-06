# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(ranger)  # <- for Random Forests

# Load data
df <- read_csv("Moeller_2025_Final_Season.csv")

# Feature Engineering (unchanged)
df <- df %>%
  mutate(
    pitch_level = case_when(
      PitchResult == "Strike Swing and Miss" ~ 1.0,
      PitchResult == "Strike Looking" ~ 0.9,
      PitchResult == "Strike Foul" & Strikes < 2 ~ 0.7,
      TRUE ~ 0
    ),
    atbat_level = case_when(
      AtBatResult == "Strike Out" ~ 2.0,
      AtBatResult == "Double Play" ~ 1.0,
      AtBatResult == "Infield Fly" ~ 1.0,
      AtBatResult == "Ground Out" ~ 1.0,
      AtBatResult == "Line Out" ~ 1.0,
      AtBatResult == "Fly Out" ~ 1.0,
      AtBatResult == "Fielders Choice" ~ 1.0,
      AtBatResult == "Single" ~ -0.4,
      AtBatResult == "Double" ~ -0.6,
      AtBatResult == "Triple" ~ -0.8,
      AtBatResult == "Home Run" ~ -1.0,
      TRUE ~ 0.5
    ),
    pitcher_success = (pitch_level + atbat_level) / 2,
    pitch_name_no = case_when(
      PitchType %in% c("Fast Ball", "Two Seam Fast Ball") ~ 1,
      PitchType %in% c("Breaking Ball", "Slider", "Curve") ~ 2,
      PitchType %in% c("Splitter", "Change Up") ~ 3,
      TRUE ~ 4
    ),
    PitchVelo = as.numeric(PitchVelo),
    Location = as.numeric(Location)
  ) %>%
  filter(
    !is.na(Location),
    !is.na(PitchVelo),
    str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE))
  )

# Train-test split
set.seed(123)
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train random forest with ranger
rf_model <- ranger(
  formula = pitcher_success ~ pitch_name_no + Strikes + PitchVelo + Location,
  data = train_data,
  num.trees = 1000,
  mtry = 2,
  importance = "impurity",
  seed = 123
)

# Predict on test set
test_preds <- predict(rf_model, data = test_data)$predictions

# Calculate RMSE
rmse <- sqrt(mean((test_data$pitcher_success - test_preds)^2))
cat("Test RMSE (Random Forest):", rmse, "\n")

# Predict on full dataset for pitcher evaluation
df$predicted_success <- predict(rf_model, data = df)$predictions

# Pitcher evaluation (same as before)
starter_only <- c("Zion Theophilus", "Luke Pappano", "Connor Fuhrer")
eligible_pitchers <- setdiff(unique(df$Pitcher), starter_only)

valid_combos <- list()
for (starter in starter_only) {
  other_combos <- combn(eligible_pitchers, 2, simplify = FALSE)
  for (combo in other_combos) {
    valid_combos[[length(valid_combos) + 1]] <- c(starter, combo)
  }
}

combo_scores <- map_df(valid_combos, function(p) {
  df %>%
    filter(Pitcher %in% p) %>%
    summarise(
      Pitcher_1 = p[1],
      Pitcher_2 = p[2],
      Pitcher_3 = p[3],
      Avg_Team_Success = mean(predicted_success, na.rm = TRUE)
    )
})

top_combos <- combo_scores %>%
  arrange(desc(Avg_Team_Success))

print(top_combos, n = 50)


###############################################################
####Individual pairinings
###############################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(xgboost)

# Load the dataset
df2 <- read.csv("Moeller_2025_Final_Season.csv")

# Improved Feature Engineering
df2 <- df2 %>%
  mutate(
    pitch_level = case_when(
      PitchResult == "Strike Swing and Miss" ~ 1.0,
      PitchResult == "Strike Looking" ~ 0.9,
      PitchResult == "Strike Foul" & Strikes < 2 ~ 0.7,
      TRUE ~ 0
    ),
    atbat_level = case_when(
      AtBatResult == "Strike Out" ~ 2.0,
      AtBatResult == "Double Play" ~ 1.0,
      AtBatResult == "Infield Fly" ~ 1.0,
      AtBatResult == "Ground Out" ~ 1.0,
      AtBatResult == "Line Out" ~ 1.0,
      AtBatResult == "Fly Out" ~ 1.0,
      AtBatResult == "Fielders Choice" ~ 1.0,
      AtBatResult == "Single" ~ -0.4,
      AtBatResult == "Double" ~ -0.6,
      AtBatResult == "Triple" ~ -0.8,
      AtBatResult == "Home Run" ~ -1.0,
      TRUE ~ 0.5
    ),
    pitcher_success = (pitch_level + atbat_level) / 2,
    pitch_name_no = case_when(
      PitchType %in% c("Fast Ball", "Two Seam Fast Ball") ~ 1,
      PitchType %in% c("Breaking Ball", "Slider", "Curve") ~ 2,
      PitchType %in% c("Splitter", "Change Up") ~ 3,
      TRUE ~ 4
    ),
    PitchVelo = as.numeric(PitchVelo),
    Location = as.numeric(Location)
  ) %>%
  filter(
    !is.na(Location),
    !is.na(PitchVelo),
    str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE))
  )

# Prepare data for XGBoost
X <- model.matrix(pitcher_success ~ pitch_name_no + Strikes + PitchVelo + Location, data = df2)[, -1]
y <- df2$pitcher_success

# Train XGBoost model
xgb_mod <- xgboost(
  data = X, 
  label = y, 
  objective = "reg:squarederror", 
  nrounds = 100, 
  verbose = 0
)

# Predict success probabilities
df2$predicted_success <- predict(xgb_mod, newdata = X)

# Exclude specific pitchers
excluded_pitchers <- c("Zion Theophilus", "Luke Pappano", "Connor Fuhrer")

# Get list of Moeller pitchers excluding the three
all_pitchers <- df2 %>%
  filter(!(Pitcher %in% excluded_pitchers)) %>%
  pull(Pitcher) %>%
  unique()

# Generate all valid 2-pitcher combinations
valid_combos <- combn(all_pitchers, 2, simplify = FALSE)

# Score each combination based on predicted success
combo_scores <- map_df(valid_combos, function(p) {
  df2 %>%
    filter(Pitcher %in% p) %>%
    summarise(,
      Pitcher_1 = p[1],
      Pitcher_2 = p[2],
      Avg_Team_Success = mean(predicted_success, na.rm = TRUE)
    )
})

# Rank combos by success
top_combos <- combo_scores %>%
  arrange(desc(Avg_Team_Success))

# Print top 50 combos
head(top_combos, 50)


##########################
#######Invidual pitchers#########
# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(xgboost)

# Load the dataset
df3 <- read.csv("Moeller_2025_Final_Season.csv")

# Improved Feature Engineering
df3 <- df3 %>%
  mutate(
    pitch_level = case_when(
      PitchResult == "Strike Swing and Miss" ~ 1.0,
      PitchResult == "Strike Looking" ~ 0.9,
      PitchResult == "Strike Foul" & Strikes < 2 ~ 0.7,
      TRUE ~ 0
    ),
    atbat_level = case_when(
      AtBatResult == "Strike Out" ~ 2.0,
      AtBatResult == "Double Play" ~ 1.0,
      AtBatResult == "Infield Fly" ~ 1.0,
      AtBatResult == "Ground Out" ~ 1.0,
      AtBatResult == "Line Out" ~ 1.0,
      AtBatResult == "Fly Out" ~ 1.0,
      AtBatResult == "Fielders Choice" ~ 1.0,
      AtBatResult == "Single" ~ -0.4,
      AtBatResult == "Double" ~ -0.6,
      AtBatResult == "Triple" ~ -0.8,
      AtBatResult == "Home Run" ~ -1.0,
      TRUE ~ 0.5
    ),
    pitcher_success = (pitch_level + atbat_level) / 2,
    pitch_name_no = case_when(
      PitchType %in% c("Fast Ball", "Two Seam Fast Ball") ~ 1,
      PitchType %in% c("Breaking Ball", "Slider", "Curve") ~ 2,
      PitchType %in% c("Splitter", "Change Up") ~ 3,
      TRUE ~ 4
    ),
    PitchVelo = as.numeric(PitchVelo),
    Location = as.numeric(Location)
  ) %>%
  filter(
    !is.na(Location),
    !is.na(PitchVelo),
    str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE))
  )

# Prepare data for XGBoost
X <- model.matrix(pitcher_success ~ pitch_name_no + Strikes + PitchVelo + Location, data = df3)[, -1]
y <- df3$pitcher_success

# Train XGBoost model
xgb_mod <- xgboost(
  data = X, 
  label = y, 
  objective = "reg:squarederror", 
  nrounds = 1000, 
  verbose = 0
)

# Predict success probabilities
df3$predicted_success <- predict(xgb_mod, newdata = X)

# Exclude specific pitchers
excluded_pitchers <- c("Zion Theophilus", "Luke Pappano", "Connor Fuhrer")

# Get list of Moeller pitchers excluding the three
all_pitchers <- df3 %>%
  filter(!(Pitcher %in% excluded_pitchers)) %>%
  pull(Pitcher) %>%
  unique()

# Generate all valid 2-pitcher combinations
valid_combos <- combn(all_pitchers, 2, simplify = FALSE)

# Score each combination based on predicted success
combo_scores <- map_df(valid_combos, function(p) {
  df3 %>%
    filter(Pitcher %in% p) %>%
    summarise(
      Pitcher_1 = p[1],
      Pitcher_2 = p[2],
      Avg_Team_Success = mean(predicted_success, na.rm = TRUE)
    )
})

# Rank combos by success
top_combos <- combo_scores %>%
  arrange(desc(Avg_Team_Success))

# Print top 50 combos
print(head(top_combos, 50))

# ================================
# NEW SECTION: Individual pitcher predicted success
# ================================
individual_pitcher_scores <- df3 %>%
  filter(!(Pitcher %in% excluded_pitchers)) %>%
  group_by(Pitcher) %>%
  summarise(
    Avg_Predicted_Success = mean(predicted_success, na.rm = TRUE),
    Total_Pitches = n()
  ) %>%
  arrange(desc(Avg_Predicted_Success))

# Print all pitchers and their predicted success
print(individual_pitcher_scores, n = Inf)

