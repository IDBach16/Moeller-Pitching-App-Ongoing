#setwd("/Users/ianbach/Desktop/MoellerPitcher2025")
# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(xgboost)

# Load the dataset
df3 <- read_csv("Moeller_2025_Final_Season.csv")

# Define location modifiers
location_modifiers <- c(
  "Chase" = 1.2,
  "Shadow" = 1.0,
  "Heart" = 0.8,
  "Waste" = 0.5,
  "Unknown" = 0.3
)

# Feature Engineering
df3 <- df3 %>%
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
      Counts == "2-0" ~ 1.0, Counts == "2-1" ~ 1.0, Counts == "2-2" ~ 1.2,
      Counts == "3-0" ~ 1.0, Counts == "3-1" ~ 1.0, Counts == "3-2" ~ 1.2,
      TRUE ~ 1.0 # Fallback to avoid NA
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
    pitcher_success = (pitch_level + atbat_level) / 2,
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
    BatterHand_num = ifelse(`Batter Hand` == "R", 0, 1),
    AttackZone = ifelse(is.na(AttackZone), "Unknown", AttackZone),
    LocationMod = location_modifiers[AttackZone],
    IsTwoStrikeCount = ifelse(Strikes == 2, 1, 0),
    IsFullCount = ifelse(Balls == 3 & Strikes == 2, 1, 0),
    IsLateInning = ifelse(Inning >= 7, 1, 0),
    AheadInCount = ifelse(Strikes > Balls, 1, 0),
    IsChaseZone = ifelse(AttackZone == "Chase", 1, 0),
    IsLeftyBatter = ifelse(BatterHand_num == 1, 1, 0),
    Outs_2 = ifelse(Outs == 2, 1, 0)
  ) %>%
  filter(!is.na(Location_scaled))  # Remove rows with NA Location

# Feature Matrix (no velocity)
X <- df3 %>%
  select(
    pitch_name_no, Strikes, Balls, Outs, Location_scaled,
    BatterHand_num, Inning, LocationMod, IsTwoStrikeCount,
    IsFullCount, IsLateInning, AheadInCount, IsChaseZone,
    IsLeftyBatter, Outs_2
  ) %>%
  as.matrix()

y <- df3$pitcher_success

# Train/test split
set.seed(123)
train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Cross-validated training
cv <- xgb.cv(
  data = X_train,
  label = y_train,
  nfold = 5,
  nrounds = 1000,
  early_stopping_rounds = 10,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

# Train final model
best_rounds <- cv$best_iteration
xgb_model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = best_rounds,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

# Evaluate RMSE
test_preds <- predict(xgb_model, X_test)
rmse <- sqrt(mean((y_test - test_preds)^2))
cat("Test RMSE (after CV, no velocity):", rmse, "\n")

# Predict on full dataset
df3$predicted_success <- predict(xgb_model, newdata = X)

# Feature importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X))
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

# Pitcher summary
excluded_pitchers <- c("Zion Theophilus", "Connor Fuhrer")
individual_pitcher_scores <- df3 %>%
  filter(
    str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE)),
    !(Pitcher %in% excluded_pitchers)
  ) %>%
  group_by(Pitcher) %>%
  summarise(
    Avg_Predicted_Success = mean(predicted_success, na.rm = TRUE),
    Total_Pitches = n()
  ) %>%
  arrange(desc(Avg_Predicted_Success))

print(individual_pitcher_scores, n = Inf)

# Optional EDA
summary(df3$Location)
hist(df3$Location, breaks = 50, main = "Histogram of Location", xlab = "Location")



#########Invdividual players
#################

# Load libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(xgboost)
library(purrr)

# Load dataset
df <- read_csv("Moeller_2025_Final_Season.csv")

# Define location modifiers
location_modifiers <- c(
  "Chase" = 1.2,
  "Shadow" = 1.0,
  "Heart" = 0.8,
  "Waste" = 0.5,
  "Unknown" = 0.3
)

# Feature Engineering
df <- df %>%
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
      Counts == "0-0" ~ 1.0,
      Counts == "0-1" ~ 1.1,
      Counts == "0-2" ~ 1.2,
      Counts == "1-0" ~ 1.0,
      Counts == "1-1" ~ 1.0,
      Counts == "1-2" ~ 1.0,
      Counts == "2-0" ~ 1.0,
      Counts == "2-1" ~ 1.0,
      Counts == "2-2" ~ 1.2,
      Counts == "3-0" ~ 1.0,
      Counts == "3-1" ~ 1.0,
      Counts == "3-2" ~ 1.2,
      TRUE ~ 1.0   # <- FIX: prevent NA
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
    pitcher_success = (pitch_level + atbat_level) / 2,
    pitch_name_no = case_when(
      PitchType %in% c("Fast Ball", "Two Seam Fast Ball") ~ 1.0,
      PitchType %in% c("Breaking Ball", "Slider", "Curve") ~ 2.0,
      PitchType %in% c("Splitter", "Change Up") ~ 3.0,
      TRUE ~ 4.0
    ),
    Location = as.numeric(Location),
    Location_scaled = scale(Location),
    Strikes = as.numeric(Strikes),
    Balls = as.numeric(Balls),
    Outs = as.numeric(Outs),
    Inning = as.numeric(Inning),
    BatterHand_num = ifelse(`Batter Hand` == "R", 0, 1),
    AttackZone = ifelse(is.na(AttackZone), "Unknown", AttackZone),
    LocationMod = location_modifiers[AttackZone],
    IsTwoStrikeCount = ifelse(Strikes == 2, 1, 0),
    IsFullCount = ifelse(Balls == 3 & Strikes == 2, 1, 0),
    IsLateInning = ifelse(Inning >= 7, 1, 0),
    AheadInCount = ifelse(Strikes > Balls, 1, 0),
    IsChaseZone = ifelse(AttackZone == "Chase", 1, 0),
    IsLeftyBatter = ifelse(BatterHand_num == 1, 1, 0),
    Outs_2 = ifelse(Outs == 2, 1, 0)
  ) %>%
  filter(!is.na(Location_scaled), str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE)))

# Design matrix (reduced + cleaned)
X <- model.matrix(
  pitcher_success ~ pitch_name_no + Strikes + Balls + Outs + Location_scaled +
    BatterHand_num + Inning + LocationMod + Counts_value,
  data = df
)[, -1]

y <- df$pitcher_success

# Train/test split
set.seed(123)
train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Cross-validation with early stopping
cv <- xgb.cv(
  data = X_train,
  label = y_train,
  nfold = 5,
  nrounds = 1000,
  early_stopping_rounds = 10,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

best_nrounds <- cv$best_iteration

# Train final model
xgb_model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = best_nrounds,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

# Predict
test_preds <- predict(xgb_model, X_test)
rmse <- sqrt(mean((y_test - test_preds)^2))
cat("Test RMSE after CV:", rmse, "\n")

# Add predicted success to full dataset
df$predicted_success <- predict(xgb_model, newdata = X)

# Define starter pitchers
starter_only <- c("Zion Theophilus", "Connor Fuhrer")

# Identify eligible pitchers (excluding starters)
eligible_pitchers <- setdiff(unique(df$Pitcher), starter_only)

# Generate all valid 3-pitcher combinations (1 starter + 2 others)
valid_combos <- list()
for (starter in starter_only) {
  combos <- combn(eligible_pitchers, 2, simplify = FALSE)
  for (combo in combos) {
    valid_combos[[length(valid_combos) + 1]] <- c(starter, combo)
  }
}

# Score each 3-pitcher combo
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

# Rank and display top 50 3-man combinations
top_combos <- combo_scores %>% arrange(desc(Avg_Team_Success))
print(top_combos, n = 50)

# Now calculate 2-pitcher combos (excluding the 3 starters)
excluded_pitchers <- starter_only

all_pitchers <- df %>%
  filter(!(Pitcher %in% excluded_pitchers)) %>%
  pull(Pitcher) %>%
  unique()

valid_combos_2 <- combn(all_pitchers, 2, simplify = FALSE)

# Score each 2-pitcher combo
pair_scores <- map_df(valid_combos_2, function(p) {
  df %>%
    filter(Pitcher %in% p) %>%
    summarise(
      Pitcher_1 = p[1],
      Pitcher_2 = p[2],
      Avg_Team_Success = mean(predicted_success, na.rm = TRUE)
    )
})

# Rank and display top 50 2-man combinations
pair_scores <- pair_scores %>% arrange(desc(Avg_Team_Success))
print(pair_scores, n = 50)

################################
##IDVModel#####

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(xgboost)

# Load the dataset
df3 <- read_csv("Moeller_2025_Final_Season.csv")

# Define location modifiers
location_modifiers <- c(
  "Chase" = 1.2,
  "Shadow" = 1.0,
  "Heart" = 0.8,
  "Waste" = 0.5,
  "Unknown" = 0.3
)

# Feature Engineering
df3 <- df3 %>%
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
      Counts == "2-0" ~ 1.0, Counts == "2-1" ~ 1.0, Counts == "2-2" ~ 1.2,
      Counts == "3-0" ~ 1.0, Counts == "3-1" ~ 1.0, Counts == "3-2" ~ 1.2,
      TRUE ~ 1.0 # Fallback to avoid NA
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
    pitcher_success = (pitch_level + atbat_level) / 2,
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
    BatterHand_num = ifelse(`Batter Hand` == "R", 0, 1),
    AttackZone = ifelse(is.na(AttackZone), "Unknown", AttackZone),
    LocationMod = location_modifiers[AttackZone],
    IsTwoStrikeCount = ifelse(Strikes == 2, 1, 0),
    IsFullCount = ifelse(Balls == 3 & Strikes == 2, 1, 0),
    IsLateInning = ifelse(Inning >= 7, 1, 0),
    AheadInCount = ifelse(Strikes > Balls, 1, 0),
    IsChaseZone = ifelse(AttackZone == "Chase", 1, 0),
    IsLeftyBatter = ifelse(BatterHand_num == 1, 1, 0),
    Outs_2 = ifelse(Outs == 2, 1, 0)
  ) %>%
  filter(!is.na(Location_scaled))  # Remove rows with NA Location

# Feature Matrix (no velocity)
X <- df3 %>%
  select(
    pitch_name_no, Strikes, Balls, Outs, Location_scaled,
    BatterHand_num, Inning, LocationMod, IsTwoStrikeCount,
    IsFullCount, IsLateInning, AheadInCount, IsChaseZone,
    IsLeftyBatter, Outs_2
  ) %>%
  as.matrix()

y <- df3$pitcher_success

# Train/test split
set.seed(123)
train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Cross-validated training
cv <- xgb.cv(
  data = X_train,
  label = y_train,
  nfold = 5,
  nrounds = 1000,
  early_stopping_rounds = 10,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

# Train final model
best_rounds <- cv$best_iteration
xgb_model <- xgboost(
  data = X_train,
  label = y_train,
  nrounds = best_rounds,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

# Evaluate RMSE
test_preds <- predict(xgb_model, X_test)
rmse <- sqrt(mean((y_test - test_preds)^2))
cat("Test RMSE (after CV, no velocity):", rmse, "\n")

# Predict on full dataset
df3$predicted_success <- predict(xgb_model, newdata = X)

# Feature importance
importance_matrix <- xgb.importance(model = xgb_model, feature_names = colnames(X))
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

# Pitcher summary
excluded_pitchers <- c("Zion Theophilus", "Connor Fuhrer")
individual_pitcher_scores <- df3 %>%
  filter(
    str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE)),
    !(Pitcher %in% excluded_pitchers)
  ) %>%
  group_by(Pitcher) %>%
  summarise(
    Avg_Predicted_Success = mean(predicted_success, na.rm = TRUE),
    Total_Pitches = n()
  ) %>%
  arrange(desc(Avg_Predicted_Success))

print(individual_pitcher_scores, n = Inf)

# Optional EDA
summary(df3$Location)
hist(df3$Location, breaks = 50, main = "Histogram of Location", xlab = "Location")
