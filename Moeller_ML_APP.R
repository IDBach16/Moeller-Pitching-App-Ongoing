# build_pitcher_success_model.R

library(dplyr)
library(stringr)
library(xgboost)
library(purrr)

# Load your raw data
df3A <- read.csv("Moeller_2025_Final_Season.csv")

# Check that data loaded
if (nrow(df3A) == 0) stop("Data did not load correctly!")

# Define location modifiers
location_modifiersML <- c(
  "Chase" = 1.2, "Shadow" = 1.0, "Heart" = 0.8,
  "Waste" = 0.5, "Unknown" = 0.3
)

# Prepare the data
df3A <- df3A %>%
  filter(str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE))) %>%
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
    BatterHand_num = ifelse(Batter.Hand == "R", 0, 1),
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

if (nrow(df3A) == 0) stop("No data left after filtering!")

# Prepare matrices
X <- df3A %>%
  select(
    pitch_name_no, Strikes, Balls, Outs, Location_scaled,
    BatterHand_num, Inning, LocationMod, IsTwoStrikeCount,
    IsFullCount, IsLateInning, AheadInCount, IsChaseZone,
    IsLeftyBatter, Outs_2
  ) %>%
  as.matrix()

y <- df3A$pitcher_success

# Train/test split
set.seed(123)
train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]

# Cross-validation
cv <- xgb.cv(
  data = X_train,
  label = y_train,
  nfold = 5,
  nrounds = 10000,
  early_stopping_rounds = 10,
  max_depth = 6,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  verbose = 0
)

best_rounds <- cv$best_iteration

# Train final model
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

# Save the model
saveRDS(xgb_model, "xgb_model_moeller.rds")

# Evaluate on test set
test_preds <- predict(xgb_model, X_test)
rmse <- round(sqrt(mean((y_test - test_preds)^2)), 3)
cat("Test RMSE:", rmse, "\n")

# Save predictions for Shiny app (optional)
df3A$predicted_success <- predict(xgb_model, newdata = X)
saveRDS(df3A, "moeller_with_predictions.rds")

cat("Model and prediction file saved!\n")


##############

##################################
# observeEvent(input$run_model, {
#   df3A <- OldBigMoe
#   req(df3A)
#   
#   location_modifiersML <- c(
#     "Chase" = 1.2, "Shadow" = 1.0, "Heart" = 0.8,
#     "Waste" = 0.5, "Unknown" = 0.3
#   )
#   
#   df3A <- df3A %>%
#     filter(str_detect(PitcherTeam, regex("Moeller", ignore_case = TRUE))) %>%
#     mutate(
#       pitch_level = case_when(
#         PitchResult == "Strike Swing and Miss" ~ 1.2,
#         PitchResult == "Strike Looking" ~ 1.0,
#         PitchResult == "Strike Foul" & Strikes < 2 ~ 1.0,
#         PitchResult == "Ball" ~ -1.0,
#         TRUE ~ 0
#       ),
#       Counts = paste0(Balls, "-", Strikes),
#       Counts_value = case_when(
#         Counts == "0-0" ~ 1.0, Counts == "0-1" ~ 1.1, Counts == "0-2" ~ 1.2,
#         Counts == "1-0" ~ 1.0, Counts == "1-1" ~ 1.0, Counts == "1-2" ~ 1.0,
#         Counts == "2-0" ~ 0.7, Counts == "2-1" ~ 0.9, Counts == "2-2" ~ 1.2,
#         Counts == "3-0" ~ 0.9, Counts == "3-1" ~ 0.9, Counts == "3-2" ~ 1.0,
#         TRUE ~ 1.0
#       ),
#       atbat_level = case_when(
#         AtBatResult == "Strike Out" ~ 1.2,
#         AtBatResult == "Double Play" ~ 2.0,
#         AtBatResult == "Infield Fly" ~ 1.0,
#         AtBatResult == "Ground Out" ~ 1.0,
#         AtBatResult == "Line Out" ~ 1.0,
#         AtBatResult == "Fly Out" ~ 1.0,
#         AtBatResult == "Fielders Choice" ~ 0.9,
#         AtBatResult == "Walk" ~ -1.1,
#         AtBatResult == "Single" ~ -1.1,
#         AtBatResult == "Double" ~ -1.2,
#         AtBatResult == "Triple" ~ -1.3,
#         AtBatResult == "Home Run" ~ -1.5,
#         TRUE ~ 0.5
#       ),
#       pitcher_success = (pitch_level + atbat_level + Counts_value) / 2,
#       pitch_name_no = case_when(
#         PitchType %in% c("Fast Ball", "Two Seam Fast Ball") ~ 1.0,
#         PitchType %in% c("Breaking Ball", "Slider", "Curve") ~ 2.0,
#         PitchType %in% c("Splitter", "Change Up") ~ 3.0,
#         TRUE ~ 4.0
#       ),
#       Location = as.numeric(Location),
#       Location_scaled = scale(Location)[, 1],
#       Balls = as.numeric(Balls),
#       Strikes = as.numeric(Strikes),
#       Outs = as.numeric(Outs),
#       Inning = as.numeric(Inning),
#       BatterHand_num = ifelse(`Batter.Hand` == "R", 0, 1),
#       AttackZone = ifelse(is.na(AttackZone), "Unknown", AttackZone),
#       LocationMod = location_modifiersML[AttackZone],
#       IsTwoStrikeCount = ifelse(Strikes == 2, 1, 0),
#       IsFullCount = ifelse(Balls == 3 & Strikes == 2, 1, 0),
#       IsLateInning = ifelse(Inning >= 7, 1, 0),
#       AheadInCount = ifelse(Strikes > Balls, 1, 0),
#       IsChaseZone = ifelse(AttackZone == "Chase", 1, 0),
#       IsLeftyBatter = ifelse(BatterHand_num == 1, 1, 0),
#       Outs_2 = ifelse(Outs == 2, 1, 0)
#     ) %>%
#     filter(!is.na(Location_scaled))
#   
#   req(nrow(df3A) > 0)
#   
#   X <- df3A %>%
#     select(
#       pitch_name_no, Strikes, Balls, Outs, Location_scaled,
#       BatterHand_num, Inning, LocationMod, IsTwoStrikeCount,
#       IsFullCount, IsLateInning, AheadInCount, IsChaseZone,
#       IsLeftyBatter, Outs_2
#     ) %>%
#     as.matrix()
#   
#   y <- df3A$pitcher_success
#   
#   set.seed(123)
#   train_idx <- sample(1:nrow(X), size = 0.8 * nrow(X))
#   X_train <- X[train_idx, ]
#   X_test <- X[-train_idx, ]
#   y_train <- y[train_idx]
#   y_test <- y[-train_idx]
#   
#   cv <- xgb.cv(
#     data = X_train,
#     label = y_train,
#     nfold = 5,
#     nrounds = 10000,
#     early_stopping_rounds = 10,
#     max_depth = 6,
#     eta = 0.01,
#     subsample = 0.8,
#     colsample_bytree = 0.8,
#     objective = "reg:squarederror",
#     verbose = 0
#   )
#   
#   best_rounds <- cv$best_iteration
#   xgb_model <- xgboost(
#     data = X_train,
#     label = y_train,
#     nrounds = best_rounds,
#     max_depth = 6,
#     eta = 0.01,
#     subsample = 0.8,
#     colsample_bytree = 0.8,
#     objective = "reg:squarederror",
#     verbose = 0
#   )
#   
#   df3A$predicted_success <- predict(xgb_model, newdata = X)
#   
#   test_preds <- predict(xgb_model, X_test)
#   rmse <- round(sqrt(mean((y_test - test_preds)^2)), 3)
#   
#   excluded_pitchers <- c("Zion Theophilus", "Luke Pappano", "Connor Fuhrer")
#   eligible_pitchers <- setdiff(unique(df3A$Pitcher), excluded_pitchers)
#   
#   valid_combos <- list()
#   for (starter in excluded_pitchers) {
#     combos <- combn(eligible_pitchers, 2, simplify = FALSE)
#     for (combo in combos) {
#       valid_combos[[length(valid_combos) + 1]] <- c(starter, combo)
#     }
#   }
#   
#   combo_scores <- purrr::map_df(valid_combos, function(p) {
#     df3A %>%
#       filter(Pitcher %in% p) %>%
#       summarise(
#         Pitcher_1 = p[1],
#         Pitcher_2 = p[2],
#         Pitcher_3 = p[3],
#         Avg_Team_Success = round(mean(predicted_success, na.rm = TRUE), 3)
#       )
#   }) %>%
#     arrange(desc(Avg_Team_Success))
#   
#   valid_combos_2 <- combn(eligible_pitchers, 2, simplify = FALSE)
#   
#   pair_scores <- purrr::map_df(valid_combos_2, function(p) {
#     df3A %>%
#       filter(Pitcher %in% p) %>%
#       summarise(
#         Pitcher_1 = p[1],
#         Pitcher_2 = p[2],
#         Avg_Team_Success = round(mean(predicted_success, na.rm = TRUE), 3)
#       )
#   }) %>%
#     arrange(desc(Avg_Team_Success))
#   
#   individual_pitcher_scores <- df3A %>%
#     filter(!(Pitcher %in% excluded_pitchers)) %>%
#     group_by(Pitcher) %>%
#     summarise(
#       Avg_Predicted_Success = round(mean(predicted_success, na.rm = TRUE), 3),
#       Total_Pitches = n()
#     ) %>%
#     arrange(desc(Avg_Predicted_Success))
#   
#   output$comboTable <- renderDT({
#     datatable(combo_scores, options = list(pageLength = 10))
#   })
#   
#   output$pairTable <- renderDT({
#     datatable(pair_scores, options = list(pageLength = 10))
#   })
#   
#   output$individualTable <- renderDT({
#     datatable(individual_pitcher_scores, options = list(pageLength = 10))
#   })
#   
#   output$rmseText <- renderText({
#     paste("Test RMSE (after CV, no velocity):", rmse)
#   })
# })
