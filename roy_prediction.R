############## Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot", "stringr", "stringi",
                 "tidymodels", "modeldata", "themis", "vip", "baguette", "janitor", "rvest",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix", "Metrics"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)

######################## Loading Data ##############################

# Run these to load data instead of the code to scrape the data

roy_data <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1zJDy4ixC7XHiGYriSsOuPnG3VvYfE9v2h3F7R9A_924/edit?gid=1791245089#gid=1791245089")

# The following data is from 1947 to 2025 found through Kaggle
advanced_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1t40BXyuciFV5tM-O8SvhtNMBzJ-cczoPLEQcLUm-858/edit?gid=1880518936#gid=1880518936")

per_100_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1AQTsO3lp8V1UNgnR2aWfbDwxZhsmrEOIT5nr_LqWrqQ/edit?gid=1300008067#gid=1300008067")

player_per_game_df <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UrdY7BQL74ZonAOIaYTKdk310W_C-OnPKaN0DnhnsM8/edit?gid=1967354614#gid=1967354614")


# We are going to only use data from 2004 and forward since our rookie data is from 2004 through 2024. Season 2025 will be used for prediction
advanced_df <- advanced_df %>% 
  filter(season >= 2004)

per_100_df <- per_100_df %>% 
  filter(season >= 2004)

player_per_game_df <- player_per_game_df %>% 
  filter(season >= 2004)


#### Do Not run this code, use code above for datasets. Code below is for reference ###
### Function to scrape Rookie of the Year (ROY) data for a given year ###
get_roy_data <- function(year) {
  # Define the URL for the given year
  url <- paste0("https://www.basketball-reference.com/awards/awards_", year, ".html")
  
  # Read the webpage content
  page <- read_html(url)
  
  # Extract all tables on the page
  tables <- page %>% html_elements("table")
  
  # Check if at least two tables exist (ROY is the second table)
  if (length(tables) < 2) {
    message("No ROY table found for year: ", year)
    return(NULL)
  }
  
  # Extract the second table (ROY voting table)
  roy_table <- tables[[2]] %>% html_table(fill = TRUE)
  
  # Print column names for debugging
  print(colnames(roy_table))
  
  # Set the first row as column names and remove it from the data
  colnames(roy_table) <- as.character(unlist(roy_table[1, ])) 
  roy_table <- roy_table[-1, ]  # Remove the first row as it is now used as column names
  
  # Add a column for the year
  roy_table <- roy_table %>%
    mutate(Year = year)
  
  return(roy_table)
}

# Scrape ROY data from 2004 to 2024
years <- 2004:2024
roy_data_list <- lapply(years, get_roy_data)

# Combine the data into a single dataframe
roy_data <- do.call(rbind, roy_data_list)

# Print the first few rows
print(head(roy_data))

# Save to CSV as backup
write.csv(roy_data, "roy_data.csv", row.names = FALSE)

# We saved the data set as CSV file to have as backup because I was having issues with scraping the data.
# Basketball reference kept blocking me from scraping the data, so once I got it to work, I saved the dataset as a file
# so I wouldn't have to scrape it again.
# For that reason, we are going to use gsheet2tbl() to read our data because I trust it more. We will do the same for all the other datasets.


################### Cleaning Datasets ##########################

### Joining the datasets that include the data from 2004 to 2025 ###
df <- player_per_game_df %>% 
  left_join(advanced_df, by = c("seas_id", "season", "player_id", "player", "birth_year", "pos", "age", "experience", "lg", "tm", "g"))

df <- df %>% 
  left_join(per_100_df, by = c("seas_id", "season", "player_id", "player", "birth_year", "pos", "age", "experience", "lg", "tm", "g",
                               "gs", "fg_percent", "ft_percent", "x3p_percent", "x2p_percent", "mp"))

# Looking at structure
str(df)

# Removing unecessary variables
df <- df %>% 
  select(-c(seas_id, player_id, birth_year, pos, lg))

# Checking NAs
colSums(is.na(df))

# Filling in NAs with 0. The players with the missing data are likely to not have attempted shots or have low playing time
df <- df %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# Checking NAs again
colSums(is.na(df))


### Creating dataset for rookies in 2025 ###
roy_2025 <- df %>% 
  filter(season == 2025,
         experience == 1)

# Removing experience variable since we don't need it anymore
roy_2025 <- roy_2025 %>% 
  select(-experience)


### Creating dataset for previous roy ###
# Looking at structure
str(roy_data)

# Changing variables to be numeric
roy_data <- roy_data %>% 
  mutate(across(-c(Rank, Player, Tm), as.numeric))

# Renaming Year as season to make it easier to join with other datasets
roy_data <- roy_data %>% 
  rename(season = Year)

# Renaming some variables to make joining easier and removing some variables that we already have data for
roy_data_cleaned <- roy_data %>% 
  rename(
    player = Player,
    age = Age,
    tm = Tm
  ) %>% 
  select(-c(G, MP, PTS, TRB, AST, STL, BLK, `FG%`, `3P%`, `FT%`, WS, `WS/48`))
  
# Joining roy data with player data
roy_df <- roy_data_cleaned %>% 
  left_join(df, by = c("player", "age", "tm", "season"))

# Removing experience since we don't need it anymore
roy_df <- roy_df %>% 
  select(-experience)


############# Exploratory Data Analysis ##################


# Select only numeric columns and compute correlation
cor_matrix <- cor(select_if(roy_df, is.numeric), use = "pairwise.complete.obs")

# Extract correlations with K%
share_cor <- cor_matrix[, "Share"] %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Correlation = ".")

# View top correlations
print(share_cor)

# Create correlation matrix heatmap
ggcorrplot(share_cor, 
           method = "square",   # Use "square" or "circle" instead of "color"
           type = "lower", 
           lab = FALSE, 
           lab_size = 3, 
           colors = c("blue", "white", "red"), 
           title = "Correlation Matrix") +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 8))

# Points per game vs Share
ggplot(data = roy_df, 
       aes(x = pts_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "ROY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Points per Game vs. ROY Share") +
  theme_bw()


# Field goals per game vs Share
ggplot(data = roy_df, 
       aes(x = fg_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "ROY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Field Goals per Game vs. ROY Share") +
  theme_bw()

# Field goals attempts per game vs Share
ggplot(data = roy_df, 
       aes(x = fga_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "ROY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Field Goal Attempts per Game vs. ROY Share") +
  theme_bw()

# Minutes played per game vs Share
ggplot(data = roy_df, 
       aes(x = mp_per_game, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "ROY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Minutes Played per Game vs. ROY Share") +
  theme_bw()


# VORP vs Share
ggplot(data = roy_df, 
       aes(x = vorp, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "ROY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Value Over Replacement Player vs. ROY Share") +
  theme_bw()

# OBPM vs Share
ggplot(data = roy_df, 
       aes(x = obpm, y = Share, color = (Rank == 1))) +
  geom_point(alpha = 0.8) +
  scale_color_manual(name = "ROY",
                     values = c("FALSE" = "red", "TRUE" = "blue"), 
                     labels = c("FALSE" = "Lost", "TRUE" = "Won")) +
  labs(title = "Offensive Box Plus/Minus vs. ROY Share") +
  theme_bw()


########################### Linear Regression Model ################################

# We are going to start with a linear model with the following set of predictors
# We will test one at a time and check their accuracy

# Features for prediction
features <- c("pts_per_game", "fga_per_game", "fta_per_game", "tov_per_game", "mp_per_game",
              "vorp", "usg_percent", "ast_per_game", "obpm") # this set of features is 76.2% accurate

features <- c("pts_per_game", "mp_per_game", "vorp", "usg_percent", "ast_per_game", "obpm") # this set is 81% accurate


target <- "Share"

# Setting seed
set.seed(123)

# Initializing counters
correct_predictions <- 0
total_years <- length(unique(roy_df$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for leave one year out cross-validation
for (yr in unique(roy_df$season)) {
  
  # Split training and test data
  train_data <- roy_df %>% filter(season != yr)
  test_data <- roy_df %>% filter(season == yr)
  
  # Train Linear Regression model
  formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
  model <- lm(formula, data = train_data)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}


# Compute final metrics
lm_average_mae <- mean(mae_values, na.rm = TRUE)
lm_average_r2 <- mean(r2_values, na.rm = TRUE)
lm_accuracy <- correct_predictions / total_years

# Print results
print(lm_average_mae)
print(lm_average_r2)
print(lm_accuracy)

# The second set of features does a great job in predicting ROY with an 81% accuracy.
# It also accounts for a good amount of the variability with and R-Squared of 0.7.


########################## Random Forest Model ############################

# We are now going to try a random forest model.
# Same idea, test one set of features at a time


# Features for prediction
features <- c("pts_per_game", "fga_per_game", "fta_per_game", "tov_per_game", "mp_per_game",
              "vorp", "usg_percent", "ast_per_game", "obpm") # this set of features is 71.4% accurate

features <- c("pts_per_game", "mp_per_game", "vorp", "usg_percent", "ast_per_game", "obpm") # this set is 81% accurate

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(roy_df$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(roy_df$season)) {
  
  # Split training and test data
  train_data <- roy_df %>% filter(season != yr)
  test_data <- roy_df %>% filter(season == yr)
  
  # Convert features into a formula format for Random Forest
  formula <- as.formula(paste("Share", "~", paste(features, collapse = " + ")))
  
  # Train Random Forest model
  rf_model <- randomForest(formula, data = train_data, ntree = 500, importance = TRUE)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(rf_model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual MVP Share values
  mae_values <- c(mae_values, mae(actual = y_test, predicted = test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
rf_accuracy <- correct_predictions / total_years
rf_average_mae <- mean(mae_values, na.rm = TRUE)
rf_average_r2 <- mean(r2_values, na.rm = TRUE)

# Print results
print(rf_average_mae)
print(rf_average_r2)
print(rf_accuracy)

# Here the second set of features is also better. The accuracy for random forest was the same as the linear model, 81% accurate.
# However, the random forest model accounted for more of the variability with an R-Squared of 0.748.
# For the fact that rf had a higher R-Squared, rf is the better model so far.



###################### Gradient Boosting Model (XGBoost) ####################

# We are now going to explore a XGBoost model since the random forest model performed relatively well.
# A XGBoost model would be ideal because it's more powerful than random forest because it boosts weak decision trees,
# reducing errors and improving predictive accuracy.

# Once again, we are going to run this model twice with both set of features.


# Features for prediction
features <- c("pts_per_game", "fga_per_game", "fta_per_game", "tov_per_game", "mp_per_game",
              "vorp", "usg_percent", "ast_per_game", "obpm") # this set of features is 66.7% accurate

features <- c("pts_per_game", "mp_per_game", "vorp", "usg_percent", "ast_per_game", "obpm") # this set is 76.2% accurate

set.seed(123)

# Initialize counters
correct_predictions <- 0
total_years <- length(unique(roy_df$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(roy_df$season)) {
  
  # Split training and test data
  train_data <- roy_df %>% filter(season != yr)
  test_data <- roy_df %>% filter(season == yr)
  
  # Convert data to matrix format for XGBoost
  X_train <- as.matrix(train_data %>% select(all_of(features)))
  y_train <- train_data$Share
  
  X_test <- as.matrix(test_data %>% select(all_of(features)))
  y_test <- test_data$Share   # Actual MVP Share values
  
  # Convert to XGBoost DMatrix format
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test)
  
  # Train XGBoost model
  xgb_model <- xgboost(data = dtrain, 
                       nrounds = 100, 
                       objective = "reg:squarederror",
                       max_depth = 6, 
                       eta = 0.1, 
                       subsample = 0.8, 
                       colsample_bytree = 0.8,
                       verbose = 0)
  
  # Predict MVP Share for test set
  test_data$Predicted_Share <- predict(xgb_model, dtest)
  
  # Compute MAE and R-Squared for this year
  mae_values <- c(mae_values, mae(y_test, test_data$Predicted_Share))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share)^2)
  
  # Get the player with the highest predicted MVP Share
  predicted_mvp <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_mvp <- test_data %>% filter(Rank == "1") %>% pull(player)
  
  # Check if prediction is correct
  if (predicted_mvp == actual_mvp) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final metrics
xgb_average_mae <- mean(mae_values)
xgb_average_r2 <- mean(r2_values)
xgb_accuracy_xgb <- correct_predictions / total_years

# Printing results
print(xgb_average_mae)
print(xgb_average_r2)
print(xgb_accuracy_xgb)

# XGB performed well but not as well as Rf. The second set of features was once again more accurate here.




################## Trying Unsupervised Models / PCA ##########################


# Drop excluded variables
excluded_vars <- c("Rank", "age", "First", "Pts Won", "Pts Max", "Share", "season")  # Target and voting data excluded and season
pca_data <- roy_df %>%
  select(-one_of(excluded_vars)) %>%
  select_if(is.numeric)  # Keep only numeric columns

# Standardize the data
pca_data_scaled <- scale(pca_data)

# Perform PCA
pca_model <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# Variance explained by each principal component
explained_variance <- data.frame(
  Principal_Component = 1:length(pca_model$sdev),
  Cumulative_Variance = cumsum((pca_model$sdev)^2 / sum((pca_model$sdev)^2))
)

# Plot the explained variance
ggplot(explained_variance, aes(x = Principal_Component, y = Cumulative_Variance)) +
  geom_line() + geom_point() +
  labs(title = "Cumulative Variance Explained by Principal Components",
       x = "Number of Principal Components",
       y = "Cumulative Explained Variance") +
  theme_minimal()

# Print explained variance table
print(explained_variance)


# Convert PCA loadings to a data frame
pca_loadings <- as.data.frame(pca_model$rotation)

# Convert to long format for plotting
loadings_long <- pca_loadings %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Principal_Component", values_to = "Loading")

# Keep only the first 10 principal components
loadings_long <- loadings_long %>%
  filter(Principal_Component %in% paste0("PC", 1:10)) %>%
  mutate(Absolute_Loading = abs(Loading))

# Ensure the PCs are ordered numerically
loadings_long$Principal_Component <- factor(loadings_long$Principal_Component, 
                                            levels = paste0("PC", 1:10))  # Ordered from PC1 to PC10

# Select top 7 contributing variables for each of the first 10 PCs
top_n <- 7  # Number of top variables per PC
top_loadings <- loadings_long %>%
  group_by(Principal_Component) %>%
  slice_max(order_by = Absolute_Loading, n = top_n)

# Plot the contributions with ordered PCs
ggplot(top_loadings, aes(x = reorder(Variable, Absolute_Loading), y = Absolute_Loading, fill = Principal_Component)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip axes for better readability
  facet_wrap(~ Principal_Component, scales = "free_y", ncol = 2) +  # Arrange in multiple columns
  labs(title = "Top Contributing Variables to First 10 Principal Components",
       x = "Variables",
       y = "Absolute Loading Strength") +
  theme_minimal()




# Prepare PCA-transformed dataset
pca_transformed <- as.data.frame(pca_model$x[, 1:13])  # Select first 13 PCs
pca_transformed$Share <- roy_df$Share  # Target variable
pca_transformed$season <- roy_df$season  # Season variable
pca_transformed$player <- roy_df$player  # Player names (for accuracy check)

# Initialize counters for evaluation metrics
correct_predictions <- 0
total_years <- length(unique(pca_transformed$season))
mae_values <- c()
r2_values <- c()

# Loop through each year for Leave-One-Year-Out Cross-Validation
for (yr in unique(pca_transformed$season)) {
  
  # Split training and test data
  train_data <- pca_transformed %>% filter(season != yr)
  test_data <- pca_transformed %>% filter(season == yr)
  
  # Train Linear Regression model using the first 13 PCs
  model <- lm(Share ~ ., data = train_data %>% select(-season, -player))  # Exclude non-numeric variables
  
  # Predict ROY Share for test set
  test_data$Predicted_Share <- predict(model, test_data)
  
  # Compute MAE and R-Squared for this year
  y_test <- test_data$Share  # Actual ROY Share values
  mae_values <- c(mae_values, mean(abs(y_test - test_data$Predicted_Share), na.rm = TRUE))
  r2_values <- c(r2_values, cor(y_test, test_data$Predicted_Share, use = "complete.obs")^2)
  
  # Get the player with the highest predicted ROY Share
  predicted_roy <- test_data %>% filter(Predicted_Share == max(Predicted_Share)) %>% pull(player)
  actual_roy <- test_data %>% filter(Share == max(Share)) %>% pull(player)  # Actual ROY winner
  
  # Check if prediction is correct
  if (length(predicted_roy) > 0 && length(actual_roy) > 0 && predicted_roy == actual_roy) {
    correct_predictions <- correct_predictions + 1
  }
}

# Compute final evaluation metrics
average_mae <- mean(mae_values, na.rm = TRUE)
average_r2 <- mean(r2_values, na.rm = TRUE)
accuracy <- correct_predictions / total_years  # Accuracy based on correct predictions

# Print results
print(average_mae)
print(average_r2)
print(accuracy)

# Looking at the results, PCA did performed well but RF still is the best model we have.


########### 2025 NBA ROY Prediction #################


# Define the list of potential candidates
candidates <- c("Kel’el Ware", "Jaylen Wells", "Stephon Castle", 
                "Zaccharie Risacher", "Zach Edey", "Alex Sarr", 
                "Dalton Knecht", "Yves Missi")

# Filter `roy_2025` to include only these candidates
roy_2025_filtered <- roy_2025 %>% filter(player %in% candidates)

# Select only the features used in the final model
features <- c("pts_per_game", "mp_per_game", "vorp", "usg_percent", "ast_per_game", "obpm")

# Ensure the filtered dataset contains only relevant features
roy_2025_selected <- roy_2025_filtered %>% select(all_of(features))

# Train the final Random Forest model using the full historical dataset
final_rf_model <- randomForest(Share ~ ., data = roy_df %>% select(all_of(features), Share), ntree = 500, importance = TRUE)

# Predict 2025 ROY Share values for selected candidates
roy_2025_filtered$Predicted_Share <- predict(final_rf_model, newdata = roy_2025_selected)

# Print the predicted 2025 ROY winner
roy_2025_filtered %>% 
  filter(Predicted_Share == max(Predicted_Share)) %>% 
  select(player)


# Stephon Castle is the predicted ROY!










































