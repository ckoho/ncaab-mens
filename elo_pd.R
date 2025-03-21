#elo_pd.R
#This computes alternative lines based on the elo ratings.

library(tidyverse)
library(readr)
library(vroom)
library(fs)
library(ggpmisc)
df_box_score_all <- tibble()
#Read in all the data and select the columns of interest.
for (season in 2008:2024) {
  df_read <-
    vroom(
      paste0(
        "C:/Users/ckoho/Documents/Inputs/NCAA/Torvik/elo_eoy/elo_results_eoy_",
        season,  
        "_mbb_box_score.csv")
    ) %>%
    select(type,
           loc,
           season,
           date,
           win,
           loss,
           team1,
           team1_pts,
           team2,
           team2_pts,
           elo_line_line,
           elo_combined_line,
           result,
           year,
           elo_adjusted_line
    )
  df_box_score_all <- df_box_score_all %>%
    bind_rows(df_read)
}
#Select the columns of interest
df_box_score_all <- df_box_score_all %>%
  mutate(elo_combined_line_round = round(elo_combined_line * 2) / 2) %>%
  mutate(elo_line_line_round = round(elo_line_line * 2) / 2) %>%
  mutate(elo_adjusted_line_round = round(elo_adjusted_line * 2) / 2) 


# # Fit a logistic model
# nls_model <- nls(p10 ~ a / (1 + exp(-b * (elo_adjusted_line_round - c))),
#                  data = df_summary_adjust,
#                  start = list(a = 1, b = 0.1, c = 0))  # Initial guesses
# 
# # Extract coefficients
# coeffs <- coef(nls_model)
# a <- coeffs["a"]
# b <- coeffs["b"]
# c_param <- coeffs["c"]
# 
# # Print the equation
# cat("Equation: p10 =", round(a, 4), "/ (1 + exp(-", round(b, 4), " * (elo_adjusted_line_round -", round(c_param, 4), ")))\n")
# 
# # Plot with fitted curve
# ggplot(df_summary_adjust, aes(x = elo_adjusted_line_round, y = p10, size = n)) +
#   geom_point() +
#   stat_function(fun = function(x) a / (1 + exp(-b * (x - c_param))), color = "red") +
#   ggtitle("Logistic Curve Fit for p10 vs. elo_adjusted_line_round")
# 
# 
# # Fit a logistic model
# nls_model <- nls(m10 ~ a / (1 + exp(-b * (elo_adjusted_line_round - c))),
#                  data = df_summary_adjust,
#                  start = list(a = 1, b = 0.1, c = 0))  # Initial guesses
# 
# # Extract coefficients
# coeffs <- coef(nls_model)
# a <- coeffs["a"]
# b <- coeffs["b"]
# c_param <- coeffs["c"]
# 
# # Print the equation
# cat("Equation: m10 =", round(a, 4), "/ (1 + exp(-", round(b, 4), " * (elo_adjusted_line_round -", round(c_param, 4), ")))\n")
# 
# # Plot with fitted curve
# ggplot(df_summary_adjust, aes(x = elo_adjusted_line_round, y = m10, size = n)) +
#   geom_point() +
#   stat_function(fun = function(x) a / (1 + exp(-b * (x - c_param))), color = "red") +
#   ggtitle("Logistic Curve Fit for m10 vs. elo_adjusted_line_round")
# 


#################################
## elo_adjusted_line_round     ##
#################################

# Define function to fit logistic regression
fit_logistic <- function(y_col) {
  formula <- as.formula(paste0(y_col, " ~ a / (1 + exp(-b * (elo_adjusted_line_round - c)))"))
  
  # Try fitting the logistic model with reasonable starting values
  tryCatch({
    nls_model <- nls(formula, 
                     data = df_summary_adjust, 
                     start = list(a = 1, b = 0.1, c = 0))  # Initial guesses
    
    # Extract coefficients
    coeffs <- coef(nls_model)
    
    # Return coefficients as a dataframe
    return(data.frame(variable = y_col, a = coeffs["a"], b = coeffs["b"], c = coeffs["c"]))
  }, error = function(e) return(NULL))  # Return NULL if fitting fails
}

df_summary_adjust <- df_box_score_all %>%
  mutate(su = if_else(team1_pts > team2_pts, 1, 0),
         !!!setNames(lapply(-60:-1, function(x) expr(if_else(team1_pts + !!x >= team2_pts, 1, 0))), paste0("m", abs(-60:-1))),
         !!!setNames(lapply(1:60, function(x) expr(if_else(team1_pts + !!x >= team2_pts, 1, 0))), paste0("p", 1:60))
  ) %>%
  group_by(elo_adjusted_line_round) %>%
  summarise(n = n(),
            across(c(su, paste0("m", 60:1), paste0("p", 1:60)), mean, .names = "{.col}"))  


# Fit a logistic model
nls_model <- nls(su ~ a / (1 + exp(-b * (elo_adjusted_line_round - c))), 
                 data = df_summary_adjust, 
                 start = list(a = 1, b = 0.1, c = 0))  # Initial guesses

# Extract coefficients
coeffs <- coef(nls_model)
a <- coeffs["a"]
b <- coeffs["b"]
c_param <- coeffs["c"]

# Print the equation
cat("Equation: ay =", round(a, 4), "/ (1 + exp(-", round(b, 4), " * (elo_adjusted_line_round -", round(c_param, 4), ")))\n")

# Plot with fitted curve
ggplot(df_summary_adjust, aes(x = elo_adjusted_line_round, y = su, size = n)) +
  geom_point() +
  stat_function(fun = function(x) a / (1 + exp(-b * (x - c_param))), color = "red") +
  ggtitle("Logistic Curve Fit for p10 vs. elo_adjusted_line_round")



# Get column names for m40 to m1 and p1 to p40
cols <- c(paste0("m", 40:1), paste0("p", 1:40))

# Fit logistic regression models for all columns
logistic_results <- lapply(cols, fit_logistic)

# Combine successful results into a dataframe
logistic_summary <- bind_rows(logistic_results)

# Print all equations
apply(logistic_summary, 1, function(row) {
  cat("Equation for", row["variable"], ": y =", round(as.numeric(row["a"]), 4), 
      "/ (1 + exp(-", round(as.numeric(row["b"]), 4), 
      " * (elo_adjusted_line_round -", round(as.numeric(row["c"]), 4), ")))\n")
})

# View summary table
print(logistic_summary)


#################################
## elo_combined_line_round         ##
#################################
# Define function to fit logistic regression
fit_logistic <- function(y_col) {
  formula <- as.formula(paste0(y_col, " ~ a / (1 + exp(-b * (elo_combined_line_round - c)))"))
  
  # Try fitting the logistic model with reasonable starting values
  tryCatch({
    nls_model <- nls(formula, 
                     data = df_summary_adjust, 
                     start = list(a = 1, b = 0.1, c = 0))  # Initial guesses
    
    # Extract coefficients
    coeffs <- coef(nls_model)
    
    # Return coefficients as a dataframe
    return(data.frame(variable = y_col, a = coeffs["a"], b = coeffs["b"], c = coeffs["c"]))
  }, error = function(e) return(NULL))  # Return NULL if fitting fails
}

df_summary_adjust <- df_box_score_all %>%
  mutate(su = if_else(team1_pts > team2_pts, 1, 0),
         !!!setNames(lapply(-60:-1, function(x) expr(if_else(team1_pts + !!x >= team2_pts, 1, 0))), paste0("m", abs(-60:-1))),
         !!!setNames(lapply(1:60, function(x) expr(if_else(team1_pts + !!x >= team2_pts, 1, 0))), paste0("p", 1:60))
  ) %>%
  group_by(elo_combined_line_round) %>%
  summarise(n = n(),
            across(c(su, paste0("m", 60:1), paste0("p", 1:60)), mean, .names = "{.col}"))  


# Fit a logistic model
nls_model <- nls(su ~ a / (1 + exp(-b * (elo_combined_line_round - c))), 
                 data = df_summary_adjust, 
                 start = list(a = 1, b = 0.1, c = 0))  # Initial guesses

# Extract coefficients
coeffs <- coef(nls_model)
a <- coeffs["a"]
b <- coeffs["b"]
c_param <- coeffs["c"]

# Print the equation
cat("Equation: ay =", round(a, 4), "/ (1 + exp(-", round(b, 4), " * (elo_combined_line_round -", round(c_param, 4), ")))\n")

# Plot with fitted curve
ggplot(df_summary_adjust, aes(x = elo_combined_line_round, y = su, size = n)) +
  geom_point() +
  stat_function(fun = function(x) a / (1 + exp(-b * (x - c_param))), color = "red") +
  ggtitle("Logistic Curve Fit for p10 vs. elo_combined_line_round")



# Get column names for m40 to m1 and p1 to p40
cols <- c(paste0("m", 40:1), paste0("p", 1:40))

# Fit logistic regression models for all columns
logistic_results <- lapply(cols, fit_logistic)

# Combine successful results into a dataframe
logistic_summary <- bind_rows(logistic_results)

# Print all equations
apply(logistic_summary, 1, function(row) {
  cat("Equation for", row["variable"], ": y =", round(as.numeric(row["a"]), 4), 
      "/ (1 + exp(-", round(as.numeric(row["b"]), 4), 
      " * (elo_combined_line_round -", round(as.numeric(row["c"]), 4), ")))\n")
})

# View summary table
print(logistic_summary)



#################################
## elo_combined_line_round     ##
#################################
# Define function to fit logistic regression
fit_logistic <- function(y_col) {
  formula <- as.formula(paste0(y_col, " ~ a / (1 + exp(-b * (elo_combined_line_round - c)))"))
  
  # Try fitting the logistic model with reasonable starting values
  tryCatch({
    nls_model <- nls(formula, 
                     data = df_summary_adjust, 
                     start = list(a = 1, b = 0.1, c = 0))  # Initial guesses
    
    # Extract coefficients
    coeffs <- coef(nls_model)
    
    # Return coefficients as a dataframe
    return(data.frame(variable = y_col, a = coeffs["a"], b = coeffs["b"], c = coeffs["c"]))
  }, error = function(e) return(NULL))  # Return NULL if fitting fails
}

df_summary_adjust <- df_box_score_all %>%
  mutate(su = if_else(team1_pts > team2_pts, 1, 0),
         !!!setNames(lapply(-60:-1, function(x) expr(if_else(team1_pts + !!x >= team2_pts, 1, 0))), paste0("m", abs(-60:-1))),
         !!!setNames(lapply(1:60, function(x) expr(if_else(team1_pts + !!x >= team2_pts, 1, 0))), paste0("p", 1:60))
  ) %>%
  group_by(elo_combined_line_round) %>%
  summarise(n = n(),
            across(c(su, paste0("m", 60:1), paste0("p", 1:60)), mean, .names = "{.col}"))  


# Fit a logistic model
nls_model <- nls(su ~ a / (1 + exp(-b * (elo_combined_line_round - c))), 
                 data = df_summary_adjust, 
                 start = list(a = 1, b = 0.1, c = 0))  # Initial guesses

# Extract coefficients
coeffs <- coef(nls_model)
a <- coeffs["a"]
b <- coeffs["b"]
c_param <- coeffs["c"]

# Print the equation
cat("Equation: ay =", round(a, 4), "/ (1 + exp(-", round(b, 4), " * (elo_combined_line_round -", round(c_param, 4), ")))\n")

# Plot with fitted curve
ggplot(df_summary_adjust, aes(x = elo_combined_line_round, y = su, size = n)) +
  geom_point() +
  stat_function(fun = function(x) a / (1 + exp(-b * (x - c_param))), color = "red") +
  ggtitle("Logistic Curve Fit for p10 vs. elo_combined_line_round")



# Get column names for m40 to m1 and p1 to p40
cols <- c(paste0("m", 40:1), paste0("p", 1:40))

# Fit logistic regression models for all columns
logistic_results <- lapply(cols, fit_logistic)

# Combine successful results into a dataframe
logistic_summary <- bind_rows(logistic_results)

# Print all equations
apply(logistic_summary, 1, function(row) {
  cat("Equation for", row["variable"], ": y =", round(as.numeric(row["a"]), 4), 
      "/ (1 + exp(-", round(as.numeric(row["b"]), 4), 
      " * (elo_combined_line_round -", round(as.numeric(row["c"]), 4), ")))\n")
})

# View summary table
print(logistic_summary)



