# Load necessary libraries
library(dplyr)
library(lubridate)
library(sjPlot)
library(survival)
library(timeROC)
library(survex)
library(AutoScore)
library(knitr)


# Clear workspace
rm(list = ls())

# Function to load data
load_data <- function(train_path, test_path) {
  train_data <- read.csv(train_path)
  test_data <- read.csv(test_path)
  
  names(train_data)[c(1, 2)] <- c("label_time", "label_status")
  names(test_data)[c(1, 2)] <- c("label_time", "label_status")
  
  return(list(train = train_data, test = test_data))
}

data1 <- load_data("data_path/Demo_data_train.csv",
          "data_path/Demo_data_test.csv")

# Function for risk score analysis
run_survival_analysis <- function(train_set, test_set) {
  set.seed(123)
  ranking <- AutoScore_rank_Survival(train_set = train_set, ntree = 200)

  # Perform parsimony survival analysis
  iAuc <- AutoScore_parsimony_Survival(
    train_set = train_set, validation_set = train_set,
    rank = ranking, max_score = 100, n_min = 1, n_max = 15,
    categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
    auc_lim_min = 0.5, auc_lim_max = "adaptive",
    cross_validation = TRUE, fold = 5, do_trace = FALSE
  )
  
  num_var <- 6
  final_variables <- names(ranking[1:num_var])
  
  cut_vec <- AutoScore_weighting_Survival(
    train_set = train_set, validation_set = train_set, 
    final_variables = final_variables, max_score = 100,
    categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
    time_point = c(12, 36, 60)
  )
  
  # Fine-tune cutoff values
  cut_vec_tuned <- cut_vec
  cut_vec_tuned$DM_duration <- c(1,5,10)
  cut_vec_tuned$Diagnosis_Age <- c(50, 60, 70)  # Example values
  
  scoring_table <- AutoScore_fine_tuning_Survival(
    train_set = train_set, validation_set = train_set, 
    final_variables = final_variables,
    cut_vec = cut_vec_tuned, max_score = 100,
    time_point = c(12, 36, 60)
  )
  
  pred_score <- AutoScore_testing_Survival(
    test_set = test_set,
    final_variables = final_variables,
    cut_vec = cut_vec_tuned,
    scoring_table = scoring_table,
    threshold = "best", with_label = TRUE,
    time_point = c(12, 36, 60)
  )
  
  return(pred_score)
}


# Main execution
pred_score <- run_survival_analysis(data1$train, data1$test)

# Output and plots
head(pred_score)
pred_score_table <- plot_survival_km(pred_score = pred_score, score_cut = c(65))


pred_score_table

#####





