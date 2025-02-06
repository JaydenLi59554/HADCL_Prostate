library(dplyr)
library(lubridate)
library(sjPlot)
library(survival)
library(timeROC)
library(survex)
library(AutoScore)
library(knitr)

rm(list = ls())
# read file ---------------------------------------------------------------
setwd("C:/Users/dclapp159_02/Documents")

data5  <- read.csv("Dataset(splitted)/5.Prostate_train.csv")

data5.te  <- read.csv("Dataset(splitted)/5.Prostate_test.csv")

# data_all <- read.csv("allcancer.csv")

# auto score 5.Pros -------------------------------------------------------

str(data5)
str(data5.te)

names(data5)[c(1,2)] <- c("label_time","label_status")
names(data5.te)[c(1,2)] <- c("label_time","label_status")

train_set.5 <- data5
valid_set.5 <- data5
test_set.5 <- data5.te

set.seed(2024)
ranking.5 <- AutoScore_rank_Survival(train_set = train_set.5,
                                     ntree = 100)

set.seed(2024)
iAuc.5 <- AutoScore_parsimony_Survival(
  train_set = train_set.5, validation_set = valid_set.5,
  rank = ranking.5, max_score = 100, n_min = 1, n_max = 18,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


num_var.5 <- 6
final_variables.5 <- names(ranking.5[c(1:num_var.5)])

set.seed(2024)
cut_vec.5 <- AutoScore_weighting_Survival(
  train_set = train_set.5,validation_set = valid_set.5,
  final_variables = final_variables.5, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)

scoring_table.5 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.5,validation_set = valid_set.5,
  final_variables = final_variables.5,
  cut_vec = cut_vec.5,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.5 <- AutoScore_testing_Survival(
  test_set = test_set.5,
  final_variables = final_variables.5,
  cut_vec = cut_vec.5,
  scoring_table = scoring_table.5,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)

head(pred_score.5)

plot_survival_km(pred_score = pred_score.5,score_cut = c(50))
cts_2g_v1.5 <- conversion_table_survival(pred_score = pred_score.5,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.5,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_pros.csv")

########  cut off tuned
summary(data5$dm_period)
cut_vec.5.2 <- cut_vec.5
cut_vec.5.2$diagnosis_Age <- c(50,60,70)
cut_vec.5.2$dm_period <- c(1,5)
cut_vec.5.2$hba1c <- c(7)
cut_vec.5.2$ldl_c <- c(3.4)
cut_vec.5.2$bmi <- c(23.9)

scoring_table.5.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.5,validation_set = valid_set.5,
  final_variables = final_variables.5,
  cut_vec = cut_vec.5.2,
  max_score = 100,
  time_point = c(12,36,60)
)


set.seed(2024)
pred_score.5.2 <- AutoScore_testing_Survival(
  test_set = test_set.5,
  final_variables = final_variables.5,
  cut_vec = cut_vec.5.2,
  scoring_table = scoring_table.5.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.714 (0.677-0.745)
# C_index:  0.71 (0.68-0.742) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.75 (0.705-0.809)
# 2         36 0.706 (0.619-0.773)
# 3         60 0.758 (0.709-0.804)

head(pred_score.5.2)

plot_survival_km(pred_score = pred_score.5.2,score_cut = c(65))
cts_2g_v2.5 <- conversion_table_survival(pred_score = pred_score.5.2,
                                         score_cut = c(65),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.5,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.pros.csv")



#####






