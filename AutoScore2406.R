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
data1  <- read.csv("Dataset(splitted)/1.col_train.csv")
data2  <- read.csv("Dataset(splitted)/2.breast_train.csv")
data3  <- read.csv("Dataset(splitted)/3.lung_train.csv")
data4  <- read.csv("Dataset(splitted)/4.liver_train.csv")
data5  <- read.csv("Dataset(splitted)/5.Prostate_train.csv")
data6  <- read.csv("Dataset(splitted)/6.Bladder_train.csv")
data7  <- read.csv("Dataset(splitted)/7.Uteri_train.csv")
data8  <- read.csv("Dataset(splitted)/8.Stomach_train.csv")
data9  <- read.csv("Dataset(splitted)/9.Pancreas_train.csv")
data10 <- read.csv("Dataset(splitted)/10.Lymphoma_train.csv")
data11 <- read.csv("Dataset(splitted)/11.Skin_train.csv")
data12 <- read.csv("Dataset(splitted)/12.Nasopharynx_train.csv")
data13 <- read.csv("Dataset(splitted)/13.Thyroid_train.csv")
data14 <- read.csv("Dataset(splitted)/14.Mye_train.csv")
data15 <- read.csv("Dataset(splitted)/15.Kidney_train.csv")
data1.te  <- read.csv("Dataset(splitted)/1.col_test.csv")
data2.te  <- read.csv("Dataset(splitted)/2.breast_test.csv")
data3.te  <- read.csv("Dataset(splitted)/3.lung_test.csv")
data4.te  <- read.csv("Dataset(splitted)/4.liver_test.csv")
data5.te  <- read.csv("Dataset(splitted)/5.Prostate_test.csv")
data6.te  <- read.csv("Dataset(splitted)/6.Bladder_test.csv")
data7.te  <- read.csv("Dataset(splitted)/7.Uteri_test.csv")
data8.te  <- read.csv("Dataset(splitted)/8.Stomach_test.csv")
data9.te  <- read.csv("Dataset(splitted)/9.Pancreas_test.csv")
data10.te <- read.csv("Dataset(splitted)/10.Lymphoma_test.csv")
data11.te <- read.csv("Dataset(splitted)/11.Skin_test.csv")
data12.te <- read.csv("Dataset(splitted)/12.Nasopharynx_test.csv")
data13.te <- read.csv("Dataset(splitted)/13.Thyroid_test.csv")
data14.te <- read.csv("Dataset(splitted)/14.Mye_test.csv")
data15.te <- read.csv("Dataset(splitted)/15.Kidney_test.csv")
# data_all <- read.csv("allcancer.csv")

# auto score 1.colo -------------------------------------------------------

str(data1)
str(data1.te)

names(data1)[c(1,2)] <- c("label_time","label_status")
names(data1.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data1
valid_set.1 <- data1
test_set.1 <- data1.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 5
final_variables.1 <- names(ranking.1[c(1:num_var.1,10)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# variable       interval     point
# 
#   diagnosis_Age  <53            0  
# [53,61)       10  
# [61,80)       19  
# [80,87)       35  
# >=87          49  
# 
# dm_period      <0.21          0  
# [0.21,7.79)   25  
# [7.79,12.7)   29  
# >=12.7        29  
# 
# ldl_c          <1.26          0  
# [1.26,1.73)    <5  
# [1.73,3)       <5  
# [3,3.86)       <5 
# >=3.86        10  
# 
# hba1c          <5.7           2  
# [5.7,6.3)      <5  
# [6.3,7.9)      0  
# [7.9,9.78)     0  
# >=9.78         <5  
# 
# creatinine     <54            3  
# [54,67)        <5  
# [67,109)       0  
# [109,159)      <5  
# >=159          <5  
# 
# bmi            <19.6          3  
# [19.6,22.2)    <5  
# [22.2,28.3)    0  
# [28.3,32.4)    0  
# >=32.4         <5  
# 
#   Integrated AUC by all time points: 0.7276807
# C_index:  0.7351049 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7377875
# 2         36 0.7119941
# 3         60 0.6832935

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.71 (0.682-0.73)
# C_index:  0.744 (0.731-0.76) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.768 (0.741-0.793)
# 2         36 0.737 (0.703-0.771)
# 3         60 0.735 (0.688-0.788)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(50))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_colo.csv")

########  cut off tuned
summary(data1$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$ldl_c <- c(3.4)
cut_vec.1.2$bmi <- c(23.9)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# ***Fine-tuned Scores: 
#   variable       interval   point
# 
#   diagnosis_Age  <50          0  
# [50,60)     20  
# [60,70)     27  
# >=70        46  
# 
# dm_period      <1           0  
# [1,5)       30  
# >=5         35  
# 
# ldl_c          <3.4         0  
# >=3.4       10  
# 
# hba1c          <7           1  
# >=7          0  
# 
# creatinine     <54          5  
# [54,67)      <5  
# [67,109)     0  
# [109,159)    <5  
# >=159        6  
# 
# bmi            <23.9        1  
# >=23.9       0  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7157331
# C_index:  0.7052893 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7222239
# 2         36 0.6897676
# 3         60 0.6625716

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60))
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.712 (0.688-0.734)
# C_index:  0.719 (0.706-0.734) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.746 (0.721-0.773)
# 2         36 0.718 (0.683-0.748)
# 3         60 0.677 (0.626-0.724)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(65))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(65),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.colo.csv")


#



# auto score 2.Breast -------------------------------------------------------

str(data2)
str(data2.te)

names(data2)[c(1,13)] <- c("label_time","label_status")
names(data2.te)[c(1,13)] <- c("label_time","label_status")

train_set.2 <- data2
valid_set.2 <- data2
test_set.2 <- data2.te

set.seed(2024)
ranking.2 <- AutoScore_rank_Survival(train_set = train_set.2,
                                     ntree = 100)

set.seed(2024)
iAuc.2 <- AutoScore_parsimony_Survival(
  train_set = train_set.2, validation_set = valid_set.2,
  rank = ranking.2, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.2),file = "AutoScore2406/iAuc.2.csv")

num_var.2 <- 6
final_variables.2 <- names(ranking.2[c(1:num_var.2)])

set.seed(2024)
cut_vec.2 <- AutoScore_weighting_Survival(
  train_set = train_set.2,validation_set = valid_set.2,
  final_variables = final_variables.2, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# ****Initial Scores: 
# 
#   variable       interval     point
# 
#   diagnosis_Age  <45            0  
# [45,53)        6  
# [53,74)       16  
# [74,83)       29  
# >=83          43  
# 
# dm_period      <6.29          0  
# [6.29,11.2)   11  
# >=11.2        17  
# 
# hdl_c          <0.87          7  
# [0.87,1.06)    5  
# [1.06,1.6)     <5  
# [1.6,1.96)     0  
# >=1.96         5  
# 
# hba1c          <5.8           8  
# [5.8,6.3)      0  
# [6.3,7.8)      0  
# [7.8,9.5)      0  
# >=9.5          6  
# 
# ldl_c          <1.29          0  
# [1.29,1.74)    <5  
# [1.74,3.05)    9  
# [3.05,3.86)   11  
# >=3.86        15  
# 
# creatinine     <47           11  
# [47,56)        0  
# [56,81)        0  
# [81,115)       <5  
# >=115          6  
# 
#   Integrated AUC by all time points: 0.7403466
# C_index:  0.7554794 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.8249568
# 2         36 0.7192418
# 3         60 0.7150137

scoring_table.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.2,validation_set = valid_set.2,
  final_variables = final_variables.2,
  cut_vec = cut_vec.2,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.2 <- AutoScore_testing_Survival(
  test_set = test_set.2,
  final_variables = final_variables.2,
  cut_vec = cut_vec.2,
  scoring_table = scoring_table.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.748 (0.706-0.779)
# C_index:  0.752 (0.725-0.777) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.824 (0.771-0.866)
# 2         36  0.76 (0.702-0.814)
# 3         60 0.785 (0.728-0.829)

head(pred_score.2)

plot_survival_km(pred_score = pred_score.2,score_cut = c(50))
cts_2g_v1.2 <- conversion_table_survival(pred_score = pred_score.2,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.2,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_Breast.csv")

########  cut off tuned
summary(data1$dm_period)
cut_vec.2.2 <- cut_vec.2
cut_vec.2.2$diagnosis_Age <- c(50,60,70)
cut_vec.2.2$dm_period <- c(1,5)
cut_vec.2.2$hba1c <- c(7)
cut_vec.2.2$ldl_c <- c(3.4)
cut_vec.2.2$hdl_c <- c(1)

scoring_table.2.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.2,validation_set = valid_set.2,
  final_variables = final_variables.2,
  cut_vec = cut_vec.2.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# ***Fine-tuned Scores: 
# 
#   variable       interval  point
#
#   diagnosis_Age  <50         0  
# [50,60)    12  
# [60,70)    21  
# >=70       40  
# 
# dm_period      <1          0  
# [1,5)      19  
# >=5        30  
# 
# hdl_c          <1          3  
# >=1         0  
# 
# hba1c          <7          0  
# >=7         0  
# 
# ldl_c          <3.4        0  
# >=3.4       8  
# 
# creatinine     <47        18  
# [47,56)     <5  
# [56,81)     0  
# [81,115)    5  
# >=115      10  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7522452
# C_index:  0.7611851 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.8041741
# 2         36 0.7375396
# 3         60 0.7191531

set.seed(2024)
pred_score.2.2 <- AutoScore_testing_Survival(
  test_set = test_set.2,
  final_variables = final_variables.2,
  cut_vec = cut_vec.2.2,
  scoring_table = scoring_table.2.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.75 (0.709-0.782)
# C_index:  0.744 (0.719-0.773) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.826 (0.785-0.865)
# 2         36 0.749 (0.684-0.801)
# 3         60 0.759 (0.694-0.816)

head(pred_score.2.2)

plot_survival_km(pred_score = pred_score.2.2,score_cut = c(50))
cts_2g_v2.2 <- conversion_table_survival(pred_score = pred_score.2.2,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.2,
           "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.Breast.csv")


#



# auto score 3.lung -------------------------------------------------------

str(data3)
str(data3.te)

names(data3)[c(1,2)] <- c("label_time","label_status")
names(data3.te)[c(1,2)] <- c("label_time","label_status")

train_set.3 <- data3
valid_set.3 <- data3
test_set.3 <- data3.te

set.seed(2024)
ranking.3 <- AutoScore_rank_Survival(train_set = train_set.3,
                                     ntree = 100)

set.seed(2024)
iAuc.3 <- AutoScore_parsimony_Survival(
  train_set = train_set.3, validation_set = valid_set.3,
  rank = ranking.3, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.3),file = "AutoScore2406/iAuc.3.csv")

num_var.3 <- 5
final_variables.3 <- names(ranking.3[c(1:num_var.3,7,8)])

set.seed(2024)
cut_vec.3 <- AutoScore_weighting_Survival(
  train_set = train_set.3,validation_set = valid_set.3,
  final_variables = final_variables.3, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# variable           interval     point
# 
#   diagnosis_Age      <56            0  
# [56,64)        9  
# [64,82)       19  
# [82,88)       36  
# >=88          46  
# 
# dm_period          <1.52          0  
# [1.52,8.39)   18  
# [8.39,13)     20  
# >=13          22  
# 
# smoke_status_cd_Y  1              9  
# not_1          0  
# 
# hba1c              <5.8           4  
# [5.8,6.3)      0  
# [6.3,7.9)      <5  
# [7.9,9.8)      <5  
# >=9.8          <5  
# 
# hdl_c              <0.8           6  
# [0.8,0.97)     6  
# [0.97,1.5)     <5  
# [1.5,1.9)      0  
# >=1.9          <5  
# 
# serum_k            <3.6           5  
# [3.6,3.9)      <5  
# [3.9,4.7)      <5  
# [4.7,5)        0  
# >=5            <5  
# 
# ldl_c              <1.35          0  
# [1.35,1.8)     <5  
# [1.8,3.1)      6  
# [3.1,3.95)     8  
# >=3.95         7  
# 
#   Integrated AUC by all time points: 0.7118029
# C_index:  0.6915967 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7003393
# 2         36 0.6779537
# 3         60 0.6523219

scoring_table.3 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.3,validation_set = valid_set.3,
  final_variables = final_variables.3,
  cut_vec = cut_vec.3,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.3 <- AutoScore_testing_Survival(
  test_set = test_set.3,
  final_variables = final_variables.3,
  cut_vec = cut_vec.3,
  scoring_table = scoring_table.3,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.705 (0.682-0.723)
# C_index:  0.68 (0.661-0.695) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.717 (0.691-0.74)
# 2         36 0.687 (0.621-0.728)
# 3         60 0.723 (0.658-0.778)

head(pred_score.3)

plot_survival_km(pred_score = pred_score.3,score_cut = c(55))
cts_2g_v1.3 <- conversion_table_survival(pred_score = pred_score.3,
                                         score_cut = c(55),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.3,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_lung.csv")

########  cut off tuned
summary(data3$dm_period)
cut_vec.3.2 <- cut_vec.3
cut_vec.3.2$diagnosis_Age <- c(50,60,70)
cut_vec.3.2$dm_period <- c(1,5)
cut_vec.3.2$hba1c <- c(7)
cut_vec.3.2$ldl_c <- c(3.4)
cut_vec.3.2$hdl_c <- c(1)


scoring_table.3.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.3,validation_set = valid_set.3,
  final_variables = final_variables.3,
  cut_vec = cut_vec.3.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# variable           interval   point
# 
#   diagnosis_Age      <50          0  
# [50,60)     15  
# [60,70)     22  
# >=70        40  
# 
# dm_period          <1           0  
# [1,5)       29  
# >=5         34  
# 
# smoke_status_cd_Y  1           11  
# not_1        0  
# 
# hba1c              <7           0  
# >=7          0  
# 
# hdl_c              <1           3  
# >=1          0  
# 
# serum_k            <3.6         7  
# [3.6,3.9)    <5  
# [3.9,4.7)    <5  
# [4.7,5)      0  
# >=5          <5  
# 
# ldl_c              <3.4         0  
# >=3.4        <5  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.6832032
# C_index:  0.6632945 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.6938715
# 2         36 0.6720875
# 3         60 0.6583626

set.seed(2024)
pred_score.3.2 <- AutoScore_testing_Survival(
  test_set = test_set.3,
  final_variables = final_variables.3,
  cut_vec = cut_vec.3.2,
  scoring_table = scoring_table.3.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.692 (0.671-0.712)
# C_index:  0.667 (0.65-0.682) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.712 (0.685-0.737)
# 2         36 0.691 (0.637-0.728)
# 3         60 0.728 (0.673-0.783)

head(pred_score.3.2)

plot_survival_km(pred_score = pred_score.3.2,score_cut = c(75))
cts_2g_v2.3 <- conversion_table_survival(pred_score = pred_score.3.2,
                                         score_cut = c(75),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.3,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.lung.csv")



#



# auto score 4.liver -------------------------------------------------------

str(data4)
str(data4.te)

names(data4)[c(1,2)] <- c("label_time","label_status")
names(data4.te)[c(1,2)] <- c("label_time","label_status")

train_set.4 <- data4
valid_set.4 <- data4
test_set.4 <- data4.te

set.seed(2024)
ranking.4 <- AutoScore_rank_Survival(train_set = train_set.4,
                                     ntree = 100)

set.seed(2024)
iAuc.4 <- AutoScore_parsimony_Survival(
  train_set = train_set.4, validation_set = valid_set.4,
  rank = ranking.4, max_score = 100, n_min = 1, n_max = 18,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.4),file = "AutoScore2406/iAuc.4.csv")

num_var.4 <- 6
final_variables.4 <- names(ranking.4[c(1:num_var.4)])

set.seed(2024)
cut_vec.4 <- AutoScore_weighting_Survival(
  train_set = train_set.4,validation_set = valid_set.4,
  final_variables = final_variables.4, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age  <52            0  
# [52,60)        9  
# [60,79)       19  
# [79,87)       35  
# >=87          49  
# 
# dm_period      <1.27          0  
# [1.27,8.59)   18  
# [8.59,13)     19  
# >=13          21  
# 
# serum_k        <3.5           6  
# [3.5,3.9)      <5  
# [3.9,4.6)      0  
# [4.6,5)        <5  
# >=5            0  
# 
# hdl_c          <0.77          5  
# [0.77,0.97)    0  
# [0.97,1.58)    <5  
# [1.58,2)       <5  
# >=2            <5  
# 
# hba1c          <5.5           1  
# [5.5,6.2)      <5 
# [6.2,8.2)      0  
# [8.2,10.1)     <5  
# >=10.1         <5  
# 
# ldl_c          <1.28          0  
# [1.28,1.75)    6  
# [1.75,3)       5  
# [3,3.72)       8  
# >=3.72        14  
# 
#   Integrated AUC by all time points: 0.7115205
# C_index:  0.6961043 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7309300
# 2         36 0.7107927
# 3         60 0.6987116

scoring_table.4 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.4,validation_set = valid_set.4,
  final_variables = final_variables.4,
  cut_vec = cut_vec.4,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.4 <- AutoScore_testing_Survival(
  test_set = test_set.4,
  final_variables = final_variables.4,
  cut_vec = cut_vec.4,
  scoring_table = scoring_table.4,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# **Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.738 (0.714-0.759)
# C_index:  0.694 (0.676-0.713) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.75 (0.725-0.775)
# 2         36 0.742 (0.708-0.778)
# 3         60  0.74 (0.691-0.796)

head(pred_score.4)

plot_survival_km(pred_score = pred_score.4,score_cut = c(50))
cts_2g_v1.4 <- conversion_table_survival(pred_score = pred_score.4,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.4,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_liver.csv")

########  cut off tuned
summary(data4$dm_period)
cut_vec.4.2 <- cut_vec.4
cut_vec.4.2$diagnosis_Age <- c(50,60,70)
cut_vec.4.2$dm_period <- c(1,5)
cut_vec.4.2$hba1c <- c(7)
cut_vec.4.2$ldl_c <- c(3.4)
cut_vec.4.2$hdl_c <- c(1)

scoring_table.4.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.4,validation_set = valid_set.4,
  final_variables = final_variables.4,
  cut_vec = cut_vec.4.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age  <50          0  
# [50,60)     15  
# [60,70)     25  
# >=70        47  
# 
# dm_period      <1           0  
# [1,5)       28  
# >=5         33  
# 
# serum_k        <3.5         9  
# [3.5,3.9)    7  
# [3.9,4.6)    <5  
# [4.6,5)      <5  
# >=5          0  
# 
# hdl_c          <1           1  
# >=1          0  
# 
# hba1c          <7           1  
# >=7          0  
# 
# ldl_c          <3.4         0  
# >=3.4       10  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7008461
# C_index:  0.6827744 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7206362
# 2         36 0.7022996
# 3         60 0.6925101

set.seed(2024)
pred_score.4.2 <- AutoScore_testing_Survival(
  test_set = test_set.4,
  final_variables = final_variables.4,
  cut_vec = cut_vec.4.2,
  scoring_table = scoring_table.4.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.754 (0.733-0.774)
# C_index:  0.704 (0.687-0.72) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.764 (0.742-0.79)
# 2         36 0.748 (0.715-0.785)
# 3         60  0.74 (0.695-0.784)

head(pred_score.4.2)

plot_survival_km(pred_score = pred_score.4.2,score_cut = c(70))
cts_2g_v2.4 <- conversion_table_survival(pred_score = pred_score.4.2,
                                         score_cut = c(70),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.4,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.liver.csv")



#




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


# write.csv(data.frame(iAuc.5),file = "AutoScore2406/iAuc.5.csv")

num_var.5 <- 6
final_variables.5 <- names(ranking.5[c(1:num_var.5)])

set.seed(2024)
cut_vec.5 <- AutoScore_weighting_Survival(
  train_set = train_set.5,validation_set = valid_set.5,
  final_variables = final_variables.5, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# variable       interval      point
# 
#   diagnosis_Age  <60             0  
# [60,66)         0  
# [66,79)        11  
# [79,86)        31  
# >=86           46  
# 
# dm_period      <0.125          0  
# [0.125,7.73)   14  
# [7.73,12.2)    17  
# >=12.2         15  
# 
# bmi            <20.2          10  
# [20.2,22.5)     <5  
# [22.5,28.1)     <5  
# [28.1,31.5)     0  
# >=31.5          5  
# 
# fasting_gc     <5              7  
# [5,5.9)         <5  
# [5.9,8.3)       0  
# [8.3,10.8)      <5  
# >=10.8          5  
# 
# ldl_c          <1.3            0  
# [1.3,1.71)      <5  
# [1.71,2.98)     7  
# [2.98,3.72)     9  
# >=3.72         16  
# 
# hba1c          <5.8            2  
# [5.8,6.2)       <5  
# [6.2,7.7)       0  
# [7.7,9.3)       <5  
# >=9.3           5  
# 
#   Integrated AUC by all time points: 0.707693
# C_index:  0.7472942 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7855568
# 2         36 0.7126685
# 3         60 0.6793249

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
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.736 (0.691-0.769)
# C_index:  0.754 (0.724-0.783) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.816 (0.771-0.868)
# 2         36   0.768 (0.7-0.831)
# 3         60  0.78 (0.731-0.825)

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
# variable       interval    point
# 
#   diagnosis_Age  <50           0  
# [50,60)      12  
# [60,70)      15  
# >=70         42  
# 
# dm_period      <1            0  
# [1,5)        22  
# >=5          27  
# 
# bmi            <23.9         4  
# >=23.9        0  
# 
# fasting_gc     <5           13  
# [5,5.9)       5  
# [5.9,8.3)     0  
# [8.3,10.8)    0  
# >=10.8       11  
# 
# ldl_c          <3.4          0  
# >=3.4        12  
# 
# hba1c          <7            0  
# >=7           0  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.6653448
# C_index:  0.6848146 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7068337
# 2         36 0.6877935
# 3         60 0.6319749

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



#




# auto score 6.blad -------------------------------------------------------

str(data6)
str(data6.te)

names(data6)[c(1,2)] <- c("label_time","label_status")
names(data6.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data6
valid_set.1 <- data6
test_set.1 <- data6.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 6
final_variables.1 <- names(ranking.1[c(1:num_var.1)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# variable       interval     point
# 
#   diagnosis_Age  <53            0  
# [53,62)        3  
# [62,81)       28  
# [81,87)       46  
# >=87          52  
# 
# dm_period      <7.61          0  
# [7.61,13.7)    6  
# >=13.7         8  
# 
# serum_k        <3.6           2  
# [3.6,3.9)      <5  
# [3.9,4.7)      0  
# [4.7,5)        <5  
# >=5            <5 
# 
# creatinine     <59.2          2  
# [59.2,74)      0  
# [74,124)       <5 
# [124,188)      8  
# >=188         12  
# 
# hba1c          <5.8           4  
# [5.8,6.3)      <5  
# [6.3,7.9)      0  
# [7.9,9.7)      <5  
# >=9.7         12  
# 
# ldl_c          <1.22          0  
# [1.22,1.67)    <5  
# [1.67,2.9)     7  
# [2.9,3.7)      9  
# >=3.7         14  
# 
#   Integrated AUC by all time points: 0.7372632
# C_index:  0.7156265 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7132890
# 2         36 0.7488875
# 3         60 0.7816339

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# **Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.789 (0.753-0.821)
# C_index:  0.758 (0.723-0.784) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.814 (0.736-0.877)
# 2         36 0.783 (0.743-0.833)
# 3         60  0.79 (0.753-0.837)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(50))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_blad.csv")

########  cut off tuned
summary(data6$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$ldl_c <- c(3.4)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# variable       interval   point
# 
#   diagnosis_Age  <50          0  
# [50,60)      8  
# [60,70)     21  
# >=70        44  
# 
# dm_period      <1           0  
# [1,5)       20  
# >=5         27  
# 
# serum_k        <3.6         4  
# [3.6,3.9)    5  
# [3.9,4.7)    0  
# [4.7,5)      <5  
# >=5          <5  
# 
# creatinine     <59.2        0  
# [59.2,74)    <5  
# [74,124)     4  
# [124,188)   10  
# >=188       14  
# 
# hba1c          <7           0  
# >=7          <5  
# 
# ldl_c          <3.4         0  
# >=3.4        8  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7656752
# C_index:  0.7444021 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7710540
# 2         36 0.7834619
# 3         60 0.8181462

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.779 (0.746-0.808)
# C_index:  0.724 (0.685-0.759) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.74 (0.667-0.794)
# 2         36 0.759 (0.712-0.804)
# 3         60 0.778 (0.741-0.822)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(65))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(65),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.blad.csv")



#




# auto score 7.uteri -------------------------------------------------------

str(data7)
str(data7.te)

names(data7)[c(1,2)] <- c("label_time","label_status")
names(data7.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data7
valid_set.1 <- data7
test_set.1 <- data7.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 18,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 7
final_variables.1 <- names(ranking.1[c(1:num_var.1)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# variable       interval     point
# 
#   diagnosis_Age  <42            4  
# [42,52)        0  
# [52,67)       14  
# [67,77)       22  
# >=77          34  
# 
# dm_period      <5.2           0  
# [5.2,9.95)    10  
# >=9.95        12  
# 
# creatinine     <48            8  
# [48,55)        <5  
# [55,80)        0  
# [80,116)       <5  
# >=116          7  
# 
# serum_k        <3.5           9  
# [3.5,3.9)      <5  
# [3.9,4.6)      <5  
# [4.6,5)        <5  
# >=5            0  
# 
# ldl_c          <1.38          0  
# [1.38,1.82)    <5  
# [1.82,3.1)     7  
# [3.1,4.01)    12  
# >=4.01        19  
# 
# bmi            <21.1          8  
# [21.1,24.1)    0  
# [24.1,32.1)    <5  
# [32.1,37.4)    6  
# >=37.4        10  
# 
# triglyceride   <0.7           7  
# [0.7,1)        <5  
# [1,2.2)        0  
# [2.2,3.5)      0  
# >=3.5          8  
#
#   Integrated AUC by all time points: 0.7683134
# C_index:  0.7692371 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7731560
# 2         36 0.7992638
# 3         60 0.7994202

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.7 (0.6-0.781)
# C_index:  0.727 (0.673-0.789) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.818 (0.725-0.888)
# 2         36   0.7 (0.619-0.799)
# 3         60 0.749 (0.671-0.824)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(35))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(35),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_stom.csv")

########  cut off tuned
summary(data7$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$ldl_c <- c(3.4)
cut_vec.1.2$bmi <- c(23.9)
cut_vec.1.2$triglyceride <- c(1.7)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# variable       interval   point
# 
#   diagnosis_Age  <50          0  
# [50,60)      8  
# [60,70)     13  
# >=70        31  
# 
# dm_period      <1           0  
# [1,5)       19  
# >=5         26  
# 
# creatinine     <48         11  
# [48,55)      0  
# [55,80)      <5  
# [80,116)     <5  
# >=116        7  
# 
# serum_k        <3.5        17  
# [3.5,3.9)    7  
# [3.9,4.6)    <5  
# [4.6,5)      <5  
# >=5          0  
# 
# ldl_c          <3.4         0  
# >=3.4       12  
# 
# bmi            <23.9        0  
# >=23.9       0  
# 
# triglyceride   <1.7         2  
# >=1.7        0  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7642143
# C_index:  0.7844729 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.8088363
# 2         36 0.8317307
# 3         60 0.8074136

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.718 (0.635-0.784)
# C_index:  0.759 (0.697-0.807) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.866 (0.765-0.921)
# 2         36 0.763 (0.675-0.835)
# 3         60 0.795 (0.695-0.875)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(40))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(40),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.stom.csv")



#




# auto score 8.stom -------------------------------------------------------

str(data8)
str(data8.te)

names(data8)[c(1,2)] <- c("label_time","label_status")
names(data8.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data8
valid_set.1 <- data8
test_set.1 <- data8.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 6
final_variables.1 <- names(ranking.1[c(1:num_var.1,8)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age  <54.9          0  
# [54.9,63)      <5  
# [63,82)       18  
# [82,89)       36  
# >=89          45  
# 
# dm_period      <1.94          0  
# [1.94,9.11)   13  
# [9.11,14)     15  
# >=14          14  
# 
# ldl_c          <1.24          2  
# [1.24,1.74)    0  
# [1.74,3)       7  
# [3,3.86)       8  
# >=3.86        16  
# 
# serum_k        <3.5           0  
# [3.5,3.9)      7  
# [3.9,4.6)      <5  
# [4.6,5)        0  
# >=5            <5  
# 
# triglyceride   <0.598         0  
# [0.598,0.8)    6  
# [0.8,1.8)      <5  
# [1.8,2.99)     5  
# >=2.99         7  
# 
# bmi            <18.9          0  
# [18.9,21.5)    <5  
# [21.5,28)      <5  
# [28,32.1)      <5 
# >=32.1         5  
# 
# hba1c          <5.7           5  
# [5.7,6.2)      <5  
# [6.2,7.7)      0  
# [7.7,9.61)     <5  
# >=9.61         <5  
# 
#   Integrated AUC by all time points: 0.7092137
# C_index:  0.6721216 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7093755
# 2         36 0.7224552
# 3         60 0.7197585

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# **Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.686 (0.631-0.734)
# C_index:  0.649 (0.611-0.684) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.707 (0.654-0.754)
# 2         36 0.726 (0.683-0.771)
# 3         60  0.724 (0.67-0.774)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(50))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(50),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_stom.csv")

########  cut off tuned
summary(data8$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$ldl_c <- c(3.4)
cut_vec.1.2$bmi <- c(23.9)
cut_vec.1.2$triglyceride <- c(1.7)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# variable       interval   point
# 
#   diagnosis_Age  <50          0  
# [50,60)     41  
# [60,70)     47  
# >=70        60  
# 
# dm_period      <1           0  
# [1,5)       15  
# >=5         21  
# 
# ldl_c          <3.4         0  
# >=3.4        7  
# 
# serum_k        <3.5         1  
# [3.5,3.9)    9  
# [3.9,4.6)    <5  
# [4.6,5)      0  
# >=5          <5  
# 
# triglyceride   <1.7         0  
# >=1.7        <5  
# 
# bmi            <23.9        0  
# >=23.9       <5  
# 
# hba1c          <7           0  
# >=7          0  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.6977394
# C_index:  0.6544327 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.6782539
# 2         36 0.7302930
# 3         60 0.6943702

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.694 (0.645-0.734)
# C_index:  0.625 (0.596-0.655) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.66 (0.621-0.702)
# 2         36 0.722 (0.668-0.769)
# 3         60  0.74 (0.691-0.788)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(82))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(82),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.stom.csv")



#








# auto score 9.panc -------------------------------------------------------

str(data9)
str(data9.te)

names(data9)[c(1,2)] <- c("label_time","label_status")
names(data9.te)[c(1,2)] <- c("label_time","label_status")

train_set.9 <- data9
valid_set.9 <- data9
test_set.9 <- data9.te

set.seed(2024)
ranking.9 <- AutoScore_rank_Survival(train_set = train_set.9,
                                     ntree = 100)

set.seed(2024)
iAuc.9 <- AutoScore_parsimony_Survival(
  train_set = train_set.9, validation_set = valid_set.9,
  rank = ranking.9, max_score = 100, n_min = 1, n_max = 15,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.9),file = "AutoScore2406/iAuc.9.csv")

num_var.9 <- 6
final_variables.9 <- names(ranking.9[c(1:num_var.9)])

set.seed(2024)
cut_vec.9 <- AutoScore_weighting_Survival(
  train_set = train_set.9,validation_set = valid_set.9,
  final_variables = final_variables.9, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age   <53             0  
# [53,61)        14  
# [61,81)        25  
# [81,88.2)      43  
# >=88.2         49  
# 
# dm_period       <0.972          0  
# [0.972,8.75)   15  
# [8.75,13.3)    17  
# >=13.3         14  
# 
# insulin_flag_Y  0              11  
# not_0           0  
# 
# hba1c           <5.9            1  
# [5.9,6.4)       0  
# [6.4,8.7)       <5  
# [8.7,11.1)     10  
# >=11.1         11  
# 
# creatinine      <51             0  
# [51,62)         6  
# [62,97)         <5  
# [97,136)        <5  
# >=136           0  
# 
# hdl_c           <0.76           5  
# [0.76,0.98)     0  
# [0.98,1.54)     <5  
# [1.54,1.89)     0  
# >=1.89          5  
# 
#   Integrated AUC by all time points: 0.7283683
# C_index:  0.6704085 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7420587
# 2         36 0.7742301
# 3         60 0.7956178

scoring_table.9 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.9,validation_set = valid_set.9,
  final_variables = final_variables.9,
  cut_vec = cut_vec.9,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.9 <- AutoScore_testing_Survival(
  test_set = test_set.9,
  final_variables = final_variables.9,
  cut_vec = cut_vec.9,
  scoring_table = scoring_table.9,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.69 (0.642-0.737)
# C_index:  0.635 (0.599-0.667) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.731 (0.684-0.783)
# 2         36 0.823 (0.765-0.881)
# 3         60   0.85 (0.77-0.919)

head(pred_score.9)

plot_survival_km(pred_score = pred_score.9,score_cut = c(62))
cts_2g_v1.9 <- conversion_table_survival(pred_score = pred_score.9,
                                         score_cut = c(62),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.9,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_panc.csv")

########  cut off tuned
summary(data8$dm_period)
cut_vec.9.2 <- cut_vec.9
cut_vec.9.2$diagnosis_Age <- c(50,75)
cut_vec.9.2$dm_period <- c(0.7,6.8)
cut_vec.9.2$hba1c <- c(7)
cut_vec.9.2$hdl_c <- c(1)

scoring_table.9.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.9,validation_set = valid_set.9,
  final_variables = final_variables.9,
  cut_vec = cut_vec.9.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age   <50          0  
# [50,75)     23  
# >=75        42  
# 
# dm_period       <0.7         0  
# [0.7,6.8)   27  
# >=6.8       31  
# 
# insulin_flag_Y  0           14  
# not_0        0  
# 
# hba1c           <7           0  
# >=7          4  
# 
# creatinine      <51          0  
# [51,62)      7  
# [62,97)      <5  
# [97,136)     <5  
# >=136        0  
# 
# hdl_c           <1           0  
# >=1          <5  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.715638
# C_index:  0.6586712 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7456384
# 2         36 0.7810224
# 3         60 0.7951985

set.seed(2024)
pred_score.9.2 <- AutoScore_testing_Survival(
  test_set = test_set.9,
  final_variables = final_variables.9,
  cut_vec = cut_vec.9.2,
  scoring_table = scoring_table.9.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.699 (0.648-0.744)
# C_index:  0.648 (0.603-0.68) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.735 (0.672-0.78)
# 2         36 0.791 (0.727-0.861)
# 3         60  0.84 (0.761-0.914)

head(pred_score.9.2)

plot_survival_km(pred_score = pred_score.9.2,score_cut = c(75))
cts_2g_v2.9 <- conversion_table_survival(pred_score = pred_score.9.2,
                                         score_cut = c(75),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.9,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.panc.csv")


#

# auto score 10.lymp -------------------------------------------------------

str(data10)
str(data10.te)

names(data10)[c(1,2)] <- c("label_time","label_status")
names(data10.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data10
valid_set.1 <- data10
test_set.1 <- data10.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 10,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 4
final_variables.1 <- names(ranking.1[c(1:num_var.1,6:8)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age  <46             0  
# [46,57)         <5  
# [57,78)        26  
# [78,85)        40  
# >=85           53  
# 
# dm_period      <7.32           0  
# [7.32,12.1)     5  
# >=12.1          6  
# 
# hdl_c          <0.717         13  
# [0.717,0.92)    <5  
# [0.92,1.47)     0  
# [1.47,1.92)     0  
# >=1.92          <5  
# 
# hba1c          <5.7            3  
# [5.7,6.3)       <5  
# [6.3,7.9)       0  
# [7.9,9.63)      2  
# >=9.63          8  
# 
# bmi            <19.6           5  
# [19.6,22.3)     0  
# [22.3,28.2)     <5  
# [28.2,31.9)     <5  
# >=31.9          <5  
# 
# creatinine     <54             0  
# [54,65)         <5  
# [65,104)        <5  
# [104,149)       <5  
# >=149           5  
# 
# ldl_c          <1.3            4  
# [1.3,1.8)       0  
# [1.8,3.08)      <5  
# [3.08,3.96)    11  
# >=3.96          6  
# 
#   Integrated AUC by all time points: 0.7893169
# C_index:  0.7547959 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7736769
# 2         36 0.7986738
# 3         60 0.7959676

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.772 (0.726-0.812)
# C_index:  0.711 (0.656-0.751) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.719 (0.659-0.77)
# 2         36 0.733 (0.662-0.784)
# 3         60 0.768 (0.706-0.828)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(45))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(45),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_lymp.csv")

########  cut off tuned
summary(data10$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$hdl_c <- c(1)
cut_vec.1.2$bmi <- c(23.9)
cut_vec.1.2$ldl_c <- c(3.4)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age  <50          0  
# [50,60)     13  
# [60,70)     36  
# >=70        50  
# 
# dm_period      <1           0  
# [1,5)       20  
# >=5         24  
# 
# hdl_c          <1           4  
# >=1          0  
# 
# hba1c          <7           1  
# >=7          0  
# 
# bmi            <23.9        0  
# >=23.9       <5  
# 
# creatinine     <54          0  
# [54,65)      <5  
# [65,104)     <5  
# [104,149)    <5  
# >=149        7  
# 
# ldl_c          <3.4         0  
# >=3.4       12  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7912031
# C_index:  0.7508171 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7688654
# 2         36 0.7916809
# 3         60 0.7816120

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.766 (0.724-0.808)
# C_index:  0.706 (0.668-0.742) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.726 (0.683-0.771)
# 2         36  0.74 (0.692-0.783)
# 3         60    0.74 (0.677-0.8)


head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(70))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(70),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.lymp.csv")



#




# auto score 11.skin -------------------------------------------------------

str(data11)
str(data11.te)

names(data11)[c(1,2)] <- c("label_time","label_status")
names(data11.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data11
valid_set.1 <- data11
test_set.1 <- data11.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 15,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 6
final_variables.1 <- names(ranking.1[c(1:num_var.1)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age   <54           0  
# [54,64)       <5  
# [64,83)      18  
# [83,89)      38  
# >=89         46  
# 
# dm_period       <0.26         0  
# [0.26,8.5)   19  
# [8.5,13.3)   25  
# >=13.3       25  
# 
# creatinine      <54           2  
# [54,66)       0  
# [66,112)      <5  
# [112,172)     7  
# >=172        16  
# 
# insulin_flag_Y  0             1  
# not_0         0  
# 
# stroke_flag_Y   0             0  
# not_0         7  
# 
# hba1c           <5.8          0  
# [5.8,6.2)     <5  
# [6.2,7.7)     <5  
# [7.7,9.6)     5  
# >=9.6         <5  
# 
#   Integrated AUC by all time points: 0.8060264
# C_index:  0.7838045 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.8017587
# 2         36 0.8112271
# 3         60 0.7871364

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.777 (0.723-0.827)
# C_index:  0.74 (0.686-0.784) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.635 (0.48-0.797)
# 2         36 0.737 (0.661-0.819)
# 3         60 0.782 (0.712-0.845)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(45))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(45),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_skin.csv")

########  cut off tuned
summary(data11$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age   <50          6  
# [50,60)      0  
# [60,70)     15  
# >=70        45  
# 
# dm_period       <1           0  
# [1,5)       17  
# >=5         26  
# 
# creatinine      <54          4  
# [54,66)      0  
# [66,112)     <5  
# [112,172)   11  
# >=172       15  
# 
# insulin_flag_Y  0            2  
# not_0        0  
# 
# stroke_flag_Y   0            0  
# not_0        6  
# 
# hba1c           <7           0  
# >=7          6  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7998888
# C_index:  0.7544004 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7635980
# 2         36 0.7512060
# 3         60 0.7651059

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.769 (0.722-0.806)
# C_index:  0.712 (0.664-0.757) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.677 (0.541-0.78)
# 2         36 0.697 (0.618-0.756)
# 3         60 0.729 (0.668-0.787)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(65))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(65),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.skin.csv")



#




# auto score 12.naso -------------------------------------------------------

str(data12)
str(data12.te)

names(data12)[c(1,2)] <- c("label_time","label_status")
names(data12.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data12
valid_set.1 <- data12
test_set.1 <- data12.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 12,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 6
final_variables.1 <- names(ranking.1[c(1:num_var.1)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age      <42.6          0  
# [42.6,51)      8  
# [51,69)       25  
# [69,78)       39  
# >=78          43  
# 
# dm_period          <6.59          0  
# [6.59,11.5)    9  
# >=11.5         9  
# 
# hba1c              <5.4           0  
# [5.4,6.1)      <5 
# [6.1,7.6)      0  
# [7.6,9.6)      7  
# >=9.6         14  
# 
# hdl_c              <0.8          14  
# [0.8,0.97)     <5  
# [0.97,1.5)     <5  
# [1.5,1.95)     <5  
# >=1.95         0  
# 
# anti_lipid_flag_Y  1              0  
# not_1          7  
# 
# bmi                <18.5         12  
# [18.5,21.3)    7  
# [21.3,27.8)    0  
# [27.8,31.4)    0  
# >=31.4         9  
#
#   Integrated AUC by all time points: 0.7781744
# C_index:  0.7478464 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7941842
# 2         36 0.7759721
# 3         60 0.7775055

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.774 (0.72-0.819)
# C_index:  0.721 (0.683-0.764) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.742 (0.67-0.804)
# 2         36 0.735 (0.657-0.805)
# 3         60 0.752 (0.677-0.819)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(40))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(40),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_naso.csv")

########  cut off tuned
summary(data12$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$hdl_c <- c(1)
cut_vec.1.2$bmi <- c(23.9)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age      <50         0  
# [50,60)    15  
# [60,70)    29  
# >=70       42  
# 
# dm_period          <1          0  
# [1,5)      33  
# >=5        38  
# 
# hba1c              <7          0  
# >=7         <5  
# 
# hdl_c              <1          0  
# >=1         <5  
# 
# anti_lipid_flag_Y  1           0  
# not_1      12  
# 
# bmi                <23.9       4  
# >=23.9      0  
#
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.8199006
# C_index:  0.7653352 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7870953
# 2         36 0.7893056
# 3         60 0.8002558

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.795 (0.754-0.848)
# C_index:  0.77 (0.732-0.808) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.794 (0.732-0.854)
# 2         36 0.795 (0.735-0.854)
# 3         60 0.817 (0.763-0.873)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(65))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(65),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.naso.csv")



#




# auto score 13.throyid -------------------------------------------------------

str(data13)
str(data13.te)

names(data13)[c(1,2)] <- c("label_time","label_status")
names(data13.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data13
valid_set.1 <- data13
test_set.1 <- data13.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 4
final_variables.1 <- names(ranking.1[c(1:num_var.1,8,9)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# diagnosis_Age  <39            3  
# [39,49)        0  
# [49,69)        9  
# [69,80)       15  
# >=80          20  
# 
# dm_period      <5.28          0  
# [5.28,9.05)    <5  
# >=9.05         <5  
# 
# serum_k        <3.6           5  
# [3.6,3.9)      0  
# [3.9,4.6)      <5  
# [4.6,4.9)      0  
# >=4.9          <5  
# 
# creatinine     <48            0  
# [48,57)       64  
# [57,87)       62  
# [87,122)      63  
# >=122         65  
# 
# hba1c          <5.8           3  
# [5.8,6.3)      <5  
# [6.3,7.7)      0  
# [7.7,9.5)      <5  
# >=9.5          <5  
# 
# ldl_c          <1.3           3  
# [1.3,1.8)      <5  
# [1.8,3.08)     <5  
# [3.08,3.8)     <5 
# >=3.8          0  
# 
#   Integrated AUC by all time points: 0.8766849
# C_index:  0.8499986 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.9202791
# 2         36 0.8251398
# 3         60 0.8541521

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.769 (0.665-0.876)
# C_index:  0.753 (0.665-0.833) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.747 (0.491-0.936)
# 2         36 0.818 (0.664-0.925)
# 3         60   0.793 (0.68-0.89)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(78))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(78),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_thyroid.csv")

########  cut off tuned
summary(data13$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$ldl_c <- c(3.4)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age  <50          0  
# [50,60)      5  
# [60,70)      9  
# >=70        15  
# 
# dm_period      <1           0  
# [1,5)        <5  
# >=5          <5  
# 
# serum_k        <3.6         7  
# [3.6,3.9)    0  
# [3.9,4.6)    <5  
# [4.6,4.9)    0  
# >=4.9        <5  
# 
# creatinine     <48          0  
# [48,57)     69  
# [57,87)     68  
# [87,122)    68  
# >=122       71  
# 
# hba1c          <7           0  
# >=7          <5  
# 
# ldl_c          <3.4         3  
# >=3.4        0  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.8758973
# C_index:  0.8457663 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.8853225
# 2         36 0.8614597
# 3         60 0.8735348

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.773 (0.688-0.861)
# C_index:  0.768 (0.693-0.832) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.863 (0.757-0.952)
# 2         36   0.87 (0.79-0.937)
# 3         60 0.804 (0.713-0.892)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(82))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(82),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.thyroid.csv")



#




# auto score 14.mye -------------------------------------------------------

str(data14)
str(data14.te)

names(data14)[c(1,2)] <- c("label_time","label_status")
names(data14.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data14
valid_set.1 <- data14
test_set.1 <- data14.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

final_variables.1 <- names(ranking.1[c(1,2,3,5:7)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)

# diagnosis_Age  <43             0  
# [43,57)        16  
# [57,79)        35  
# [79,86)        46  
# >=86           54  
# 
# dm_period      <0.329          0  
# [0.329,7.9)    22  
# [7.9,12.5)     23  
# >=12.5         19  
# 
# serum_k        <3.5            0  
# [3.5,3.9)       <5  
# [3.9,4.6)       6  
# [4.6,5)         7  
# >=5             <5  
# 
# hba1c          <5.64           0  
# [5.64,6.2)      <5  
# [6.2,7.8)       <5  
# [7.8,9.75)      0  
# >=9.75          6  
# 
# hdl_c          <0.744          6  
# [0.744,0.92)    <5  
# [0.92,1.47)     <5 
# [1.47,1.88)     <5  
# >=1.88          0  
# 
# creatinine     <53             1  
# [53,65)         <5 
# [65,113)        <5  
# [113,202)       0  
# >=202           <5
# 
#   Integrated AUC by all time points: 0.8012259
# C_index:  0.7396488 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7703857
# 2         36 0.8093596
# 3         60 0.8435241

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.768 (0.712-0.809)
# C_index:  0.74 (0.712-0.773) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.794 (0.736-0.839)
# 2         36  0.83 (0.771-0.875)
# 3         60 0.887 (0.846-0.921)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(72))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(72),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_mye.csv")

########  cut off tuned
summary(data14$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$ldl_c <- c(3.4)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# diagnosis_Age  <50             0  
# [50,60)        23  
# [60,70)        35  
# >=70           52  
# 
# dm_period      <1              0  
# [1,5)          17  
# >=5            18  
# 
# serum_k        <3.5            0  
# [3.5,3.9)       5  
# [3.9,4.6)       8  
# [4.6,5)         8  
# >=5             6  
# 
# hdl_c          <0.744         12  
# [0.744,0.92)    8  
# [0.92,1.47)     6  
# [1.47,1.88)     0  
# >=1.88          8  
# 
# hba1c          <7              3  
# >=7             0  
# 
# creatinine     <53             5  
# [53,65)         5  
# [65,113)        8  
# [113,202)       <5  
# >=202           0  
#
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.8131821
# C_index:  0.7520085 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.8011732
# 2         36 0.8261948
# 3         60 0.8513379

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.785 (0.746-0.83)
# C_index:  0.725 (0.688-0.764) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.776 (0.724-0.822)
# 2         36 0.838 (0.781-0.884)
# 3         60 0.885 (0.843-0.924)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(80))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(80),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.mye.csv")



#




# auto score 15.kidney -------------------------------------------------------

str(data15)
str(data15.te)

names(data15)[c(1,2)] <- c("label_time","label_status")
names(data15.te)[c(1,2)] <- c("label_time","label_status")

train_set.1 <- data15
valid_set.1 <- data15
test_set.1 <- data15.te

set.seed(2024)
ranking.1 <- AutoScore_rank_Survival(train_set = train_set.1,
                                     ntree = 100)

set.seed(2024)
iAuc.1 <- AutoScore_parsimony_Survival(
  train_set = train_set.1, validation_set = valid_set.1,
  rank = ranking.1, max_score = 100, n_min = 1, n_max = 15,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive",
  cross_validation = TRUE, fold = 5, do_trace = FALSE
)


# write.csv(data.frame(iAuc.1),file = "AutoScore2406/iAuc.1.csv")

num_var.1 <- 5
final_variables.1 <- names(ranking.1[c(1:num_var.1,7)])

set.seed(2024)
cut_vec.1 <- AutoScore_weighting_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1, max_score = 100,
  categorize = "quantile", quantiles = c(0,0.05,0.2,0.8,0.95,1),
  time_point = c(12,36,60)
)
# dm_period          <0.423          0  
# [0.423,8.13)   31  
# [8.13,12.5)    36  
# >=12.5         36  
# 
# diagnosis_Age      <47             0  
# [47,56)         8  
# [56,76)        17  
# [76,85)        28  
# >=85           32  
# 
# ldl_c              <1.22           0  
# [1.22,1.72)     <5  
# [1.72,3.04)     7  
# [3.04,3.98)     <5  
# >=3.98          8  
# 
# hba1c              <5.7            3  
# [5.7,6.2)       <5  
# [6.2,7.9)       <5  
# [7.9,9.6)       0  
# >=9.6           5  
# 
# anti_lipid_flag_Y  1               0  
# not_1           7  
# 
# creatinine         <61.7          12  
# [61.7,75)       0  
# [75,145)        <5  
# [145,263)       <5  
# >=263           7  
# 
#   Integrated AUC by all time points: 0.7808801
# C_index:  0.7359192 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7511276
# 2         36 0.7872863
# 3         60 0.7941563

scoring_table.1 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  max_score = 100,
  time_point = c(12,36,60)
)

set.seed(2024)
pred_score.1 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1,
  scoring_table = scoring_table.1,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.778 (0.719-0.818)
# C_index:  0.726 (0.681-0.766) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12  0.752 (0.68-0.827)
# 2         36 0.757 (0.698-0.816)
# 3         60 0.748 (0.679-0.814)

head(pred_score.1)

plot_survival_km(pred_score = pred_score.1,score_cut = c(60))
cts_2g_v1.1 <- conversion_table_survival(pred_score = pred_score.1,
                                         score_cut = c(60),
                                         time_point = c(12,36,60))


write.csv(cts_2g_v1.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_kidney.csv")

########  cut off tuned
summary(data15$dm_period)
cut_vec.1.2 <- cut_vec.1
cut_vec.1.2$diagnosis_Age <- c(50,60,70)
cut_vec.1.2$dm_period <- c(1,5)
cut_vec.1.2$hba1c <- c(7)
cut_vec.1.2$ldl_c <- c(3.4)

scoring_table.1.2 <- AutoScore_fine_tuning_Survival(
  train_set = train_set.1,validation_set = valid_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  max_score = 100,
  time_point = c(12,36,60)
)
# dm_period          <1           0  
# [1,5)       30  
# >=5         35  
# 
# diagnosis_Age      <50          0  
# [50,60)     15  
# [60,70)     23  
# >=70        35  
# 
# ldl_c              <3.4         4  
# >=3.4        0  
# 
# hba1c              <7           0  
# >=7          0  
# 
# anti_lipid_flag_Y  1            0  
# not_1       11  
# 
# creatinine         <61.7       15  
# [61.7,75)    0  
# [75,145)     <5  
# [145,263)    <5  
# >=263        8  
# 
#   ***Performance (based on validation set, after fine-tuning):
#   Integrated AUC by all time points: 0.7705117
# C_index:  0.7284728 
# The AUC(t) are shown as bwlow:
#   time_point     AUC_t
# 1         12 0.7584833
# 2         36 0.7745110
# 3         60 0.7954500

set.seed(2024)
pred_score.1.2 <- AutoScore_testing_Survival(
  test_set = test_set.1,
  final_variables = final_variables.1,
  cut_vec = cut_vec.1.2,
  scoring_table = scoring_table.1.2,
  threshold = "best",
  with_label = TRUE,
  time_point = c(12,36,60)
)
# ***Performance using AutoScore (based on unseen test Set):
#   Integrated AUC by all time points: 0.741 (0.681-0.79)
# C_index:  0.679 (0.626-0.73) 
# The AUC(t) are shown as bwlow:
#   time_point               AUC_t
# 1         12 0.705 (0.614-0.781)
# 2         36 0.718 (0.647-0.785)
# 3         60 0.692 (0.626-0.767)

head(pred_score.1.2)

plot_survival_km(pred_score = pred_score.1.2,score_cut = c(65))
cts_2g_v2.1 <- conversion_table_survival(pred_score = pred_score.1.2,
                                         score_cut = c(65),
                                         time_point = c(12,36,60))

write.csv(cts_2g_v2.1,
          "C:/Users/dclapp159_02/Documents/AutoScore2406/conversion_table_2g_tuned.kidney.csv")



#








#####






