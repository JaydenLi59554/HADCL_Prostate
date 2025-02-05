library(survminer)
library(survex)
library(AutoScore)
library(knitr)

# read file ---------------------------------------------------------------
rm(list = ls())
data5  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/5.Prostate.csv")



# survivaltime ------------------------------------------------------------

summary(data5$time)[3]
# 55.83333

# km survival time Prostate --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/5.Prostate.csv")

colnames(data4)
str(data4)
data4$family_dm_flag_Y <- as.factor(data4$family_dm_flag_Y)
data4$ht_flag_Y <- as.factor(data4$ht_flag_Y)
data4$insulin_flag_Y <- as.factor(data4$insulin_flag_Y)
data4$anti_htn_flag_Y <- as.factor(data4$anti_htn_flag_Y)
data4$anti_lipid_flag_Y <- as.factor(data4$anti_lipid_flag_Y)
data4$chd_flag_Y <- as.factor(data4$chd_flag_Y)
data4$stroke_flag_Y <- as.factor(data4$stroke_flag_Y)
# data4$dialysis_flag <- as.factor(data4$dialysis_flag)
data4$central_obesity_flag_Y <- as.factor(data4$central_obesity_flag_Y)
data4$sex_M <- as.factor(data4$sex_M)
data4$diabetic_drug_Y <- as.factor(data4$diabetic_drug_Y)
# data4$diagnosis_year_group <- as.factor(data4$diagnosis_year_group)
data4$smoke_status_cd_Y <- as.factor(data4$smoke_status_cd_Y)
data4$alcohol_status_cd_Y <- as.factor(data4$alcohol_status_cd_Y)

colnames(data4)[1:2] <- c("label_time","label_status")
AutoScore::compute_descriptive_table(data4)

data4$bmi <- as.factor(ifelse(data4$bmi <= 23.9 ,0,1))
data4$hba1c <-  as.factor(ifelse(data4$hba1c < 7 ,0,1))
data4$fasting_gc <-  as.factor(ifelse(data4$fasting_gc < 7 ,0,1))
data4$ldl_c <-  as.factor(ifelse(data4$ldl_c < 3.4 ,0,1))
data4$hdl_c <-  as.factor(ifelse(data4$hdl_c > 1 ,0,1))
data4$triglyceride <-  as.factor(ifelse(data4$triglyceride < 1.7 ,0,1))
data4$total_clt <-  as.factor(ifelse(data4$total_clt < 6.2 ,0,1))
# data4$serum_k1 <-  as.factor(ifelse(data4$serum_k < 4.9 & data4$serum_k > 3.6 ,0,1))
data4$Age_group <- ifelse(data4$diagnosis_Age <= 50 , "<=50",
                          ifelse(data4$diagnosis_Age >= 75, ">=75","50-75"))

data4$Age_group <- factor(data4$Age_group,levels = c("<=50","50-75",">=75"))

view_df(data4 ,show.frq = T,show.prc = T, show.na = T)
data2 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

# fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
# med2 <- surv_median(fit.sex)

fit.lipid <- survfit(Surv(time,status)~anti_lipid_flag_Y,data = data4)
med3 <- surv_median(fit.lipid)

fit.ldl <- survfit(Surv(time,status)~ldl_c,data = data4)
med4 <- surv_median(fit.ldl)

fit.cob <- survfit(Surv(time,status)~central_obesity_flag_Y,data = data4)
med5 <- surv_median(fit.cob)

fit.bmi <- survfit(Surv(time,status)~bmi,data = data4)
med6 <- surv_median(fit.bmi)

fit.family_dm_flag_Y <- survfit(Surv(time,status)~family_dm_flag_Y,data = data4)
med7 <- surv_median(fit.family_dm_flag_Y)

fit.total_clt <- survfit(Surv(time,status)~total_clt,data = data4)
med8 <- surv_median(fit.total_clt)

fit.smoke_status_cd_Y <- survfit(Surv(time,status)~smoke_status_cd_Y,data = data4)
med9 <- surv_median(fit.smoke_status_cd_Y)

fit.hba1c <- survfit(Surv(time,status)~hba1c,data = data4)
med10 <- surv_median(fit.hba1c)

fit.stroke_flag_Y <- survfit(Surv(time,status)~stroke_flag_Y,data = data4)
med11 <- surv_median(fit.stroke_flag_Y)

fit.triglyceride <- survfit(Surv(time,status)~triglyceride,data = data4)
med12 <- surv_median(fit.triglyceride)

insulin_flag_Y <- survfit(Surv(time,status)~insulin_flag_Y,data = data4)
med13 <- surv_median(insulin_flag_Y)

hdl_c <- survfit(Surv(time,status)~hdl_c,data = data4)
med14 <- surv_median(hdl_c)

alcohol_status_cd_Y <- survfit(Surv(time,status)~alcohol_status_cd_Y,data = data4)
med15 <- surv_median(alcohol_status_cd_Y)

anti_htn_flag_Y <- survfit(Surv(time,status)~anti_htn_flag_Y,data = data4)
med16 <- surv_median(anti_htn_flag_Y)

chd_flag_Y <- survfit(Surv(time,status)~chd_flag_Y,data = data4)
med17 <- surv_median(chd_flag_Y)

diabetic_drug_Y <- survfit(Surv(time,status)~diabetic_drug_Y,data = data4)
med18 <- surv_median(diabetic_drug_Y)

ht_flag_Y <- survfit(Surv(time,status)~ht_flag_Y,data = data4)
med19 <- surv_median(ht_flag_Y)

survtime1 <- rbind(med1,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_prostate.csv")

####



