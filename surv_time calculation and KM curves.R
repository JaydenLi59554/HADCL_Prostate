library(survminer)
library(survex)
library(AutoScore)
library(knitr)

# read file ---------------------------------------------------------------
rm(list = ls())
data1  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/1.Colorectal.csv")
data2  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/2.Breast.csv")
data3  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/3.Lung.csv")
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/4.Liver.csv")
data5  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/5.Prostate.csv")
data6  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/6.Bladder.csv")
data7  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/7.Uteri.csv")
data8  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/8.Stomach.csv")
data9  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/9.Pancreas.csv")
data10 <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/10.Lymphoma.csv")
data11 <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/11.Skin.csv")
data12 <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/12.Nasopharynx.csv")
data13 <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/13.Thyroid.csv")
data14 <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/14.Mye.csv")
data15 <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/15.Kidney.csv")
data_all <- read.csv("C:/Users/dclapp159_02/Documents/allcancer.csv")
data_dm <- read.csv("C:/Users/dclapp159_02/Documents/dm_data_com.edr.csv")


# survivaltime ------------------------------------------------------------

summary(data1$time)[3]
# 51.87778 
summary(data2$time)[3]
# 87.26667 
summary(data3$time)[3]
# 9.166667 
summary(data4$time)[3]
# 19.33333
summary(data5$time)[3]
# 55.83333
summary(data6$time)[3]
# 71 
summary(data7$time)[3]
# 89.3
summary(data8$time)[3]
# 16.26667
summary(data9$time)[3]
# 3.380556 
summary(data10$time)[3]
# 48.7 
summary(data11$time)[3]
# 64.96667
summary(data12$time)[3]
# 71.52292 
summary(data13$time)[3]
# 103.5
summary(data14$time)[3]
# 27.4
summary(data15$time)[3]
# 41.32153
data_dm$time <- data_dm$time/24/(365/12)
summary(data_dm$time)[3]
# 43.80205


# km survival time colo --------------------------------------------------------

data1  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/1.Colorectal.csv")
colnames(data1)
str(data1)
data1$family_dm_flag_Y <- as.factor(data1$family_dm_flag_Y)
data1$ht_flag_Y <- as.factor(data1$ht_flag_Y)
data1$insulin_flag_Y <- as.factor(data1$insulin_flag_Y)
data1$anti_htn_flag_Y <- as.factor(data1$anti_htn_flag_Y)
data1$anti_lipid_flag_Y <- as.factor(data1$anti_lipid_flag_Y)
data1$chd_flag_Y <- as.factor(data1$chd_flag_Y)
data1$stroke_flag_Y <- as.factor(data1$stroke_flag_Y)
# data1$dialysis_flag <- as.factor(data1$dialysis_flag)
data1$central_obesity_flag_Y <- as.factor(data1$central_obesity_flag_Y)
data1$sex_M <- as.factor(data1$sex_M)
data1$diabetic_drug_Y <- as.factor(data1$diabetic_drug_Y)
# data1$diagnosis_year_group <- as.factor(data1$diagnosis_year_group)
data1$smoke_status_cd_Y <- as.factor(data1$smoke_status_cd_Y)
data1$alcohol_status_cd_Y <- as.factor(data1$alcohol_status_cd_Y)


data1$bmi <- as.factor(ifelse(data1$bmi <= 23.9 ,0,1))
data1$hba1c <-  as.factor(ifelse(data1$hba1c < 7 ,0,1))
data1$fasting_gc <-  as.factor(ifelse(data1$fasting_gc < 7 ,0,1))
data1$ldl_c <-  as.factor(ifelse(data1$ldl_c < 3.4 ,0,1))
data1$hdl_c <-  as.factor(ifelse(data1$hdl_c > 1 ,0,1))
data1$triglyceride <-  as.factor(ifelse(data1$triglyceride < 1.7 ,0,1))
data1$total_clt <-  as.factor(ifelse(data1$total_clt < 6.2 ,0,1))
# data1$serum_k1 <-  as.factor(ifelse(data1$serum_k < 4.9 & data1$serum_k > 3.6 ,0,1))
data1$Age_group <- ifelse(data1$diagnosis_Age <= 50 , "<=50",
                          ifelse(data1$diagnosis_Age >= 75, ">=75","50-75"))

data1$Age_group <- factor(data1$Age_group,levels = c("<=50","50-75",">=75"))

view_df(data1 ,show.frq = T,show.prc = T, show.na = T)
data2 <- data1[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data1)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data1)
med2 <- surv_median(fit.sex)

fit.lipid <- survfit(Surv(time,status)~anti_lipid_flag_Y,data = data1)
med3 <- surv_median(fit.lipid)

fit.ldl <- survfit(Surv(time,status)~ldl_c,data = data1)
med4 <- surv_median(fit.ldl)

fit.cob <- survfit(Surv(time,status)~central_obesity_flag_Y,data = data1)
med5 <- surv_median(fit.cob)

fit.bmi <- survfit(Surv(time,status)~bmi,data = data1)
med6 <- surv_median(fit.bmi)

fit.family_dm_flag_Y <- survfit(Surv(time,status)~family_dm_flag_Y,data = data1)
med7 <- surv_median(fit.family_dm_flag_Y)

fit.total_clt <- survfit(Surv(time,status)~total_clt,data = data1)
med8 <- surv_median(fit.total_clt)

fit.smoke_status_cd_Y <- survfit(Surv(time,status)~smoke_status_cd_Y,data = data1)
med9 <- surv_median(fit.smoke_status_cd_Y)

fit.hba1c <- survfit(Surv(time,status)~hba1c,data = data1)
med10 <- surv_median(fit.hba1c)

fit.stroke_flag_Y <- survfit(Surv(time,status)~stroke_flag_Y,data = data1)
med11 <- surv_median(fit.stroke_flag_Y)

fit.triglyceride <- survfit(Surv(time,status)~triglyceride,data = data1)
med12 <- surv_median(fit.triglyceride)

insulin_flag_Y <- survfit(Surv(time,status)~insulin_flag_Y,data = data1)
med13 <- surv_median(insulin_flag_Y)

hdl_c <- survfit(Surv(time,status)~hdl_c,data = data1)
med14 <- surv_median(hdl_c)

alcohol_status_cd_Y <- survfit(Surv(time,status)~alcohol_status_cd_Y,data = data1)
med15 <- surv_median(alcohol_status_cd_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
      med12,med13,med14,med15)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_colo.csv")

colnames(data1)[1:2] <- c("label_time","label_status")
colnames(data1)
AutoScore::compute_descriptive_table(data1)

#

# km survival time breast --------------------------------------------------------

data2  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/2.Breast.csv")
colnames(data2)
str(data2)
data2$family_dm_flag_Y <- as.factor(data2$family_dm_flag_Y)
data2$ht_flag_Y <- as.factor(data2$ht_flag_Y)
data2$insulin_flag_Y <- as.factor(data2$insulin_flag_Y)
data2$anti_htn_flag_Y <- as.factor(data2$anti_htn_flag_Y)
data2$anti_lipid_flag_Y <- as.factor(data2$anti_lipid_flag_Y)
data2$chd_flag_Y <- as.factor(data2$chd_flag_Y)
data2$stroke_flag_Y <- as.factor(data2$stroke_flag_Y)
# data2$dialysis_flag <- as.factor(data2$dialysis_flag)
data2$central_obesity_flag_Y <- as.factor(data2$central_obesity_flag_Y)
data2$sex_M <- as.factor(data2$sex_M)
data2$diabetic_drug_Y <- as.factor(data2$diabetic_drug_Y)
# data2$diagnosis_year_group <- as.factor(data2$diagnosis_year_group)
data2$smoke_status_cd_Y <- as.factor(data2$smoke_status_cd_Y)
data2$alcohol_status_cd_Y <- as.factor(data2$alcohol_status_cd_Y)

colnames(data2)[1:2] <- c("label_time","label_status")
colnames(data2)
AutoScore::compute_descriptive_table(data2)

data2$bmi <- as.factor(ifelse(data2$bmi <= 23.9 ,0,1))
data2$hba1c <-  as.factor(ifelse(data2$hba1c < 7 ,0,1))
data2$fasting_gc <-  as.factor(ifelse(data2$fasting_gc < 7 ,0,1))
data2$ldl_c <-  as.factor(ifelse(data2$ldl_c < 3.4 ,0,1))
data2$hdl_c <-  as.factor(ifelse(data2$hdl_c > 1 ,0,1))
data2$triglyceride <-  as.factor(ifelse(data2$triglyceride < 1.7 ,0,1))
data2$total_clt <-  as.factor(ifelse(data2$total_clt < 6.2 ,0,1))
# data2$serum_k1 <-  as.factor(ifelse(data2$serum_k < 4.9 & data2$serum_k > 3.6 ,0,1))
data2$Age_group <- ifelse(data2$diagnosis_Age <= 50 , "<=50",
                          ifelse(data2$diagnosis_Age >= 75, ">=75","50-75"))

data2$Age_group <- factor(data2$Age_group,levels = c("<=50","50-75",">=75"))

view_df(data2 ,show.frq = T,show.prc = T, show.na = T)
data2 <- data2[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data2)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data2)
med2 <- surv_median(fit.sex)

fit.lipid <- survfit(Surv(time,status)~anti_lipid_flag_Y,data = data2)
med3 <- surv_median(fit.lipid)

fit.ldl <- survfit(Surv(time,status)~ldl_c,data = data2)
med4 <- surv_median(fit.ldl)

fit.cob <- survfit(Surv(time,status)~central_obesity_flag_Y,data = data2)
med5 <- surv_median(fit.cob)

fit.bmi <- survfit(Surv(time,status)~bmi,data = data2)
med6 <- surv_median(fit.bmi)

fit.family_dm_flag_Y <- survfit(Surv(time,status)~family_dm_flag_Y,data = data2)
med7 <- surv_median(fit.family_dm_flag_Y)

fit.total_clt <- survfit(Surv(time,status)~total_clt,data = data2)
med8 <- surv_median(fit.total_clt)

fit.smoke_status_cd_Y <- survfit(Surv(time,status)~smoke_status_cd_Y,data = data2)
med9 <- surv_median(fit.smoke_status_cd_Y)

fit.hba1c <- survfit(Surv(time,status)~hba1c,data = data2)
med10 <- surv_median(fit.hba1c)

fit.stroke_flag_Y <- survfit(Surv(time,status)~stroke_flag_Y,data = data2)
med11 <- surv_median(fit.stroke_flag_Y)

fit.triglyceride <- survfit(Surv(time,status)~triglyceride,data = data2)
med12 <- surv_median(fit.triglyceride)

insulin_flag_Y <- survfit(Surv(time,status)~insulin_flag_Y,data = data2)
med13 <- surv_median(insulin_flag_Y)

hdl_c <- survfit(Surv(time,status)~hdl_c,data = data2)
med14 <- surv_median(hdl_c)

alcohol_status_cd_Y <- survfit(Surv(time,status)~alcohol_status_cd_Y,data = data2)
med15 <- surv_median(alcohol_status_cd_Y)

anti_htn_flag_Y <- survfit(Surv(time,status)~anti_htn_flag_Y,data = data2)
med16 <- surv_median(anti_htn_flag_Y)

chd_flag_Y <- survfit(Surv(time,status)~chd_flag_Y,data = data2)
med17 <- surv_median(chd_flag_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_breast.csv")


# km survival time lung --------------------------------------------------------

data3  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/3.Lung.csv")
colnames(data3)
str(data3)
data3$family_dm_flag_Y <- as.factor(data3$family_dm_flag_Y)
data3$ht_flag_Y <- as.factor(data3$ht_flag_Y)
data3$insulin_flag_Y <- as.factor(data3$insulin_flag_Y)
data3$anti_htn_flag_Y <- as.factor(data3$anti_htn_flag_Y)
data3$anti_lipid_flag_Y <- as.factor(data3$anti_lipid_flag_Y)
data3$chd_flag_Y <- as.factor(data3$chd_flag_Y)
data3$stroke_flag_Y <- as.factor(data3$stroke_flag_Y)
# data3$dialysis_flag <- as.factor(data3$dialysis_flag)
data3$central_obesity_flag_Y <- as.factor(data3$central_obesity_flag_Y)
data3$sex_M <- as.factor(data3$sex_M)
data3$diabetic_drug_Y <- as.factor(data3$diabetic_drug_Y)
# data3$diagnosis_year_group <- as.factor(data3$diagnosis_year_group)
data3$smoke_status_cd_Y <- as.factor(data3$smoke_status_cd_Y)
data3$alcohol_status_cd_Y <- as.factor(data3$alcohol_status_cd_Y)


colnames(data3)[1:2] <- c("label_time","label_status")
colnames(data3)
AutoScore::compute_descriptive_table(data3)

data3$bmi <- as.factor(ifelse(data3$bmi <= 23.9 ,0,1))
data3$hba1c <-  as.factor(ifelse(data3$hba1c < 7 ,0,1))
data3$fasting_gc <-  as.factor(ifelse(data3$fasting_gc < 7 ,0,1))
data3$ldl_c <-  as.factor(ifelse(data3$ldl_c < 3.4 ,0,1))
data3$hdl_c <-  as.factor(ifelse(data3$hdl_c > 1 ,0,1))
data3$triglyceride <-  as.factor(ifelse(data3$triglyceride < 1.7 ,0,1))
data3$total_clt <-  as.factor(ifelse(data3$total_clt < 6.2 ,0,1))
# data3$serum_k1 <-  as.factor(ifelse(data3$serum_k < 4.9 & data3$serum_k > 3.6 ,0,1))
data3$Age_group <- ifelse(data3$diagnosis_Age <= 50 , "<=50",
                          ifelse(data3$diagnosis_Age >= 75, ">=75","50-75"))

data3$Age_group <- factor(data3$Age_group,levels = c("<=50","50-75",">=75"))

view_df(data3 ,show.frq = T,show.prc = T, show.na = T)
data3 <- data3[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data3)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data3)
med2 <- surv_median(fit.sex)

fit.lipid <- survfit(Surv(time,status)~anti_lipid_flag_Y,data = data3)
med3 <- surv_median(fit.lipid)

fit.ldl <- survfit(Surv(time,status)~ldl_c,data = data3)
med4 <- surv_median(fit.ldl)

fit.cob <- survfit(Surv(time,status)~central_obesity_flag_Y,data = data3)
med5 <- surv_median(fit.cob)

fit.bmi <- survfit(Surv(time,status)~bmi,data = data3)
med6 <- surv_median(fit.bmi)

fit.family_dm_flag_Y <- survfit(Surv(time,status)~family_dm_flag_Y,data = data3)
med7 <- surv_median(fit.family_dm_flag_Y)

fit.total_clt <- survfit(Surv(time,status)~total_clt,data = data3)
med8 <- surv_median(fit.total_clt)

fit.smoke_status_cd_Y <- survfit(Surv(time,status)~smoke_status_cd_Y,data = data3)
med9 <- surv_median(fit.smoke_status_cd_Y)

fit.hba1c <- survfit(Surv(time,status)~hba1c,data = data3)
med10 <- surv_median(fit.hba1c)

fit.stroke_flag_Y <- survfit(Surv(time,status)~stroke_flag_Y,data = data3)
med11 <- surv_median(fit.stroke_flag_Y)

fit.triglyceride <- survfit(Surv(time,status)~triglyceride,data = data3)
med12 <- surv_median(fit.triglyceride)

insulin_flag_Y <- survfit(Surv(time,status)~insulin_flag_Y,data = data3)
med13 <- surv_median(insulin_flag_Y)

hdl_c <- survfit(Surv(time,status)~hdl_c,data = data3)
med14 <- surv_median(hdl_c)

alcohol_status_cd_Y <- survfit(Surv(time,status)~alcohol_status_cd_Y,data = data3)
med15 <- surv_median(alcohol_status_cd_Y)

anti_htn_flag_Y <- survfit(Surv(time,status)~anti_htn_flag_Y,data = data3)
med16 <- surv_median(anti_htn_flag_Y)

chd_flag_Y <- survfit(Surv(time,status)~chd_flag_Y,data = data3)
med17 <- surv_median(chd_flag_Y)

diabetic_drug_Y <- survfit(Surv(time,status)~diabetic_drug_Y,data = data3)
med18 <- surv_median(diabetic_drug_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_lung.csv")

# km survival time liver --------------------------------------------------------

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
colnames(data4)
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

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_liver.csv")

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


# km survival time bladder --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/6.Bladder.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_bladder.csv")

# km survival time Uteri --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/7.Uteri.csv")

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
data4 <- data4[,-13]

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

# ht_flag_Y <- survfit(Surv(time,status)~ht_flag_Y,data = data4)
# med19 <- surv_median(ht_flag_Y)

survtime1 <- rbind(med1,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_Uteri.csv")

# km survival time stomach --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/8.Stomach.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_stomach.csv")


# km survival time pancreas --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/9.Pancreas.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_pancreas.csv")


# km survival time 10.Lymphoma --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/10.Lymphoma.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

# ht_flag_Y <- survfit(Surv(time,status)~ht_flag_Y,data = data4)
# med19 <- surv_median(ht_flag_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_lymphoma.csv")


# km survival time skin --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/11.Skin.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_skin.csv")


# km survival time 12.Nasopharynx --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/12.Nasopharynx.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

# ht_flag_Y <- survfit(Surv(time,status)~ht_flag_Y,data = data4)
# med19 <- surv_median(ht_flag_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_Nasopharynx.csv")


# km survival time thyroid --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/13.Thyroid.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

# ht_flag_Y <- survfit(Surv(time,status)~ht_flag_Y,data = data4)
# med19 <- surv_median(ht_flag_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_thyroid.csv")


# km survival time mye --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/14.Mye.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18,med19)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_mye.csv")


# km survival time kidney --------------------------------------------------------
rm(list = ls())
data4  <- read.csv("C:/Users/dclapp159_02/Documents/K-M_data/15.Kidney.csv")

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
data4 <- data4[,-13]

fit.age <- survfit(Surv(time,status)~ Age_group,data = data4)
med1 <- surv_median(fit.age)

fit.sex <- survfit(Surv(time,status)~sex_M,data = data4)
med2 <- surv_median(fit.sex)

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

# ht_flag_Y <- survfit(Surv(time,status)~ht_flag_Y,data = data4)
# med19 <- surv_median(ht_flag_Y)

survtime1 <- rbind(med1,med2,med3,med4,med5,med6,med7,med8,med9,med10,med11,
                   med12,med13,med14,med15,med16,med17,med18)

setwd("C:/Users/dclapp159_02/Documents")
write.csv(survtime1,"AutoScore2406/M_survtime/survtime_kidney.csv")


#####



