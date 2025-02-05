library(dplyr)
library(lubridate)

d01 <- gzfile("C:/Users/dclapp159_02/Documents/patient_data_0.csv.gz","rt")
D01 <- read.table(d01,header = T,sep = "\01",quote = "", comment.char = "#")
for (i in 1:length(colnames(D01))) {
  D01[which(D01[,i] == ""),i] <- NA
}
# nrow(D01) [1] 499334
# colnames(D01)

d02 <- gzfile("C:/Users/dclapp159_02/Documents/dmcs_data_D02.csv.gz","rt")
D02 <- read.table(d02,header = T,sep = "\01",quote = "", comment.char = "#")
for (i in 1:length(colnames(D02))) {
  D02[which(D02[,i] == ""),i] <- NA
}
# colnames(D02)

d06 <- gzfile("C:/Users/dclapp159_02/Documents/mdl_dx_progress_0.csv.gz","rt")
D06 <- read.table(d06,header = T,sep = "\01",quote = "", comment.char = "#")
# colnames(D06)

map <- gzfile("C:/Users/dclapp159_02/Documents/map_icd10_diag.csv.gz","rt")
Map <- read.table(map,header = T,sep = "\01",quote = "", comment.char = "#")
head(Map)


##### Slection in D06 #####

### Deal with final rank
str(D06)
table(D06$final_rank)
D06[is.na(D06$final_rank),"final_rank"] <- 2   # deal with NA 
table(D06$final_rank)

### Select the first diagnosis record
grouped.06 <- D06[order(D06$pseudo_patient_key,D06$final_rank,D06$diff_in_hour_reference_dtm),]
# grouped.06[c(1:20),c(1,2,3,4,9)] # check
# length(unique(grouped.06$pseudo_patient_key)) # pateints number 515703

### Output the first diagnosis record 
uni06 <- aggregate(x = grouped.06,by = list(grouped.06$pseudo_patient_key), 
                   FUN = function(x) x[1])[,-1]
str(uni06)
# nrow(uni06) # patients number 515703
colnames(uni06)

##### Merge D06 with D02 ##### 

colnames(D02)
# length(unique(D02$pseudo_patient_key)) # [1] 58033
d0206 <- merge(D02,uni06[,c(1:3,9)],
               by.x = "pseudo_patient_key", by.y = "pseudo_patient_key")
colnames(d0206)
str(d0206[,c(1:3,328:331)])

##### Merge d0206 with D01 ##### 

str(D01)
d126 <- merge(d0206,D01[,c(1:5)],
              by.x = "pseudo_patient_key", by.y = "pseudo_patient_key")
colnames(d126)

### Calculate diagnosis age & diagnosis year & Age_group & diagnosis_year_group
str(d126[,c("dob_Y","reference_dtm")])
d126$dob_Y <- as.Date(d126$dob_Y)
d126$reference_dtm <- as.Date(d126$reference_dtm)

d126$diagnosis_Age <- as.numeric(round(difftime(d126$reference_dtm,d126$dob_Y,"UTC",units = "days")/365,0))
str(d126$diagnosis_Age)
d126$diagnosis_year <- as.numeric(substr(d126[,"reference_dtm"],1,4))
str(d126$diagnosis_year)
d126$Age_group <- ifelse(d126$diagnosis_Age <= 50 , "<=50",
                         ifelse(d126$diagnosis_Age >= 75, ">=75","50-75"))
table(d126$Age_group)
d126$diagnosis_year_group <- ifelse(d126$diagnosis_year <= 2004 , "2000-2004",
                                    ifelse(d126$diagnosis_year <= 2009  ,"2005-2009",
                                           ifelse(d126$diagnosis_year <= 2014, "2010-2014","2015-2020")))
# table(d126$diagnosis_year_group)
# 2000-2004 2005-2009 2010-2014 2015-2020 
# 17611     22569     41524     87379 

### Calculate dm_period(days) & status & surviving time(hours)

d126$dx_dtm <-as.Date(d126$dx_dtm)
d126$dm_period <- as.numeric(round(difftime(d126$reference_dtm,d126$dx_dtm,"UTC",units = "days"),0))
# length(which(d126$dm_period <= 0)) # 26041
d126$dm_period[which(d126$dm_period <= 0)] <- 0
summary(d126$dm_period)

d126$death_date_Y <- as.Date(d126$death_date_Y)
# table(d126$death_date_Y,useNA = "always")
# 2010-01-01 2011-01-01 2012-01-01 2013-01-01 2014-01-01 2015-01-01 
#      69        428       1255       2294       3234       4792       
# 2016-01-01 2017-01-01 2018-01-01 2019-01-01 2020-01-01   <NA> 
#     5955       7116       8303       9316      11050    115271 

d126$death_date <- as.numeric(substr(d126[,"death_date_Y"],1,4))
d126[is.na(d126$death_date),"death_date"] <- 9999 # set a number for patients living after 2020
# table(d126$death_date,useNA = "always")
# 2010   2011   2012   2013   2014   2015   2016   2017   2018   2019   2020   9999   <NA> 
#   69    428   1255   2294   3234   4792   5955   7116   8303   9316  11050 115271      0 

a04 <- which(d126$diagnosis_year_group == "2000-2004")
a09 <- which(d126$diagnosis_year_group == "2005-2009")
a14 <- which(d126$diagnosis_year_group == "2010-2014")

d126$status <- ifelse(d126$death_date == 9999,0,1)
d126[a04,"status"] <- 
  ifelse(d126[a04,"death_date"] <= 2009,1,0)
d126[a09,"status"] <- 
  ifelse(d126[a09,"death_date"] <= 2014,1,0)  
d126[a14,"status"] <- 
  ifelse(d126[a14,"death_date"] <= 2019,1,0)
# table(d126$status,useNA = "always")
#        0      1   <NA> 
#   123353  45730      0 

d126[a04,"time"] <- 
  ifelse(d126[a04,"death_date"] <= 2009,
         d126[a04,"diff_in_hour_death_date"] - d126[a04,"diff_in_hour_reference_dtm"],
         difftime("2009-12-01",d126[a04,"reference_dtm"],"UTC",units = "hours"))
d126[a09,"time"] <- 
  ifelse(d126[a09,"death_date"] <= 2014,
         d126[a09,"diff_in_hour_death_date"] - d126[a09,"diff_in_hour_reference_dtm"],
         difftime("2014-12-01",d126[a09,"reference_dtm"],"UTC",units = "hours"))
d126[a14,"time"] <- 
  ifelse(d126[a14,"death_date"] <= 2019,
         d126[a14,"diff_in_hour_death_date"] - d126[a14,"diff_in_hour_reference_dtm"],
         difftime("2019-12-01",d126[a14,"reference_dtm"],"UTC",units = "hours"))
# length(which(d126$time < 0)) # 1199
d126$time[which(d126$time <= 0)] <- 0
summary(d126$time)

##### Select the patients #####

### Calculate the difftime between cancer diagnosis and screening of DM patients
difftime <- abs(d126$diff_in_hour_reference_dtm - d126$diff_in_hour_assessment_dtm)
d126$difftime <- difftime
head(d126$difftime)

### Order to find the shortest difftime
d126.1 <- d126[order(d126$pseudo_patient_key,d126$difftime),]
# d126.1[c(1:25),c(1,284,328,330,332,344)] # check

### Select the DM2 patients
table(d126.1$dm_type_cd)
dmtypex1 <- d126.1[which(d126$dm_type_cd != 1),]
# length(unique(dmtypex1$pseudo_patient_key)) # 657
name1 <- unique(dmtypex1$pseudo_patient_key)
# head(name1)

### Correct the dm type variable
for (i in 1:length(name1)) {
  if(1 %in% d126.1[which(d126.1$pseudo_patient_key == name1[i]),"dm_type_cd"] == TRUE){
    d126.1[which(d126.1$pseudo_patient_key == name1[i]),"dm_type_cd"] <- 1
  } 
}
# table(d126.1$dm_type_cd)
# table(d126$dm_type_cd)
d126.2 <- d126.1[which(d126.1$dm_type_cd == 1),]
# nrow(d126.2) #164957


##### Merge d126 with Map(cancer description) ##### 

colnames(Map)
d126m <- merge(d126.2,Map,by.x = "term_id",
               by.y = "term_id")
# which(colnames(d126m)=="icd10_cd")  #345

### select the first three code
d126m[,345] <- substr(d126m[,345],1,3)

### Sort the ICD-10 code
sort(table(d126m[,345]),decreasing = TRUE)
# C50   C18   C34   C61   C22   C20   C54   C67   C16   C44   C25   C85   C73   C11   C53   C64 
# 27240 20980 15054 12329 12007  9391  6938  6414  5555  4242  3910  3858  3597  3379  2758  2700 
# C19   C56   C90   C78   C79   C92   C15   C49   C02   C32   C24   C23   C77   C91   C43   C80 
# 1987  1601  1432  1344  1335  1236  1168  1084  1062  1037   934   721   517   512   499   464 
# C06   C71   C65   C82   C17   C21   C66   C07   C51   C48   C95   C09   C60   C57   C81   C62 
# 424   409   406   360   349   339   290   266   247   246   241   226   208   188   188   187 
# C31   C55   C03   C30   C05   C38   C12   C37   C41   C69   C13   C26   C40   C63   C83   C74 
# 169   169   160   158   148   146   142   142   132   120   116   116   109   104   103    97 
# C01   C52   C04   C08   C93   C39   C68   C10   C75   C00   C14   C76   C84   C88   C33   D47 
# 94    92    89    78    69    66    58    57    48    42    35    35    33    33    32    24 
# C72   C86   C96   C58   D48   K56   C46   C70   K92   C94 
# 23    11    10     9     8     7     5     5     <5    <5 


sort(table(d126m[d126m$icd10_cd == "C50",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C50"] <- "Cancer of breast"

sort(table(d126m[d126m$icd10_cd == "C18",346]),decreasing = TRUE)
table(d126m$icd10_cd[grep("rect",d126m$full_desc)])
table(d126m$full_desc[grep("rect",d126m$full_desc)])
d126m$short_desc[d126m$icd10_cd == "C18"] <- "Cancer of colorectal"
d126m$short_desc[d126m$icd10_cd == "C19"] <- "Cancer of colorectal"
d126m$short_desc[d126m$icd10_cd == "C20"] <- "Cancer of colorectal"
d126m$short_desc[d126m$icd10_cd == "C21"] <- "Cancer of colorectal"

sort(table(d126m[d126m$icd10_cd == "C34",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C34"] <- "Cancer of lung"

sort(table(d126m[d126m$icd10_cd == "C22",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C22"] <- "Cancer of liver"

sort(table(d126m[d126m$icd10_cd == "C61",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C61"] <- "Cancer of prostate"

sort(table(d126m[d126m$icd10_cd == "C67",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C67"] <- "Cancer of bladder"

sort(table(d126m[d126m$icd10_cd == "C54",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C54"] <- "Cancer of uteri"

sort(table(d126m[d126m$icd10_cd == "C16",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C16"] <- "Cancer of stomach"

sort(table(d126m[d126m$icd10_cd == "C25",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C25"] <- "Cancer of pancreas"

sort(table(d126m[d126m$icd10_cd == "C85",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C82",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C83",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C84",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C86",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C96",346]),decreasing = TRUE) 
d126m$short_desc[d126m$icd10_cd == "C85"] <- "Non-Hodgkin lymphoma"
d126m$short_desc[d126m$icd10_cd == "C82"] <- "Non-Hodgkin lymphoma"
d126m$short_desc[d126m$icd10_cd == "C83"] <- "Non-Hodgkin lymphoma"
d126m$short_desc[d126m$icd10_cd == "C84"] <- "Non-Hodgkin lymphoma"
d126m$short_desc[d126m$icd10_cd == "C86"] <- "Non-Hodgkin lymphoma"
d126m$short_desc[d126m$icd10_cd == "C96"] <- "Non-Hodgkin lymphoma"

sort(table(d126m[d126m$icd10_cd == "C81",346]),decreasing = TRUE) 
d126m$short_desc[d126m$icd10_cd == "C81"] <- "Hodgkin lymphomas"

sort(table(d126m[d126m$icd10_cd == "C90",346]),decreasing = TRUE) 
d126m$short_desc[d126m$icd10_cd == "C90"] <- "Multiple myeloma"

sort(table(d126m[d126m$icd10_cd == "C91",346]),decreasing = TRUE) 
d126m$short_desc[d126m$icd10_cd == "C91"] <- "Lymphoid leukaemia"

sort(table(d126m[d126m$icd10_cd == "C92",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C93",346]),decreasing = TRUE) 
sort(table(d126m[d126m$icd10_cd == "C94",346]),decreasing = TRUE) 
d126m$short_desc[d126m$icd10_cd == "C92"] <- "Multiple myeloma"
d126m$short_desc[d126m$icd10_cd == "C93"] <- "Multiple myeloma"
d126m$short_desc[d126m$icd10_cd == "C94"] <- "Multiple myeloma"

sort(table(d126m[d126m$icd10_cd == "C44",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C44"] <- "Cancer of skin"

sort(table(d126m[d126m$icd10_cd == "C11",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C11"] <- "Cancer of nasopharynx"

sort(table(d126m[d126m$icd10_cd == "C73",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C73"] <- "Cancer of thyroid"

sort(table(d126m[d126m$icd10_cd == "C64",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C64"] <- "Cancer of kidney"

sort(table(d126m[d126m$icd10_cd == "C53",346]),decreasing = TRUE)
d126m$short_desc[d126m$icd10_cd == "C53"] <- "Cancer of cervix uteri"

nrow(d126m) # 164957
length(which(is.na(d126m$short_desc) == TRUE)) # 18324
d126m[which(is.na(d126m$short_desc) == TRUE),"short_desc"] <- d126m[which(is.na(d126m$short_desc) == TRUE),"full_desc"]

### sort the first 20 cancer
colnames(d126m)
sort(table(d126m[,346]),decreasing = TRUE)[1:20]


############################################
### Select the shortest difftime records ###
############################################

d126m.sd <- aggregate(x = d126m, by = list(d126m$pseudo_patient_key),
                      FUN = function(x) x[1])
nrow(d126m.sd) #[1] 54910

### Select the variables 
a <- c("time","status","pseudo_patient_key", "family_dm_flag" ,  "ht_flag" ,     
       "smoke_status_cd" , "alcohol_status_cd" , "insulin_flag"  ,  "anti_htn_flag" ,     
       "anti_lipid_flag" , "bmi"  ,  "hba1c" ,  "fasting_gc" ,
       "total_clt" ,"ldl_c"  , "hdl_c" , "triglyceride"  ,      
       "serum_k"  , "creatinine" ,"dialysis_flag" ,"chd_flag" ,           
       "stroke_flag" ,"pad_flag" , "ckd_cd" ,"central_obesity_flag",
       "anti_diabetic_drug_flag", "oha_flag", "su_flag", "metf_flag",
       "gluco_flag", "glita_flag", "megl_flag","dm_period","sex",
       "diagnosis_Age", "Age_group","diagnosis_year","diagnosis_year_group","short_desc" )

aaa <- rep(NA,length(a))
for (i in 1:length(a)) {
  aaa[i] <- which(colnames(d126m.sd) == a[i])
}
aaa

dsd <- d126m.sd[,c(aaa)]
colnames(dsd)
view_df(dsd,show.frq = T,show.prc = T, show.na = T)
# table(dsd$ht_flag,useNA = "always")

##### classify the variable related to diabetic drug #####
### check the missing value in seven variables related to diabetic drug
VIM::aggr(dsd[,c(26:32)],prop = FALSE, number = TRUE)
for (i in 26:32) {
  dsd[is.na(dsd[,i]),i] <- ""
}
dsd$diabetic_drug <- ifelse(dsd$anti_diabetic_drug_flag == "Y" |
                              dsd$oha_flag == "Y" |
                              dsd$su_flag =="Y" |
                              dsd$megl_flag == "Y" |
                              dsd$gluco_flag == "Y" |
                              dsd$glita_flag == "Y" |
                              dsd$megl_flag == "Y",1,0)
# dsd[1:10,c(26:32,40)] # check
table(dsd$diabetic_drug,useNA = "always")
colnames(dsd)
write.csv(dsd,"C:/Users/dclapp159_02/Documents/earliestdata-126m_0202.csv",row.names = FALSE)



##########################################################################
### Select the shortest difftime records (replace NA with nearest data ###
##########################################################################

d126m.sd2 <- aggregate(x = d126m, by = list(d126m$pseudo_patient_key),
                       FUN = function(x) x[1])
nrow(d126m.sd2) #[1] 54910

dsd2 <- d126m.sd[,c(aaa)]
colnames(dsd2)
view_df(dsd2,show.frq = T,show.prc = T, show.na = T)

####  find the nearest data (family_dm_flag) 
nrow(dsd2[is.na(dsd2$family_dm_flag),])                                # 1636
Q <- rep(NA,nrow(dsd2[is.na(dsd2$family_dm_flag),]))                   # prepare for saving the patient key
family_dm_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$family_dm_flag),])))
# prepare for saviung the nearest data
for (i in 1:nrow(dsd2[is.na(dsd2$family_dm_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$family_dm_flag),"pseudo_patient_key"][i]     # input the patient key of NA data
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),  
                             "family_dm_flag"]) == FALSE))             # find the nearest data 
  family_dm_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                   "family_dm_flag"][a]                       # put the data in data frame
}
family_dm_flag$final[40:55]
length(which(is.na(family_dm_flag$final) == TRUE)) # 460
dsd2[is.na(dsd2$family_dm_flag),"family_dm_flag"] <- family_dm_flag$final
nrow(dsd2[is.na(dsd2$family_dm_flag),]) #[1] 460


####  find the nearest data (ht_flag) 
nrow(dsd2[is.na(dsd2$ht_flag),]) #8349
Q <- rep(NA,nrow(dsd2[is.na(dsd2$ht_flag),]))
ht_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$ht_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$ht_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$ht_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "ht_flag"]) == FALSE))
  ht_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                            "ht_flag"][a]
}
length(which(is.na(ht_flag$final) == TRUE)) #[1] 4891
dsd2[is.na(dsd2$ht_flag),"ht_flag"] <- ht_flag$final
nrow(dsd2[is.na(dsd2$ht_flag),]) #[1] 4891

####  find the nearest data (smoke_status_cd) 
nrow(dsd2[is.na(dsd2$smoke_status_cd),]) # 346
Q <- rep(NA,nrow(dsd2[is.na(dsd2$smoke_status_cd),]))
smoke_status_cd <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$smoke_status_cd),])))
for (i in 1:nrow(dsd2[is.na(dsd2$smoke_status_cd),])) {
  Q[i] <- dsd2[is.na(dsd2$smoke_status_cd),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "smoke_status_cd"]) == FALSE))
  smoke_status_cd$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                    "smoke_status_cd"][a]
}
length(which(is.na(smoke_status_cd$final) == TRUE)) #[1] 130
dsd2[is.na(dsd2$smoke_status_cd),"smoke_status_cd"] <- smoke_status_cd$final
nrow(dsd2[is.na(dsd2$smoke_status_cd),]) #[1] 130

####  find the nearest data (alcohol_status_cd) 
nrow(dsd2[is.na(dsd2$alcohol_status_cd),]) # 4145
Q <- rep(NA,nrow(dsd2[is.na(dsd2$alcohol_status_cd),]))
alcohol_status_cd <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$alcohol_status_cd),])))
for (i in 1:nrow(dsd2[is.na(dsd2$alcohol_status_cd),])) {
  Q[i] <- dsd2[is.na(dsd2$alcohol_status_cd),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "alcohol_status_cd"]) == FALSE))
  alcohol_status_cd$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                      "alcohol_status_cd"][a]
}
length(which(is.na(alcohol_status_cd$final) == TRUE)) #[1] 503
dsd2[is.na(dsd2$alcohol_status_cd),"alcohol_status_cd"] <- alcohol_status_cd$final
nrow(dsd2[is.na(dsd2$alcohol_status_cd),]) #[1] 503

####  find the nearest data ( insulin_flag ) 
nrow(dsd2[is.na(dsd2$insulin_flag ),]) # 4566
Q <- rep(NA,nrow(dsd2[is.na(dsd2$insulin_flag ),]))
insulin_flag  <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$insulin_flag ),])))
for (i in 1:nrow(dsd2[is.na(dsd2$insulin_flag ),])) {
  Q[i] <- dsd2[is.na(dsd2$insulin_flag ),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "insulin_flag"]) == FALSE))
  insulin_flag $final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                  "insulin_flag"][a]
}
length(which(is.na(insulin_flag $final) == TRUE)) #[1] 847
dsd2[is.na(dsd2$insulin_flag ),"insulin_flag"] <-  insulin_flag $final
nrow(dsd2[is.na(dsd2$insulin_flag ),]) #[1] 847

####  find the nearest data (anti_htn_flag) 
nrow(dsd2[is.na(dsd2$anti_htn_flag),]) # 4351
Q <- rep(NA,nrow(dsd2[is.na(dsd2$anti_htn_flag),]))
anti_htn_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$anti_htn_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$anti_htn_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$anti_htn_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "anti_htn_flag"]) == FALSE))
  anti_htn_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                  "anti_htn_flag"][a]
}
length(which(is.na(anti_htn_flag$final) == TRUE)) #[1] 753
dsd2[is.na(dsd2$anti_htn_flag ),"anti_htn_flag"] <-  anti_htn_flag $final
nrow(dsd2[is.na(dsd2$anti_htn_flag ),]) #[1] 753

####  find the nearest data (anti_lipid_flag) 
nrow(dsd2[is.na(dsd2$anti_lipid_flag),]) # 5382
Q <- rep(NA,nrow(dsd2[is.na(dsd2$anti_lipid_flag),]))
anti_lipid_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$anti_lipid_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$anti_lipid_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$anti_lipid_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "anti_lipid_flag"]) == FALSE))
  anti_lipid_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                    "anti_lipid_flag"][a]
}
length(which(is.na(anti_lipid_flag$final) == TRUE)) #[1] 1159
dsd2[is.na(dsd2$anti_lipid_flag ),"anti_lipid_flag"] <-  anti_lipid_flag $final
nrow(dsd2[is.na(dsd2$anti_lipid_flag ),]) #[1] 1159


####  find the nearest data (bmi) 
nrow(dsd2[is.na(dsd2$bmi),]) # 2984
Q <- rep(NA,nrow(dsd2[is.na(dsd2$bmi),]))
bmi <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$bmi),])))
for (i in 1:nrow(dsd2[is.na(dsd2$bmi),])) {
  Q[i] <- dsd2[is.na(dsd2$bmi),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "bmi"]) == FALSE))
  bmi$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                        "bmi"][a]
}
length(which(is.na(bmi$final) == TRUE)) #[1] 612
dsd2[is.na(dsd2$bmi ),"bmi"] <-  bmi $final
nrow(dsd2[is.na(dsd2$bmi ),]) #[1] 612


####  find the nearest data (hba1c) 
nrow(dsd2[is.na(dsd2$hba1c),]) # 3361
Q <- rep(NA,nrow(dsd2[is.na(dsd2$hba1c),]))
hba1c <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$hba1c),])))
for (i in 1:nrow(dsd2[is.na(dsd2$hba1c),])) {
  Q[i] <- dsd2[is.na(dsd2$hba1c),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "hba1c"]) == FALSE))
  hba1c$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                          "hba1c"][a]
}
length(which(is.na( hba1c$final) == TRUE)) #[1] 737
dsd2[is.na(dsd2$hba1c ),"hba1c"] <-  hba1c $final
nrow(dsd2[is.na(dsd2$ hba1c ),]) #[1] 737


####  find the nearest data (fasting_gc) 
nrow(dsd2[is.na(dsd2$fasting_gc),]) # 8458
Q <- rep(NA,nrow(dsd2[is.na(dsd2$fasting_gc),]))
fasting_gc <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$fasting_gc),])))
for (i in 1:nrow(dsd2[is.na(dsd2$fasting_gc),])) {
  Q[i] <- dsd2[is.na(dsd2$fasting_gc),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "fasting_gc"]) == FALSE))
  fasting_gc$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                               "fasting_gc"][a]
}
length(which(is.na(fasting_gc$final) == TRUE)) #[1] 3475
dsd2[is.na(dsd2$fasting_gc ),"fasting_gc"] <-  fasting_gc $final
nrow(dsd2[is.na(dsd2$fasting_gc ),]) #[1] 3475

####  find the nearest data (total_clt) 
nrow(dsd2[is.na(dsd2$total_clt),]) # 2726
Q <- rep(NA,nrow(dsd2[is.na(dsd2$total_clt),]))
total_clt <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$total_clt),])))
for (i in 1:nrow(dsd2[is.na(dsd2$total_clt),])) {
  Q[i] <- dsd2[is.na(dsd2$total_clt),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "total_clt"]) == FALSE))
  total_clt$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                              "total_clt"][a]
}
length(which(is.na(total_clt$final) == TRUE)) #[1] 437
dsd2[is.na(dsd2$total_clt ),"total_clt"] <-  total_clt $final
nrow(dsd2[is.na(dsd2$total_clt ),]) #[1] 437


####  find the nearest data (ldl_c) 
nrow(dsd2[is.na(dsd2$ldl_c),]) # 3359
Q <- rep(NA,nrow(dsd2[is.na(dsd2$ldl_c),]))
ldl_c <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$ldl_c),])))
for (i in 1:nrow(dsd2[is.na(dsd2$ldl_c),])) {
  Q[i] <- dsd2[is.na(dsd2$ldl_c),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "ldl_c"]) == FALSE))
  ldl_c$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                          "ldl_c"][a]
}
length(which(is.na(ldl_c$final) == TRUE)) #[1] 684
dsd2[is.na(dsd2$ldl_c ),"ldl_c"] <-  ldl_c $final
nrow(dsd2[is.na(dsd2$ldl_c ),]) #[1] 684


####  find the nearest data (hdl_c) 
nrow(dsd2[is.na(dsd2$hdl_c),]) # 2919
Q <- rep(NA,nrow(dsd2[is.na(dsd2$hdl_c),]))
hdl_c <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$hdl_c),])))
for (i in 1:nrow(dsd2[is.na(dsd2$hdl_c),])) {
  Q[i] <- dsd2[is.na(dsd2$hdl_c),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "hdl_c"]) == FALSE))
  hdl_c$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                          "hdl_c"][a]
}
length(which(is.na(hdl_c$final) == TRUE)) #[1] 488
dsd2[is.na(dsd2$hdl_c ),"hdl_c"] <-  hdl_c $final
nrow(dsd2[is.na(dsd2$hdl_c ),]) #[1] 488


####  find the nearest data (triglyceride) 
nrow(dsd2[is.na(dsd2$triglyceride),]) # 2732
Q <- rep(NA,nrow(dsd2[is.na(dsd2$triglyceride),]))
triglyceride <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$triglyceride),])))
for (i in 1:nrow(dsd2[is.na(dsd2$triglyceride),])) {
  Q[i] <- dsd2[is.na(dsd2$triglyceride),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "triglyceride"]) == FALSE))
  triglyceride$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                 "triglyceride"][a]
}
length(which(is.na(triglyceride$final) == TRUE)) #[1] 439
dsd2[is.na(dsd2$triglyceride ),"triglyceride"] <-  triglyceride $final
nrow(dsd2[is.na(dsd2$triglyceride ),]) #[1] 439


####  find the nearest data (serum_k) 
nrow(dsd2[is.na(dsd2$serum_k),]) # 2906
Q <- rep(NA,nrow(dsd2[is.na(dsd2$serum_k),]))
serum_k <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$serum_k),])))
for (i in 1:nrow(dsd2[is.na(dsd2$serum_k),])) {
  Q[i] <- dsd2[is.na(dsd2$serum_k),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "serum_k"]) == FALSE))
  serum_k$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                            "serum_k"][a]
}
length(which(is.na(serum_k$final) == TRUE)) #[1] 411
dsd2[is.na(dsd2$serum_k ),"serum_k"] <-  serum_k $final
nrow(dsd2[is.na(dsd2$serum_k ),]) #[1] 411


####  find the nearest data (creatinine) 
nrow(dsd2[is.na(dsd2$creatinine),]) # 2965
Q <- rep(NA,nrow(dsd2[is.na(dsd2$creatinine),]))
creatinine <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$creatinine),])))
for (i in 1:nrow(dsd2[is.na(dsd2$creatinine),])) {
  Q[i] <- dsd2[is.na(dsd2$creatinine),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "creatinine"]) == FALSE))
  creatinine$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                               "creatinine"][a]
}
length(which(is.na(creatinine$final) == TRUE)) #[1] 559
dsd2[is.na(dsd2$creatinine),"creatinine"] <-  creatinine$final
nrow(dsd2[is.na(dsd2$creatinine),]) #[1] 559


####  find the nearest data (dialysis_flag) 
nrow(dsd2[is.na(dsd2$dialysis_flag),]) # 3861
Q <- rep(NA,nrow(dsd2[is.na(dsd2$dialysis_flag),]))
dialysis_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$dialysis_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$dialysis_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$dialysis_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "dialysis_flag"]) == FALSE))
  dialysis_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                  "dialysis_flag"][a]
}
length(which(is.na(dialysis_flag$final) == TRUE)) #[1] 1220
dsd2[is.na(dsd2$dialysis_flag ),"dialysis_flag"] <-  dialysis_flag $final
nrow(dsd2[is.na(dsd2$dialysis_flag ),]) #[1] 1220

####  find the nearest data (chd_flag) 
nrow(dsd2[is.na(dsd2$chd_flag),]) # 299
Q <- rep(NA,nrow(dsd2[is.na(dsd2$chd_flag),]))
chd_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$chd_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$chd_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$chd_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "chd_flag"]) == FALSE))
  chd_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "chd_flag"][a]
}
length(which(is.na(chd_flag$final) == TRUE)) #[1]  60
dsd2[is.na(dsd2$chd_flag ),"chd_flag"] <-  chd_flag $final
nrow(dsd2[is.na(dsd2$chd_flag ),]) #[1]  60


####  find the nearest data (stroke_flag) 
nrow(dsd2[is.na(dsd2$stroke_flag),]) # 299
Q <- rep(NA,nrow(dsd2[is.na(dsd2$stroke_flag),]))
stroke_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$stroke_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$stroke_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$stroke_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "stroke_flag"]) == FALSE))
  stroke_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                "stroke_flag"][a]
}
length(which(is.na(stroke_flag$final) == TRUE)) #[1] 60
dsd2[is.na(dsd2$stroke_flag ),"stroke_flag"] <-  stroke_flag $final
nrow(dsd2[is.na(dsd2$stroke_flag ),]) #[1] 60


####  find the nearest data (pad_flag) 
nrow(dsd2[is.na(dsd2$pad_flag),]) # 23419
Q <- rep(NA,nrow(dsd2[is.na(dsd2$pad_flag),]))
pad_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$pad_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$pad_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$pad_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "pad_flag"]) == FALSE))
  pad_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "pad_flag"][a]
}
length(which(is.na(pad_flag$final) == TRUE)) #[1] 11660
dsd2[is.na(dsd2$pad_flag ),"pad_flag"] <-  pad_flag $final
nrow(dsd2[is.na(dsd2$pad_flag ),]) #[1] 11660


####  find the nearest data (ckd_cd) 
nrow(dsd2[is.na(dsd2$ckd_cd),]) # 23419
Q <- rep(NA,nrow(dsd2[is.na(dsd2$ckd_cd),]))
ckd_cd <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$ckd_cd),])))
for (i in 1:nrow(dsd2[is.na(dsd2$ckd_cd),])) {
  Q[i] <- dsd2[is.na(dsd2$ckd_cd),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "ckd_cd"]) == FALSE))
  ckd_cd$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                           "ckd_cd"][a]
}
length(which(is.na(ckd_cd$final) == TRUE)) #[1] 11660
dsd2[is.na(dsd2$ckd_cd ),"ckd_cd"] <-  ckd_cd $final
nrow(dsd2[is.na(dsd2$ckd_cd ),]) #[1] 11660


####  find the nearest data (central_obesity_flag) 
nrow(dsd2[is.na(dsd2$central_obesity_flag),]) # 299
Q <- rep(NA,nrow(dsd2[is.na(dsd2$central_obesity_flag),]))
central_obesity_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$central_obesity_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$central_obesity_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$central_obesity_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "central_obesity_flag"]) == FALSE))
  central_obesity_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                         "central_obesity_flag"][a]
}
length(which(is.na(central_obesity_flag$final) == TRUE)) #[1] 60
dsd2[is.na(dsd2$central_obesity_flag ),"central_obesity_flag"] <-  central_obesity_flag $final
nrow(dsd2[is.na(dsd2$central_obesity_flag ),]) #[1] 60


####  find the nearest data (anti_diabetic_drug_flag) 
nrow(dsd2[is.na(dsd2$anti_diabetic_drug_flag),]) # 25179
Q <- rep(NA,nrow(dsd2[is.na(dsd2$anti_diabetic_drug_flag),]))
anti_diabetic_drug_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$anti_diabetic_drug_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$anti_diabetic_drug_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$anti_diabetic_drug_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "anti_diabetic_drug_flag"]) == FALSE))
  anti_diabetic_drug_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                                            "anti_diabetic_drug_flag"][a]
}
length(which(is.na(anti_diabetic_drug_flag$final) == TRUE)) #[1] 11972
dsd2[is.na(dsd2$anti_diabetic_drug_flag ),"anti_diabetic_drug_flag"] <- anti_diabetic_drug_flag $final
nrow(dsd2[is.na(dsd2$anti_diabetic_drug_flag ),]) #[1] 11972


####  find the nearest data (oha_flag) 
nrow(dsd2[is.na(dsd2$oha_flag),]) # 35187
Q <- rep(NA,nrow(dsd2[is.na(dsd2$oha_flag),]))
oha_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$oha_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$oha_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$oha_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "oha_flag"]) == FALSE))
  oha_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "oha_flag"][a]
}
length(which(is.na(oha_flag$final) == TRUE)) #[1] 21033
dsd2[is.na(dsd2$oha_flag ),"oha_flag"] <- oha_flag $final
nrow(dsd2[is.na(dsd2$oha_flag ),]) #[1] 21033


####  find the nearest data (su_flag) 
nrow(dsd2[is.na(dsd2$su_flag),]) # 34217
Q <- rep(NA,nrow(dsd2[is.na(dsd2$su_flag),]))
su_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$su_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$su_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$su_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "su_flag"]) == FALSE))
  su_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                            "su_flag"][a]
}
length(which(is.na(su_flag$final) == TRUE)) #[1] 20053
dsd2[is.na(dsd2$su_flag ),"su_flag"] <- su_flag $final
nrow(dsd2[is.na(dsd2$su_flag ),]) #[1] 20053


####  find the nearest data (metf_flag) 
nrow(dsd2[is.na(dsd2$metf_flag),]) # 34085
Q <- rep(NA,nrow(dsd2[is.na(dsd2$metf_flag),]))
metf_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$metf_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$metf_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$metf_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "metf_flag"]) == FALSE))
  metf_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                              "metf_flag"][a]
}
length(which(is.na(metf_flag$final) == TRUE)) #[1] 19924
dsd2[is.na(dsd2$metf_flag ),"metf_flag"] <- metf_flag $final
nrow(dsd2[is.na(dsd2$metf_flag ),]) #[1] 19924


####  find the nearest data (gluco_flag) 
nrow(dsd2[is.na(dsd2$gluco_flag),]) # 35385
Q <- rep(NA,nrow(dsd2[is.na(dsd2$gluco_flag),]))
gluco_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$gluco_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$gluco_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$gluco_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "gluco_flag"]) == FALSE))
  gluco_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                               "gluco_flag"][a]
}
length(which(is.na(gluco_flag$final) == TRUE)) #[1] 21282
dsd2[is.na(dsd2$gluco_flag ),"gluco_flag"] <- gluco_flag $final
nrow(dsd2[is.na(dsd2$gluco_flag ),]) #[1] 21282


####  find the nearest data (glita_flag) 
nrow(dsd2[is.na(dsd2$glita_flag),]) # 35387
Q <- rep(NA,nrow(dsd2[is.na(dsd2$glita_flag),]))
glita_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$glita_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$glita_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$glita_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "glita_flag"]) == FALSE))
  glita_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                               "glita_flag"][a]
}
length(which(is.na(glita_flag$final) == TRUE)) #[1] 21277
dsd2[is.na(dsd2$glita_flag ),"glita_flag"] <- glita_flag $final
nrow(dsd2[is.na(dsd2$glita_flag ),]) #[1] 21277


####  find the nearest data (megl_flag) 
nrow(dsd2[is.na(dsd2$megl_flag),]) # 35412
Q <- rep(NA,nrow(dsd2[is.na(dsd2$megl_flag),]))
megl_flag <- data.frame(final = rep(NA,nrow(dsd2[is.na(dsd2$megl_flag),])))
for (i in 1:nrow(dsd2[is.na(dsd2$megl_flag),])) {
  Q[i] <- dsd2[is.na(dsd2$megl_flag),"pseudo_patient_key"][i]
  a <- min(which(is.na(d126m[which(d126m$pseudo_patient_key == Q[i]),
                             "megl_flag"]) == FALSE))
  megl_flag$final[i] <- d126m[which(d126m$pseudo_patient_key == Q[i]),
                              "megl_flag"][a]
}
length(which(is.na(megl_flag$final) == TRUE)) #[1] 21312
dsd2[is.na(dsd2$megl_flag ),"megl_flag"] <- megl_flag $final
nrow(dsd2[is.na(dsd2$megl_flag ),]) #[1] 21312


for (i in 26:32) {
  dsd2[is.na(dsd2[,i]),i] <- ""
}
dsd2$diabetic_drug <- ifelse(dsd2$anti_diabetic_drug_flag == "Y" |
                               dsd2$oha_flag == "Y" |
                               dsd2$su_flag =="Y" |
                               dsd2$megl_flag == "Y" |
                               dsd2$gluco_flag == "Y" |
                               dsd2$glita_flag == "Y" |
                               dsd2$megl_flag == "Y",1,0)
# dsd2[1:10,c(26:32,40)] # check
table(dsd2$diabetic_drug,useNA = "always")
colnames(dsd2)
write.csv(dsd2,"C:/Users/dclapp159_02/Documents/earliestdata-replaced-126m_0202.csv",row.names = FALSE)

 

#################################
### Select the latest records ###
#################################
d126m.1 <- d126m
d126m.1$assessment_dtm <- as.Date(d126m$assessment_dtm)
d126m.2 <- d126m[order(d126m.1$pseudo_patient_key,d126m.1$assessment_dtm,decreasing = TRUE),]
d126m.ld <- aggregate(x = d126m.2, by = list(d126m.2$pseudo_patient_key),
                      FUN = function(x) x[1])
nrow(d126m.ld) #[1] 54910

dld <- d126m.ld[,c(aaa)]
colnames(dld)
for (i in 26:32) {
  dld[is.na(dld[,i]),i] <- ""
}
dld$diabetic_drug <- ifelse(dld$anti_diabetic_drug_flag == "Y" |
                              dld$oha_flag == "Y" |
                              dld$su_flag =="Y" |
                              dld$megl_flag == "Y" |
                              dld$gluco_flag == "Y" |
                              dld$glita_flag == "Y" |
                              dld$megl_flag == "Y",1,0)

table(dld$diabetic_drug,useNA = "always")
colnames(dld)
write.csv(dld,"C:/Users/dclapp159_02/Documents/latestdata-126m_0202.csv",row.names = FALSE)

