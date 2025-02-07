library(survminer)
library(survex)
library(AutoScore)
library(knitr)

# read file ---------------------------------------------------------------
rm(list = ls())
data <- read.csv("file_path/file_name.csv")

# conversion of factor variables
factor_vars <- c("categorical_variables1", "categorical_variables2", "categorical_variables3")
data[factor_vars] <- lapply(data[factor_vars], as.factor)

# rename survival variables
colnames(data)[1:2] <- c("label_time", "label_status")

# Create categorical variables
data <- within(data, {
  continue_variables1 <- cut(continue_variables1, breaks = c(-Inf, 23.9, Inf), labels = c(0,1))
  continue_variables2 <- cut(continue_variables2, breaks = c(-Inf, 7, Inf), labels = c(0,1))
  continue_variables3 <- cut(continue_variables3, breaks = c(-Inf, 7, Inf), labels = c(0,1))
  continue_variables4 <- cut(continue_variables4, 
                  breaks = c(-Inf, 50, 75, Inf),
                  labels = c("<=50", "50-75", ">=75"))
})

analysis_vars <- c("variables1", "variables2", "variables3",...,"variablesX")

surv_results <- lapply(analysis_vars, function(var){
  formula <- as.formula(paste("Surv(label_time, label_status) ~", var))
  fit <- survfit(formula, data = data)
  med <- surv_median(fit)
  cbind(variable = var, med)
})

survtime1 <- do.call(rbind, surv_results)
write.csv(survtime1, "file_path/survtime_prostate.csv")

####



