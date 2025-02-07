# Load necessary libraries
library(dplyr)
library(lubridate)

# Function to load and clean data
load_and_clean_data <- function(file_path) {
  data <- read.table(gzfile(file_path, "rt"), header = TRUE, sep = "\01", quote = "", comment.char = "#")
  data[data == ""] <- NA  # Replace empty strings with NA
  return(data)
}

# Load data
D01 <- load_and_clean_data("file_path/D01.csv.gz")
D02 <- load_and_clean_data("file_path/D02.csv.gz")
D06 <- load_and_clean_data("file_path/D06.csv.gz")
Map <- load_and_clean_data("file_path/map.csv.gz")

# Select the first diagnosis record
process_diagnosis <- function(df) {
  df %>%
    mutate(diagnosis = ifelse(is.na(diagnosis), 2, diagnosis)) %>%
    arrange(patient_id, diagnosis, time1) %>%
    group_by(patient_id) %>%
    slice(1) %>%
    ungroup()
}
D06_1 <- process_diagnosis(D06)

# merge data
merged_data <- D02 %>%
  inner_join(D06_1[, c("patient_id", "variable1", "variable2")], 
            by = "patient_id") %>%
  inner_join(D01[, c("patient_id", "variable3", "variable4")], 
            by = "patient_id")

# calculate time 
calculate_time_vars <- function(df) {
  df %>%
    mutate(
      date1 = as.Date(date1),
      date2 = as.Date(date2),
      time1 = as.numeric(time1),
      time2 = as.numeric(time2),
      Age = round(as.numeric(difftime(date2, date1, units = "days"))/365, 1),
      age_group = cut(diagnosis_age, breaks = c(0, 50, 75, Inf), 
                     labels = c("≤50", "50-75", "≥75")),
      assessment_time = abs(time1 - time2)     
    ) %>%
  arrange(patient_id, assessment_time)
}

final_df <- calculate_time_vars(merged_data)

# merge data
merged_data2 <- final_df %>%
  inner_join(Map[, c("patient_id", "variable1", "variable2")], 
            by = "patient_id") 

# Sort the ICD-10 code and combine cancer
sort(table(merged_data2[,icdcode]),decreasing = TRUE)

merged_data2$cancer_type[merged_data2$icdcode == "Cxx"] <- "Cancer of xx"  # other ICD codes are the same

# Extract the first record of each patient (baseline time point)
merged_data3 <- aggregate(x = merged_data2, 
                      by = list(merged_data2$patient_id),
                      FUN = function(x) x[which.min(x$visit_date)])  

# Define a function to find the nearest non-missing value based on the time difference
fill_nearest_by_time <- function(main_df, 
                                 full_df, 
                                 id_col, 
                                 time_col, 
                                 target_col) {
  # Get the records that need to be filled
  na_records <- which(is.na(main_df[[target_col]]))
  cat("Number of NA in", target_col, ":", length(na_records), "\n")
  
  # Iterate over each missing value
  for (i in na_records) {
    patient_id <- main_df[[id_col]][i]
    baseline_time <- main_df[[time_col]][i]
    
    # Filter the records that match the criteria in the full data
    candidate_records <- full_df[
      full_df[[id_col]] == patient_id & 
      !is.na(full_df[[target_col]]), 
      c(time_col, target_col)
    ]
    
    # If a candidate record is found
    if (nrow(candidate_records) > 0) {
      # Calculate the absolute value of the time difference
      time_diff <- abs(as.numeric(difftime(
        candidate_records[[time_col]],
        baseline_time,
        units = "days"
      )))
      
      # Find the minimum time difference record
      nearest_record <- candidate_records[which.min(time_diff), ]
      
      # Fill in the data
      main_df[[target_col]][i] <- nearest_record[[target_col]]
    }
  }
  
  cat("Remaining NA after filling:", sum(is.na(main_df[[target_col]])), "\n")
  return(main_df)
}

# example
example <- fill_nearest_by_time(
  main_df = merged_data3, 
  full_df = merged_data2,
  id_col = "patient_id",
  time_col = "visit_date", 
  target_col = "variable1"
)

# Integrate drug usage
data$diabetic_drug <- ifelse(data$drug1 == "Y" |
                               data$drug2 == "Y" |
                               data$drug3 =="Y" |
                               data$drug4 == "Y" |
                               data$drug5 == "Y" |
                               data$drug6 == "Y" |
                               data$drug7 == "Y",1,0)

# out put file
write.csv(data,"file_path/file_name.csv",row.names = FALSE)

 

