# Survival-Analysis-Clinical

## R
Before setting up the package, users should have version 4.2.1 or higher, and several packages set up from CRAN.


### Data integration and cleaning
**File:** `Data preparation.R`     
This script focuses on integrating various datasets relevant to survival analysis. It involves cleaning the data by handling missing values, removing duplicates, and standardizing formats to ensure consistency across the dataset. The goal is to prepare a high-quality dataset suitable for subsequent analysis. The files named `Demo_data_xx` were the clean version of the target data.

### Median survival time calculation 
**File:** `Data processing.R`         
This part of the analysis calculates the median survival time for the patient cohort. It involves using statistical methods to analyze the survival data and determine the median time until the event of interest (e.g., death or disease progression). This calculation is crucial for understanding the overall prognosis of the study population. The file named `Demo_data` was used to calculate the median survival time.

### Risk scoring system construction
**File:** `Data processing2.R`   
This script develops a risk-scoring system based on clinical and demographic factors. It uses statistical techniques to identify significant predictors of survival and assigns scores to each factor. The resulting risk scores can help stratify patients based on their likelihood of survival, aiding in clinical decision-making. The files named `Demo_data_train` and `Demo_data_test` were used to construct the risk score system.

---
## Python

### Survival model development and comparison
**File:** `Model evaluation`          
 In this section, various survival models are developed and compared using Python. The focus is on evaluating different statistical techniques to find the best-fit model for predicting survival outcomes. This involves assessing model performance through metrics like concordance index and visualizations such as survival curves. The file `Demo_data` was used to develop and compare the survival model.
