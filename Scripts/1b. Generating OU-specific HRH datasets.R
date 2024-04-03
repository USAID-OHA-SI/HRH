### GOAL OF THIS SCRIPT IS TO CREATE OU-SPECIFIC RAW DATAFILES FOR FY23 RAW DATASET


library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(openxlsx)
library(tibble)

# Turn off warnings
options(dplyr.summarise.inform = FALSE)

# Import the raw HRH dataset
load(file = "./4. Outputs/RDS/FY23_cleanHRH.rds")

# Create list of operating units that we want QC reports for
OU_list <- HRH_clean %>%
  distinct(operating_unit) %>%
  pull(operating_unit)

# Remove any variables we don't need
HRH_clean <- HRH_clean %>%
  select(-DREAMS_budget_23, -DREAMS_tagged_23, -DREAMS_AGYWs_23)

# Loop through each OU to create a dataset for each OU
for (i in 1:length(OU_list)) {
  
  OU <- OU_list[i] # loop through each OU in the OU list
  
  cleanHRH <- HRH_clean %>%
    filter(fiscal_year == max(fiscal_year)) %>% # Filter for latest year
    filter(operating_unit == OU) 
  
  # Import the excel reporting template to be used
  wb <- loadWorkbook("./1. Data/HRH_OU_raw_data_template.xlsx")
  
  # Load the data frames into each Excel sheet as needed
  writeData(wb, sheet = 1, cleanHRH, startCol = 1, startRow = 1, colNames = TRUE)

  # Establish the workbook name based on the OU
  wbName <- paste0("./4. Outputs/OU raw datasets/Post clean/", OU, "_FY23_Unredacted_HRH_Post_Clean.xlsx")
  
  # Export each QC report in Excel
  saveWorkbook(wb, wbName, overwrite = TRUE) 
  
  # print progress
  print(paste0(OU, " - workbook complete"))
  
}






