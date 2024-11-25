### PURPOSE: OU-SPECIFIC RAW DATAFILES FOR FY24 RAW DATASET
### COde developed by: Kyle Borces

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
load(file = "./4. Outputs/RDS/FY24_cleanHRH.rds")

# Create list of operating units that we want QC reports for
OU_list <- HRH_clean %>%
  distinct(operating_unit) %>%
  pull(operating_unit)

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
  wbName <- paste0("./4. Outputs/OU raw datasets/Pre clean/", OU, "_FY24_Unredacted_HRH_Pre_Clean_20241125.xlsx")
  
  # Export each QC report in Excel
  saveWorkbook(wb, wbName, overwrite = TRUE) 
  
  # print progress
  print(paste0(OU, " - workbook complete"))
  
}





