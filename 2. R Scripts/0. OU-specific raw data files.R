### GOAL OF THIS SCRIPT IS TO CREATE OU-SPECIFIC RAW DATAFILES FOR FY22 PRE-CLEAN RAW DATASET

# Set working library
setwd('/Users/kborces/Documents/HRH Inventory/FY22 Analysis')

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
HRH_data_orig <- read_excel("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20230118_PostClean_Adjusted.xlsx") %>%
  select(-missingNGA_flags)


# Create list of operating units that we want QC reports for
OU_list <- HRH_data_orig %>%
  distinct(operating_unit) %>%
  pull(operating_unit)

# Loop through each OU to create a dataset for each OU
for (i in 1:length(OU_list)) {
  
  OU <- OU_list[i] # loop through each OU in the OU list
  
  cleanHRH <- HRH_data_orig %>%
    filter(fiscal_year == max(fiscal_year)) %>% # Filter for latest year
    filter(operating_unit == OU) 
  
  # Import the excel reporting template to be used
  wb <- loadWorkbook("./1. Data/HRH_OU_raw_data_template.xlsx")
  
  # Load the data frames into each Excel sheet as needed
  writeData(wb, sheet = 1, cleanHRH, startCol = 1, startRow = 1, colNames = TRUE)

  # Establish the workbook name based on the OU
  wbName <- paste0("./4. Outputs/OU raw datasets/", OU, "_FY22_Unredacted_HRH_Post_Clean.xlsx")
  
  # Export each QC report in Excel
  saveWorkbook(wb, wbName, overwrite = TRUE) 
  
  # print progress
  print(paste0(OU, " - workbook complete"))
  
}






