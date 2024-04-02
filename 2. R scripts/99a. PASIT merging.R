### PURPOSE: TO MERGE ALL THE PASIT DATASETS WITH HRH DATA TOGETHER

# Code developed by: Kyle Borces
# Last updated: July 2023

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(openxlsx)

# Compile all FAST files
workbookPathways_FAST <- c(
  "./1. Data/PASIT/Botswana_COP23_FAST_FINAL (1).xlsx",
  "./1. Data/PASIT/Brazil_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Burundi_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Caribbean_Region_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Central_America_Region_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Cote_d_Ivoire_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Democratic_Republic_of_the_Congo_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Dominican_Republic_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Eswatini_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Ethiopia_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Haiti_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Kenya_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Lesotho_COP23_FAST_Final.xlsx",
  "./1. Data/PASIT/Malawi_COP23_FAST_FINAL.xlsx",
  "./1. Data/PASIT/Mozambique_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Namibia_COP23_FAST_Final.xlsx",
  "./1. Data/PASIT/Nigeria_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Rwanda_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/SouthAfrica_COP23_FAST_V5_FINAL.xlsx",
  "./1. Data/PASIT/SouthSudan_COP23_FAST_V5.xlsx",
  "./1. Data/PASIT/Ukraine_COP23_FAST_FINAL.xlsx",
  "./1. Data/PASIT/Vietnam_COP23_FAST_V5_Final.xlsx",
  "./1. Data/PASIT/Zambia_COP23_FAST_V4.xlsx",
  "./1. Data/PASIT/Zimbabwe_COP23_FAST_V4.xlsx"
)

# Compile all individual PASIT files
workbookPathways <- c(
  "./1. Data/PASIT/Angola COP23 PASIT.xlsx",
  "./1. Data/PASIT/Asia Region COP23 PASIT.xlsx",
  "./1. Data/PASIT/Benin COP23 PASIT.xlsx",
  "./1. Data/PASIT/Burkina Faso COP23 PASIT.xlsx",
  "./1. Data/PASIT/Burma COP23 PASIT.xlsx",
  "./1. Data/PASIT/Cambodia COP23 PASIT.xlsx",
  "./1. Data/PASIT/Cameroon COP23 PASIT.xlsx",
  "./1. Data/PASIT/Ghana COP23 PASIT.xlsx",
  "./1. Data/PASIT/India COP23 PASIT.xlsx",
  "./1. Data/PASIT/Indonesia COP23 PASIT.xlsx",
  "./1. Data/PASIT/Kazakhstan COP23 PASIT.xlsx",
  "./1. Data/PASIT/Kyrgyzstan COP23 PASIT.xlsx",
  "./1. Data/PASIT/Laos COP23 PASIT.xlsx",
  "./1. Data/PASIT/Liberia COP23 PASIT.xlsx",
  "./1. Data/PASIT/Mali COP23 PASIT.xlsx",
  "./1. Data/PASIT/Nepal COP23 PASIT.xlsx",
  "./1. Data/PASIT/Papua New Guinea COP23 PASIT.xlsx",
  "./1. Data/PASIT/Philippines COP23 PASIT.xlsx",
  "./1. Data/PASIT/Senegal COP23 PASIT.xlsx",
  "./1. Data/PASIT/Sierra Leone COP23 PASIT.xlsx",
  "./1. Data/PASIT/Tajikistan COP23 PASIT.xlsx",
  "./1. Data/PASIT/Tanzania COP23 PASIT.xlsx",
  "./1. Data/PASIT/Thailand COP23 PASIT.xlsx",
  "./1. Data/PASIT/Togo COP23 PASIT.xlsx",
  "./1. Data/PASIT/Uganda COP23 PASIT.xlsx",
  "./1. Data/PASIT/West Africa Region COP23 PASIT.xlsx"
)


### FAST FILES ####

# Create a function that cleans up each FAST file
updateMaster <- function(newDatPath) { 
  
  # Import the PASIT tab only
  FAST_data <- read_excel(newDatPath, sheet = 16, skip = 3) 
  tidyFAST_data <- FAST_data %>%
    clean_names() %>%
    select(pasit_id:message) %>%
    mutate(HRH_keywordflags = if_else(grepl("HRIS", short_activity_description) == TRUE |
                                      grepl("HRH", short_activity_description) == TRUE |
                                      grepl("Health worker", short_activity_description) == TRUE |
                                      grepl("Cadre", short_activity_description) == TRUE |
                                      grepl("Staff", short_activity_description) == TRUE |
                                      grepl("Human resources", short_activity_description) == TRUE, "TRUE", "FALSE")) %>%
    filter(sub_program_area == "ASP: Human resources for health" | HRH_keywordflags == "TRUE") %>%
    select(-final_year_2_funding) %>%
    mutate(file_name = newDatPath)
  
  # Return the cleaned up PASIT tab
  return(tidyFAST_data)
  
}

FASTlist = list()

# Create the for loop that loops through each file pathway of the PASIT datasets
for (i in 1 : length(workbookPathways_FAST)) {
  
  # run the updateMaster function and save each iteration in the list
  FASTlist[[i]] <- updateMaster(workbookPathways_FAST[i])
  
  # notify that this has been accomplished
  print (paste("FAST dataset update ", i, " of ", length(workbookPathways_FAST), "complete: added ", 
               workbookPathways_FAST[i]))
  
  # update tracker to move to the next file pathway or stop
  i <- i + 1
}

FAST_df = do.call(rbind, FASTlist)

#### PASIT FILES #####

# Create a function that cleans up each FAST file
updateMaster <- function(newDatPath) { 
  
  # Import the PASIT tab only
  PASIT_data <- read_excel(newDatPath, sheet = 1, skip = 3) 
  tidyPASIT_data <- PASIT_data %>%
    clean_names() %>%
    select(pasit_id:message) %>%
    mutate(HRH_keywordflags = if_else(grepl("HRIS", short_activity_description) == TRUE |
                                        grepl("HRH", short_activity_description) == TRUE |
                                        grepl("Health worker", short_activity_description) == TRUE |
                                        grepl("Cadre", short_activity_description) == TRUE |
                                        grepl("Staff", short_activity_description) == TRUE |
                                        grepl("Human resources", short_activity_description) == TRUE, "TRUE", "FALSE")) %>%
    filter(sub_program_area == "ASP: Human resources for health" | HRH_keywordflags == "TRUE") %>%
    mutate(file_name = newDatPath)
  
  # Return the cleaned up PASIT tab
  return(tidyPASIT_data)
  
}

PASITlist = list()

# Create the for loop that loops through each file pathway of the PASIT datasets
for (i in 1 : length(workbookPathways)) {
  
  # run the updateMaster function and save each iteration in the list
  PASITlist[[i]] <- updateMaster(workbookPathways[i])
  
  # notify that this has been accomplished
  print (paste("PASIT dataset update ", i, " of ", length(workbookPathways), "complete: added ", 
               workbookPathways[i]))
  
  # update tracker to move to the next file pathway or stop
  i <- i + 1
}

PASIT_df = do.call(rbind, PASITlist)

# Combine both the FAST and PASIT data frames
merged <- rbind(FAST_df, PASIT_df) %>%
  select(-pasit_id) %>%
  select(file_name, funding_agency:(ncol(FAST_df)-1))

# Clean up the country names by keeping only the first word
merged$file_name <- gsub("./1. Data/PASIT/", "", merged$file_name)
merged <- separate(merged, col = file_name, into = c('Country', 'Misc1'), sep = "_")
merged <- separate(merged, col = Country, into = c('OU', 'Misc2'), sep = " ")
merged = subset(merged, select = -c(Misc1, Misc2)) # Drop misc columns

# Rename some country names
merged <- merged %>%
  mutate(OU = case_when(OU == "Caribbean" ~ "Caribbean Region", 
                      OU == "Central" ~ "Central America Region",
                      OU == "Cote" ~ "Cote d'Ivoire",
                      OU == "Dominican" ~ "Dominican Republic",
                      OU == "SouthAfrica" ~ "South Africa",
                      OU == "Asia" ~ "Asia Region",
                      OU == "Papua" ~ "Papua New Guinea", 
                      OU == "SouthSudan" ~ "South Sudan",
                      TRUE ~ OU)) %>%
  arrange(OU) %>%
  select(-HRH_keywordflags)

# Select only columns we want
merged <- merged %>%
  select(OU, funding_agency, mechanism_name, mechanism_id, prime_partner, sub_program_area, activity_category, 
         cop_23_beneficiary, status_of_activity, activity_implementation_start, unique_activity_title_optional, 
         short_activity_description, gap_activity_will_address, activity_budget, remaining_budget, 
         measurable_interim_output_by_end_of_fy24, measurable_interim_output_by_end_of_fy25, 
         budget_continuation_for_year_2, actual_year_2_budget, remaining_year_2_budget, 
         measurable_expected_outcome_from_activity, nature_of_health_system_investment, length_of_pepfar_investment_in_gap, 
         location_of_investment, notes)

# Export as excel file
write_xlsx(merged,"./4. Outputs/Combined_PASIT_HRHonly_COP23.xlsx") 

### -------- Creating the Summary Tables ---------------


# Import the cleaned up FY22 HRH inventory dataset since we need to create OU-level summaries
HSD22 <- read_excel("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20230118_PostClean_Adjusted.xlsx") 

# Create a summary table of PASIT budgets by OU (HCW: Ancillary, HCW: Clinical, Other Staff, Program Management, Total)
HSD_ERcategory <- HSD22 %>%
  filter(fiscal_year == 2022) %>%
  filter(GHSC_UN_keywordflags == "FALSE") %>%
  group_by(operating_unit, er_category) %>%
  summarise(actual_annual_spend = sum(actual_annual_spend, na.rm = T)) %>%
  spread(er_category, actual_annual_spend) %>%
  adorn_totals(where = "col")

# Create a summary table of SD vs NSD
HSD_SDvsNSD <- HSD22 %>%
  filter(fiscal_year == 2022) %>%
  filter(GHSC_UN_keywordflags == "FALSE") %>%
  group_by(operating_unit, interaction_type) %>%
  summarise(FY22_HRH_Inventory = sum(actual_annual_spend, na.rm = T)) %>%
  spread(interaction_type, FY22_HRH_Inventory)

# Merge the two tables
HRH22_merged <- left_join(HSD_ERcategory, HSD_SDvsNSD, by = "operating_unit")

#####

## Do the same thing, but for USAID only
    HSD_ERcategory <- HSD22 %>%
      filter(fiscal_year == 2022) %>%
      filter(GHSC_UN_keywordflags == "FALSE") %>%
      filter(funding_agency == "USAID") %>%
      group_by(operating_unit, er_category) %>%
      summarise(actual_annual_spend = sum(actual_annual_spend, na.rm = T)) %>%
      spread(er_category, actual_annual_spend) %>%
      adorn_totals(where = "col")

    # Create a summary table of SD vs NSD
    HSD_SDvsNSD <- HSD22 %>%
      filter(fiscal_year == 2022) %>%
      filter(GHSC_UN_keywordflags == "FALSE") %>%
      filter(funding_agency == "USAID") %>%
      group_by(operating_unit, interaction_type) %>%
      summarise(FY22_HRH_Inventory = sum(actual_annual_spend, na.rm = T)) %>%
      spread(interaction_type, FY22_HRH_Inventory)
    
    # Merge the two tables
    HRH22_merged_USAID <- left_join(HSD_ERcategory, HSD_SDvsNSD, by = "operating_unit")

#####    
    
# Create a summary table of PASIT HRH Budgets by OU - All agencies
PASIT23_merged <- merged %>%
  group_by(OU) %>%
  summarise(PASIT_FY23_HRHinvestments = sum(activity_budget, na.rm = T)) %>%
  rename(operating_unit = OU)

## USAID only - Create a summary table of PASIT HRH Budgets by OU - All agencies
      PASIT23_merged_USAID <- merged %>%
        filter(funding_agency == "USAID") %>%
        group_by(OU) %>%
        summarise(PASIT_FY23_HRHinvestments = sum(activity_budget, na.rm = T)) %>%
        rename(operating_unit = OU)
      
#####

# Merge the HRH Inventory and PASIT datasets, all agencies and USAID versions
HRH_PASIT_summary <- merge(HRH22_merged, PASIT23_merged, by = "operating_unit", all = TRUE)
HRH_PASIT_summary_USAID <- merge(HRH22_merged_USAID, PASIT23_merged_USAID, by = "operating_unit", all = TRUE)
  
# Import the excel reporting template to be used
wb <- loadWorkbook("./1. Data/PASIT_shell.xlsx")

# Load the data frames into each Excel sheet as needed
writeData(wb, sheet = 1, HRH_PASIT_summary, startCol = 2, startRow = 3, colNames = TRUE)
writeData(wb, sheet = 2, HRH_PASIT_summary_USAID, startCol = 2, startRow = 3, colNames = TRUE)
writeData(wb, sheet = 3, merged, startCol = 2, startRow = 3, colNames = TRUE)

# Establish the workbook name based on the OU
wbName <- paste0("./4. Outputs/PASIT_Review_20220807.xlsx")

# Export each QC report in Excel
saveWorkbook(wb, wbName, overwrite = TRUE) 

