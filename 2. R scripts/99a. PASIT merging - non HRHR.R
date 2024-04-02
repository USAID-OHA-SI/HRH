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
#    filter(sub_program_area == "ASP: Human resources for health" | HRH_keywordflags == "TRUE") %>%
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

# Remove any empty rows from the templates
FAST_df <- FAST_df %>%
  filter(!is.na(funding_agency) & !is.na(mechanism_name) & !is.na(mechanism_id) & !is.na(prime_partner))

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
#    filter(sub_program_area == "ASP: Human resources for health" | HRH_keywordflags == "TRUE") %>%
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

# Remove any empty rows from the templates
PASIT_df <- PASIT_df %>%
  filter(!is.na(funding_agency) & !is.na(mechanism_name) & !is.na(mechanism_id) & !is.na(prime_partner))

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
write_xlsx(merged,"./4. Outputs/Combined_GlobalPASIT_COP23.xlsx") 
