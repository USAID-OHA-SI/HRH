### THE PURPOSE OF THIS SCRIPT IS TO DO SOME DATA CLEANING OF THE 2023 RAW HRH DATASET

## 1.installing and loading relevant packages/libraries
library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)

## Import the 2021, 2022, and HRH dataset as .txt file
HRH_data_orig_24 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY24_not_redacted_20241115.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_23 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY23_not_redacted_20241115.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_22 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY22_not_redacted_20241115.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_21 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY21_not_redacted_20241115.txt", header = TRUE, stringsAsFactors = FALSE)

#HRH_Structured_Datasets_Site_IM_FY21_not_redacted_20241115

## EDIT: import also the CDC CoAg list
CoAg <- read_excel("./1. Data/FY24 PEPFAR G2G Mechanisms.xlsx")

## Row bind all three years
HRH_clean <- rbind(HRH_data_orig_24, HRH_data_orig_23, HRH_data_orig_22, HRH_data_orig_21)

# If the operating unit is regional, psnuuid = snu1uid
HRH_clean <- HRH_clean %>%
  mutate(psnuuid = case_when(operating_unit == "Asia Region" | 
                             operating_unit == "West Africa Region" | 
                             operating_unit == "Western Hemisphere Region" |
                             operating_unit == "Caribbean Region" |
                             operating_unit == "Central Asia Region" |
                             operating_unit == "Central and South America Region" |
                             operating_unit == "Pacific Region" |
                             operating_unit == "South and Southeast Asia Region" |
                             operating_unit == "West Africa Region 1" |
                             operating_unit == "West Africa Region 2" 
                                  ~ snu1uid,
                             TRUE ~ psnuuid))

# Rename columns to match FY21 column names so that it automatically updates in Tableau
HRH_clean <- HRH_clean %>%
  rename(annual_expenditure = actual_salary_expenditure,
         annual_fringe = actual_fringe_expenditure,
         is_multi_site = roving,
         is_indigenous_prime_partner = local_prime_partner,
         is_outside_ou = position_based)
          
# Rename the SD vs NSD column to be consistent 
HRH_clean <- HRH_clean %>%
  mutate(interaction_type = case_when(interaction_type == "Non Service Delivery" ~ "Non-Service Delivery",
                                      interaction_type == "Direct Service Delivery" ~ "Direct Service Delivery",
                                      TRUE ~ interaction_type))

# Rename IP program management to program management
HRH_clean <- HRH_clean %>%
  mutate(er_category = if_else(er_category == "Implementing Mechanism Program Management Staff", "Program Management", er_category))

# Add a keyword flag if staff belong to UN-related mechanisms (due to reporting discrepancies)
HRH_clean <- HRH_clean %>%
  mutate(UN_keywordflags = if_else(grepl("UNAID", mech_name) == TRUE |
                                        grepl("UNICEF", mech_name) == TRUE |
                                        grepl("World Health Organization", mech_name) == TRUE |
                                        grepl("U.N.", mech_name) == TRUE | 
                                        grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE")) 

# Cleaning up the funding_agency categories
HRH_clean <- HRH_clean %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" | funding_agency == "USAID/WCF" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ funding_agency))

# Merge the G2G partners for both CDC and USAID
CoAg <- CoAg %>%
  select(`Mechanism ID`, `Partner Org Type (revised)`) %>%
  rename(mech_code = `Mechanism ID`,
         g2g_partner_type = `Partner Org Type (revised)`)
CoAg$mech_code <- as.integer(CoAg$mech_code)

HRH_clean <- left_join(HRH_clean, CoAg, by = "mech_code")

# creating an .rds file for use in analysis of FY22 HRH data
save(HRH_clean, file = "./4. Outputs/RDS/FY24_cleanHRH.rds")

## 4. Export as csv
write.csv(HRH_clean, "./1. Data/HRH_Structured_Datasets_Site_IM_FY21-24_not_redacted_20241115_CLEAN.csv", row.names=FALSE)




