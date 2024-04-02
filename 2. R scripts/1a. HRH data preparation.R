### THE PURPOSE OF THIS SCRIPT IS TO DO SOME DATA CLEANING OF THE 2023 RAW HRH DATASET

# Test

## 1.installing and loading relevant packages/libraries
library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)

## Import the 2021, 2022, and HRH dataset as .txt file
HRH_data_orig_23 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY23_not_redacted_20231215.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_22 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY22_not_redacted_20231215.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_21 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY21_not_redacted_20231215.txt", header = TRUE, stringsAsFactors = FALSE)

## EDIT: import also the CDC CoAg list
CoAg <- read_excel("./1. Data/COP21-22 CDC G2G and parastatals.xlsx")

## Row bind all three years
HRH_clean <- rbind(HRH_data_orig_23, HRH_data_orig_22, HRH_data_orig_21)

# If the operating unit is regional, psnuuid = snu1uid
HRH_clean <- HRH_clean %>%
  mutate(psnuuid = case_when(operating_unit == "Asia Region" | operating_unit == "West Africa Region" | operating_unit == "Western Hemisphere Region" ~ snu1uid,
                             TRUE ~ psnuuid))


# Rename columns to match FY21 column names so that it automatically updates in Tableau
HRH_clean <- HRH_clean %>%
  rename(annual_expenditure = actual_salary_expenditure,
         annual_fringe = actual_fringe_expenditure,
         is_multi_site = roving) 

# Rename the SD vs NSD column to be consistent 
HRH_clean <- HRH_clean %>%
  mutate(interaction_type = case_when(interaction_type == "Non Service Delivery" ~ "Non-Service Delivery",
                                    interaction_type == "Service Delivery" ~ "Direct Service Delivery",
                                    TRUE ~ interaction_type))

# Rename IP program management to program management
HRH_clean <- HRH_clean %>%
  mutate(er_category = if_else(er_category == "Implementing Mechanism Program Management Staff", "Program Management", er_category))

# Add a keyword flag if staff belong to GHSC mechanisms or UN-related mechanisms (due to reporting discrepancies)
HRH_clean <- HRH_clean %>%
  mutate(GHSC_UN_keywordflags = if_else(grepl("^GHSC", mech_name) == TRUE | 
                                 grepl("UNAID", mech_name) == TRUE |
                                 grepl("UNICEF", mech_name) == TRUE |
                                 grepl("World Health Organization", mech_name) == TRUE |
                                 grepl("U.N.", mech_name) == TRUE | 
                                 grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE")) 

# Cleaning the is_community_primarily variable  to be "yes" when "community" is in employment title
#HRH_clean <- HRH_clean %>%
#  mutate(is_community_adjusted = if_else(grepl("community", employment_title, ignore.case = TRUE), "Yes", is_community_primarily))

# EDIT: Also adding three DREAMS-related variables to create lower bound and upper bound estimates of DREAMS related staff
  
      # Import mechs with DREAMS budgets
        FY23_budget <- read_excel("./1. Data/Comprehensive_Budget_Datasets_COP17-23_20231114.xlsx")
      # Clean up the DREAMS mechs
        DREAMS_mechs_23 <- FY23_budget %>%
          filter(implementation_year == 2023,
                 fundingagency == "USAID" | fundingagency == "USAID/WCF",
                 initiative_name == "DREAMS",
                 record_type == "Implementing Mechanism",
                 cop_budget_total > 0) %>%
          distinct(mech_code) %>%
          pull(mech_code)
        
        HRH_clean <- HRH_clean %>%
          mutate(DREAMS_comments = if_else(grepl("DREAM", comments) == TRUE, "TRUE", "FALSE"), # if DREAMS were included in comments column
                 DREAMS_title = if_else(grepl("DREAM", employment_title) == TRUE, "TRUE", "FALSE"), # if DREAMS was in the employment title
                 DREAMS_budget_23 = if_else(fiscal_year == 2023 & (mech_code %in% DREAMS_mechs_23), "TRUE", "FALSE"), # if received a DREAMS budget
                 DREAMS_tagged_23 = if_else(fiscal_year == 2023 & (DREAMS_comments == "TRUE" | DREAMS_title == "TRUE"), "TRUE", "FALSE"), # if DREAMS was in comments + DREAMS in employment title
                 DREAMS_AGYWs_23 = if_else(DREAMS_budget_23 == "TRUE" & sub_beneficiary == "Young women & adolescent females", "TRUE", "FALSE")) %>% # if received a DREAMS budget + beneficiary is AGYWs
          select(-DREAMS_comments, -DREAMS_title) # remove variables we don't need anymore

# Cleaning up the funding_agency categories
HRH_clean <- HRH_clean %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" | funding_agency == "USAID/WCF" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ "Other agencies"))

# creating an .rds file for use in analysis of FY22 HRH data
save(HRH_clean, file = "./4. Outputs/RDS/FY23_cleanHRH.rds")

## 4. Export as csv
write.csv(HRH_clean, "./1. Data/HRH_Structured_Datasets_Site_IM_FY21-23_not_redacted_20231127_CLEAN.csv", row.names=FALSE)





