### PURPOSE: To create a mock FY24 dataset to facilitate mock-up of FY24 HRH dashboard
### Code developed by: Kyle Borces
### Last updated: Sep 2024

## 1.installing and loading relevant packages/libraries
library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)

## Uploading files

## Import the 2021, 2022, and HRH dataset as .txt file
HRH_data_orig_23 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY23_not_redacted_20231215.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_22 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY22_not_redacted_20231215.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_data_orig_21 <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY21_not_redacted_20231215.txt", header = TRUE, stringsAsFactors = FALSE)
HRH_clean <- rbind(HRH_data_orig_23, HRH_data_orig_22, HRH_data_orig_21)

# If the operating unit is regional, psnuuid = snu1uid
HRH_clean <- HRH_clean %>%
  mutate(psnuuid = case_when(operating_unit == "Asia Region" | operating_unit == "West Africa Region" | operating_unit == "Western Hemisphere Region" ~ snu1uid,
                             TRUE ~ psnuuid))

# Rename columns to match FY21 column names so that it automatically updates in Tableau
HRH_clean <- HRH_clean %>%
  rename(annual_expenditure = actual_salary_expenditure,
         is_multi_site = roving,
         annual_fringe = actual_fringe_expenditure)

# Rename the SD vs NSD column to be consistent with both 2021 and 2022
HRH_clean <- HRH_clean %>%
  mutate(interaction_type = case_when(interaction_type == "Non Service Delivery" ~ "Non-Service Delivery",
                                      interaction_type == "Service Delivery" ~ "Direct Service Delivery",
                                      TRUE ~ interaction_type))

# Rename IP program management to program management
HRH_clean <- HRH_clean %>%
  mutate(er_category = if_else(er_category == "Implementing Mechanism Program Management Staff", "Program Management", er_category))

# EDIT: Add a keyword flag if staff belong to GHSC mechanisms or UN-related mechanisms (due to reporting discrepancies)
HRH_clean <- HRH_clean %>%
  mutate(GHSC_UN_keywordflags = if_else(grepl("^GHSC", mech_name) == TRUE | 
                                          grepl("UNAID", mech_name) == TRUE |
                                          grepl("UNICEF", mech_name) == TRUE |
                                          grepl("World Health Organization", mech_name) == TRUE |
                                          grepl("U.N.", mech_name) == TRUE | 
                                          grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE")) 


# Clean up the funding_agency categories
HRH_clean <- HRH_clean %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" | funding_agency == "USAID/WCF" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ "Other agencies"))

# Create a data frame that replicates the 2023 entries and append to the original HRH dataset
dummy24 <- HRH_clean %>%
  filter(fiscal_year == 2023) %>%
  mutate(fiscal_year = 2024)
HRH_dummy24 <- rbind(HRH_clean, dummy24)

## Create a dummy dataset with 2024 data that slight changes to the expenditure columns by 5-10%
HRH_dummy24$factor_salary <- sample(seq(0.96, 1.10, by = 0.01),replace = T, nrow(HRH_dummy24))
HRH_dummy24$factor_fringe <- sample(seq(0.96, 1.10, by = 0.01),replace = T, nrow(HRH_dummy24))
HRH_dummy24$factor_nonMonetary <- sample(seq(0.96, 1.10, by = 0.01),replace = T, nrow(HRH_dummy24))

# Set all NA values in expenditure columns to zero
HRH_dummy24$annual_expenditure[is.na(HRH_dummy24$annual_expenditure)] <- 0
HRH_dummy24$annual_fringe[is.na(HRH_dummy24$annual_fringe)] <- 0
HRH_dummy24$actual_non_monetary_expenditure[is.na(HRH_dummy24$actual_non_monetary_expenditure)] <- 0
HRH_dummy24$actual_annual_spend[is.na(HRH_dummy24$actual_annual_spend)] <- 0

# Revise the 2024 dummy data
HRH_dummy24 <- HRH_dummy24 %>%
  mutate(annual_expenditure = case_when(fiscal_year == 2024 ~ annual_expenditure * factor_salary, TRUE ~ annual_expenditure),
         annual_fringe = case_when(fiscal_year == 2024 ~ annual_fringe * factor_fringe, TRUE ~ annual_fringe),
         actual_non_monetary_expenditure = case_when(fiscal_year == 2024 ~ actual_non_monetary_expenditure * factor_nonMonetary, TRUE ~ actual_non_monetary_expenditure),
         actual_annual_spend = case_when(fiscal_year == 2024 ~ annual_expenditure + annual_fringe + actual_non_monetary_expenditure, TRUE ~ actual_annual_spend))

#remove the factors
HRH_dummy24 <- HRH_dummy24 %>%
  select(-factor_salary, -factor_fringe, -factor_nonMonetary)

## 4. Export to Excel
write.csv(HRH_dummy24,"./4. Outputs/HRH_Structured_Datasets_Site_IM_FY21-24_not_redacted_2024DUMMY.csv") 



