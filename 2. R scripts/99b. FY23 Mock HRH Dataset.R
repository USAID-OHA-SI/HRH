### THE PURPOSE OF THIS SCRIPT IS TO DO INITIAL DATA CLEANING OF THE RAW FY23HRH DATASET.
### Code developed by: Kyle Borces
### Last updated: July 2023

## 1.installing and loading relevant packages/libraries
library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)

## Uploading files
HRH_data_orig <- read_excel("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20230117_Adjusted.xlsx") # read in from NGA post-cleaning dataset
localPartner <- read_excel("./1. Data/PartnerType for Tableau.xlsx") #import local dataset

## Perform a left join of the local partner dataset
HRH_data_orig$mech_code <- as.numeric(HRH_data_orig$mech_code) # set as numeric first
HRH_clean <- left_join(HRH_data_orig, localPartner, by = c("mech_code" = "Mechanism_ID"))

# If the operating unit is regional, psnuuid = snu1uid
HRH_clean <- HRH_clean %>%
  mutate(psnuuid = case_when(operating_unit == "Asia Region" | operating_unit == "West Africa Region" | operating_unit == "Western Hemisphere Region" ~ snu1uid,
                             TRUE ~ psnuuid))

# Rename columns to match FY21 column names so that it automatically updates in Tableau
HRH_clean <- HRH_clean %>%
  rename(annual_expenditure = actual_salary_expenditure,
         is_multi_site = roving,
         annual_fringe = actual_fringe_expenditure) %>%
  select(-G2G) # remove G2G variable

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
  mutate(GHSC_UN_keywordflags = if_else(grepl("GHSC", mech_name) == TRUE | 
                                 grepl("UNAID", mech_name) == TRUE |
                                 grepl("World Health Organization", mech_name) == TRUE |
                                 grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE")) 

# EDIT (01/12/23) - cleaning the is_community_primarily variable  to be "yes" when "community" is in employment title
#HRH_clean <- HRH_clean %>%
#  mutate(is_community_adjusted = if_else(grepl("community", employment_title, ignore.case = TRUE), "Yes", is_community_primarily))

# EDIT (01/12/23) - cleaning up the funding_agency categories
HRH_clean <- HRH_clean %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" | funding_agency == "USAID/WCF" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ "Other agencies"))

# creating an .rds file for use in analysis of FY22 HRH data
save(HRH_clean, file = "./4. Outputs/FY22_cleanHRH.rds")

# Create a data frame that replicates the 2022 entries and append to the original HRH dataset
dummy23 <- HRH_clean %>%
  filter(fiscal_year == 2022) %>%
  mutate(fiscal_year = 2023)
HRH_dummy23 <- rbind(HRH_clean, dummy23)

## Create a dummy dataset with 2023 data that slightly changes the expenditure columns by 5-10%
HRH_dummy23$factor_salary <- sample(seq(0.96, 1.10, by = 0.01),replace = T, nrow(HRH_dummy23))
HRH_dummy23$factor_fringe <- sample(seq(0.96, 1.10, by = 0.01),replace = T, nrow(HRH_dummy23))
HRH_dummy23$factor_nonMonetary <- sample(seq(0.96, 1.10, by = 0.01),replace = T, nrow(HRH_dummy23))

# Set all NA values in expenditure columns to zero
HRH_dummy23$annual_expenditure[is.na(HRH_dummy23$annual_expenditure)] <- 0
HRH_dummy23$annual_fringe[is.na(HRH_dummy23$annual_fringe)] <- 0
HRH_dummy23$actual_non_monetary_expenditure[is.na(HRH_dummy23$actual_non_monetary_expenditure)] <- 0
HRH_dummy23$actual_annual_spend[is.na(HRH_dummy23$actual_annual_spend)] <- 0

# Revise the 2023 dummy data
HRH_dummy23 <- HRH_dummy23 %>%
  mutate(annual_expenditure = case_when(fiscal_year == 2023 ~ annual_expenditure * factor_salary, TRUE ~ annual_expenditure),
         annual_fringe = case_when(fiscal_year == 2023 ~ annual_fringe * factor_fringe, TRUE ~ annual_fringe),
         actual_non_monetary_expenditure = case_when(fiscal_year == 2023 ~ actual_non_monetary_expenditure * factor_nonMonetary, TRUE ~ actual_non_monetary_expenditure),
         actual_annual_spend = case_when(fiscal_year == 2023 ~ annual_expenditure + annual_fringe + actual_non_monetary_expenditure, TRUE ~ actual_annual_spend))

iris %>% 
  rowwise() %>% 
  mutate(sumVar = sum(c_across(Sepal.Length:Petal.Width)))

#remove the factors
HRH_dummy23 <- HRH_dummy23 %>%
  select(-factor_salary, -factor_fringe, -factor_nonMonetary)

## 4. Export to Excel
write.csv(HRH_dummy23,"./4. Outputs/HRH_Structured_Datasets_Site_IM_FY21-23_not_redacted_2023DUMMY.csv") 






