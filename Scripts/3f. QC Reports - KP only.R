####################################################################################################################
## Title: HRH Structured Dataset - Data validation checks
## Purpose: This code performs a series of data quality checks on the HRH structured dataset
## Developer: By Kyle Borces
## Last updated: 11/29/2023
####################################################################################################################

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

## ------------------ Import the needed files for HRH, ER, and HRH-ER datasets --------
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-24_20241213.txt") # read in FSD dataset
load(file = "./4. Outputs/RDS/FY24_cleanHRH.rds") # cleaned HRH dataset
load(file = "./4. Outputs/RDS/HRH_ER_merged_21_24.rds") # HRH-ER merged dataset
KP_mechs <- read_excel("./4. Outputs/FY24_KP_mechs.xlsx")

### ----- Do some data cleaning ---- ###

# rename the df's if needed
HRH_data_orig <- HRH_clean
HRH_ER_merged <- finalMerge
rm(finalMerge)
rm(HRH_clean)

# Filtering for only USAID mechanisms
HRH_data_orig <- HRH_data_orig %>%
  filter(funding_agency == "USAID")

fin_data_orig <- fin_data_orig %>%
  filter(fundingagency == "USAID" | fundingagency == "USAID/WCF")

HRH_ER_merged <- HRH_ER_merged %>%
  filter(funding_agency == "USAID")

# Remove the keyword flags in the merged dataset
HRH_ER_merged <- HRH_ER_merged %>%
  filter(UN_keywordflags == "FALSE")

###---------- Identifying any GHSC or multilateral mechanisms to be removed from HRH and ER dataset --------------------------

# pull ER mech codes to be removed by keyword flag
removedERmechs <- fin_data_orig %>%
  mutate(keyword_flags = if_else(grepl("UNAID", mech_name) == TRUE |
                                   grepl("UNICEF", mech_name) == TRUE |
                                   grepl("World Health Organization", mech_name) == TRUE |
                                   grepl("U.N.", mech_name) == TRUE | 
                                   grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE"))  %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# pull HRH mech codes to be removed via keyword flag
removedHRHmechs <- HRH_data_orig %>%
  mutate(keyword_flags = if_else(grepl("UNAID", mech_name) == TRUE |
                                   grepl("UNICEF", mech_name) == TRUE |
                                   grepl("World Health Organization", mech_name) == TRUE |
                                   grepl("U.N.", mech_name) == TRUE | 
                                   grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE"))  %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# combined list of HRH and ER codes to be removed
removedMechs <- c(removedERmechs, removedHRHmechs)
removedMechs <- unique(removedMechs)


## ------------- Do some initial data cleaning for ER ------------------------
fin_data2124 <- fin_data_orig %>% 
  filter(implementation_year == 2021 | implementation_year == 2022 | implementation_year == 2023 | implementation_year == 2024) %>%
  
  # Add prime/sub column
  mutate(ER_prime_or_sub = case_when(implementation_year == 2021 & cost_category == "Subrecipient" ~ "Sub",
                                     implementation_year == 2022 & subrecipient_name != "None" ~ "Sub",
                                     implementation_year == 2023 & subrecipient_name != "None" ~ "Sub",
                                     implementation_year == 2024 & subrecipient_name != "None" ~ "Sub",
                                     TRUE ~ "Prime")) %>%
  
  # Adding a column that allows us to know if a row of ER data is relevant to staffing. Note that we decided to remove Subrecipient as a relevant sub_cost_category for 2022 
  # since this pertains to subrecipient costs less than 25k (which includes both HRH and non-HRH costs)
  mutate(HRH_relevant = case_when(implementation_year == 2021  & (sub_cost_category == "Salaries- Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Salaries- Health Care Workers- Ancillary" |
                                                                    sub_cost_category == "Salaries- Other Staff" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Ancillary" |
                                                                    cost_category == "Fringe Benefits" |
                                                                    cost_category == "Subrecipient") ~ "Y",
                                  implementation_year == 2022  & (sub_cost_category == "Salaries- Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Salaries- Health Care Workers- Ancillary" |
                                                                    sub_cost_category == "Salaries- Other Staff" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Ancillary" |
                                                                    cost_category == "Fringe Benefits") ~ "Y",
                                  implementation_year == 2023  & (sub_cost_category == "Salaries- Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Salaries- Health Care Workers- Ancillary" |
                                                                    sub_cost_category == "Salaries- Other Staff" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Ancillary" |
                                                                    cost_category == "Fringe Benefits") ~ "Y",
                                  implementation_year == 2024  & (sub_cost_category == "Salaries- Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Salaries- Health Care Workers- Ancillary" |
                                                                    sub_cost_category == "Salaries- Other Staff" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Clinical" |
                                                                    sub_cost_category == "Contracted Health Care Workers- Ancillary" |
                                                                    cost_category == "Fringe Benefits") ~ "Y",
                                  TRUE ~ "N"))   %>%
  
  # Renaming columns so we can differentiate between the ER columns and HRH columns when we combine the two datasets.
  rename(ER_year = implementation_year,
         ER_operatingunit = operatingunit,
         ER_program = program,
         ER_mech_code = mech_code,
         ER_mech_name = mech_name,
         ER_prime_partner_name = prime_partner_name,
         ER_sub_cost_category = sub_cost_category,
         ER_cost_category = cost_category,
         ER_fundingagency = fundingagency,
         ER_expenditure_amt = expenditure_amt) %>%
  
  # Filter out South Africa in ER dataset since this was not reported in HRH for 2021
  filter(!(ER_year == 2021 & ER_operatingunit == "South Africa")) %>%
  
  # Simplifying funding agencies
  mutate(ER_fundingagency = case_when((ER_fundingagency == "USAID") | (ER_fundingagency == "USAID/WCF") ~ "USAID",
                                      TRUE ~ "Other"))

# Now remove the mech codes with keyword flags from ER/HRH
fin_data2124 <- fin_data2124 %>%
  filter(!ER_mech_code %in% removedMechs)


## --------------Do some data cleaning for HRH -------------------------

# Set non-monetary expenditures to zero since we cannot do a 1:1 comparison with ER
HRH_data2124 <- HRH_data_orig %>%
  mutate(actual_non_monetary_expenditure = 0)

# Pivoting the HRH dataset so that the three columns of "annual expenditure", "annual fringe", and "nonmonetary_costs" are put into a single column called "HRH_expenditure_amt"
#    This needs to be done to match the structure of the ER dataset so the two can properly merge

HRH_data2124 <- HRH_data2124 %>% # sort columns first so that the three columns of interest are next to each other, then convert to wide format
  select(1:fiscal_year,
         annual_expenditure,
         annual_fringe,
         actual_non_monetary_expenditure,
         actual_annual_spend:ncol(HRH_data_orig)) %>%
  pivot_longer(cols=annual_expenditure:actual_non_monetary_expenditure, names_to='salary_or_fringe', values_to = 'HRH_expenditure_amt')

# Creating a sub_cost_category to match the ER dataset's sub_cost_category. This logic is based on Sarah's Tableau Prep code from 2021.
HRH_data2124 <- HRH_data2124 %>% 
  mutate(sub_cost_category = case_when((salary_or_fringe == "annual_fringe") & (prime_or_sub == "Prime") ~ "Fringe Benefits",
                                       (prime_or_sub == "Sub") ~ "Subrecipient",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Salary") & (er_category == "HCW: Clinical") ~ "Salaries- Health Care Workers- Clinical",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Salary") & (er_category == "HCW: Ancillary") ~ "Salaries- Health Care Workers- Ancillary",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Salary") & ((er_category == "Other Staff") | (er_category=="Program Management")) ~ "Salaries- Other Staff",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Contract") & (er_category == "HCW: Clinical") ~ "Contracted Health Care Workers- Clinical",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Contract") & (er_category == "HCW: Ancillary") ~ "Contracted Health Care Workers- Ancillary",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Contract") & ((er_category == "Other Staff") | (er_category=="Program Management")) ~ "Contracted- Other Staff*",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Non-Monetary ONLY") ~ "Non-monetary only*")) %>%
  
  # Create a cost_category to help match ER dataset structure. This is based on ER's current classification system 
  mutate(cost_category = case_when(sub_cost_category == "Salaries- Health Care Workers- Clinical" | sub_cost_category == "Salaries- Health Care Workers- Ancillary" | sub_cost_category == "Salaries- Other Staff" ~ "Personnel",
                                   sub_cost_category == "Contracted Health Care Workers- Clinical" | sub_cost_category == "Contracted Health Care Workers- Ancillary" | sub_cost_category == "Contracted- Other Staff*" ~ "Contractual",
                                   sub_cost_category == "Non-monetary only*" ~ "Non-monetary only",
                                   sub_cost_category == "Fringe Benefits" ~ "Fringe Benefits",
                                   sub_cost_category == "Subrecipient" ~ "Subrecipient")) %>%
  
  # Cleaning up the HRH data: Simplifying funding agencies to be only USAID, CDC, or Other.
  mutate(HRH_funding_agency = case_when(((funding_agency == "USAID") | (funding_agency == "USAID/WCF"))  ~ "USAID", 
                                        (funding_agency == "HHS/CDC") ~ "CDC",
                                        TRUE ~ "Other")) %>%
  ungroup() %>%
  select(-funding_agency)

# Now remove the mech codes with keyword flags from ER/HRH
HRH_data2124 <- HRH_data2124 %>%
  filter(!mech_code %in% removedMechs)

# EDIT: revert back the SD vs NSD names to be consistent with ER
HRH_data2124 <- HRH_data2124 %>%
  mutate(interaction_type = case_when(interaction_type == "Non-Service Delivery" ~ "Non Service Delivery",
                                      interaction_type == "Direct Service Delivery" ~ "Direct Service Delivery",
                                      TRUE ~ interaction_type))

fin_data2124 <- fin_data2124 %>%
  mutate(interaction_type = case_when(interaction_type == "Non Service Delivery" ~ "Non Service Delivery",
                                      interaction_type == "Service Delivery" ~ "Direct Service Delivery",
                                      TRUE ~ interaction_type))

## ------------- Create QC reports for each OU ----------

# Create list of operating units that we want QC reports for
OU_list <- HRH_ER_merged %>%
  distinct(operating_unit) %>%
  pull(operating_unit)

## Execute a for loop that creates a QC report for each OU

# First, create the lists to store the results of each loop
topLevelList = list()
totalCountList = list()
kpList = list()


for (i in 1:length(OU_list)) {
  
  OU <- OU_list[i] # loop through each OU in the OU list
  
  # Clean up the KP mechs 
  KP_mechs_OU <- KP_mechs %>%
    filter(funding_agency == "USAID",
           operatingunit == OU) %>%
    select(mech_code) %>%
    pull(mech_code)
  
  # Top level summary by Prime/Sub
  topLevel <- HRH_ER_merged %>%
    filter(year == max(year)) %>%
    filter(operating_unit == OU) %>%
    filter(mech_code %in% KP_mechs_OU) %>%
    group_by(year, operating_unit, country, mech_code, mech_name, prime_partner_name,  prime_or_sub) %>%
    summarise(ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y"], na.rm = T),
              HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T)) %>%
    ungroup() %>%
    mutate(pct_difference_fromER = (HRH_expenditure_amt - ER_expenditure_amt)/ER_expenditure_amt * 100,
           action_item = if_else(abs(pct_difference_fromER) > 15, "Reported HRH expenditures are significantly different from ER staffing expenditures by at least 15%. Please review for closer alignment.", ""))
  topLevel$pct_difference_fromER[is.nan(topLevel$pct_difference_fromER)] <- NA # convert NaN's to NA 
  topLevel$pct_difference_fromER <- ifelse(is.infinite(topLevel$pct_difference_fromER), 100, topLevel$pct_difference_fromER) # set infinite values to 100
  
  # Review count of ER and HRH submissions 
  totalCount <- HRH_ER_merged %>%
    filter(year == max(year)) %>%
    filter(operating_unit == OU) %>%
    filter(mech_code %in% KP_mechs_OU) %>%
    group_by(operating_unit, country, year, mech_code, mech_name, prime_partner_name) %>%
    summarise(reported_in_ER = sum(ER_expenditure_amt, na.rm = T),
              reported_in_HRH = sum(HRH_expenditure_amt, na.rm = T)) %>%
    ungroup() %>%
    mutate(reported_in_ER = if_else(reported_in_ER > 0, "Yes", "No"),
           reported_in_HRH = if_else(reported_in_HRH > 0, "Yes", "No"),
           action_item = if_else(reported_in_ER == "Yes" & reported_in_HRH == "No", "ER staffing expenditures were reported, but HRH expenditures were NOT reported. Please review and confirm HRH report submission.","")) %>%
    arrange(mech_code)
  
  ########## KP checks ################
  KP_check <- HRH_data_orig %>%
    filter(fiscal_year == max(fiscal_year),
           operating_unit == OU,
           mech_code %in% KP_mechs_OU) %>%
    group_by(fiscal_year, operating_unit, country, mech_code, mech_name, prime_partner_name) %>%
    summarise(total_KP_staff = length(individual_count[targeted_beneficiary == "Key Populations"])) %>%
    mutate(action_item = if_else(total_KP_staff == 0, "No Key Populations (KP) staff were reported for this mechanism, even though this mechanism seems to have KP targets. Please review and ensure that all KP staff are reported accurately", ""))
  
  
  ### Filter for only rows with action items #####
  
  topLevel <- topLevel %>% 
    select(-ER_expenditure_amt, -HRH_expenditure_amt) %>% # exclude the expenditure rows
    filter(action_item != "") 
  
  totalCount <- totalCount %>% filter(action_item != "")
  
  KP_check <- KP_check %>% filter(action_item != "")

  ## Now load each iteration into each  long list
  topLevelList[[i]] <- topLevel
  totalCountList[[i]] <- totalCount
  kpList[[i]] <- KP_check
  
}


## Now bind the rows together in each list
topLevel = do.call(rbind, topLevelList)
totalCount = do.call(rbind, totalCountList)
KP_check = do.call(rbind, kpList)

# arrange by country and mech code
topLevel <- topLevel %>% arrange(country, mech_code) 
totalCount <- totalCount %>% arrange(country, mech_code) 
KP_check <- KP_check %>% arrange(country, mech_code)


## --------------- Load each data frame into the Excel templates---------------

# Import the excel reporting template to be used
wb <- loadWorkbook("./1. Data/HRH_QC_Reporting_Template_20231118_vF - KP.xlsx")

# Set workbook title
wbTitle <- paste0("FY24 HRH Data Quality Checks - KP mechs only") 

# Load the data frames into each Excel sheet as needed
writeData(wb, sheet = 1, wbTitle, startCol = 2, startRow = 3, colNames = FALSE)
writeData(wb, sheet = 2, totalCount, startCol = 2, startRow = 4, colNames = FALSE)
writeData(wb, sheet = 3, KP_check, startCol = 2, startRow = 4, colNames = FALSE)

# Establish the workbook name based on the OU
wbName <- paste0("./4. Outputs/QC Reports/KPs only/FY24 HRH Data Quality Checks - KPs only.xlsx")

# Export each QC report in Excel
saveWorkbook(wb, wbName, overwrite = TRUE) 

# print progress
print(paste0("KPs Consolidated workbook complete"))


 
