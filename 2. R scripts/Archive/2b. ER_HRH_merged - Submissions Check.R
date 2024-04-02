####################################################################################################################
## Title: ER and HRH Merged Dataset
## Purpose: This code cleans and merges the HRH Inventory dataset and ER dataset, which is then used in the HRH Inventory Tableau Dashboard.
## Developer: By Caroline Kasman & Kyle Borces
## Last updated: By Kyle Borces, 11/23/2022
####################################################################################################################

############################################ General prep ##########################################################

## 1.installing and loading relevant packages/libraries. 
setwd('/Users/kborces/Documents/HRH Inventory/FY22 Analysis')

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(readxl)
library(clipr)

## 2.  Uploading files
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-22_20221216.txt") # read in as .txt
HRH_data_orig <- read_excel("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20230118_PostClean_Adjusted.xlsx") 


###---------- Identifying any GHSC or multilateral mechanisms to be removed from HRH and ER dataset --------------------------

# pull ER mech codes to be removed by keyword flag
removedERmechs <- fin_data_orig %>%
  mutate(keyword_flags = if_else(grepl("GHSC", mech_name) == TRUE | 
                                   grepl("UNAID", mech_name) == TRUE |
                                   grepl("World Health Organization", mech_name) == TRUE |
                                   grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE")) %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# pull HRH mech codes to be removed via keyword flag
removedHRHmechs <- HRH_data_orig %>%
  mutate(keyword_flags = if_else(grepl("GHSC", mech_name) == TRUE | 
                                   grepl("UNAID", mech_name) == TRUE |
                                   grepl("World Health Organization", mech_name) == TRUE |
                                   grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE")) %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# combined list of HRH and ER codes to be removed
removedMechs <- c(removedERmechs, removedHRHmechs)
removedMechs <- unique(removedMechs)

############################################ ER cleaning ##########################################################
## 3. Filtering down to just 2021 and 2022 
fin_data2122 <- fin_data_orig %>% 
  filter(implementation_year == 2021 | implementation_year == 2022)    


## 4.Adding in a prime_or_sub column to match HRH dataset's prime_or_sub column - applicable to both 2021 and 2022
fin_data2122 <- fin_data2122 %>%
  mutate(ER_prime_or_sub = case_when(implementation_year == 2021 & cost_category == "Subrecipient" ~ "Sub",
                                     implementation_year == 2022 & subrecipient_name != "None" ~ "Sub",
                                     TRUE ~ "Prime"))

## 5. Simplifying down dataset by summarizing them
simple_findata <- fin_data2122 %>%                                       
  group_by(implementation_year,
           operatingunit, 
           program,
           fundingagency, 
           prime_partner_name, 
           ER_prime_or_sub, 
           mech_code, 
           mech_name, 
           cost_category,
           sub_cost_category) %>%                         
  summarise_at(vars(expenditure_amt), sum, na.rm = T) 

## 6. Adding a column that allows us to know if a row of ER data is relevant to staffing. Note that we decided to remove Subrecipient as a relevant sub_cost_category for 2022 
# since this pertains to subrecipient costs less than 25k (which includes both HRH and non-HRH costs)
simple_findata <- simple_findata %>% 
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
                                      TRUE ~ "N")) 



## 7. Renaming columns so we can differentiate between the ER columns and HRH columns when we combine the two datasets.
simple_findata <- simple_findata %>%
        rename(ER_year = implementation_year,
               ER_operatingunit = operatingunit,
               ER_program = program,
               ER_mech_code = mech_code,
               ER_mech_name = mech_name,
               ER_prime_partner_name = prime_partner_name,
               ER_sub_cost_category = sub_cost_category,
               ER_cost_category = cost_category,
               ER_fundingagency = fundingagency,
               ER_expenditure_amt = expenditure_amt)

## 8. Filter out South Africa in ER dataset since this was not reported in HRH for 2021
  simple_findata <- simple_findata %>%
    filter(!(ER_year == 2021 & ER_operatingunit == "South Africa")) 

  
## 9. Cleaning up the ER data: Simplifying funding agencies to be only USAID, CDC, or Other.
simple_findata <- simple_findata %>% 
  mutate(ER_fundingagency = case_when((ER_fundingagency == "USAID") | (ER_fundingagency == "USAID/WCF") ~ "USAID",
                                      (ER_fundingagency == "HHS/CDC") ~ "CDC", 
                                      TRUE ~ "Other"))



############################################ HRH cleaning ##########################################################

## 10. Set NA values to zero for annual fringe or annual expenditure - since we can likely assume that their real value is zero
HRH_data2122 <- HRH_data_orig %>%
  mutate(annual_expenditure = if_else(is.na(annual_expenditure) == TRUE, 0, annual_expenditure),
         annual_fringe = if_else(is.na(annual_fringe) == TRUE, 0, annual_fringe))

## 11. Pivoting the HRH dataset so that the three columns of "annual expenditure", "annual fringe", and "nonmonetary_costs" are put into a single column called "HRH_expenditure_amt"
##     This needs to be done to match the structure of the ER dataset so the two can properly merge

HRH_data2122 <- HRH_data2122 %>% #sort columns first so that the three columns of interest are next to each other
  select(1:fiscal_year,
         annual_expenditure,
         annual_fringe,
         actual_non_monetary_expenditure,
         actual_annual_spend:ncol(HRH_data_orig)) %>%
  pivot_longer(cols=annual_expenditure:actual_non_monetary_expenditure, names_to='salary_or_fringe', values_to = 'HRH_expenditure_amt')


## 12. Creating a sub_cost_category to match the ER dataset's sub_cost_category. This logic is based on Sarah's Tableau Prep code from 2021. 
#      Note that in both 2021 and 2022, subrecipient rows were not disaggregated for cost_category or sub_cost category - they are labeled under "subrecipients" only
HRH_data2122 <- HRH_data2122 %>% 
  mutate(sub_cost_category = case_when((prime_or_sub == "Prime") & (salary_or_fringe == "annual_fringe") ~ "Fringe Benefits",
                                       (prime_or_sub == "Sub") ~ "Subrecipient",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Salary") & (er_category == "HCW: Clinical") ~ "Salaries- Health Care Workers- Clinical",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Salary") & (er_category == "HCW: Ancillary") ~ "Salaries- Health Care Workers- Ancillary",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Salary") & ((er_category == "Other Staff") | (er_category=="Program Management")) ~ "Salaries- Other Staff",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Contract") & (er_category == "HCW: Clinical") ~ "Contracted Health Care Workers- Clinical",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Contract") & (er_category == "HCW: Ancillary") ~ "Contracted Health Care Workers- Ancillary",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Contract") & ((er_category == "Other Staff") | (er_category=="Program Management")) ~ "Contracted- Other Staff*",
                                       (prime_or_sub == "Prime") & (mode_of_hiring == "Non-Monetary ONLY") ~ "Non-monetary only*"))

## 13. Create a cost_category to help match ER dataset structure. This is based on ER's current classification system 
HRH_data2122 <- HRH_data2122 %>%
  mutate(cost_category = case_when(sub_cost_category == "Salaries- Health Care Workers- Clinical" | sub_cost_category == "Salaries- Health Care Workers- Ancillary" | sub_cost_category == "Salaries- Other Staff" ~ "Personnel",
                                   sub_cost_category == "Contracted Health Care Workers- Clinical" | sub_cost_category == "Contracted Health Care Workers- Ancillary" | sub_cost_category == "Contracted- Other Staff*" ~ "Contractual",
                                   sub_cost_category == "Non-monetary only*" ~ "Non-monetary only",
                                   sub_cost_category == "Fringe Benefits" ~ "Fringe Benefits",
                                   sub_cost_category == "Subrecipient" ~ "Subrecipient"))

## 14. Aggregate columns using the grouped variables we're interested in
simple_hrhdata <- HRH_data2122 %>%
  group_by(fiscal_year,
           operating_unit, 
           program,
           funding_agency,
           prime_partner_name,
           prime_or_sub,
           mech_code, 
           mech_name,
           cost_category,
           sub_cost_category) %>%                      
  summarise_at(vars(HRH_expenditure_amt), sum, na.rm = T) 

## 15. Cleaning up the HRH data: Simplifying funding agencies to be only USAID, CDC, or Other.
simple_hrhdata <- simple_hrhdata %>% 
  mutate(HRH_funding_agency = case_when(funding_agency == "USAID"  ~ "USAID", 
                                        funding_agency == "CDC" ~ "CDC",
                                        TRUE ~ "Other")) %>%
  ungroup() %>%
  select(-funding_agency)


#### Calculating HRH and ER submission totals ####

ER_dataQuality_check <- simple_findata %>%
  ungroup() %>%
  filter(ER_year == 2022,
         ER_fundingagency == "USAID",
         HRH_relevant == "Y") %>%
  group_by(ER_year, ER_fundingagency, HRH_relevant, ER_mech_code, ER_mech_name, ER_prime_partner_name) %>%
  summarise(ER_staffing_expenditure = sum(ER_expenditure_amt, na.rm = T)) %>%
  ungroup() %>%
  mutate(submitted_to_ER = if_else(ER_staffing_expenditure > 0, 1, 0)) %>%
  adorn_totals()

#Copy to clipboard
clipr::write_clip(ER_dataQuality_check)  

