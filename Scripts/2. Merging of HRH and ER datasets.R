####################################################################################################################
## Title: ER and HRH Merged Dataset
## Purpose: This code cleans and merges the HRH Inventory dataset and ER dataset, which is then used in the HRH Inventory Tableau Dashboard
## Code developed by: Kyle Borces
####################################################################################################################

############################################ General prep ##########################################################

## 1.installing and loading relevant packages/libraries. 

library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(readxl)
library(writexl)

## 2.  Uploading files
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-24_20241115.txt") # read in FSD dataset
load(file = "./4. Outputs/RDS/FY24_cleanHRH.rds") # cleaned HRH dataset

###---------- Identifying any UN multilateral mechanisms to be removed form HRH and ER dataset --------------------------

# pull ER mech codes to be removed by keyword flag
removedERmechs <- fin_data_orig %>%
  mutate(keyword_flags = if_else(grepl("^GHSC-", mech_name) == TRUE |
                                 grepl("UNAID", mech_name) == TRUE |
                                 grepl("UNICEF", mech_name) == TRUE |
                                 grepl("World Health Organization", mech_name) == TRUE |
                                 grepl("U.N.", mech_name) == TRUE | 
                                 grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE"))  %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# pull HRH mech codes to be removed via keyword flag
removedHRHmechs <- HRH_clean %>%
  rename(keyword_flags = UN_keywordflags) %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# combined list of HRH and ER codes to be removed
removedMechs <- c(removedERmechs, removedHRHmechs)
removedMechs <- unique(removedMechs)

############################################ ER cleaning ##########################################################
## 3. Filtering down to just 2021, 2022, 2023, and 2024
fin_data2124 <- fin_data_orig %>% 
  filter(implementation_year == 2021 | 
         implementation_year == 2022 | 
         implementation_year == 2023 | 
         implementation_year == 2024
         )    

## 4.Adding in a prime_or_sub column to match HRH dataset's prime_or_sub column
fin_data2124 <- fin_data2124 %>%
  mutate(ER_prime_or_sub = case_when(implementation_year == 2021 & cost_category == "Subrecipient" ~ "Sub",
                                     implementation_year == 2022 & subrecipient_name != "None" ~ "Sub",
                                     implementation_year == 2023 & subrecipient_name != "None" ~ "Sub",
                                     implementation_year == 2024 & subrecipient_name != "None" ~ "Sub",
                                     TRUE ~ "Prime"))

## 5. Simplifying down dataset by summarizing them
simple_findata <- fin_data2124 %>%                                       
  group_by(implementation_year,
           operatingunit, 
           country,
           program,
           fundingagency, 
           prime_partner_name, 
           ER_prime_or_sub, 
           mech_code, 
           mech_name, 
           cost_category,
           sub_cost_category) %>%                         
  summarise_at(vars(expenditure_amt), sum, na.rm = T) %>%
  ungroup() 

## 6. Adding a column that allows us to know if a row of ER data is relevant to staffing. Note that we decided to remove Subrecipient as a relevant cost_category for 2022 and up
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
                                  TRUE ~ "N")) 


## 7. Renaming columns so we can differentiate between the ER columns and HRH columns when we combine the two datasets.
simple_findata <- simple_findata %>%
  rename(ER_year = implementation_year,
         ER_operatingunit = operatingunit,
         ER_country = country,
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

# EDIT: To add the UN variable that can filter out UN mechanisms when needed
simple_findata <- simple_findata %>%
  mutate(UN_keywordflags = if_else(ER_mech_code %in% removedMechs, "TRUE", "FALSE")) 


############################################ HRH cleaning ##########################################################

## 10. Set NA values to zero for annual fringe or annual expenditure since we can likely assume that their real value is zero
HRH_data2124 <- HRH_clean %>%
  mutate(annual_expenditure = if_else(is.na(annual_expenditure) == TRUE, 0, annual_expenditure),
         annual_fringe = if_else(is.na(annual_fringe) == TRUE, 0, annual_fringe),
         
         # NOTE: WE HAVE DECIDED TO SET NON MONETARY VALUES TO ZERO IN HRH DATASET FOR THE HRH-ER COMPARISONS SINCE WE CANNOT DO A 1:1 COMPARISON WITH ER
         actual_non_monetary_expenditure = 0)

## 11. Pivoting the HRH dataset so that the three columns of "annual expenditure", "annual fringe", and "nonmonetary_costs" are put into a single column called "HRH_expenditure_amt"
##     This needs to be done to match the structure of the ER dataset so the two can properly merge

HRH_data2124 <- HRH_data2124 %>% #sort columns first so that the three columns of interest are next to each other
  select(1:fiscal_year,
         annual_expenditure,
         annual_fringe,
         actual_non_monetary_expenditure,
         actual_annual_spend:ncol(HRH_clean)) %>%
  pivot_longer(cols=annual_expenditure:actual_non_monetary_expenditure, names_to='salary_or_fringe', values_to = 'HRH_expenditure_amt')

## 12. Creating a sub_cost_category to match the ER dataset's sub_cost_category. This logic is based on Sarah's Tableau Prep code from 2021. 
HRH_data2124 <- HRH_data2124 %>% 
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
HRH_data2124 <- HRH_data2124 %>%
  mutate(cost_category = case_when(sub_cost_category == "Salaries- Health Care Workers- Clinical" | sub_cost_category == "Salaries- Health Care Workers- Ancillary" | sub_cost_category == "Salaries- Other Staff" ~ "Personnel",
                                   sub_cost_category == "Contracted Health Care Workers- Clinical" | sub_cost_category == "Contracted Health Care Workers- Ancillary" | sub_cost_category == "Contracted- Other Staff*" ~ "Contractual",
                                   sub_cost_category == "Non-monetary only*" ~ "Non-monetary only",
                                   sub_cost_category == "Fringe Benefits" ~ "Fringe Benefits",
                                   sub_cost_category == "Subrecipient" ~ "Subrecipient"))

## 14. Aggregate columns using the grouped variables we're interested in
simple_hrhdata <- HRH_data2124 %>%
  group_by(fiscal_year,
           operating_unit, 
           country,
           program,
           funding_agency,
           prime_partner_name,
           prime_or_sub,
           mech_code, 
           mech_name,
           cost_category,
           sub_cost_category) %>%                      
  summarise_at(vars(HRH_expenditure_amt), sum, na.rm = T) %>%
  ungroup() 

## 15. Cleaning up the HRH data: Simplifying funding agencies to be only USAID, CDC, or Other.
simple_hrhdata <- simple_hrhdata %>% 
  mutate(HRH_funding_agency = case_when(funding_agency == "USAID"  ~ "USAID", 
                                        funding_agency == "CDC" ~ "CDC",
                                        TRUE ~ "Other")) %>%
  ungroup() %>%
  select(-funding_agency)

# Create keyword flags from HRH dataset
simple_hrhdata <- simple_hrhdata %>%
  mutate(HRH_UN_keywordflags = if_else(mech_code %in% removedMechs, "TRUE", "FALSE")) 


############################################ Merging the ER and HRH datasets ##########################################################

## 16. Merge the two datasets using a full join using our key list of grouped variables
HRH_ER_merged <- full_join(simple_findata, simple_hrhdata, by = c("ER_year" = "fiscal_year",
                                                                  "ER_operatingunit" = "operating_unit",
                                                                  "ER_country" = "country",
                                                                  "ER_program" = "program",
                                                                  "ER_fundingagency" = "HRH_funding_agency",
                                                                  "ER_prime_partner_name" = "prime_partner_name",
                                                                  "ER_prime_or_sub" = "prime_or_sub",
                                                                  "ER_mech_code" = "mech_code",
                                                                  "ER_mech_name" = "mech_name",
                                                                  "ER_cost_category" = "cost_category",
                                                                  "UN_keywordflags" = "HRH_UN_keywordflags",
                                                                  "ER_sub_cost_category" = "sub_cost_category"))


## 17. Rename the matching columns (since they are equivalent in the ER and HRH datasets) for better readability
HRH_ER_merged <- HRH_ER_merged %>%
  rename(year = ER_year,
         operating_unit = ER_operatingunit,
         country = ER_country,
         program = ER_program,
         funding_agency = ER_fundingagency,
         prime_partner_name = ER_prime_partner_name,
         prime_or_sub = ER_prime_or_sub,
         mech_code = ER_mech_code,
         mech_name = ER_mech_name,
         cost_category = ER_cost_category,
         sub_cost_category = ER_sub_cost_category) 

########################################### Determining missing HRH mechanisms ########################################################

## 18. Create a summary table that shows the total sum of HRH expenditures and the total sum of ER expenditures for each mech code
missing_mechs <- HRH_ER_merged %>%
  group_by(year, operating_unit, country, mech_code) %>%
  summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T), 
            ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y" | sub_cost_category == "Other Contracts" | sub_cost_category == "Contracted Interventions"], na.rm = T)) %>% # NOTE: SUMMING ER STAFFING EXPENDITURES ONLY
  ungroup()

## 19. Now create a column that records down the mech codes where ER expenditure is positive and when HRH expenditure is zero.
missing_mechs <- missing_mechs %>%
  
  # If total ER is greater than zero and total HRH is equal to zero, then this tells us that the HRH mechanism was NOT reported (i.e. missing)
  #  If missing, write down the missing mech code. If not missing, then just be NA
  mutate(missingMechanisms_fromHRH = if_else((ER_expenditure_amt > 0) & (HRH_expenditure_amt == 0), 
                                             mech_code, NA_real_)) %>% 
  
  # Lastly, filter out all NA values so that we can get the full list of missing HRH mechanisms
  filter(!is.na(missingMechanisms_fromHRH)) %>%
  
  # Filter out the expenditure amount columns since we no longer need them
  select(-ER_expenditure_amt, -HRH_expenditure_amt)

## 20. Now, merge this list of missing HRH mechanisms into the joined_data dataframe using a left join
finalMerge <- left_join(HRH_ER_merged, missing_mechs, by = c("year", "operating_unit", "country", "mech_code"))

## 21. Re-sort the columns for better readability
finalMerge <- finalMerge %>%
  select(year,
         operating_unit, 
         country,
         program,
         funding_agency, 
         prime_partner_name, 
         prime_or_sub, 
         mech_code, 
         mech_name, 
         cost_category, 
         sub_cost_category, 
         HRH_relevant, 
         UN_keywordflags,
         missingMechanisms_fromHRH, 
         ER_expenditure_amt, 
         HRH_expenditure_amt)

## Check the totals to make sure that they make sense
yearCheck <- finalMerge %>% 
  group_by(year) %>%
  summarise(ER_expenditure_amt = sum(ER_expenditure_amt, na.rm = T),
            HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T))

########################################### Exporting merged dataset ##########################################################

write.csv(finalMerge, "./4. Outputs/HRH_ER_merged_21_24.csv", row.names=FALSE)
save(finalMerge, file = "./4. Outputs/RDS/HRH_ER_merged_21_24.rds")

#### NOTE: MAKE SURE TO CHANGE DATA TYPE FROM NUMBER(WHOLE) TO NUMBER(DECIMAL) IN TABLEAU DASHBOARD


