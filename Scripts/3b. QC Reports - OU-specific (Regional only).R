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
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-23_20231215.txt") # read in FSD dataset
load(file = "./4. Outputs/RDS/FY23_cleanHRH.rds") # cleaned HRH dataset
load(file = "./4. Outputs/RDS/HRH_ER_merged_21_23.rds") # HRH-ER merged dataset
OVC_mechs <- read_excel("./4. Outputs/FY23_OVC_mechs.xlsx")
FY23_budget <- read_excel("./1. Data/Comprehensive_Budget_Datasets_COP17-23_20231114.xlsx")


# Clean up the OVC mechs 
OVC_mechs <- OVC_mechs %>%
  filter(funding_agency == "USAID") %>%
  select(mech_code) %>%
  pull(mech_code)

# Clean up the DREAMS mechs
DREAMS_mechs <- FY23_budget %>%
  filter(implementation_year == 2023,
         fundingagency == "USAID" | fundingagency == "USAID/WCF",
         initiative_name == "DREAMS",
         record_type == "Implementing Mechanism",
         cop_budget_total > 0) %>%
  distinct(mech_code) %>%
  pull(mech_code)

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
  filter(GHSC_UN_keywordflags == "FALSE")

###---------- Identifying any GHSC or multilateral mechanisms to be removed from HRH and ER dataset --------------------------

# pull ER mech codes to be removed by keyword flag
removedERmechs <- fin_data_orig %>%
  mutate(keyword_flags = if_else(grepl("^GHSC", mech_name) == TRUE | 
                                   grepl("UNAID", mech_name) == TRUE |
                                   grepl("UNICEF", mech_name) == TRUE |
                                   grepl("World Health Organization", mech_name) == TRUE |
                                   grepl("U.N.", mech_name) == TRUE | 
                                   grepl("United Nation", mech_name) == TRUE, "TRUE", "FALSE"))  %>%
  filter(keyword_flags == "TRUE") %>%
  pull(mech_code)

# pull HRH mech codes to be removed via keyword flag
removedHRHmechs <- HRH_data_orig %>%
  mutate(keyword_flags = if_else(grepl("^GHSC", mech_name) == TRUE | 
                                   grepl("UNAID", mech_name) == TRUE |
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
fin_data2123 <- fin_data_orig %>% 
  filter(implementation_year == 2021 | implementation_year == 2022 | implementation_year == 2023) %>%
  
  # Add prime/sub column
  mutate(ER_prime_or_sub = case_when(implementation_year == 2021 & cost_category == "Subrecipient" ~ "Sub",
                                     implementation_year == 2022 & subrecipient_name != "None" ~ "Sub",
                                     implementation_year == 2023 & subrecipient_name != "None" ~ "Sub",
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
  fin_data2123 <- fin_data2123 %>%
    filter(!ER_mech_code %in% removedMechs)


## --------------Do some data cleaning for HRH -------------------------

# Set non-monetary expenditures to zero since we cannot do a 1:1 comparison with ER
HRH_data2123 <- HRH_data_orig %>%
    mutate(actual_non_monetary_expenditure = 0)
  
# Pivoting the HRH dataset so that the three columns of "annual expenditure", "annual fringe", and "nonmonetary_costs" are put into a single column called "HRH_expenditure_amt"
#    This needs to be done to match the structure of the ER dataset so the two can properly merge

HRH_data2123 <- HRH_data2123 %>% # sort columns first so that the three columns of interest are next to each other, then convert to wide format
  select(1:fiscal_year,
         annual_expenditure,
         annual_fringe,
         actual_non_monetary_expenditure,
         actual_annual_spend:ncol(HRH_data_orig)) %>%
  pivot_longer(cols=annual_expenditure:actual_non_monetary_expenditure, names_to='salary_or_fringe', values_to = 'HRH_expenditure_amt')

# Creating a sub_cost_category to match the ER dataset's sub_cost_category. This logic is based on Sarah's Tableau Prep code from 2021.
HRH_data2123 <- HRH_data2123 %>% 
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
HRH_data2123 <- HRH_data2123 %>%
  filter(!mech_code %in% removedMechs)

# EDIT: revert back the SD vs NSD names to be consistent with ER
HRH_data2123 <- HRH_data2123 %>%
mutate(interaction_type = case_when(interaction_type == "Non-Service Delivery" ~ "Non Service Delivery",
                                      interaction_type == "Direct Service Delivery" ~ "Direct Service Delivery",
                                      TRUE ~ interaction_type))

fin_data2123 <- fin_data2123 %>%
  mutate(interaction_type = case_when(interaction_type == "Non Service Delivery" ~ "Non Service Delivery",
                                      interaction_type == "Service Delivery" ~ "Direct Service Delivery",
                                      TRUE ~ interaction_type))

## ------------- Create QC reports for each OU ----------

# Create list of operating units that we want QC reports for. 

# PLEASE EDIT FOR FY24 NEW REGIONS!!

OU_list <- HRH_ER_merged %>%
  distinct(operating_unit) %>%
  filter(operating_unit == "West Africa Region" |
         operating_unit == "Asia Region" |
         operating_unit == "Western Hemisphere Region") %>%
  pull(operating_unit)

## Execute a for loop that creates a QC report for each OU

for (i in 1:length(OU_list)) {
  
  OU <- OU_list[i] # loop through each OU in the OU list
  
      # Top level summary by Prime/Sub
      topLevel <- HRH_ER_merged %>%
        filter(year == max(year)) %>%
        filter(operating_unit == OU) %>%
        group_by(year, operating_unit, country, mech_code, mech_name, prime_or_sub) %>%
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
        group_by(operating_unit, country, year, mech_code, mech_name) %>%
        summarise(reported_in_ER = sum(ER_expenditure_amt, na.rm = T),
                  reported_in_HRH = sum(HRH_expenditure_amt, na.rm = T)) %>%
        ungroup() %>%
        mutate(reported_in_ER = if_else(reported_in_ER > 0, "Yes", "No"),
               reported_in_HRH = if_else(reported_in_HRH > 0, "Yes", "No"),
               action_item = if_else(reported_in_ER == "Yes" & reported_in_HRH == "No", "ER staffing expenditures were reported, but HRH expenditures were NOT reported. Please review and confirm HRH report submission.","")) %>%
        arrange(mech_code)
      
      # Breakdown of employment titles for "Other Staff" 
      Breakdown_other <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU) %>%
        group_by(fiscal_year, operating_unit, country, mech_code, mech_name) %>%
        summarise(total_num_staff = n(),
                  total_other_staff = sum(employment_title == "Other Program Management Staff" |
                                            employment_title == "Other Professional Staff" |
                                            employment_title == "Other supportive staff not listed" |
                                            employment_title == "Other community-based cadre" |
                                            employment_title == "Other clinical provider not listed", na.rm = T)) %>%
        ungroup() %>%
        mutate(pct_ofTotal = total_other_staff / total_num_staff * 100)
      
      otherPull <- HRH_data_orig %>% # pull employment titles of "other" staff
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU,
               employment_title == "Other Program Management Staff" |
                 employment_title == "Other Professional Staff" |
                 employment_title == "Other supportive staff not listed" |
                 employment_title == "Other community-based cadre" |
                 employment_title == "Other clinical provider not listed") %>%
        group_by(fiscal_year, operating_unit, country, mech_code, mech_name) %>%
        summarise(other_employmentTitles = paste(unique(employment_title), collapse = ", "))
      
      Breakdown_other <- left_join(Breakdown_other, otherPull, by = c("fiscal_year", "operating_unit", "country", "mech_code", "mech_name")) %>%
        mutate(action_item = if_else(pct_ofTotal > 20, "Over 20% of employment titles were reported under the `other` categories. Please review the employment titles identified in column I and determine if a more specific employment title can better reflect their roles/responsibilities", ""))
      
    # OVC Check: Filter the HRH dataset for all OVC mechs that had OVC_serv > 1, then summarize total staff with beneficiary = OVC
      OVC_check <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU,
               mech_code %in% OVC_mechs) %>%
        group_by(fiscal_year, operating_unit, country, mech_code, mech_name) %>%
        summarise(total_OVC_staff = length(individual_count[beneficiary == "OVC"])) %>%
        mutate(action_item = if_else(total_OVC_staff == 0, "No OVC staff was reported for this mechanism, but our records indicate this mechanism has OVC_SERV targets. Please review and ensure that the beneficiary column for OVC staff is accurate", ""))
      
   # DREAMS Check: Filter the HRH dataset for DREAM(S) related mech names, and then summarize total staff with DREAMS keyword search in Comments column
      DREAMS_check <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU,
               mech_code %in% DREAMS_mechs) %>%
        mutate(DREAMS_keyword = if_else(grepl("DREAM", comments) == TRUE, "TRUE", "FALSE")) %>%
        group_by(fiscal_year, operating_unit, country, mech_code, mech_name) %>%
        summarise(total_DREAMS_staff = length(individual_count[DREAMS_keyword == "TRUE"])) %>%
        mutate(action_item = if_else(total_DREAMS_staff == 0, "No DREAMS staff was reported for this mechanism, but records indicate this mechanism received some budget for DREAMS. Please review and ensure that all staff working on DREAMS are counted by typing 'DREAMS' in the Comments column", ""))
      
      
      ## Create a summary of ALL data flags for each mechanism
      
      # First, pull the mech_codes that trigger each of our data flags
      overall_primeSub <- topLevel %>% # prime + sub totals
        group_by(year, mech_code, mech_name) %>%
        summarise(ER_expenditure_amt = sum(ER_expenditure_amt, na.rm = T),
                  HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T)) %>%
        ungroup() %>%
        mutate(pct_difference_fromER = (HRH_expenditure_amt - ER_expenditure_amt)/ER_expenditure_amt * 100,
               action_item = if_else(abs(pct_difference_fromER) > 15, "Reported HRH expenditures are different from ER staffing expenditures by at least 15%. Please review for closer alignment.", "")) %>%
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_prime <- topLevel %>% # prime only
        filter(prime_or_sub == "Prime",
               action_item != "") %>%
        pull(mech_code)
      
      overall_sub <- topLevel %>% # sub only
        filter(prime_or_sub == "Sub",
               action_item != "") %>%
        pull(mech_code)
      
      topLevel2 <- topLevel %>% # topLevel df, but with action items only
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_missingHRH <- totalCount %>% # HRH submissions
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_other <- Breakdown_other %>% # other breakdown
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_OVC <- OVC_check %>% # OVC mechs errors
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_DREAMS <- DREAMS_check %>% # DREAMS mechs errors
        filter(action_item != "") %>%
        pull(mech_code)
        
      # Build the overall summary table that outlines how many data quality flags were triggered for each mechanism
      
      overall_wide <- HRH_ER_merged %>%
        filter(year == max(year),
               operating_unit == OU) %>%
        distinct(year, operating_unit, country, mech_code, mech_name, prime_partner_name) %>%
        mutate(`Report was submitted for ER, but NOT for HRH` = if_else(mech_code %in% overall_missingHRH, "Yes", "No"),
               `HRH staffing expenditure (prime + sub) is different by > 15% of staffing expenditure reported to ER` = if_else(mech_code %in% overall_primeSub, "Yes", "No"),
               `PRIME staffing expenditure is different by > 15% of PRIME staffing expenditure reported to ER` = if_else(mech_code %in% overall_prime, "Yes", "No"),
               `SUB staffing expenditure is different by > 15% of SUB staffing expenditure reported to ER` = if_else(mech_code %in% overall_sub, "Yes", "No"),
        #       `SERVICE DELIVERY staffing expenditure is different by > 30% of SERVICE DELIVERY staffing expenditure reported to ER` = if_else(mech_code %in% overall_SD, "Yes", "No"),
        #       `NON SERVICE DELIVERY staffing expenditure is different by > 30% of NON SERVICE DELIVERY staffing expenditure reported to ER` = if_else(mech_code %in% overall_NSD, "Yes", "No"),
               `Staff reported under 'other' employment titles is > 20% of total staff` = if_else(mech_code %in% overall_other, "Yes", "No"),
        #       `Some staff reported with months of work = 0` = if_else(mech_code %in% overall_FTE_monthsWorked, "Yes", "No"),
               `No staff reported with OVC beneficiaries, even though this mechanism has OVC program targets` = if_else(mech_code %in% overall_OVC, "Yes", "No"),
               `No staff reported under DREAMS, even though this mechanism has a DREAMS-related budget` = if_else(mech_code %in% overall_DREAMS, "Yes", "No")) %>%
        arrange(mech_code) 
      
      # Also build the overall summary table in long format
      overall_long <- overall_wide %>%
        gather(key = dataQuality_flag, value = Yes_or_No, 7:ncol(overall_wide)) %>%
        arrange(mech_code) %>%
        mutate(action_items = case_when(dataQuality_flag == "Report was submitted for ER, but NOT for HRH" & Yes_or_No == "Yes" ~ "ER staffing expenditures were reported, but HRH expenditures were NOT reported. Please ensure HRH report is completed, uploaded, and submitted in DATIM",
                                        dataQuality_flag == "HRH staffing expenditure (prime + sub) is different by > 15% of staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Total HRH expenditures were different from total ER staffing expenditures by at least 15%. Please review for closer alignment",
                                        dataQuality_flag == "PRIME staffing expenditure is different by > 15% of PRIME staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Prime HRH expenditures were different from Prime ER staffing expenditures by at least 15%. Please review for closer alignment",
                                        dataQuality_flag == "SUB staffing expenditure is different by > 15% of SUB staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Sub HRH expenditures were different from Sub ER staffing expenditures by at least 15%. Please review for closer alignment",
                    #                    dataQuality_flag == "SERVICE DELIVERY staffing expenditure is different by > 30% of SERVICE DELIVERY staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Service-Delivery HRH expenditures were different from Service-Delivery ER staffing expenditures by at least 30%. Please review for closer alignment",
                    #                    dataQuality_flag == "NON SERVICE DELIVERY staffing expenditure is different by > 30% of NON SERVICE DELIVERY staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Non-Service-Delivery HRH expenditures were different from Non-Service-Delivery ER staffing expenditures by at least 30%. Please review for closer alignment",
                                        dataQuality_flag == "Staff reported under 'other' employment titles is > 20% of total staff" & Yes_or_No == "Yes" ~ "Over 20% of employment titles were reported under `other` categories. Please review all 'other' employment titles that were submitted, and determine whether a more specific employment title will better reflect their roles/responsibilities",
                    #                    dataQuality_flag == "Some staff reported with months of work = 0" & Yes_or_No == "Yes" ~ "Some staff were reported to have months of work = 0. Please double check these rows",
                                        dataQuality_flag == "No staff reported with OVC beneficiaries, even though this mechanism has OVC program targets" & Yes_or_No == "Yes" ~ "No OVC staff was reported for this mechanism, but our records indicate this mechanism has OVC_SERV targets. Please review and ensure that the beneficiary column for OVC staff is accurate",
                                        dataQuality_flag == "No staff reported under DREAMS, even though this mechanism has a DREAMS-related budget" & Yes_or_No == "Yes" ~ "No DREAMS staff was reported for this mechanism, but records indicate this mechanism received some budget for DREAMS. Please review and ensure that all staff working on DREAMS are counted by typing 'DREAMS' in the Comments column",
                                        TRUE ~ "")) %>%
        select(year, operating_unit, country, mech_code, mech_name, prime_partner_name, dataQuality_flag, Yes_or_No, action_items)
      
      # Add a note on what we consider under "other" employment title if flagged
      overall_long <- overall_long %>%
        mutate(Notes = if_else(dataQuality_flag == "Staff reported under 'other' employment titles is > 20% of total staff" & Yes_or_No == "Yes", "Note: `other` employment titles include: Other Program Management Staff, Other Professional Staff, Other supportive staff not listed, Other community-based cadre, and Other clinical provider not listed", ""))

      # Filter for only mech_codes with data flags 
      toKeep <- overall_long %>%
        filter(Yes_or_No == "Yes") %>%
        distinct(mech_code, .keep_all = TRUE) %>%
        pull(mech_code)
      
      overall_long <- overall_long %>%
        filter(mech_code %in% toKeep) %>%
        filter(Yes_or_No == "Yes") %>%
        select(-Yes_or_No)
      
 ### EDIT: filter for only rows with action items in the data frames to be used
      topLevel <- topLevel %>% filter(mech_code %in% topLevel2) %>%
                  select(-ER_expenditure_amt, -HRH_expenditure_amt) %>% # exclude the expenditure rows
                  filter(action_item != "") # edit: only show action items
      
      totalCount <- totalCount %>% filter(mech_code %in% overall_missingHRH)
      Breakdown_other <- Breakdown_other %>% filter(mech_code %in% overall_other) %>%
                  select(-total_num_staff, -pct_ofTotal) # only show the total number of other staff
      
      OVC_check <- OVC_check %>% filter(mech_code %in% overall_OVC)
      DREAMS_check <- DREAMS_check %>% filter(mech_code %in% overall_DREAMS)
      
      ## --------------- Load each data frame into the Excel templates---------------
      
      # Import the excel reporting template to be used
      wb <- loadWorkbook("./1. Data/HRH_QC_Reporting_Template_20231118_vF - Deep Dive (Regional).xlsx")
      
      # Set workbook title
      wbTitle <- paste0(OU, " - ", "FY23 Data Quality Checks on Submitted HRH Reporting Templates") 
      
      # Load the data frames into each Excel sheet as needed
      writeData(wb, sheet = 1, wbTitle, startCol = 2, startRow = 3, colNames = FALSE)
      writeData(wb, sheet = 2, overall_long, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 3, totalCount, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 4, topLevel, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 5, Breakdown_other, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 6, OVC_check, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 7, DREAMS_check, startCol = 2, startRow = 4, colNames = FALSE)

      # Establish the workbook name based on the OU
      wbName <- paste0("./4. Outputs/QC Reports/Regional level/FY23 HRH Data Quality Checks - Final - ", OU, ".xlsx")
      
      # Export each QC report in Excel
      saveWorkbook(wb, wbName, overwrite = TRUE) #to automate later
      
      # print progress
      print(paste0(OU, " - workbook complete"))
}

 

