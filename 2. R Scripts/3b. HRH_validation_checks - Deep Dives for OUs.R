####################################################################################################################
## Title: HRH Structured Dataset - Data validation checks
## Purpose: This code performs a series of data quality checks on the HRH structured datset
## Developer: By Kyle Borces
## Last updated: By Kyle Borces, 11/18/2022
####################################################################################################################

## Open the relevant libraries
setwd('/Users/kborces/Documents/HRH Inventory/FY22 Analysis')

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

## ------------- Import the needed files for HRH, ER, and HRH-ER datasets --------
HRH_data_orig <- read_excel("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20230118_PostClean_Adjusted.xlsx") 
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-22_20221216.txt")
HRH_ER_merged <- read_excel("./4. Outputs/HRH_ER_merged_21_22.xlsx")  

# EDIT: Filtering for only USAID mechanisms
HRH_data_orig <- HRH_data_orig %>%
  filter(funding_agency == "USAID" | funding_agency == "USAID/WCF")

fin_data_orig <- fin_data_orig %>%
  filter(fundingagency == "USAID" | fundingagency == "USAID/WCF")

HRH_ER_merged <- HRH_ER_merged %>%
  filter(funding_agency == "USAID")

## ------------- Identifying any GHSC or multilateral mechanisms to be removed from HRH and ER dataset  --------------------------

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


## ------------- Do some initial data cleaning for ER ------------------------
fin_data2122 <- fin_data_orig %>% 
  filter(implementation_year == 2021 | implementation_year == 2022) %>%
  
  # Add prime/sub column
  mutate(ER_prime_or_sub = case_when(implementation_year == 2021 & cost_category == "Subrecipient" ~ "Sub",
                                     implementation_year == 2022 & subrecipient_name != "None" ~ "Sub",
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
                                  TRUE ~ "N"))  %>%

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
                                      (ER_fundingagency == "HHS/CDC") ~ "CDC", 
                                      TRUE ~ "Other"))


  # Now remove the mech codes with keyword flags from ER/HRH 
  fin_data2122 <- fin_data2122 %>%
    filter(!ER_mech_code %in% removedMechs)


## --------------Do some data cleaning for HRH -------------------------

# Set non-monetary expenditures to zero since we cannot do a 1:1 comparison with ER
HRH_data2122 <- HRH_data_orig %>%
    mutate(actual_non_monetary_expenditure = 0)
  
# Pivoting the HRH dataset so that the three columns of "annual expenditure", "annual fringe", and "nonmonetary_costs" are put into a single column called "HRH_expenditure_amt"
#    This needs to be done to match the structure of the ER dataset so the two can properly merge

HRH_data2122 <- HRH_data2122 %>% # sort columns first so that the three columns of interest are next to each other, then convert to wide format
  select(1:fiscal_year,
         annual_expenditure,
         annual_fringe,
         actual_non_monetary_expenditure,
         actual_annual_spend:ncol(HRH_data_orig)) %>%
  pivot_longer(cols=annual_expenditure:actual_non_monetary_expenditure, names_to='salary_or_fringe', values_to = 'HRH_expenditure_amt')

# Creating a sub_cost_category to match the ER dataset's sub_cost_category. This logic is based on Sarah's Tableau Prep code from 2021.
HRH_data2122 <- HRH_data2122 %>% 
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
HRH_data2122 <- HRH_data2122 %>%
  filter(!mech_code %in% removedMechs)


## ------------- Create QC reports for each OU ----------

# Create list of operating units that we want QC reports for
OU_list <- HRH_ER_merged %>%
  distinct(operating_unit) %>%
  pull(operating_unit)

## Execute a for loop that creates a QC report for each OU

for (i in 1:length(OU_list)) {
  
  OU <- OU_list[i] # loop through each OU in the OU list

      # Create SD and NSD summary at global level
      SDvsNSD_ER <- fin_data2122 %>%
        filter(ER_year == max(ER_year)) %>%
        filter(ER_operatingunit == OU) %>%
        group_by(ER_year, ER_mech_code, ER_mech_name, ER_operatingunit, interaction_type) %>%
        summarise(ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y"], na.rm = T)) 
      SDvsNSD_HRH <- HRH_data2122 %>%
        filter(fiscal_year == max(fiscal_year)) %>%
        filter(operating_unit == OU) %>%
        group_by(fiscal_year, mech_code, mech_name, operating_unit, interaction_type) %>%
        summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T)) 
      SDvsNSD <- left_join(SDvsNSD_HRH, SDvsNSD_ER, by = c("fiscal_year" = "ER_year", "mech_code" = "ER_mech_code", "mech_name" = "ER_mech_name", "operating_unit" = "ER_operatingunit", "interaction_type")) %>%
        arrange(desc(interaction_type)) %>%
        mutate(HRH_expenditure_amt = if_else(is.na(HRH_expenditure_amt), 0, HRH_expenditure_amt),
               ER_expenditure_amt = if_else(is.na(ER_expenditure_amt), 0, ER_expenditure_amt),
               pct_difference_fromER = (HRH_expenditure_amt - ER_expenditure_amt) / ER_expenditure_amt * 100,
               action_item = if_else(abs(pct_difference_fromER) > 10, "Reported HRH expenditures are significantly different from ER staffing expenditures by at least 10%. Please review for closer alignment.", "")) %>%
        arrange(mech_code)
      SDvsNSD$pct_difference_fromER <- ifelse(is.infinite(SDvsNSD$pct_difference_fromER), 100, SDvsNSD$pct_difference_fromER)
          
      # Top level summary by Prime/Sub
      topLevel <- HRH_ER_merged %>%
        filter(year == max(year)) %>%
        filter(operating_unit == OU) %>%
        group_by(year, operating_unit, mech_code, mech_name, prime_or_sub) %>%
        summarise(ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y"], na.rm = T),
                  HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T)) %>%
        ungroup() %>%
        mutate(pct_difference_fromER = (HRH_expenditure_amt - ER_expenditure_amt)/ER_expenditure_amt * 100,
               action_item = if_else(abs(pct_difference_fromER) > 10, "Reported HRH expenditures are significantly different from ER staffing expenditures by at least 10%. Please review for closer alignment.", ""))
      topLevel$pct_difference_fromER[is.nan(topLevel$pct_difference_fromER)] <- NA # convert NaN's to NA 
      topLevel$pct_difference_fromER <- ifelse(is.infinite(topLevel$pct_difference_fromER), 100, topLevel$pct_difference_fromER) # set infinite values to 100
        
      # Review count of ER and HRH submissions 
      totalCount <- HRH_ER_merged %>%
        filter(year == max(year)) %>%
        filter(operating_unit == OU) %>%
        group_by(operating_unit, year, mech_code, mech_name) %>%
        summarise(reported_in_ER = sum(ER_expenditure_amt, na.rm = T),
                  reported_in_HRH = sum(HRH_expenditure_amt, na.rm = T)) %>%
        ungroup() %>%
        mutate(reported_in_ER = if_else(reported_in_ER > 0, "Yes", "No"),
               reported_in_HRH = if_else(reported_in_HRH > 0, "Yes", "No"),
               action_item = if_else(reported_in_ER == "Yes" & reported_in_HRH == "No", "ER staffing expenditures were reported, but HRH expenditures were NOT reported. Please review and confirm HRH report submission.","")) %>%
        arrange(mech_code)
      
      # Count the number of data quality errors for above site workers
      aboveSite <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
                        operating_unit == OU) %>%
        group_by(fiscal_year, operating_unit, mech_code, mech_name) %>%
        summarise(SD_aboveService_check = sum(site_level == "Above Site" & interaction_type == "Service Delivery", na.rm = T),
                  SD_PM_check = sum(site_level == "Program Management" & interaction_type == "Service Delivery", na.rm = T),
                  action_item = if_else(SD_aboveService_check > 0 | SD_PM_check > 0, "Staff were reported as 'Service Delivery' under Interaction Type even though they are primarily Program Management or Above Service Workers. Please double check that all PM or Above Service workers are reported as 'Non Service Delivery'", ""))
      
      # Count any errors on FTE and months worked
      FTE_monthsWorked <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU) %>%
        group_by(fiscal_year, operating_unit, mech_code, mech_name) %>%
        summarise(FTE_greaterThan_1 = sum(avg_fte_per_month > 1, na.rm = T),
                  months_greaterThan_12 = sum(months_of_work > 12, na.rm = T))
      
      # Count how many are based outside of OU and entered something in the geographic fields
      outsideOU <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU) %>%
        group_by(fiscal_year, operating_unit, mech_code, mech_name) %>%
        summarise(error_basedOutside = sum(is_outside_ou == "Yes" & 
                                              (is.na(snu1) == FALSE | 
                                                 psnu != "Data reported above PNSU Level" | 
                                                 community != "Data reported above Community Level" | 
                                                 facility != "Data reported above Facility level"), na.rm = T),
                  action_item = if_else(error_basedOutside > 0, "Please check that all reported staff based outside of OU have their geographic columns left as blank", ""))
      
      
      # Breakdown of employment titles for "Other Staff" 
      Breakdown_other <- HRH_data_orig %>%
        filter(fiscal_year == max(fiscal_year),
               operating_unit == OU) %>%
        group_by(fiscal_year, operating_unit, mech_code, mech_name) %>%
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
        group_by(fiscal_year, operating_unit, mech_code, mech_name) %>%
        summarise(other_employmentTitles = paste(unique(employment_title), collapse = ", "))
      
      Breakdown_other <- left_join(Breakdown_other, otherPull, by = c("fiscal_year", "operating_unit", "mech_code", "mech_name")) %>%
        mutate(action_item = if_else(pct_ofTotal > 20, "Over 20% of employment titles were reported under the `other` categories. Please review the employment titles identified in column I and determine if a more specific employment title can better reflect their roles/responsibilities", ""))
      
      
      ## Create a summary of ALL data flags for each mechanism
      
      # First, pull the mech_codes that trigger each of our data flags
      overall_missingHRH <- totalCount %>%
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_primeSub <- topLevel %>%
        group_by(year, mech_code, mech_name) %>%
        summarise(ER_expenditure_amt = sum(ER_expenditure_amt, na.rm = T),
                  HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T)) %>%
        ungroup() %>%
        mutate(pct_difference_fromER = (HRH_expenditure_amt - ER_expenditure_amt)/ER_expenditure_amt * 100,
               action_item = if_else(abs(pct_difference_fromER) > 10, "Reported HRH expenditures are different from ER staffing expenditures by at least 10%. Please review for closer alignment.", "")) %>%
        filter(action_item != "") %>%
        pull(mech_code)
      
      overall_prime <- topLevel %>%
        filter(prime_or_sub == "Prime",
               action_item != "") %>%
        pull(mech_code)
      
      overall_sub <- topLevel %>%
        filter(prime_or_sub == "Sub",
               action_item != "") %>%
        pull(mech_code)
      
      overall_SD <- SDvsNSD %>%
        filter(interaction_type == "Service Delivery",
               action_item != "") %>%
        pull(mech_code)
      
      overall_NSD <- SDvsNSD %>%
        filter(interaction_type == "Non Service Delivery",
               action_item != "") %>%
        pull(mech_code)
        
      overall_other <- Breakdown_other %>%
        filter(action_item != "") %>%
        pull(mech_code)
      
      # Build the overall summary table that outlines how many data quality flags were triggered for each mechanism
      
      overall_wide <- HRH_ER_merged %>%
        filter(year == max(year),
               operating_unit == OU) %>%
        distinct(year, operating_unit, mech_code, mech_name, prime_partner_name) %>%
        mutate(`Report was submitted for ER, but NOT for HRH` = if_else(mech_code %in% overall_missingHRH, "Yes", "No"),
               `HRH staffing expenditure (prime + sub) is different by > 10% of staffing expenditure reported to ER` = if_else(mech_code %in% overall_primeSub, "Yes", "No"),
               `PRIME staffing expenditure is different by > 10% of PRIME staffing expenditure reported to ER` = if_else(mech_code %in% overall_prime, "Yes", "No"),
               `SUB staffing expenditure is different by > 10% of SUB staffing expenditure reported to ER` = if_else(mech_code %in% overall_sub, "Yes", "No"),
        #       `SERVICE DELIVERY staffing expenditure is different by > 10% of SERVICE DELIVERY staffing expenditure reported to ER` = if_else(mech_code %in% overall_SD, "Yes", "No"),
        #       `NON SERVICE DELIVERY staffing expenditure is different by > 10% of NON SERVICE DELIVERY staffing expenditure reported to ER` = if_else(mech_code %in% overall_NSD, "Yes", "No"),
               `Staff reported under 'other' employment titles is > 20% of total staff` = if_else(mech_code %in% overall_other, "Yes", "No")) %>%
        arrange(mech_code) %>%
        filter()
      
      # Also build the overall summary table in long format
      overall_long <- overall_wide %>%
        gather(key = dataQuality_flag, value = Yes_or_No, 6:ncol(overall_wide)) %>%
        arrange(mech_code) %>%
        mutate(action_items = case_when(dataQuality_flag == "Report was submitted for ER, but NOT for HRH" & Yes_or_No == "Yes" ~ "ER staffing expenditures were reported, but HRH expenditures were NOT reported. Please ensure HRH report is completed, uploaded, and submitted in DATIM",
                                        dataQuality_flag == "HRH staffing expenditure (prime + sub) is different by > 10% of staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Total HRH expenditures were different from total ER staffing expenditures by at least 10%. Please review for closer alignment",
                                        dataQuality_flag == "PRIME staffing expenditure is different by > 10% of PRIME staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Prime HRH expenditures were different from Prime ER staffing expenditures by at least 10%. Please review for closer alignment",
                                        dataQuality_flag == "SUB staffing expenditure is different by > 10% of SUB staffing expenditure reported to ER" & Yes_or_No == "Yes" ~ "Sub HRH expenditures were different from Sub ER staffing expenditures by at least 10%. Please review for closer alignment",
                    #                    dataQuality_flag == "Is SERVICE DELIVERY staffing expenditure different by > 10% of SERVICE DELIVERY staffing expenditure reported to ER?" & Yes_or_No == "Yes" ~ "Service-Delivery HRH expenditures were different from Service-Delivery ER staffing expenditures by at least 10%. Please review for closer alignment",
                    #                    dataQuality_flag == "Is NON SERVICE DELIVERY staffing expenditure different by > 10% of NON SERVICE DELIVERY staffing expenditure reported to ER?" & Yes_or_No == "Yes" ~ "Non-Service-Delivery HRH expenditures were different from Non-Service-Delivery ER staffing expenditures by at least 10%. Please review for closer alignment",
                                        dataQuality_flag == "Staff reported under 'other' employment titles is > 20% of total staff" & Yes_or_No == "Yes" ~ "Over 20% of employment titles were reported under `other` categories. Please review all 'other' employment titles that were submitted, and determine whether a more specific employment title will better reflect their roles/responsibilities",
                                        TRUE ~ "")) %>%
        select(year, operating_unit, mech_code, mech_name, prime_partner_name, dataQuality_flag, Yes_or_No, action_items)
      
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
      
      ## --------------- Load each data frame into the Excel templates---------------
      
      # Import the excel reporting template to be used
      wb <- loadWorkbook("./1. Data/HRH_QC_Reporting_Template_20221118_vF - Deep Dive.xlsx")
      
      # Set workbook title
      wbTitle <- paste0(OU, " - ", "Data Quality Checks on Submitted HRH Reporting Templates") 
      
      # Load the data frames into each Excel sheet as needed
      writeData(wb, sheet = 1, wbTitle, startCol = 2, startRow = 3, colNames = FALSE)
      writeData(wb, sheet = 2, overall_long, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 3, topLevel, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 4, totalCount, startCol = 2, startRow = 4, colNames = FALSE)
      writeData(wb, sheet = 5, Breakdown_other, startCol = 2, startRow = 4, colNames = FALSE)

      # Establish the workbook name based on the OU
      wbName <- paste0("./4. Outputs/QC Reports/Deep dives/HRH Data Quality Checks - Deep Dive - ", OU, ".xlsx")
      
      # Export each QC report in Excel
      saveWorkbook(wb, wbName, overwrite = TRUE) 
      
      # print progress
      print(paste0(OU, " - workbook complete"))
}







