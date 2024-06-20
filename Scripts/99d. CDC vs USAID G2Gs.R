#### Purpose of this script is to examine average staffing profile of CDC vs USAID G2G mechanisms

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)

# Load the FY23 data
load(file = "./4. Outputs/RDS/FY23_cleanHRH.rds")
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-23_20231215.txt")

# Create CDC list and USAID list of mech codes for later use
CDC_mechs <- HRH_clean %>%
  filter(fiscal_year == 2023,
         funding_agency == "CDC") %>%
  mutate(dedup = duplicated(mech_code)) %>%
  filter(dedup == "FALSE") %>%
  pull(mech_code)

USAID_mechs <- HRH_clean %>%
  filter(fiscal_year == 2023,
         funding_agency == "USAID") %>%
  mutate(dedup = duplicated(mech_code)) %>%
  filter(dedup == "FALSE") %>%
  pull(mech_code)

##### ------------- Create median profiles of CDC vs USAID G2G mechanisms -------------

## Avg individual count
CDC_count <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_fte = sum(annual_fte, na.rm = T)) %>%
  ungroup() %>%
  group_by(funding_agency) %>%
  summarise(avg_fte = mean(sum_fte, na.rm = T))
# CONCLUSION: CDC G2Gs are almost double the staff size compared to a USAID G2G

## Avg HRH expenditure size
CDC_expenditure <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_expenditure = sum(actual_annual_spend, na.rm = T)) %>%
  ungroup() %>%
  group_by(funding_agency) %>%
  summarise(avg_expenditure = mean(sum_expenditure, na.rm = T))
# CONCLUSION: CDC G2Gs spend three times as much on staffing compared to USAID G2G

## Look into HRH as % of total G2G expenditure
G2G_mechs <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  mutate(mech_code_dedup = duplicated(mech_code)) %>%
  filter(mech_code_dedup == FALSE) %>%
  pull(mech_code)

ER_G2Gs <- fin_data_orig %>%
  filter(implementation_year == 2023,
         mech_code %in% G2G_mechs) %>%
  group_by(mech_code) %>%
  summarise(expenditure_amt = sum(expenditure_amt, na.rm = T))

HRH_G2Gs <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(actual_annual_spend = sum(actual_annual_spend, na.rm = T))

merged_G2G <- left_join(HRH_G2Gs, ER_G2Gs, by = "mech_code") %>%
  mutate(pct_of_total = actual_annual_spend / expenditure_amt) %>%
  group_by(funding_agency) %>%
  summarise(avg_pct_of_total = mean(pct_of_total, na.rm = T))

# CONCLUSION: On median, CDC G2Gs spends about 42% on HRH while USAID G2Gs spend about 44% on HRH - very similar levels. Nothing going on here

## Average staffing mix 
CDC_mix_expenditure <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code, er_category) %>%
  summarise(sum_expenditure = sum(actual_annual_spend, na.rm = T)) 

CDC_mix_expenditure_total <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_expenditure_total = sum(actual_annual_spend, na.rm = T))

CDC_mix_expenditure <- left_join(CDC_mix_expenditure, CDC_mix_expenditure_total, by = c("mech_code", "funding_agency")) %>%
  mutate(pct_of_total = sum_expenditure / sum_expenditure_total) 

join_df <- expand.grid(mech_code = unique(CDC_mix_expenditure$mech_code), # Create empty data frame to paste the zero values into, and then merge
                       er_category = c("HCW: Ancillary", "HCW: Clinical", "Other Staff", "Program Management"))

CDC_mix_expenditure <- left_join(join_df, CDC_mix_expenditure, by =c("mech_code", "er_category")) # Use the mech code lists to fill in CDC or USAID mech codes
CDC_mix_expenditure <- CDC_mix_expenditure %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", funding_agency),
         funding_agency = if_else(mech_code %in% USAID_mechs, "USAID", funding_agency))

#CDC_mix_expenditure[is.na(CDC_mix_expenditure)] <- 0 # now set all NA values to zero

CDC_mix_expenditure <- CDC_mix_expenditure %>% # Now find the average % of total
  group_by(funding_agency, er_category) %>%
  summarise(avg_expenditure = mean(sum_expenditure, na.rm = T))

# fte
CDC_mix_fte <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code, er_category) %>%
  summarise(sum_fte = sum(annual_fte, na.rm = T)) 

CDC_mix_fte_total <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_fte_total = sum(annual_fte, na.rm = T))

CDC_mix_fte <- left_join(CDC_mix_fte, CDC_mix_fte_total, by = c("mech_code", "funding_agency")) %>%
  mutate(pct_of_total = sum_fte / sum_fte_total) 

join_df <- expand.grid(mech_code = unique(CDC_mix_fte$mech_code), # Create empty data frame to paste the zero values into, and then merge
                       er_category = c("HCW: Ancillary", "HCW: Clinical", "Other Staff", "Program Management"))

CDC_mix_fte <- left_join(join_df, CDC_mix_fte, by =c("mech_code", "er_category")) # Use the mech code lists to fill in CDC or USAID mech codes
CDC_mix_fte <- CDC_mix_fte %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", funding_agency),
         funding_agency = if_else(mech_code %in% USAID_mechs, "USAID", funding_agency))

#CDC_mix_fte[is.na(CDC_mix_fte)] <- 0 # now set all NA values to zero

CDC_mix_fte <- CDC_mix_fte %>% # Now find the average % of total
  group_by(funding_agency, er_category) %>%
  summarise(avg_fte = mean(sum_fte, na.rm = T))

## Avg SD vs NSD split - using expenditure

CDC_SD_NSD_expenditure <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code, interaction_type) %>%
  summarise(sum_expenditure = sum(actual_annual_spend, na.rm = T)) 

CDC_SD_NSD_expenditure_total <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_expenditure_total = sum(actual_annual_spend, na.rm = T))

CDC_SD_NSD_expenditure <- left_join(CDC_SD_NSD_expenditure, CDC_SD_NSD_expenditure_total, by = c("mech_code", "funding_agency")) %>%
  mutate(pct_of_total = sum_expenditure / sum_expenditure_total) 

join_df <- expand.grid(mech_code = unique(CDC_SD_NSD_expenditure$mech_code), # Create empty data frame to paste the zero values into, and then merge
                       interaction_type = c("Direct Service Delivery", "Non-Service Delivery"))

CDC_SD_NSD_expenditure <- left_join(join_df, CDC_SD_NSD_expenditure, by =c("mech_code", "interaction_type")) # Use the mech code lists to fill in CDC or USAID mech codes
CDC_SD_NSD_expenditure <- CDC_SD_NSD_expenditure %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", funding_agency),
         funding_agency = if_else(mech_code %in% USAID_mechs, "USAID", funding_agency))

#CDC_SD_NSD_expenditure[is.na(CDC_SD_NSD_expenditure)] <- 0 # now set all NA values to zero

CDC_SD_NSD_expenditure <- CDC_SD_NSD_expenditure %>% # Now find the average % of total
#  select(-sum_expenditure, -sum_expenditure_total) %>%
  group_by(funding_agency, interaction_type) %>%
  summarise(avg_expenditure = mean(sum_expenditure, na.rm = T))

# fte

CDC_SD_NSD_fte <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code, interaction_type) %>%
  summarise(sum_fte = sum(annual_fte, na.rm = T)) 

CDC_SD_NSD_fte_total <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_fte_total = sum(annual_fte, na.rm = T))

CDC_SD_NSD_fte <- left_join(CDC_SD_NSD_fte, CDC_SD_NSD_fte_total, by = c("mech_code", "funding_agency")) %>%
  mutate(pct_of_total = sum_fte / sum_fte_total) 

join_df <- expand.grid(mech_code = unique(CDC_SD_NSD_fte$mech_code), # Create empty data frame to paste the zero values into, and then merge
                       interaction_type = c("Direct Service Delivery", "Non-Service Delivery"))

CDC_SD_NSD_fte <- left_join(join_df, CDC_SD_NSD_fte, by =c("mech_code", "interaction_type")) # Use the mech code lists to fill in CDC or USAID mech codes
CDC_SD_NSD_fte <- CDC_SD_NSD_fte %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", funding_agency),
         funding_agency = if_else(mech_code %in% USAID_mechs, "USAID", funding_agency))

#CDC_SD_NSD_fte[is.na(CDC_SD_NSD_fte)] <- 0 # now set all NA values to zero

CDC_SD_NSD_fte <- CDC_SD_NSD_fte %>% # Now find the average % of total
#  select(-sum_fte, -sum_fte_total) %>%
  group_by(funding_agency, interaction_type) %>%
  summarise(avg_fte = mean(sum_fte, na.rm = T))

# Secondment status of PM staff

PM_secondment_expenditure <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency",
         er_category == "Program Management") %>%
  group_by(funding_agency, mech_code, moh.secondment) %>%
  summarise(sum_expenditure = sum(actual_annual_spend, na.rm = T)) 

PM_secondment_expenditure_total <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency",
         er_category == "Program Management") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_expenditure_total = sum(actual_annual_spend, na.rm = T))

PM_secondment_expenditure <- left_join(PM_secondment_expenditure, PM_secondment_expenditure_total, by = c("mech_code", "funding_agency")) %>%
  mutate(pct_of_total = sum_expenditure / sum_expenditure_total) 

join_df <- expand.grid(mech_code = unique(PM_secondment_expenditure$mech_code), # Create empty data frame to paste the zero values into, and then merge
                       moh.secondment = c("Yes-Moh Staff", "Yes-Seconded To Moh", "No"))

PM_secondment_expenditure <- left_join(join_df, PM_secondment_expenditure, by =c("mech_code", "moh.secondment")) # Use the mech code lists to fill in CDC or USAID mech codes
PM_secondment_expenditure <- PM_secondment_expenditure %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", funding_agency),
         funding_agency = if_else(mech_code %in% USAID_mechs, "USAID", funding_agency))

PM_secondment_expenditure[is.na(PM_secondment_expenditure)] <- 0 # now set all NA values to zero

PM_secondment_expenditure <- PM_secondment_expenditure %>% # Now find the average % of total
  select(-sum_expenditure, -sum_expenditure_total) %>%
  group_by(funding_agency, moh.secondment) %>%
  summarise(avg_pct_of_total = mean(pct_of_total, na.rm = T))



# Create summary of mechanism level ER expenditure totals, merge the HRH expenditure totals,
## then subtract the HRH total from ER expenditure totals. 
## Finally, rbind the ER table to the HRH dataset using dummy variables
ER_G2Gs <- fin_data_orig %>%
  filter(implementation_year == 2023,
         mech_code %in% G2G_mechs) %>%
  group_by(implementation_year, operatingunit, mech_code) %>%
  summarise(expenditure_amt = sum(expenditure_amt, na.rm = T)) %>%
  rename(fiscal_year = implementation_year,
         operating_unit = operatingunit)

HRH_G2Gs <- HRH_clean %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(fiscal_year, operating_unit, mech_code) %>%
  summarise(actual_annual_spend = sum(actual_annual_spend, na.rm = T))

merged_G2G <- left_join(ER_G2Gs, HRH_G2Gs, by = c("fiscal_year", "operating_unit", "mech_code")) %>%
  rename(ER_expenditure_amt = expenditure_amt,
         HRH_expenditure_amt = actual_annual_spend) %>%
  mutate(ER_topline = ER_expenditure_amt - HRH_expenditure_amt) %>%
  mutate(g2g_partner_type = "Government Agency") %>%
  select(-ER_expenditure_amt, -HRH_expenditure_amt) %>%
  rename(actual_annual_spend = ER_topline) %>%
  mutate(ER_topline = "TRUE") %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", "USAID"))

HRH_ERtopline <- rbind.fill(HRH_clean, merged_G2G) %>%
  mutate(er_category = if_else(is.na(er_category), "non-HRH", er_category)) %>%
  

write.csv(HRH_ERtopline, "./1. Data/HRH_ER_topline_G2Gs_20240424.csv", row.names=FALSE)

# ER Topline analysis

## Average staffing mix 
topline_exp <- HRH_ERtopline %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code, er_category) %>%
  summarise(sum_expenditure = sum(actual_annual_spend, na.rm = T)) 

topline_exp_total <- HRH_ERtopline %>%
  filter(fiscal_year == 2023,
         g2g_partner_type == "Government Agency") %>%
  group_by(funding_agency, mech_code) %>%
  summarise(sum_expenditure_total = sum(actual_annual_spend, na.rm = T))

topline_exp <- left_join(topline_exp, topline_exp_total, by = c("mech_code", "funding_agency")) %>%
  mutate(pct_of_total = sum_expenditure / sum_expenditure_total) 

join_df <- expand.grid(mech_code = unique(topline_exp$mech_code), # Create empty data frame to paste the zero values into, and then merge
                       er_category = c("HCW: Ancillary", "HCW: Clinical", "Other Staff", "Program Management", "non-HRH"))

topline_exp <- left_join(join_df, topline_exp, by =c("mech_code", "er_category")) # Use the mech code lists to fill in CDC or USAID mech codes
topline_exp <- topline_exp %>%
  mutate(funding_agency = if_else(mech_code %in% CDC_mechs, "CDC", funding_agency),
         funding_agency = if_else(mech_code %in% USAID_mechs, "USAID", funding_agency))

topline_exp[is.na(topline_exp)] <- 0

topline_exp_avg <- topline_exp %>% # Now find the average % of total
  group_by(funding_agency, er_category) %>%
  summarise(avg_pct_of_total = mean(pct_of_total, na.rm = T))


# Average g2g expenditure by agency
avg_g2g <- topline_exp %>%
  group_by(funding_agency, mech_code) %>%
  summarise(total_expenditure = sum(sum_expenditure, na.rm = T)) %>%
  group_by(funding_agency) %>%
  summarise(avg_expenditure = mean(total_expenditure, na.rm = T))


####  For countries with G2G mecanisms, what % of their HRH is going to G2G mechs on average?
G2G_OUs <- HRH_clean %>%
  filter(fiscal_year == 2023) %>%
  filter(funding_agency != "Other agencies") %>%
  group_by(operating_unit, g2g_partner_type) %>%
  summarise(HRH_G2Gs = sum(individual_count, na.rm = T)) %>%
  filter(g2g_partner_type == "Government Agency") %>%
  pull(operating_unit)

G2G_totals <- HRH_clean %>% # for later use
  filter(fiscal_year == 2023) %>%
  filter(funding_agency != "Other agencies") %>%
  filter(g2g_partner_type == "Government Agency" | is.na(g2g_partner_type) == TRUE) %>%
  filter(operating_unit %in% G2G_OUs) %>%
  group_by(operating_unit,funding_agency) %>%
  summarise(HRH_G2Gs = sum(individual_count, na.rm = T)) 

HRH_asPCT_ofG2G <- HRH_clean %>%
  filter(fiscal_year == 2023) %>%
  filter(g2g_partner_type == "Government Agency" | is.na(g2g_partner_type) == TRUE) %>%
  filter(operating_unit %in% G2G_OUs) %>%
  filter(funding_agency != "Other agencies") %>%
  group_by(fiscal_year, funding_agency,operating_unit, g2g_partner_type) %>%
  summarise(G2G_totalHRH = sum(individual_count, na.rm = T)) %>%
  mutate(g2g_partner_type = if_else(is.na(g2g_partner_type) == TRUE, "Non-G2G", g2g_partner_type))

HRH_asPCT_ofG2G <- left_join(HRH_asPCT_ofG2G, G2G_totals, by = c("operating_unit", "funding_agency")) %>%
  mutate(pct_of_total = G2G_totalHRH / HRH_G2Gs)

avg_pct_HRH <- HRH_asPCT_ofG2G %>%
  group_by(fiscal_year, funding_agency, g2g_partner_type) %>%
  summarise(avg_pct_HRH = mean(pct_of_total, na.rm = T))

