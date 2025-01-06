### PURPOSE: Deeper dive review of HRH Inventory data quality
### Code developed by: Kyle Borces
### Last Updated: Nov 3, 2024

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(openxlsx)
library(tibble)


### ------------------ Import the needed files for HRH, ER, and HRH-ER datasets -------- ###
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-24_20241213.txt") # read in FSD dataset
load(file = "./4. Outputs/RDS/FY24_cleanHRH.rds") # cleaned HRH dataset
load(file = "./4. Outputs/RDS/HRH_ER_merged_21_24.rds") # HRH-ER merged dataset
OVC_mechs <- read_excel("./4. Outputs/FY24_OVC_mechs.xlsx")
FY24_budget <- read.delim("./1. Data/Comprehensive_Budget_Datasets_COP17-24_20241115.txt", header = TRUE, stringsAsFactors = FALSE)
G2Gs <- read_excel("./1. Data/FY24 PEPFAR G2G Mechanisms.xlsx")


###---------------------Areas to review-------------------------------------###

# Global submission rate calculation
submission_rate <- finalMerge %>%
  filter(year == 2024,
         funding_agency == "USAID") %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T), 
            ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y" | 
                                                        sub_cost_category == "Other Contracts" | 
                                                        sub_cost_category == "Contracted Interventions"], na.rm = T)) %>% # 
  ungroup() %>%
  filter(HRH_expenditure_amt > 0 | ER_expenditure_amt > 0) %>%
  group_by(year, funding_agency) %>%
  summarise(number_of_mechs_withStaffing = n(),
            number_of_HRH_submissions = sum(HRH_expenditure_amt > 0)) %>%
  ungroup() %>%
  mutate(global_submission_rate = number_of_HRH_submissions / number_of_mechs_withStaffing)


### ----------- G2G data pull --------------

# Number and percentage of all FTE that are under government agency and government parastatal mechanisms (we want these two G2G types disaggregated)

G2G_count <- HRH_clean %>%
  filter(fiscal_year == 2024,
         funding_agency == "USAID") %>%
  group_by(fiscal_year, funding_agency, operating_unit, g2g_partner_type) %>%
  summarise(annual_fte = sum(annual_fte, na.rm = T)) %>%
  mutate(g2g_partner_type = if_else(is.na(g2g_partner_type), "Non-G2G", g2g_partner_type)) %>%
  mutate(g2g_partner_type = case_when(g2g_partner_type == "Government Agency" ~ "G2G - Government Agency",
                                      g2g_partner_type == "Parastatal" ~ "G2G - Parastatal",
                                      TRUE ~ g2g_partner_type)) %>%
  adorn_totals()

write_xlsx(G2G_count,"./4. Outputs/FY24_G2G_VS_nonG2G_byOU.xlsx") 

# Number and percentage of all FTE that are government staff (MoH), other government staff, and seconded to government

seconded_count <- HRH_clean %>%
  filter(fiscal_year == 2024,
         funding_agency == "USAID") %>%
  group_by(fiscal_year, funding_agency, operating_unit, g2g_partner_type, moh.secondment) %>%
  summarise(annual_fte = sum(annual_fte, na.rm = T)) %>%
  mutate(g2g_partner_type = if_else(is.na(g2g_partner_type), "Non-G2G", g2g_partner_type)) %>%
  mutate(g2g_partner_type = case_when(g2g_partner_type == "Government Agency" ~ "G2G - Government Agency",
                                      g2g_partner_type == "Parastatal" ~ "G2G - Parastatal",
                                      TRUE ~ g2g_partner_type)) %>%
  adorn_totals()

write_xlsx(seconded_count,"./4. Outputs/FY24_G2G_VS_nonG2G_seconded.xlsx") 

# G2G vs non-G2G submission rate

G2Gs <- read_excel("./1. Data/FY24 PEPFAR G2G Mechanisms.xlsx")
G2G_mechs <- G2Gs %>%
  filter(`Funding Agency (group)` == "USAID") %>%
  rename(mech_code = `Mechanism ID`) %>%
  select(mech_code) %>%
  pull(mech_code)

G2G_submission_rate <- finalMerge %>%
  filter(year == 2024,
         funding_agency == "USAID",
         mech_code %in% G2G_mechs) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T), 
            ER_staffing_expenditure_amt = sum(ER_expenditure_amt, na.rm = T)) %>% # NOTE: SUMMING ER STAFFING EXPENDITURES ONLY
  ungroup() %>%
  filter(HRH_expenditure_amt > 0 | ER_staffing_expenditure_amt > 0) %>%
  group_by(year, funding_agency) %>%
  summarise(number_of_mechs_withStaffing = n(),
            number_of_HRH_submissions = sum(HRH_expenditure_amt > 0)) %>%
  ungroup() %>%
  mutate(global_submission_rate = number_of_HRH_submissions / number_of_mechs_withStaffing)

#63% submission rate for G2G

Non_G2G_submission_rate <- finalMerge %>%
  filter(year == 2024,
            funding_agency == "USAID",
         !mech_code %in% G2G_mechs) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T), 
            ER_staffing_expenditure_amt = sum(ER_expenditure_amt, na.rm = T)) %>% # NOTE: SUMMING ER STAFFING EXPENDITURES ONLY
  ungroup() %>%
  filter(HRH_expenditure_amt > 0 | ER_staffing_expenditure_amt > 0) %>%
  group_by(year, funding_agency) %>%
  summarise(number_of_mechs_withStaffing = n(),
            number_of_HRH_submissions = sum(HRH_expenditure_amt > 0)) %>%
  ungroup() %>%
  mutate(global_submission_rate = number_of_HRH_submissions / number_of_mechs_withStaffing)

#92% submission rate for non-G2Gs

### --------------------- Ranking of countries and mechs with high seconded/gov staff-------------------

promising_g2g_OUs <- HRH_clean %>%
  filter(fiscal_year == 2024) %>%
  mutate(G2G_tiers = case_when(operating_unit == "Zambia" |
                               operating_unit == "South Africa" |
                               operating_unit == "Mozambique" |
                               operating_unit == "Uganda" |
                               operating_unit == "Tanzania" |
                               operating_unit == "Nigeria" |
                               operating_unit == "Kenya" |
                               operating_unit == "Malawi" ~ "Tier 1",
                                  operating_unit == "Cote d'Ivoire" |
                                  operating_unit == "Lesotho" |
                                  operating_unit == "Botswana" |
                                  operating_unit == "Dominican Republic" ~ "Tier 2",
                                      operating_unit == "Zimbabwe" |
                                      operating_unit == "Ethiopia" |
                                      operating_unit == "Rwanda" |
                                      operating_unit == "Vietnam" ~ "Tier 3",
                               TRUE ~ "NA")) %>%
  mutate(government_equivalent_staff = case_when(cadre == "Social Work and Case Management" | 
                                                 cadre == "Medical" |
                                                 cadre == "Nursing / Midwifery" |
                                                 cadre == "Laboratory" |
                                                 cadre == "Pharmacy" |
                                                 cadre == "Mental Health Staff" ~ "Y",
                                                 TRUE ~ "N"))

promising_g2g_OUs_summary_country <- promising_g2g_OUs %>%
  filter(funding_agency == "USAID") %>%
  group_by(fiscal_year, funding_agency, operating_unit, G2G_tiers) %>%
  summarise(Total_number_of_staff = sum(annual_fte, na.rm = T),
            Number_of_service_delivery_staff = sum(annual_fte[interaction_type == "Direct Service Delivery"], na.rm = T),
            Number_of_gov_seconded_staff = sum(annual_fte[moh.secondment != "No"], na.rm = T)) %>%
  filter(!G2G_tiers == "NA") %>%
  ungroup() %>%
  mutate(pct_serviceDelivery_staff = Number_of_service_delivery_staff / Total_number_of_staff,
         pct_gov_seconded_staff = Number_of_gov_seconded_staff / Total_number_of_staff) %>%
  arrange(G2G_tiers)

promising_g2g_OUs_summary_mechs <- promising_g2g_OUs %>%
  filter(funding_agency == "USAID") %>%
  group_by(fiscal_year, funding_agency, operating_unit, G2G_tiers, mech_code, mech_name, prime_partner_name) %>%
  summarise(Total_number_of_staff = sum(annual_fte, na.rm = T),
            Number_of_service_delivery_staff = sum(annual_fte[interaction_type == "Direct Service Delivery"], na.rm = T),
            Number_of_gov_seconded_staff = sum(annual_fte[moh.secondment != "No"], na.rm = T)) %>%
  filter(!G2G_tiers == "NA") %>%
  ungroup() %>%
  mutate(pct_serviceDelivery_staff = Number_of_service_delivery_staff / Total_number_of_staff,
         pct_gov_seconded_staff = Number_of_gov_seconded_staff / Total_number_of_staff) %>%
  arrange(G2G_tiers) %>%
  filter(!pct_serviceDelivery_staff == 0) %>%
  filter(!pct_gov_seconded_staff == 0)

write_xlsx(promising_g2g_OUs_summary_country,"./4. Outputs/Top_OUs_mechs_forG2Gs_byOU_20241215.xlsx") 
write_xlsx(promising_g2g_OUs_summary_mechs,"./4. Outputs/Top_OUs_mechs_forG2Gs_byMechs_20241215.xlsx")

# secondment breakdown
seconded_count <- HRH_clean %>%
  filter(fiscal_year == 2024,
         funding_agency == "USAID") %>%
  group_by(fiscal_year, funding_agency, operating_unit, moh.secondment) %>%
  summarise(annual_fte = sum(annual_fte, na.rm = T)) %>%
  adorn_totals()

write_xlsx(seconded_count,"./4. Outputs/FY24_gov_seconded_staff_byOU.xlsx") 