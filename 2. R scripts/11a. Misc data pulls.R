### GOAL: MISCELLANEOUS DATA PULLS BASED ON AD HOC DATA REQUESTS FROM GLOBAL AND OU TEAMS



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

# Load the .rds file for the clean FY22 HRH dataset
load(file = "./4. Outputs/RDS/FY22_cleanHRH.rds")



### ---------------------Data pulls for % of USAID PEPFAR staffing that is G2G------------

malawiG2G <- HRH_clean %>%
  filter(fiscal_year == 2022) %>%
  filter(operating_unit == "Malawi") %>%
  filter(funding_agency_fing == "USAID") %>%
  select(-G2G) %>%
  mutate(G2G = if_else(grepl("G2G", mech_name, ignore.case = FALSE) == TRUE, "Yes", "No")) %>%
  group_by(fiscal_year, operating_unit, funding_agency_fing, G2G) %>%
  summarise(USAID_PEPFAR_Staffing_Expenditure = sum(actual_annual_spend, na.rm = TRUE))

### -------------------Investigating reduction in post-clean CDC data-----------

post_cleanCDC <- HRH_clean %>%
  filter(funding_agency == "HHS/CDC")

pre_cleanCDC <- read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20221110.txt") %>%
  filter(funding_agency_fing == "HHS/CDC")

post_cleanCDC_breakdown <- post_cleanCDC %>%
  filter(fiscal_year == 2022) %>%
  group_by(fiscal_year, funding_agency, operating_unit, mech_code) %>%
  summarise(n_reported_staff = n(),
            actual_annual_spend = sum(actual_annual_spend, na.rm = T))

pre_cleanCDC_breakdown <- pre_cleanCDC %>%
  filter(fiscal_year == 2022) %>%
  rename(funding_agency = funding_agency_fing) %>%
  group_by(fiscal_year, funding_agency, operating_unit, mech_code) %>%
  summarise(n_reported_staff = n(),
            actual_annual_spend = sum(actual_annual_spend, na.rm = T))

comparison <- full_join(post_cleanCDC_breakdown, pre_cleanCDC_breakdown, by = c("fiscal_year", "funding_agency", "operating_unit", "mech_code"),
                        suffix = c(".POST", ".PRE")) 

comparison <- comparison %>%
  mutate(expenditure_difference = actual_annual_spend.POST - actual_annual_spend.PRE) %>%
  adorn_totals()

write_xlsx(comparison,"./4. Outputs/CDC_deepDive_PostVSClean.xlsx") 


### --------- Reviewing ER submissions for ER staffing expenditures only vs without ------------###

globalCheck_ER <- finalMerge %>%
  filter(year == 2022,
         #      HRH_relevant == "Y", # with ER expenditures only
         GHSC_UN_keywordflags == "FALSE"
  ) %>%
  mutate(ER_submission = if_else(ER_expenditure_amt > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(ER_submission = sum(ER_submission, na.rm = T)) %>%
  ungroup() %>%
  mutate(ER_submission = if_else(ER_submission > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency) %>%
  summarise(ER_submission = sum(ER_submission, na.rm = T)) 

globalCheck_HRH <- finalMerge %>%
  filter(year == 2022,
#         GHSC_UN_keywordflags == "FALSE"
  ) %>%
  mutate(HRH_submission = if_else(HRH_expenditure_amt > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(HRH_submission = sum(HRH_submission, na.rm = T)) %>%
  ungroup() %>%
  mutate(HRH_submission = if_else(HRH_submission > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency) %>%
  summarise(HRH_submission = sum(HRH_submission, na.rm = T)) 

# May need to re-calculate missing mechs variable.. with the GHSC_UN mechs already filtered out

missingHRH <- finalMerge %>%
  filter(year == 2022,
         GHSC_UN_keywordflags == "FALSE"
  ) %>%
  mutate(missingMechanisms_fromHRH = if_else(is.na(missingMechanisms_fromHRH) == FALSE, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(missingMechanisms_fromHRH = sum(missingMechanisms_fromHRH, na.rm = T)) %>%
  ungroup() %>%
  mutate(missingMechanisms_fromHRH = if_else(missingMechanisms_fromHRH > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency) %>%
  summarise(missingMechanisms_fromHRH = sum(missingMechanisms_fromHRH, na.rm = T)) 





missingPrime <- finalMerge %>%
  filter(year == 2022,
         HRH_relevant == "Y", # with ER expenditures only
         GHSC_UN_keywordflags == "FALSE",
         prime_or_sub == "Prime"
  ) %>%
  mutate(missingPrime = if_else(is.na(missingMechanisms_fromHRH) == FALSE, ER_expenditure_amt, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(missingPrime = sum(missingPrime, na.rm = T)) %>%
  ungroup() %>%
  group_by(year, funding_agency) %>%
  summarise(missingPrime = sum(missingPrime, na.rm = T))

missingSub <- finalMerge %>%
  filter(year == 2022,
         HRH_relevant == "Y", # with ER expenditures only
         GHSC_UN_keywordflags == "FALSE",
         prime_or_sub == "Sub"
  ) %>%
  mutate(missingSub = if_else(is.na(missingMechanisms_fromHRH) == FALSE, ER_expenditure_amt, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(missingSub = sum(missingSub, na.rm = T)) %>%
  ungroup() %>%
  group_by(year, funding_agency) %>%
  summarise(missingSub = sum(missingSub, na.rm = T))

# combine
merged_withoutERstaffing <- left_join(globalCheck_ER, globalCheck_HRH, by = c("year", "funding_agency"))
merged_withoutERstaffing <- left_join(merged_withoutERstaffing, missingHRH, by = c("year", "funding_agency"))
merged_withoutERstaffing <- left_join(merged_withoutERstaffing, missingPrime, by = c("year", "funding_agency"))
merged_withoutERstaffing <- left_join(merged_withoutERstaffing, missingSub, by = c("year", "funding_agency")) %>%
  arrange(desc(ER_submission)) %>%
  adorn_totals()

# Copy to clipboard
library(clipr)
clipr::write_clip(merged_withoutERstaffing) 

### Now calculate WITH ER staffing expenditures as the criteria!

# Replace the missingHRH variable with an updated version where we only count ER staffing submissions
missing_mechs <- HRH_ER_merged %>%
  group_by(year, mech_code) %>%
  summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T), # na.rm = TRUE just tells R to ignore the NA values when summing
            ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y"], na.rm = T)) # na.rm = TRUE just tells R to ignore the NA values when summing
missing_mechs <- missing_mechs %>%
  mutate(missingMechanisms_fromHRH = if_else((ER_expenditure_amt > 0) & (HRH_expenditure_amt == 0), 
                                             mech_code, NA_real_)) %>% 
  filter(!is.na(missingMechanisms_fromHRH)) %>%
  select(-ER_expenditure_amt, -HRH_expenditure_amt)
finalMerge2 <- finalMerge %>%
  select(-missingMechanisms_fromHRH)
finalMerge2 <- left_join(finalMerge2, missing_mechs, by = c("year", "mech_code"))

#Execute code again
globalCheck_ER <- finalMerge2 %>%
  filter(year == 2022,
         HRH_relevant == "Y", # with ER expenditures only
         GHSC_UN_keywordflags == "FALSE"
  ) %>%
  mutate(ER_submission = if_else(ER_expenditure_amt > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(ER_submission = sum(ER_submission, na.rm = T)) %>%
  ungroup() %>%
  mutate(ER_submission = if_else(ER_submission > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency) %>%
  summarise(ER_submission = sum(ER_submission, na.rm = T)) 

globalCheck_HRH <- finalMerge2 %>%
  filter(year == 2022,
         GHSC_UN_keywordflags == "FALSE"
  ) %>%
  mutate(HRH_submission = if_else(HRH_expenditure_amt > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(HRH_submission = sum(HRH_submission, na.rm = T)) %>%
  ungroup() %>%
  mutate(HRH_submission = if_else(HRH_submission > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency) %>%
  summarise(HRH_submission = sum(HRH_submission, na.rm = T)) 

missingHRH <- finalMerge2 %>%
  filter(year == 2022,
         GHSC_UN_keywordflags == "FALSE"
  ) %>%
  mutate(missingMechanisms_fromHRH = if_else(is.na(missingMechanisms_fromHRH) == FALSE, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(missingMechanisms_fromHRH = sum(missingMechanisms_fromHRH, na.rm = T)) %>%
  ungroup() %>%
  mutate(missingMechanisms_fromHRH = if_else(missingMechanisms_fromHRH > 0, 1, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency) %>%
  summarise(missingMechanisms_fromHRH = sum(missingMechanisms_fromHRH, na.rm = T)) 

missingPrime <- finalMerge2 %>%
  filter(year == 2022,
         HRH_relevant == "Y", # with ER expenditures only
         GHSC_UN_keywordflags == "FALSE",
         prime_or_sub == "Prime"
  ) %>%
  mutate(missingPrime = if_else(is.na(missingMechanisms_fromHRH) == FALSE, ER_expenditure_amt, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(missingPrime = sum(missingPrime, na.rm = T)) %>%
  ungroup() %>%
  group_by(year, funding_agency) %>%
  summarise(missingPrime = sum(missingPrime, na.rm = T))

missingSub <- finalMerge2 %>%
  filter(year == 2022,
         HRH_relevant == "Y", # with ER expenditures only
         GHSC_UN_keywordflags == "FALSE",
         prime_or_sub == "Sub"
  ) %>%
  mutate(missingSub = if_else(is.na(missingMechanisms_fromHRH) == FALSE, ER_expenditure_amt, as.numeric(NA_real_))) %>%
  group_by(year, funding_agency, mech_code) %>%
  summarise(missingSub = sum(missingSub, na.rm = T)) %>%
  ungroup() %>%
  group_by(year, funding_agency) %>%
  summarise(missingSub = sum(missingSub, na.rm = T))

# combine
merged_withERstaffing <- left_join(globalCheck_ER, globalCheck_HRH, by = c("year", "funding_agency"))
merged_withERstaffing <- left_join(merged_withERstaffing, missingHRH, by = c("year", "funding_agency"))
merged_withERstaffing <- left_join(merged_withERstaffing, missingPrime, by = c("year", "funding_agency"))
merged_withERstaffing <- left_join(merged_withERstaffing, missingSub, by = c("year", "funding_agency")) %>%
  arrange(desc(ER_submission)) %>%
  adorn_totals()

# Copy to clipboard
library(clipr)
clipr::write_clip(merged_withERstaffing) 


#### ---------- Tanzania data checks ----------

TZcheck <- finalMerge %>%
  filter(year == 2022,
         GHSC_UN_keywordflags == "FALSE",
         operating_unit == "Tanzania") %>%
  group_by(year, operating_unit, funding_agency, mech_code, prime_partner_name) %>%
  summarise(HRH_expenditure_amt = sum(HRH_expenditure_amt, na.rm = T),
            ER_expenditure_amt = sum(ER_expenditure_amt[HRH_relevant == "Y"], na.rm = T))

TZcheck <- HRH_clean %>%
  filter(fiscal_year == 2022,
         GHSC_UN_keywordflags == "FALSE",
         operating_unit == "Tanzania") %>%
  group_by(fiscal_year, operating_unit, funding_agency) %>%
  summarise(actual_annual_spend = sum(actual_annual_spend, na.rm = T),
            individual_count = sum(individual_count, na.rm = T),
            annual_fte = sum(annual_fte, na.rm = T)) %>%
  adorn_totals()

### ----------- 

#Lesotho supply chain investigating. If 50/50, which ones they want to use. Write DREAMS in comments if any % DREAMS. 
#It's like program area - not perfect. They will have to select one. If 50/50, put OVC and then type DREAMS

# Do the Rwanda data, and then supply chain data. Email to all HRH POC's

Lesotho_GHSC <- HRH_clean %>%
  filter(fiscal_year == 2022,
         funding_agency == "USAID",
         operating_unit == "Lesotho", 
         GHSC_UN_keywordflags == "TRUE")


#### --------- Find FY23 submission rates for Oct 30 2023 using last year's total submissions as denominator

### NOTE: PLEASE FILTER FOR USAID ONLY, AND FOR SUCCESSFUL SUBMISSIONS ONLY, AND DELETE THE FIRST TWO ROWS
FY23_submissions <- read_excel("./1. Data/HRH Inventory Monitoring Dossier (FY2023).xlsx") %>%
  clean_names() %>%
  rename("mech_name" = "x5")

# Calculate unique submissions by OU in FY23 submissions df
FY23_num <- FY23_submissions %>%
  group_by(agency, operating_unit) %>%
  tally()

# Calculate unique submissions by OU (for USAID only and for successful submissions only) in FY22 HRH clean df
FY22_den <- HRH_clean %>%
  filter(funding_agency == "USAID",
         fiscal_year == 2022) %>%
  group_by(funding_agency, operating_unit, mech_code) %>%
  tally() %>%
  ungroup() %>%
  group_by(funding_agency, operating_unit) %>%
  tally() %>%
  rename("agency" = "funding_agency")

# Merge the two datasets
merged <- full_join(FY23_num, FY22_den, by = "operating_unit") %>%
  select(-agency.x, -agency.y) %>%
  rename("FY23_uploads_and_submissions" = "n.x",
         "FY22_total_mech_submissions" = "n.y") 

merged <- replace(merged,is.na(merged),0) %>%
  mutate(submission_rate = FY23_uploads_and_submissions / FY22_total_mech_submissions)

#Export the file
write_xlsx(merged,"./4. Outputs/FY23_submissions_update.xlsx") 


### -------- Pull a list of mech codes where OVC_SERV > 0 for 2023 within USAID

#Import 2023 MER targets
rawMER <- read.delim("./1. Data/MER_Structured_Datasets_PSNU_IM_FY20-23_20221216_v2_1.txt", header = TRUE, stringsAsFactors = FALSE)
#load("./4. Outputs/RDS/FY23_rawMER.rds")

## 3. Do some data cleaning
cleanMER <- rawMER %>%
  
  # Filter for only the relevant years
  filter(fiscal_year == 2023) %>%
  
  # Only select relevant indicators for now
  filter(grepl("^OVC", indicator)) %>%
  
  # Select only the relevant columns
  select(-operatingunituid, -snu1uid, -psnuuid, - snuprioritization, -typemilitary, prime_partner_duns, -prime_partner_uei,
         -award_number, -otherdisaggregate, -otherdisaggregate_sub, -statustb, -statuscx, -hiv_treatment_status,
         -starts_with("qtr"), -source_name) %>% 
  
  # Filter for only relevant disaggregates
  filter(standardizeddisaggregate == "Total Numerator")

## Disaggregate further
cleanMER <- cleanMER %>%
  group_by(fiscal_year, operatingunit, snu1, psnu, prime_partner_name, funding_agency, mech_code, mech_name, indicator, standardizeddisaggregate) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE),
            targets = sum(targets, na.rm = TRUE))

# Set all MER indicator columns as numeric
cleanMER[, grepl("^OVC", names(cleanMER))] <- lapply(cleanMER[, grepl("^OVC", names(cleanMER))], as.numeric)

# Filter for OVC only
cleanMER <- cleanMER %>%
  filter(indicator == "OVC_SERV",
         targets > 0)

# Create a summary list of mech codes with OVC_SERV > 0 
OVC_mechs <- cleanMER %>%
  group_by(fiscal_year, funding_agency, indicator, operatingunit, prime_partner_name, mech_code, mech_name) %>%
  tally()

#Export the file
write_xlsx(OVC_mechs,"./4. Outputs/FY23_OVC_mechs.xlsx") 





