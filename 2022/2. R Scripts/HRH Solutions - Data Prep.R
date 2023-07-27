### THE PURPOSE OF THIS SCRIPT IS TO CONVERT THE DATA PREPARATION PROCESS FROM 
###     TABLEAU PREP TO R FOR HRH SOLUTIONS

## Load relevant libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)
library(sqldf)
library(janitor)
library(clipr)
library(openxlsx)


##### ENTER A) THE OPERATING UNIT; B) THE FUNDING AGENCY OF INTEREST (i.e. "USAID", "CDC", or "All"). 
##### ADJUST YEARS IF NEEDED

filterOU <- "Uganda"
filterAgency <- "All" # All or USAID
Year <- 2022 # Update this so that Year == 2023 when filtering the MER DATASET!!
COP_Planning_Year <- "COP 23"
Target_Scenario <- "FY 23"


### -----------------Import the HRH and MER datasets---------------------

# Load the raw HRH dataset (we do not do want any filters on GHSC or UN mechanisms)
HRH_data_orig <- read.delim("./Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20221216.txt") 
rawMER <- read.delim("./Data/MER_Structured_Datasets_PSNU_IM_FY20-23_20221216_v2_1.txt", header = TRUE, stringsAsFactors = FALSE)

## -----------------Data cleaning the HRH dataset---------------

# If the operating unit is regional, psnuuid = snu1uid
HRH_clean <- HRH_data_orig %>%
  mutate(psnuuid = case_when(operating_unit == "Asia Region" | operating_unit == "West Africa Region" | operating_unit == "Western Hemisphere Region" ~ snu1uid,
                             TRUE ~ psnuuid))

# Group up the funding agencies
HRH_clean <- HRH_clean %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" | funding_agency == "USAID/WCF" ~ "USAID",
                                         funding_agency == "HHS/CDC" ~ "CDC",
                                         TRUE ~ "Other agencies"))

# Rename columns to match FY21 column names so that it automatically updates in Tableau
HRH_clean <- HRH_clean %>%
  rename(is_multi_site = roving,
         moh.secondment = moh.secondment,
         annual_expenditure = actual_salary_expenditure,
         annual_fringe = actual_fringe_expenditure)

# Rename the SD vs NSD column to be consistent in both years
HRH_clean <- HRH_clean %>%
  mutate(interaction_type = case_when(interaction_type == "Non-Service Delivery" ~ "Non Service Delivery",
                                      interaction_type == "Direct Service Delivery" ~ "Service Delivery",
                                      TRUE ~ interaction_type))

# Rename IP program management to program management
HRH_clean <- HRH_clean %>%
  mutate(er_category = if_else(er_category == "Implementing Mechanism Program Management Staff", "Program Management", er_category))


## ------------------Filter for relevant funding agencies ------------

# Create a conditional statement to filter funding agency based on user input
if (filterAgency != "All") {
  cleanHRH <- HRH_clean %>%
    filter(funding_agency == filterAgency)
} else if (filterAgency == "All") {
  cleanHRH <- HRH_clean
}

# Filter for specific OU of interest
cleanHRH <- cleanHRH %>%
  filter(operating_unit == filterOU)

# Filter for specific year 
cleanHRH <- cleanHRH %>%
  filter(fiscal_year == Year)

## Create calculated fields for determining the annual FTE for specific cadre groups

# Clinical-Medical
cleanHRH <- cleanHRH %>%
  mutate(TX_clinicalMedical = case_when(employment_title == "Doctor" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Clinical Officer" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Medical Assistant" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte),
         HTS_clinicalMedical = case_when(employment_title == "Doctor" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Clinical Officer" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Medical Assistant" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte),
         PrEP_clinicalMedical = case_when(employment_title == "Doctor" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Clinical Officer" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Medical Assistant" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte))

# Clinical-Nursing
cleanHRH <- cleanHRH %>%
  mutate(TX_clinicalNursing = case_when(employment_title == "Nurse" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Auxiliary Nurse" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Nursing Assistant" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Other clinical provider not listed" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Midwife" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                        employment_title == "Auxiliary Midwife" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte),
         HTS_clinicalNursing = case_when(employment_title == "Nurse" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Auxiliary Nurse" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Nursing Assistant" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Other clinical provider not listed" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Midwife" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                         employment_title == "Auxiliary Midwife" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte),
         PrEP_clinicalNursing = case_when(employment_title == "Nurse" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Auxiliary Nurse" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Nursing Assistant" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Other clinical provider not listed" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Midwife" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                          employment_title == "Auxiliary Midwife" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte))

# Lay Counselor
cleanHRH <- cleanHRH %>%
  mutate(TX_layCounselor = case_when(employment_title == "Lay Counselor" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "HIV Diagnostic Assistant" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Lay worker providing adherence support" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Testing and Counseling Provider" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte),
         HTS_layCounselor = case_when(employment_title == "Lay Counselor" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "HIV Diagnostic Assistant" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Lay worker providing adherence support" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Testing and Counseling Provider" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte),
         PrEP_layCounselor = case_when(employment_title == "Lay Counselor" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "HIV Diagnostic Assistant" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Lay worker providing adherence support" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Testing and Counseling Provider" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte))

# Lay CHW
cleanHRH <- cleanHRH %>%
  mutate(TX_layCHW = case_when(employment_title == "Community Health Worker" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Expert Client" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Other community-based cadre" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Linkage Navigator" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Peer Educator" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Peer Navigator" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Community Mobilizer / Facilitator" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte),
         HTS_layCHW = case_when(employment_title == "Community Health Worker" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Expert Client" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Other community-based cadre" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Linkage Navigator" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Peer Educator" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Peer Navigator" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Community Mobilizer / Facilitator" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte),
         PrEP_layCHW = case_when(employment_title == "Community Health Worker" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Expert Client" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Other community-based cadre" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Linkage Navigator" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Peer Educator" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Peer Navigator" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                               employment_title == "Community Mobilizer / Facilitator" & program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte))

# Case Managers
cleanHRH <- cleanHRH %>%
  mutate(TX_caseManager =  case_when(employment_title == "Case Manager/ Case Worker" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Clinical Social Worker" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Clinical Case Manager" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Social Welfare Assistant" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Social Worker" & program == "C&T" & interaction_type == "Service Delivery" ~ annual_fte),
         HTS_caseManager = case_when(employment_title == "Case Manager/ Case Worker" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Clinical Social Worker" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Clinical Case Manager" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Social Welfare Assistant" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte,
                                     employment_title == "Social Worker" & program == "HTS" & interaction_type == "Service Delivery" ~ annual_fte),
         PrEP_caseManager = case_when(employment_title == "Case Manager/ Case Worker" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                      employment_title == "Clinical Social Worker" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                      employment_title == "Clinical Case Manager" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                      employment_title == "Social Welfare Assistant" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte,
                                      employment_title == "Social Worker" & sub_program == "PrEP" & interaction_type == "Service Delivery" ~ annual_fte))
         
# Pharmacist
cleanHRH <- cleanHRH %>%
  mutate(TX_pharmacy =  case_when(employment_title == "Pharmacist" & program == "C&T" ~ annual_fte,
                                  employment_title == "Pharmacy Assistant" & program == "C&T" ~ annual_fte,
                                  employment_title == "Pharmacy Technician" & program == "C&T" ~ annual_fte),
         HTS_pharmacy =  case_when(employment_title == "Pharmacist" & program == "HTS" ~ annual_fte,
                                  employment_title == "Pharmacy Assistant" & program == "HTS" ~ annual_fte,
                                  employment_title == "Pharmacy Technician" & program == "HTS" ~ annual_fte),
         PrEP_pharmacy =  case_when(employment_title == "Pharmacist" & sub_program == "PrEP" ~ annual_fte,
                                  employment_title == "Pharmacy Assistant" & sub_program == "PrEP"  ~ annual_fte,
                                  employment_title == "Pharmacy Technician" & sub_program == "PrEP" ~ annual_fte))
         
# Laboratory
cleanHRH <- cleanHRH %>%
  mutate(TX_laboratory =  case_when(employment_title == "Laboratory Technologist/Technician" & program == "C&T" ~ annual_fte,
                                  employment_title == "Laboratory Assistant/Phlebotomist" & program == "C&T" ~ annual_fte),
         HTS_laboratory =  case_when(employment_title == "Laboratory Technologist/Technician" & program == "HTS"~ annual_fte,
                                  employment_title == "Laboratory Assistant/Phlebotomist" & program == "HTS"~ annual_fte),
         PrEP_laboratory =  case_when(employment_title == "Laboratory Technologist/Technician" & sub_program == "PrEP" ~ annual_fte,
                                     employment_title == "Laboratory Assistant/Phlebotomist" & sub_program == "PrEP" ~ annual_fte))
         
# Data Clerk
cleanHRH <- cleanHRH %>%
  mutate(TX_dataClerk = case_when(employment_title == "Data Clerk" & program == "C&T" & site_level == "Site" ~ annual_fte,
                                  employment_title == "Data Officer" & program == "C&T" & site_level == "Site" ~ annual_fte,
                                  employment_title == "Data Managers" & program == "C&T" & site_level == "Site" ~ annual_fte),
         HTS_dataClerk = case_when(employment_title == "Data Clerk" & program == "HTS" & site_level == "Site" ~ annual_fte,
                                  employment_title == "Data Officer" & program == "HTS" & site_level == "Site" ~ annual_fte,
                                  employment_title == "Data Managers" & program == "HTS" & site_level == "Site" ~ annual_fte),
         PrEP_dataClerk = case_when(employment_title == "Data Clerk" & sub_program == "PrEP" & site_level == "Site" ~ annual_fte,
                                  employment_title == "Data Officer" & sub_program == "PrEP" & site_level == "Site" ~ annual_fte,
                                  employment_title == "Data Managers" & sub_program == "PrEP" & site_level == "Site" ~ annual_fte))


# Group by psnuid and psnu and summarize sum each of the annual FTE variables for each cadre
cleanHRH <- cleanHRH %>%
  group_by(psnuuid, psnu) %>%
  summarise(PrEP_clinicalMedical = sum(PrEP_clinicalMedical, na.rm = T),
            HTS_clinicalMedical = sum(HTS_clinicalMedical, na.rm = T),
            TX_clinicalMedical = sum(TX_clinicalMedical, na.rm = T),
            PrEP_clinicalNursing = sum(PrEP_clinicalNursing, na.rm = T),
            HTS_clinicalNursing = sum(HTS_clinicalNursing, na.rm = T),
            TX_clinicalNursing = sum(TX_clinicalNursing, na.rm = T),
            PrEP_layCounselor = sum(PrEP_layCounselor, na.rm = T),
            HTS_layCounselor = sum(HTS_layCounselor, na.rm = T),
            TX_layCounselor = sum(TX_layCounselor, na.rm = T),
            PrEP_layCHW = sum(PrEP_layCHW, na.rm = T),
            HTS_layCHW = sum(HTS_layCHW, na.rm = T),
            TX_layCHW = sum(TX_layCHW, na.rm = T),
            PrEP_caseManager = sum(PrEP_caseManager, na.rm = T),
            HTS_caseManager = sum(HTS_caseManager, na.rm = T),
            TX_caseManager = sum(TX_caseManager, na.rm = T),
            PrEP_pharmacy = sum(PrEP_pharmacy, na.rm = T),
            HTS_pharmacy = sum(HTS_pharmacy, na.rm = T),
            TX_pharmacy = sum(TX_pharmacy, na.rm = T),
            PrEP_laboratory = sum(PrEP_laboratory, na.rm = T),
            HTS_laboratory = sum(HTS_laboratory, na.rm = T),
            TX_laboratory = sum(TX_laboratory, na.rm = T),
            PrEP_dataClerk = sum(PrEP_dataClerk, na.rm = T),
            HTS_dataClerk = sum(HTS_dataClerk, na.rm = T),
            TX_dataClerk = sum(TX_dataClerk, na.rm = T))


# For Uganda, exclude Military Uganda under psnu 
#cleanHRH <- cleanHRH %>%
#  filter(psnu != "_Military Uganda")


## ----------------Data cleaning for MER dataset----------------------------

## Filter for specific year and funding agency
cleanMER <- rawMER %>%
  filter(fiscal_year == Year+1, # increase by one year since MER targets are a year ahead
         operatingunit == filterOU) %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ "Other agencies"))

# Create a conditional statement to filter funding agency based on user input
if (filterAgency != "All") {
  cleanMER <- cleanMER %>%
    filter(funding_agency == filterAgency)
} else if (filterAgency == "All") {
  cleanMER <- cleanMER
}

## Prepare a table of PrEP_CT from last year since we'll take this cumulative value as the 2023 estimate
cleanMER_prev <- rawMER %>%
  filter(fiscal_year == Year, # remove the +1 since we want the previous year
         operatingunit == filterOU) %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ "Other agencies"))
    if (filterAgency != "All") {
      cleanMER_prev <- cleanMER_prev %>%
        filter(funding_agency == filterAgency)
    } else if (filterAgency == "All") {
      cleanMER_prev <- cleanMER_prev
    }

    # Take the cumulative value of PrEP_CT from previous year
    cleanMER_prev <- cleanMER_prev %>%
      mutate(PrEP_CT = if_else(indicator == "PrEP_CT" & standardizeddisaggregate == "Total Numerator", cumulative, NA_real_)) %>%
      group_by(psnuuid, psnu) %>%
      summarise(PrEP_CT = sum(PrEP_CT, na.rm = T)) %>%
      ungroup() 

# Create calculated fields for the list of MER targets of interest
cleanMER <- cleanMER %>%
  mutate(facilityIndex = if_else(indicator == "HTS_TST" & modality == "Index", targets, NA_real_),
         indexMod = if_else(indicator == "HTS_TST" & modality == "IndexMod", targets, NA_real_),
         HTS_TST_STI = if_else(indicator == "HTS_TST" & modality == "STI Clinic", targets, NA_real_),
         inpat = if_else(indicator == "HTS_TST" & modality == "Inpat", targets, NA_real_),
         HTS_Self = if_else(indicator == "HTS_SELF" & standardizeddisaggregate == "Total Numerator", targets, NA_real_),
         otherPITC = if_else(indicator == "HTS_TST" & modality == "OtherPITC", targets, NA_real_),
         postANC1 = if_else(indicator == "HTS_TST" & modality == "Post ANC1", targets, NA_real_),
         PMTCT_ANC = if_else(indicator == "HTS_TST" & modality == "PMTCT ANC", targets, NA_real_),
         PMTCT_NEW = if_else(indicator == "PMTCT_ART" & otherdisaggregate == "Life-long ART, New", targets, NA_real_),
         PMTCT_Already = if_else(indicator == "PMTCT_ART" & otherdisaggregate == "Life-long ART, Already", targets, NA_real_),
         PrEP_NEW = if_else(indicator == "PrEP_NEW" & standardizeddisaggregate == "Total Numerator", targets, NA_real_), 
         TX_CURR_KP = if_else(indicator == "TX_CURR" & standardizeddisaggregate == "KeyPop/HIVStatus", targets, NA_real_),
         TX_CURR = if_else(indicator == "TX_CURR" & standardizeddisaggregate == "Total Numerator", targets, NA_real_),
         TX_NEW_KP = if_else(indicator == "TX_NEW" & standardizeddisaggregate == "KeyPop/HIVStatus", targets, NA_real_),
         TX_NEW = if_else(indicator == "TX_NEW" & standardizeddisaggregate == "Total Numerator", targets, NA_real_),
         TX_PVLS = if_else(indicator == "TX_PVLS" & numeratordenom == "D", targets, NA_real_),
         HTS_TST_Mobile = if_else(indicator == "HTS_TST" & modality == "MobileMod", targets, NA_real_)) 


# Group by psnuid and psnu, then summarize sum for each of the new target variables
cleanMER <- cleanMER %>%
  group_by(psnuuid, psnu) %>%
  summarise(PrEP_NEW = sum(PrEP_NEW, na.rm = T),
            HTS_Self = sum(HTS_Self, na.rm = T),
            HTS_Mobile = sum(HTS_TST_Mobile, na.rm = T),
            HTS_indexMod = sum(indexMod, na.rm = T),
            HTS_facilityIndex = sum(facilityIndex, na.rm = T),
            HTS_PMTCT_ANC = sum(PMTCT_ANC, na.rm = T),
            HTS_postANC1 = sum(postANC1, na.rm = T),
            HTS_STI = sum(HTS_TST_STI, na.rm = T),
            HTS_otherPITC = sum(otherPITC, na.rm = T),
            HTS_inpat = sum(inpat, na.rm = T),
            TX_NEW = sum(TX_NEW, na.rm = T),
            TX_NEW_KP = sum(TX_NEW_KP, na.rm = T),
            TX_CURR = sum(TX_CURR, na.rm = T),
            TX_CURR_KP = sum(TX_CURR_KP, na.rm = T),
            PMTCT_NEW = sum(PMTCT_NEW, na.rm = T),
            PMTCT_Already = sum(PMTCT_Already, na.rm = T),
            TX_PVLS = sum(TX_PVLS, na.rm = T))

# Merge the PrEP_CT table from last year into the final MER dataset
cleanMER <- left_join(cleanMER, cleanMER_prev, by = c("psnu", "psnuuid"))

# Re-arrange the order to match DCT
cleanMER <- cleanMER %>%
  select(psnuuid, psnu, PrEP_NEW, PrEP_CT, HTS_Self:TX_PVLS)

# For Uganda, exclude Military Uganda under psnu 
#cleanMER <- cleanMER %>%
#  filter(psnu != "_Military Uganda")
            


## ----------------Join the HRH and MER datasets using full_join ------------

merged <- full_join(cleanHRH, cleanMER, by = "psnuuid") %>% # use a full join since there are psnu's without targets, and places with targets without staff
  
# if the psnu name is NA, then put the psnu name from the other dataset
  mutate(psnu.x = if_else(is.na(psnu.x), psnu.y, psnu.x)) %>%
  
# Do additional data cleaning 
  select(-psnu.y) %>%
  rename(psnu = psnu.x) %>%
  filter(psnu != "Data reported above PSNU Level")  %>% # remove above site reporting
  arrange(psnu) # sort by psnu

# Set all NA values (i.e. non-matching psnu's with values of zero)
  merged[is.na(merged)] <- 0
  
# Ungroup the data frame
  merged <- merged %>%
    ungroup()


## ---------------- Calculate the average salary for each cadre --------------

# For Uganda, exclude Military Uganda under psnu 
# HRH_clean <- HRH_clean %>%
#    filter(psnu != "_Military Uganda")  
  
# Filter for relevant modes of hiring
renumeration <- HRH_clean %>%
  filter(mode_of_hiring == "Salary" | mode_of_hiring == "Contract",
         
       # Filter for OU of interest
         operating_unit == filterOU,
       
       # Filter for the relevant year
         fiscal_year == Year) 
  
  # Create a conditional statement to filter funding agency based on user input
  if (filterAgency != "All") {
    renumeration <- renumeration %>%
      filter(funding_agency == filterAgency)
  } else if (filterAgency == "All") {
    renumeration <- renumeration
  }

# Create calculated fields for the FTE and salary for specific cadres of interest (note: revisit the "CHW" label)
renumeration <- renumeration %>%
  mutate(All_clinicalMedical = case_when(employment_title == "Doctor" | employment_title == "Clinical Officer" | employment_title == "Medical Assistant" ~ equivalent_annual_salary, TRUE ~ NA_real_),
         All_clinicalNursing = case_when(employment_title == "Nurse" | employment_title == "Auxiliary Nurse" | employment_title == "Nursing Assistant" | employment_title == "Other clinical provider not listed" | employment_title == "Midwife" | employment_title == "Auxiliary Midwife" ~ equivalent_annual_salary, TRUE ~ NA_real_),
         All_layCounselor = case_when(employment_title == "Lay Counselor" | employment_title == "HIV Diagnostic Assistant" | employment_title == "Lay worker providing adherence support" | employment_title == "Testing and Counseling Provider" ~ equivalent_annual_salary),
         All_layCHW = case_when(employment_title == "Community Health Worker" | employment_title == "Expert Client" | employment_title == "Other community-based cadre" | employment_title == "Linkage Navigator" | employment_title == "Peer Educator" | employment_title == "Peer Navigator" | employment_title == "Community Mobilizer / Facilitator" ~ equivalent_annual_salary, TRUE ~ NA_real_),
         All_caseManager = case_when(employment_title == "Case Manager / Case Worker" | employment_title == "Clinical Social Worker" | employment_title == "Clinical Case Manager" | employment_title == "Social Welfare Assistant" | employment_title == "Social Worker" ~ equivalent_annual_salary, TRUE ~ NA_real_),
         All_pharmacy = case_when(employment_title == "Pharmacist" | employment_title == "Pharmacy Assistant" | employment_title == "Pharmacy Technician" ~ equivalent_annual_salary, TRUE ~ NA_real_),
         All_laboratory = case_when(employment_title == "Laboratory Technologist/Technician" | employment_title == "Laboratory Assistant/Phlebotomist" ~ equivalent_annual_salary, TRUE ~ NA_real_),
         All_dataClerk = case_when(employment_title == "Data Clerk" | employment_title == "Data Officer" | employment_title == "Data Managers" ~ equivalent_annual_salary, TRUE ~ NA_real_))

# Filter for only FTE = 1 and months of work = 12
renumeration <- renumeration %>%
  filter(avg_fte_per_month == 1,
         months_of_work >= 8)

# Take the median of each calculated cadre of interest
renumeration <- renumeration %>%
  summarise(All_clinicalMedical = median(All_clinicalMedical, na.rm = T),
            All_clinicalNursing = median(All_clinicalNursing, na.rm = T),
            All_layCounselor = median(All_layCounselor, na.rm = T),
            All_layCHW = median(All_layCHW, na.rm = T),
            All_caseManager = median(All_caseManager, na.rm = T),
            All_pharmacy = median(All_pharmacy, na.rm = T),
            All_laboratory = median(All_laboratory, na.rm = T),
            All_dataClerk = median(All_dataClerk, na.rm = T))

# Gather all the grouped cadres into one column
renumeration <- renumeration %>%
  gather(key = cadreGroup, value = median_annual_salary, starts_with("All_"))


###------------------Create the data frames to be pasted into DCT if needed------------------------------------

PSNU_list <- merged %>%
  select(psnuuid, psnu)

Program_Targets <- merged %>%
  select(psnuuid, psnu, PrEP_NEW:TX_PVLS)

HRH_clinicalMedical <- merged %>%
  select(ends_with("clinicalMedical"))

HRH_clinicalNursing <- merged %>%
  select(ends_with("clinicalNursing"))

HRH_layCounselor <- merged %>%
  select(ends_with("layCounselor"))

HRH_layCHW <- merged %>%
  select(ends_with("layCHW"))

HRH_caseManager <- merged %>%
  select(ends_with("caseManager"))

HRH_Pharmacy <- merged %>%
  select(ends_with("pharmacy"))

HRH_Laboratory <- merged %>%
  select(ends_with("laboratory"))

HRH_dataClerk <- merged %>%
  select(ends_with("dataClerk"))

medianSalary <- renumeration %>%
  select(median_annual_salary)


###------------------Export the remuneration table, and the MER tables as Excel files-----------------

fileName_remuneration <- paste0("./Output/Intm tables/Remuneration table - ", filterAgency, " - ",  filterOU, " - ", COP_Planning_Year, ".xlsx")
write.xlsx(renumeration, fileName_remuneration, overwrite = TRUE) 

fileName_HRH_MER <- paste0("./Output/Intm tables/HRH MER table - ", filterAgency, " - ",  filterOU, " - ", COP_Planning_Year, ".xlsx")
write.xlsx(merged, fileName_HRH_MER, overwrite = TRUE) 

