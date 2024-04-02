####################################################################################################################
## Title: Trimming down MER dataset
## Purpose: This code cleans up the MER dataset to the indicators we mainly need before importing into Tableau
## Developer: By Kyle Borces
## Last updated: By Kyle Borces, 11/27/2022
####################################################################################################################

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(readxl)

## 2a.  Uploading MER structured dataset from Panorama at PSNU level
#rawMER <- read.delim("./1. Data/MER_Structured_Datasets_PSNU_IM_FY21-24_20231114_v1_1.txt", header = TRUE, stringsAsFactors = FALSE)
#rawMER <- read.delim("./MER_Structured_Datasets_PSNU_IM_FY21-24_20231114_v1_1.txt", header = TRUE, stringsAsFactors = FALSE)


## 2b.Save the data frame as an .rds for later use
#save(rawMER, file = "./4. Outputs/RDS/FY23_rawMER.rds")
load(file = "./4. Outputs/RDS/FY23_rawMER.rds")


## 3. Do some data cleaning
cleanMER <- rawMER %>%
  
  # Filter for only the relevant years
  filter(fiscal_year == 2023) 
  
  # Only select relevant indicators for now
cleanMER <- cleanMER %>%
  filter(grepl("^OVC", indicator) | 
         grepl("^HTS", indicator) | 
         grepl("^TX", indicator) |
         grepl("^AGYW", indicator) |
         grepl("^TB", indicator) |
         grepl("^PMTCT", indicator) |
         grepl("^KP", indicator) |
         grepl("^AGYW", indicator) |
         grepl("^PrEP", indicator))
  
  # Select only the relevant columns
cleanMER <- cleanMER %>%
  select(-operatingunituid, -snu1uid, -psnuuid, - snuprioritization, -typemilitary, prime_partner_duns, -prime_partner_uei,
         -award_number, -otherdisaggregate, -otherdisaggregate_sub, -statustb, -statuscx, -hiv_treatment_status,
         -starts_with("qtr"), -source_name) %>% 
  
  # Filter for only relevant disaggregates
   filter(standardizeddisaggregate == "Total Numerator" | (indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator")) %>% # keep tx_pvls denominator  
   mutate(indicator = case_when(indicator == "TX_PVLS" & standardizeddisaggregate == "Total Numerator" ~ "TX_PVLS_N",
                                indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator" ~ "TX_PVLS_D",
                                TRUE ~ indicator))
  
  
## Disaggregate further
cleanMER <- cleanMER %>%
  group_by(fiscal_year, operatingunit, snu1, psnu, prime_partner_name, funding_agency, mech_code, mech_name, indicator, standardizeddisaggregate) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE),
            targets = sum(targets, na.rm = TRUE))

## 3. Create a % targets achieved column
cleanMER <- cleanMER %>%
  mutate(pct_targetAchieved = cumulative / targets) %>%
  select(-standardizeddisaggregate) # remove disaggregate column


## 4. Convert indicators column into wide format to show both targets and cumulative values of the filtered indicators
cleanMER <- cleanMER %>%
  gather(targCum, Value, (cumulative:pct_targetAchieved)) %>%
  unite(col = "indicator_final", c("indicator", "targCum"), sep = "-") %>%
#  mutate(row = row_number()) %>% # create row number as a unique identifier before using spread command
  spread(indicator_final, Value) 
#  select(-row)

# Set all MER indicator columns as numeric
cleanMER[, grepl("^OVC", names(cleanMER))] <- lapply(cleanMER[, grepl("^OVC", names(cleanMER))], as.numeric)
cleanMER[, grepl("^HTS", names(cleanMER))] <- lapply(cleanMER[, grepl("^HTS", names(cleanMER))], as.numeric)
cleanMER[, grepl("^TX", names(cleanMER))] <- lapply(cleanMER[, grepl("^TX", names(cleanMER))], as.numeric)
cleanMER[, grepl("^TB", names(cleanMER))] <- lapply(cleanMER[, grepl("^TB", names(cleanMER))], as.numeric)
cleanMER[, grepl("^PMTCT", names(cleanMER))] <- lapply(cleanMER[, grepl("^PMTCT", names(cleanMER))], as.numeric)
cleanMER[, grepl("^KP", names(cleanMER))] <- lapply(cleanMER[, grepl("^KP", names(cleanMER))], as.numeric)
cleanMER[, grepl("^AGYW", names(cleanMER))] <- lapply(cleanMER[, grepl("^AGYW", names(cleanMER))], as.numeric)
cleanMER[, grepl("^PrEP", names(cleanMER))] <- lapply(cleanMER[, grepl("^PrEP", names(cleanMER))], as.numeric)


## 5. Export as xls
write_xlsx(cleanMER,"./1. Data/MER_Structured_Datasets_PSNU_IM_FY23_ONLY_CLEAN.xlsx") 

  






