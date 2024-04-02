### PURPOSE: To create a clean site level MER dataset, that we can use to link HRH inventory data. 
#           This is only for USAID 

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)
library(readxl)

## 2a.  Uploading MER structured dataset from Panorama at PSNU level
Lesotho_rawMER <- read.delim("./1. Data/MER_Structured_Datasets_Site_IM_FY21-24_20231215_v2_1_Lesotho.txt", header = TRUE, stringsAsFactors = FALSE)


## 2b.Save the data frame as an .rds for later use
save(Lesotho_rawMER, file = "./4. Outputs/RDS/FY23_rawMER_Lesotho.rds")

Lesotho_rawMER <- Lesotho_rawMER %>%
  filter(fiscal_year == 2021 | fiscal_year == 2022 | fiscal_year == 2023)


## 3. Do some data cleaning
cleanMER <- Lesotho_rawMER %>%
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
  select(-operatingunituid, -snu1uid, -psnuuid, - snuprioritization, -typemilitary, -prime_partner_duns, -prime_partner_uei,
         -award_number, -otherdisaggregate, -otherdisaggregate_sub, -statustb, -statuscx, -hiv_treatment_status,
         -starts_with("qtr"), -source_name) %>% 
  
  # Filter for only relevant disaggregates
  filter(standardizeddisaggregate == "Total Numerator" | (indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator")) %>% # keep tx_pvls denominator  
  mutate(indicator = case_when(indicator == "TX_PVLS" & standardizeddisaggregate == "Total Numerator" ~ "TX_PVLS_N",
                               indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator" ~ "TX_PVLS_D",
                               TRUE ~ indicator))


## Disaggregate further
cleanMER <- cleanMER %>%
  group_by(fiscal_year, operatingunit, funding_agency, prime_partner_name, mech_code, mech_name, snu1, psnu, community, facility, indicator, standardizeddisaggregate) %>%
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

cleanMER <- cleanMER %>%
  mutate(funding_agency = case_when(funding_agency == "USAID" | funding_agency == "USAID/WCF" ~ "USAID",
                                    funding_agency == "HHS/CDC" ~ "CDC",
                                    TRUE ~ "Other agencies")) %>%
  filter(funding_agency == "USAID") 

## 5. Export as xls
write_xlsx(cleanMER,"./1. Data/Lesotho_MER_Structured_Datasets_PSNU_Facility_FY21_FY23_CLEAN.xlsx")


