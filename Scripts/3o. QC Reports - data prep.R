### PURPOSE: TO PREPARE DATASETS WE NEED FOR THE QC REPORTS.


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

