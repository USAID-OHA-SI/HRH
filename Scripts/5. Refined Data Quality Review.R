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


## ------------------ Import the needed files for HRH, ER, and HRH-ER datasets --------
fin_data_orig <- read.delim("./1. Data/Financial_Structured_Datasets_COP17-24_20241115.txt") # read in FSD dataset
load(file = "./4. Outputs/RDS/FY24_cleanHRH.rds") # cleaned HRH dataset
load(file = "./4. Outputs/RDS/HRH_ER_merged_21_24.rds") # HRH-ER merged dataset
OVC_mechs <- read_excel("./4. Outputs/FY24_OVC_mechs.xlsx")
FY24_budget <- read.delim("./1. Data/Comprehensive_Budget_Datasets_COP17-24_20241115.txt", header = TRUE, stringsAsFactors = FALSE)
G2Gs <- read_excel("./1. Data/FY24 PEPFAR G2G Mechanisms.xlsx")


###---------------------Areas to review-------------------------------------###



## Number of months worked (range from 1-12)

## Average FTE per month should be less than or equal to 1.005

## Community staff and the "Primarily community" column

## Technical Assistance titles mapped to the "is primarily TA" column

## Look at any outlier actual annnual spend (check for currency conversion issues)

## Check the number of staff that reported to SNU, compared to PSNU at country level

## Employment title to program area mapping

## Mapping mechanisms to primary beneficiaries somehow? How to check beneficiaries?

## Check Primary Program Area under "Above Site: Institutional Prevention"

# check Harry's validation checks from last year





