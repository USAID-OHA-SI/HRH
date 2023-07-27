### PURPOSE: TO TRANSFORM THE NON-SUBMITTED, POST-CLEAN NIGERIA DATASETS SO THAT THEY
#     CAN BE ADDED INTO THE FY22 HRH STRUCTURED DATASET

# Code developed by: Kyle Borces
# Last updated: January 17, 2023

# Installing and loading relevant packages/libraries. Note that I'm using my local folders here, so file paths will need to be changed

library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(tidyverse)
library(sqldf)
library(janitor)


# Import the post-clean HRH dataset
HRH_data_orig<-read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20221216.txt") 

# Import the pre-clean HRH dataset for reference only
HRH_pre_clean<-read.delim("./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20221110.txt") # import pre-clean data for reference also

# Import the unique ID's for DATIM geographic levels (where SNU1/PSNU = level 4, community = level 5, facility = level 6)
uniqueIDs <- read_excel("./1. Data/NGA post clean/Data Exchange Organisation Units.xlsx")
uniqueIDs$orgunit_internal_id <- as.character(uniqueIDs$orgunit_internal_id) # set as string

# Now create a vector that contains the file pathways of the 12 Nigeria post-clean datasets
workbookPathways <- c(
  "./1. Data/NGA post clean/COP21FY22 - HRH - 70267 - NG.xlsx",
  "./1. Data/NGA post clean/COP21FY22_HRH_160525.xlsx",
  "./1. Data/NGA post clean/Copy of hrh_template_Nigeria_20221029_18656.xlsx",
  "./1. Data/NGA post clean/FY22_HRH_85251.xlsx",
  "./1. Data/NGA post clean/HRH Nigeria_20221025_160521 AHNI ACE-1 (2).xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221004_81858 FINAL.xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221011_18657_1.xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221017_160523_ACE3 Porject .xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221018_81860kpc2.xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221101_160527_FY22 Q4_ACE5___.xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221108_81862 R.xlsx",
  "./1. Data/NGA post clean/hrh_template_Nigeria_20221110_18442 -FINAL.xlsx"
)

# Create a function that cleans up each post-clean dataset
updateMaster <- function(newDatPath) { 

    # Import the post-clean Nigeria datasets
    NGA_data <- read_excel(newDatPath, sheet = 3) # this contains the staffing data
    NGA_basic <- read_excel(newDatPath, sheet = 2, range = "B2:D14") # this contains the meta-data

    # Data cleaning of the NGA data frames
    NGA_basic <- NGA_basic %>%
      clean_names() %>%
      filter(is.na(operating_unit_country) == FALSE) %>%
      select(-x2)
    
    NGA_data <- NGA_data %>%
      clean_names() %>%
      select(-level4check:-level9check) %>%
      filter(is.na(employed_through_prime_or_sub_ip) == FALSE)
    
    ### --------Transformation of NGA datasets into the HRH structured dataset-------------
    
    tidyNGA <- NGA_data %>%
      mutate(orgunituid = "placeholder", # dummy variable since the HRH team does not use this
             sitename = if_else(is.na(facility) == TRUE, "Data reported above Site level", as.character(facility)),
             operatingunituid = "PqlFzhuPcF1",
             operating_unit = "Nigeria",
             country = "Nigeria")
    tidyNGA$psnu <- str_to_title(tidyNGA$psnu) #convert to proper names
    
    # Add in the DATIM geographic ID's
    SNU_PSNU_IDs <- uniqueIDs %>%
      filter(orgunit_level == 4) %>%
      distinct(orgunit_internal_id, .keep_all = TRUE) %>%
      select(orgunit_internal_id, orgunit_name)
    SNU_PSNU_IDs$orgunit_name <- str_to_title(SNU_PSNU_IDs$orgunit_name) # convert to proper names
    
    # Merge the psnu and snu1 id's. Note that some staff are "Data reported above PSNU level"
    tidyNGA <- left_join(tidyNGA, SNU_PSNU_IDs, by = c("psnu" = "orgunit_name"))
    tidyNGA <-tidyNGA %>%
      mutate(snu1uid = orgunit_internal_id) %>%
      mutate(snu1uid = if_else(psnu == "Data reported above PSNU level", operatingunituid, snu1uid)) %>% 
      mutate(snu1 = psnu) %>%
      mutate(snu1 = if_else(psnu == "Data reported above PSNU level", as.character(NA_real_), snu1)) %>%
      mutate(psnuuid = snu1uid) %>%
      mutate(psnuuid = if_else(psnu == "Data reported above PSNU level", "?", psnuuid)) %>%
      mutate(communityuid = "placeholder", # dummy variable since the HRH team does not use this
             community = "placeholder", # dummy variable since the HRH team does not use this
             facilityuid = "placeholder") %>% # dummy variable since the HRH team does not use this
      mutate(mech_code = NGA_basic$nigeria[4]) %>%
      mutate(mech_name = NGA_basic$nigeria[5]) %>%
      mutate(funding_agency = NGA_basic$nigeria[1])
    
    # Add in prime partner names, award number, prime partner uei
    prime_partners <- HRH_pre_clean %>%
      filter(fiscal_year == 2022) %>%
      filter(operating_unit == "Nigeria") %>%
      distinct(mech_code, award_number, prime_partner_name, prime_partner_uei) %>%
      mutate(mech_code = as.character(mech_code))
    tidyNGA <- left_join(tidyNGA, prime_partners, by = "mech_code")
    
    # Calculate ER categories
    tidyNGA <- tidyNGA %>%
      mutate(er_category = case_when(grepl("^Ancillary", employment_title) == TRUE ~ "HCW: Ancillary",
                                     grepl("^Clinical", employment_title) == TRUE ~ "HCW: Clinical",
                                     grepl("^Other", employment_title) == TRUE ~ "Other Staff",
                                     grepl("IP Prg Mgmt", employment_title) == TRUE ~ "Implementing Mechanism Program Management Staff"))
    
    # Add in the cadre groups using the pre-clean HRH data
    cadres <- HRH_pre_clean %>%
      filter(fiscal_year == 2022) %>%
      filter(operating_unit == "Nigeria") %>%
      distinct(employment_title, cadre)
    
    # Clean up the employment titles before merging
    tidyNGA$employment_title <- gsub("Ancillary: ", "", tidyNGA$employment_title) 
    tidyNGA$employment_title <- gsub("Clinical: ", "", tidyNGA$employment_title)
    tidyNGA$employment_title <- gsub("Other: ", "", tidyNGA$employment_title)
    tidyNGA$employment_title <- gsub("IP Prg Mgmt: ", "", tidyNGA$employment_title)
    tidyNGA <- left_join(tidyNGA, cadres, by = "employment_title")
    
    # Add the site level variable
    tidyNGA <- tidyNGA %>%
      mutate(site_level = case_when(grepl("Program Management", primary_program_area) == TRUE ~ "Program Management",
                                    grepl("Site Level", primary_program_area) == TRUE ~ "Site",
                                    grepl("Above Site", primary_program_area) == TRUE ~ "Above Site"))
    
    # Add primary program area
    tidyNGA <- tidyNGA %>%
      mutate(program = case_when(grepl("Above Site", primary_program_area) == TRUE ~ "ASP",
                                 grepl("Program Management", primary_program_area) == TRUE ~ "PM",
                                 grepl("SE:", primary_program_area) == TRUE ~ "SE",
                                 grepl("C&T:", primary_program_area) == TRUE ~ "C&T",
                                 grepl("HTS:", primary_program_area) == TRUE ~ "HTS",
                                 grepl("PREV:", primary_program_area) == TRUE ~ "PREV"))
    
    # Add the sub program area
    tidyNGA$sub_program <- gsub("Program Management", "IM Program Management", tidyNGA$primary_program_area)
    tidyNGA$sub_program <- gsub("Above Site: ", "", tidyNGA$sub_program)
    tidyNGA$sub_program <- gsub("Site Level: SE: ", "", tidyNGA$sub_program)
    tidyNGA$sub_program <- gsub("Site Level: C&T: ", "", tidyNGA$sub_program)
    tidyNGA$sub_program <- gsub("Site Level: HTS: ", "", tidyNGA$sub_program)
    tidyNGA$sub_program <- gsub("Site Level: PREV: ", "", tidyNGA$sub_program)
    
    # Rename the interaction type
    tidyNGA <- tidyNGA %>%
      rename(interaction_type = deliver_services_directly_to_beneficiaries)
    
    # Add the primary beneficiary
    tidyNGA$beneficiary <- gsub("Females: Adolescent Girls and Young Women", "Females", tidyNGA$primary_beneficiary)
    tidyNGA$beneficiary <- gsub("Males: Adolescent Boys and Young Men", "Males", tidyNGA$beneficiary)
    tidyNGA$beneficiary <- gsub("Non-Targeted Pop: Children", "Non-Targeted Pop", tidyNGA$beneficiary)
    tidyNGA$beneficiary <- gsub("non-Targeted Pop", "Non-Targeted Pop", tidyNGA$beneficiary)
    
    # Add the sub beneficiary
    tidyNGA$sub_beneficiary <- gsub("Females: Adolescent Girls and Young Women", "Young women & adolescent females", tidyNGA$primary_beneficiary)
    tidyNGA$sub_beneficiary <- gsub("Males: Adolescent Boys and Young Men", "Young men & adolescent males", tidyNGA$sub_beneficiary)
    tidyNGA$sub_beneficiary <- gsub("Non-Targeted Pop: Children", "Children", tidyNGA$sub_beneficiary)
    tidyNGA$sub_beneficiary <- gsub("non-Targeted Pop", "Not disaggregated", tidyNGA$sub_beneficiary)
    tidyNGA <- tidyNGA %>%
      mutate(sub_beneficiary = case_when(primary_beneficiary == "Females" | primary_beneficiary == "Males" | primary_beneficiary == "Non-Targeted Pop" ~ "Not disaggregated",
                                         primary_beneficiary == "Key Pops" | primary_beneficiary == "OVC" | primary_beneficiary == "Pregnant & Breastfeeding Women" | primary_beneficiary == "Priority Pops" ~ "Not disaggregated",
                                         TRUE ~ sub_beneficiary))

    # Rename existing variables and create dummy variables as needed
    tidyNGA <- tidyNGA %>%
        select(-primary_beneficiary) %>%
        mutate(subrecipient_uei = "placeholder") %>% # dummy variable since the HRH team does not use this
        rename(prime_or_sub = employed_through_prime_or_sub_ip) %>%
        rename(subrecipient_name = if_sub_select_ip_name) %>%
        rename(mode_of_hiring = mode_of_hire,
               roving = work_in_or_support_multiple_facility_sites_roving_staff) %>%
        mutate(work_location = case_when(site_level == "Above Site" | site_level == "Program Management" ~ "Above Site",
                                         roving == "Yes" ~ "Roving",
                                         roving == "No" & primarily_support_work_in_the_community == "Yes" ~ "Community",
                                         is.na(facility) == FALSE ~ "Facility")) %>%
        rename(is_covid_support = in_past_year_provided_support_for_the_covid_response,
               moh.secondment = moh_staff_or_seconded_to_moh,
               is_outside_ou = position_based_outside_of_ou,
               is_tech_assist = provide_technical_assistance,
               is_community_primarily = primarily_support_work_in_the_community) %>%
        mutate(fiscal_year = 2022)
    
    # Clean up the expenditure variables
    tidyNGA <- tidyNGA %>%
      rename(actual_salary_expenditure = sum_of_annual_pepfar_expenditure_excluding_fringe_and_non_monetary,
             actual_fringe_expenditure = annual_pepfar_fringe_expenditure_excluding_non_monetary,
             actual_non_monetary_expenditure = annual_pepfar_non_monetary_expenditure_excluding_fringe,
             avg_fte_per_month = average_fte_per_month,
             months_of_work = months_of_work_in_past_year) %>%
      
    # Calculate the total spend for each staff
      rowwise() %>%
      mutate(actual_annual_spend = sum(actual_salary_expenditure, actual_fringe_expenditure, actual_non_monetary_expenditure, na.rm = T)) %>%
      ungroup()
    
    # Calculate the equivalent annual spend, equivalent annual salary, and annual fte
    tidyNGA <- tidyNGA %>%
      mutate(equivalent_annual_spend = (12 * actual_annual_spend) /
                                              (avg_fte_per_month * months_of_work)) %>%
      mutate(equivalent_annual_salary = (12 * actual_salary_expenditure) /
                                        (avg_fte_per_month * months_of_work)) %>%
      mutate(annual_fte = (avg_fte_per_month * months_of_work) / 12) %>%
      mutate(individual_count = 1)
    
    # Now re-arrange all columns to be the same as the HRH structured dataset
    tidyNGA <- tidyNGA %>%
      select(colnames(HRH_data_orig))
    
    # Return the cleaned up NGA dataset
    return(tidyNGA)
    
    }

### ---------- Now call the master function that cleans each dataset -----------------

# First, create a lists that stores each cleaned up dataset after every loop
consolidatedList = list() 

# Create the for loop that loops through each file pathway of the NGA datasets
for (i in 1 : length(workbookPathways)) {
  
  # run the updateMaster function and save each iteration in the list
  consolidatedList[[i]] <- updateMaster(workbookPathways[i])
  
  # notify that this has been accomplished
  print (paste("Nigeria post-clean dataset update ", i, " of ", length(workbookPathways), "complete: added ", 
               workbookPathways[i]))
  
  # update tracker to move to the next file pathway or stop
  i <- i + 1
}

## After the consolidated list that conatins each clean NGA dataset is completed,
#   bind all the data frames together into one data frame
final_List = do.call(rbind, consolidatedList)

# Add a variable that labels that they come from the 12 NGA missing mechanisms
final_List <- final_List %>%
  mutate(missingNGA_flags = "TRUE")

# Lastly, append the tidy NGA dataset into the HRH post-clean dataset
HRH_post_clean <- rbind.fill(HRH_data_orig, final_List) %>%
  arrange(operating_unit) # sort by OU

# Export the adjusted HRH post-clean dataset
write_xlsx(HRH_post_clean,"./1. Data/HRH_Structured_Datasets_Site_IM_FY21-22_not_redacted_20230117_Adjusted.xlsx") 





