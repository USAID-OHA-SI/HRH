# This is the iterator file with parameters to be passed to the .Rmd file

library(fivethirtyeight)
library(tidyverse)

# import HRH dataset
load(file = "./4. Outputs/RDS/FY24_cleanHRH.rds") 

# Filter for the most recent year
HRH_clean <- HRH_clean %>%
  filter(fiscal_year == max(fiscal_year))

# Loop through all countries
operating_unit <- c("Botswana",
                    "Ethiopia",
                    "Cote d'Ivoire",
                    "Eswatini",
                    "Haiti",
                    "Lesotho",
                    "Malawi",
                    "Mozambique",
                    "Namibia",
                    "Nigeria",
                    "South Africa",
                    "Tanzania",
                    "Zimbabwe",
                    "Kenya",
                #    "Cameroon",
                    "Uganda",
                    "Zambia")

# create a table for each country
reports <- tibble(
  output_file = stringr::str_c("4. Outputs/Automated Reports/Country level/", operating_unit, " - FY24 HRH Summary.doc"),
  params = map(operating_unit, ~list(operating_unit = .))
)

head(reports)

# render a word doc for each operating unit using our R markdown file
reports %>%
  pwalk(rmarkdown::render, input = "Automated-Reports-by-Country.Rmd")


