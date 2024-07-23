# This is the iterator file with parameters to be passed to the .Rmd file

library(fivethirtyeight)
library(tidyverse)

# import HRH dataset
load(file = "./4. Outputs/RDS/FY23_cleanHRH.rds") 

# specify which OU's to loop through
operating_unit <- c("Tanzania", "Lesotho", "Eswatini", "Botswana", "Ethiopia", "Zambia", "Kenya", "Uganda") 

# create a table for each operating unit
reports <- tibble(
  output_file = stringr::str_c("4. Outputs/Automated Reports/", operating_unit, "- FY23 HRH Summary.doc"),
  params = map(operating_unit, ~list(operating_unit = .))
)

head(reports)

# render a word doc for each operating unit using our R markdown file
reports %>%
  pwalk(rmarkdown::render, input = "Automated Reports - OU.Rmd")


