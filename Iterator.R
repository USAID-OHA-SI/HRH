# This is the iterator file with parameters to be passed to the .Rmd file

library(fivethirtyeight)
library(tidyverse)

#import HRH dataset
load(file = "./4. Outputs/RDS/FY23_cleanHRH.rds") 

#specify which OU's to loop through
operating_unit <- c("Tanzania", "Lesotho", "Eswatini", "Botswana", "Ethiopia", "Zambia", "Kenya", "Uganda") 
#operating_unit <- unique(HRH_clean$operating_unit)

#create a table for each operating unit
reports <- tibble(
  output_file = stringr::str_c("4. Outputs/Automated Reports/", operating_unit, "- FY23 HRH Summary.doc"),
  params = map(operating_unit, ~list(operating_unit = .))
)


#render a word doc for each operating unit using our R markdown file
head(reports)

reports %>%
  pwalk(rmarkdown::render, input = "Automated Reports - OU.Rmd")

### Move log files elsewhere, to avoid cluttering working directory

# selects files in current working directory
#src_path <- dirname(rstudioapi::getSourceEditorContext()$path)
#curr_files <- list.files(src_path)
#curr_files <- curr_files[str_detect(curr_files, ".log")]
#new_log_loc <- paste0(log_dir,sep="/", curr_files)

### Note: renaming a file with a new path is akin to moving it
#file.rename(from = curr_files,
#            to = new_log_loc)
