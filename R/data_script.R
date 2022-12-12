library(tidyverse)

# list all files in data folder
files <- list.files("data")

# loop over each file, iteratively add to dataframe
raw <- map_df(files, function(x) {
  
  # read file
  tmp <- read_csv(paste("data", x, sep = "/"), skip = 1)
  
  # clean column names
  names(tmp) <- str_replace_all(str_to_lower(names(tmp)), " ", "_")
  
  # fix school years to match other datasets
  tmp <- tmp %>%
    mutate(school_year = paste(paste("20", as.numeric(str_extract(school_year, "\\d{2}$")) - 1, sep = ""), 
                               str_extract(school_year, "\\d{2}$"), sep = "-")) %>%
    modify_if(is.numeric, as.character)
  
  return(tmp)
})

# create dpi_true_id, which serves as a join field with most tables
raw_ids <- raw %>%
  mutate(dpi_true_id = paste(
    str_extract(assignment_work_agency, "^\\d{4}"),
    str_extract(assignment_work_school, "^\\d{4}"),
    sep = "_"
  ))

saveRDS(raw_ids, "staff.rda")

