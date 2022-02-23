

# DESCRIPTION -------------------------------------------------------------


# This script reads the original csv file contaning the list of effect sizes
#  used in the Maldonado et al., 2020 meta-analysis and makes adjustments


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(data.table)



# READING CSV FILES -------------------------------------------------------


ef_file <- "data/maldonado2020_ef.csv"

ef_data <- read.csv(ef_file)



# CLEAN CSV ---------------------------------------------------------------

# replace empty cells by NAs
ef_data[ef_data == ""] <- NA


# fill in missing information about the studies
ef_data <- ef_data %>% 
  fill(study, .direction = "down") %>% 
  fill(Citation, .direction = "down") %>% 
  fill(n_1, .direction = "down") %>% 
  fill(n_2, .direction = "down") %>% 
  fill(mean_age_1, .direction = "down") %>% 
  fill(mean_age_2, .direction = "down") %>% 
  mutate(year = as.numeric(gsub(".*?([0-9]+).*", "\\1", study)),
         row_num = 1:n()) %>% 
select(!X)

    

ef_data <- ef_data[, c(ncol(ef_data), 1, ncol(ef_data)-1, 2, 3:(ncol(ef_data)-2))]




# SAVE --------------------------------------------------------------------

 
write_csv(ef_data, file = "data/ef_data.csv")

