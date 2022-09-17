

#' `covidence_extraction_format()` creates "pref_ageing_formated.csv", by first reading the .csv file 
#' that was extracted from Covidence (i.e.,*_ageing_extracted.csv) for a specified econ. preference 
#' and  will format it such as to allow the processing and analysis of the data
#' @param preference A character string to specify for which econ. preference should the extracted 
#' data be formatted. Can either be 'risk', 'time' or  'social'
#' @returns A data frame with details of all the studies of the specified econ. preference (e.g., 
#' first author, year of publication, sample size) that is saved as a csv file in a designated folder.
#' @examples
#' covidence_extraction_format(preference = 'risk')



library(tidyverse) # data wrangling
library(janitor) # clean column names
library(readxl) # open excel file


covidence_extraction_format <- function(preference) {
  
  
  cols <- read_xlsx("data/study_data_codebook.xlsx", sheet = "codebook") #order of columns
  
  if (preference == "risk") {
    
    
    
    # RISK --------------------------------------------------------------------
    dat <- read_csv("data/covidence/risk_ageing_extracted.csv", col_types = cols())
    
    #cleaning columns names
    dat <- clean_names(dat)
    colnames(dat)[602:769] <- paste0("dv_", colnames(dat)[602:769]) # indicate that these last columns are related to the outcome of interest  
    
    
    dat_l <- dat %>% 
      select(first_author:dv_summary_stats_in_figure_outcome_12) %>% 
      mutate_all(., as.character()) %>% 
      mutate_if(is.logical, as.character) %>%
      mutate_if(is.double, as.character) %>% 
      pivot_longer(-c(first_author:title_of_article),
                   names_to = "var",
                   values_to = "val") %>% 
      rowwise() %>% 
      mutate(outcome_num = unlist(str_split(var, "_outcome_", n = 2))[2],
             info = unlist(str_split(var, "_outcome_", n = 2))[1]) %>% 
      ungroup() %>% 
      filter(!is.na(val)) %>% 
      select(-var)
    
    #publication ID
    pub_id <- dat_l %>% distinct(title_of_article) %>% mutate(pub_id = as.character(1:n()))
    
    dat_lw <- dat_l %>% pivot_wider(names_from = info, values_from = val) %>% 
      left_join(pub_id, by = "title_of_article") %>% 
      mutate(sample_code = if_else(is.na(grp_sample_code),sample_code , grp_sample_code)) %>% # had to rename these sets of columns in the raw data otherwise there are duplicates that are generated
      select(-grp_sample_code) %>% 
      rename(domain_frame = frame)
    
    # place columns in the correct order
    df <- data.frame(matrix(nrow = 1, ncol = nrow(cols)))
    colnames(df) <- cols$column_name
    df <- df %>% 
      bind_rows(dat_lw) %>% 
      filter(!is.na(pub_id))
    
  }
  
  if (preference == "time") {
    
    
    
    # TIME --------------------------------------------------------------------
    
    dat <- read_csv("data/covidence/time_ageing_extracted.csv", col_types = cols())
    
    #cleaning columns names
    dat <- clean_names(dat)
    colnames(dat)[602:769] <- paste0("dv_", colnames(dat)[602:769]) # indicate that these last columns are related to the outcome of interest  
    
    
    dat_l <- dat %>% 
      select(first_author:dv_summary_stats_in_figure_outcome_12) %>% 
      mutate_all(., as.character()) %>% 
      mutate_if(is.logical, as.character) %>%
      mutate_if(is.double, as.character) %>% 
      pivot_longer(-c(first_author:title_of_article),
                   names_to = "var",
                   values_to = "val") %>% 
      rowwise() %>% 
      mutate(outcome_num = unlist(str_split(var, "_outcome_", n = 2))[2],
             info = unlist(str_split(var, "_outcome_", n = 2))[1]) %>% 
      ungroup() %>% 
      filter(!is.na(val)) %>% 
      select(-var)
    
    #publication ID
    pub_id <- dat_l %>% distinct(title_of_article) %>% mutate(pub_id = as.character(1:n()))
    
    dat_lw <- dat_l %>% pivot_wider(names_from = info, values_from = val) %>% 
      left_join(pub_id, by = "title_of_article") %>% 
      mutate(sample_code = if_else(is.na(grp_sample_code),sample_code , grp_sample_code)) %>% # had to rename these sets of columns in the raw data otherwise there are duplicates that are generated
      select(-grp_sample_code) %>% 
      rename(domain_frame = frame)
    
    # place columns in the correct order
    df <- data.frame(matrix(nrow = 1, ncol = nrow(cols)))
    colnames(df) <- cols$column_name
    df <- df %>% 
      bind_rows(dat_lw) %>% 
      filter(!is.na(pub_id))
    
  }
  
  
  
  if (preference == "social") {
    
    # SOCIAL --------------------------------------------------------------------
    
    dat <- read_csv("data/covidence/social_ageing_extracted.csv", col_types = cols())
    
    #cleaning columns names
    dat <- clean_names(dat)
    colnames(dat)[602:769] <- paste0("dv_", colnames(dat)[602:769]) # indicate that these last columns are related to the outcome of interest  
    
    
    dat_l <- dat %>% 
      select(first_author:dv_summary_stats_in_figure_outcome_12) %>% 
      mutate_all(., as.character()) %>% 
      mutate_if(is.logical, as.character) %>%
      mutate_if(is.double, as.character) %>% 
      pivot_longer(-c(first_author:title_of_article),
                   names_to = "var",
                   values_to = "val") %>% 
      rowwise() %>% 
      mutate(outcome_num = unlist(str_split(var, "_outcome_", n = 2))[2],
             info = unlist(str_split(var, "_outcome_", n = 2))[1]) %>% 
      ungroup() %>% 
      filter(!is.na(val)) %>% 
      select(-var)
    
    #publication ID
    pub_id <- dat_l %>% distinct(title_of_article) %>% mutate(pub_id = as.character(1:n()))
    
    dat_lw <- dat_l %>% pivot_wider(names_from = info, values_from = val) %>% 
      left_join(pub_id, by = "title_of_article") %>% 
      mutate(sample_code = if_else(is.na(grp_sample_code),sample_code , grp_sample_code)) %>% # had to rename these sets of columns in the raw data otherwise there are duplicates that are generated
      select(-grp_sample_code)
    
    # place columns in the correct order
    df <- data.frame(matrix(nrow = 1, ncol = nrow(cols)))
    colnames(df) <- cols$column_name
    df <- df %>% 
      bind_rows(dat_lw) %>% 
      filter(!is.na(pub_id))
    
  }
  
  
  
  # SAVE OUTPUT -------------------------------------------------------------
  
  
  write_csv(df, sprintf("data/covidence/%s_ageing_formated.csv", preference))
  
  print(
    sprintf("%s_ageing_formated.csv created successfully! Saved in: data/covidence/",
            preference) 
  )
  
  
  
}









