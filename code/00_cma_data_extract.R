

# DESCRIPTION -------------------------------------------------------------


# This script reads the sheets in the excel workbook that contains the effect
#  sizes that were manually extracted from the different meta-analyses that
#  rersults form our serch. It compiles these it into a singles csv file

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(data.table)



# READING XLSX FILES -------------------------------------------------------


cma_file <- "cumulative_ma_data.xlsx"
# list the sheets in excel workbook


ma_list <- excel_sheets(cma_file)
counter <- 1
data_list <- NULL


for (CurrMA in ma_list) {
  
  if (tolower(CurrMA) %like% "mata") {
    
    ma_data <-data.frame(read_xlsx(cma_file, CurrMA))
    
    colnames(ma_data) <- c("study",
                           "year",
                           "task",
                           "n_young",
                           "n_old",
                           "n",
                           "mean_age_young",
                           "mean_age_old",
                           "mean_edu_younger",
                           "mean_edu_older",
                           "g",
                           "g_gain_dom",
                           "g_loss_dom")   
    
    ma_data[ma_data == "-"] <- NA
    ma_data <- ma_data %>% mutate(pref = "risk",
                                  ma_origin = CurrMA)
  }
  
  
  if (tolower(CurrMA) %like% "seaman") {
    
    ma_data <-data.frame(read_xlsx(cma_file, CurrMA))
    
    colnames(ma_data) <- c("ignore",
                           "study",
                           "year",
                           "n",
                           "g",
                           "w_fix",
                           "w_rang")   
    
    
    ma_data <- ma_data %>% mutate(pref = "time",
                                  ma_origin = CurrMA) %>% 
      select(-ignore) %>% filter(!is.na(study))
  }
  
  
  if (tolower(CurrMA) %like% "sparrow") {
    
    ma_data <-data.frame(read_xlsx(cma_file, CurrMA))
    
    colnames(ma_data) <- c("study",
                           "year",
                           "task",
                           "n_young",
                           "n_old",
                           "mean_age_young",
                           "mean_age_old",
                           "task_type",
                           "task_inc",
                           "task_anon",
                           "g")   
    
    
    ma_data <- ma_data %>% mutate(pref = "altruism",
                                  ma_origin = CurrMA)
  } 
  

  
  if (tolower(CurrMA) %like% "best") {
    
    ma_data <-data.frame(read_xlsx(cma_file, CurrMA))
    
    colnames(ma_data) <- c("study",
                           "year",
                           "n_young",
                           "n_old",
                           "task_scen",
                           "task_stak",
                           "g_pos_fram",
                           "g_neg_fram")   
    
    ma_data[ma_data == "-"] <- NA
    ma_data <- ma_data %>% mutate(pref = "risk",
                                  ma_origin = CurrMA)
  } 
  
  
  data_list[[counter]] <- ma_data %>% 
    mutate(across(everything(), as.character))
  
  counter <- counter + 1
}



# standard df, consistent for all meta-analyses
standard_df <- tibble(
  pref = character(),
  ma_origin = character(),
  study = character(),
  year = character(),
  n = character(),
  n_young = character(),
  n_old = character(),
  mean_age_young = character(),
  mean_age_old = character(),
  mean_edu_younger = character(),
  mean_edu_older = character(),
  task = character(),
  task_type = character(),
  task_inc = character(),
  task_anon = character(),
  task_scen = character(),
  task_stak = character(),
  g = character(),
  g_pos_fram = character(),
  g_neg_fram = character(),
  w_fix = character(),
  w_rang = character(),
  g_gain_dom = character(),
  g_loss_dom = character())


tidydata <- bind_rows(data_list) %>%
  bind_rows(standard_df)  %>% select(colnames(standard_df))




write_csv(tidydata, file = "cma_data.csv")
