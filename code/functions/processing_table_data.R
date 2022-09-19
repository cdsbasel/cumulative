
#' `processing_table_data()` creates "processed_table_data_*.csv", by first reading the data
#'  originating from large tables reported in the papers of the studies that investigated 
#'  the specified econ. preference, calculates summary statistics that reflect age effects 
#'  (e.g., means, SDs) and formats the output such as to allow further analysis.
#' @param preference A character string to specify for which econ. preference should the raw data
#'  file be summarized. Can either be 'risk', 'time' or 'social'
#' @returns A data frame with the summarized information of the table data of the studies of
#'  the specified econ. preference (e.g., first author, year of publication, sample size) that 
#'  is saved as a csv file in a designated folder.
#' @examples
#' processing_table_data(preference = 'risk')


# library(tidyverse) # for data wrangling
# library(janitor)  # for renaming column names


processing_table_data <- function(preference) {
  
  
  if (preference == "risk") {
    # RISK: HESS 2018 --------------------------------------------------------
    
    data_hess <- function() {
      dat <- read_csv("data/tables/risk/Hess_2018/hess_2018_table4.csv",
                      col_types = cols())
      
      
      dat_hess <- dat %>% 
        mutate(dv_young_mean = 1 - dv_young_mean, # switch from safe to risky choice
               dv_old_mean = 1 - dv_old_mean) %>% 
        group_by(prob_level, condition,  first_author, year_of_publication, title_of_article) %>% 
        summarise(dv_young_mean = mean(dv_young_mean),
                  dv_young_sd = sqrt(mean(dv_young_sd^2)),
                  dv_old_mean = mean(dv_old_mean),
                  dv_old_sd = sqrt(mean(dv_old_sd^2)),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(dv_description = sprintf("Proportion of times the risky option was chosen in %s %s trials", 
                                        condition, prob_level),
               dv_units = "Proportion risky choice",
               paper_section = "Experiment 2",
               outcome_num = case_when(condition == "Description only" & grepl("Low", prob_level) ~ 1,
                                       condition == "Description only" & grepl("High", prob_level) ~ 2,
                                       condition == "Description and experience" & grepl("Low", prob_level) ~ 3,
                                       condition == "Description and experience" & grepl("High", prob_level) ~ 4,
                                       condition == "Experience only" & grepl("Low", prob_level) ~ 5,
                                       condition == "Experience only" & grepl("High", prob_level) ~ 6)) %>% 
        select(-c(condition, prob_level))
      
      
      return(dat_hess)
      
    }
    
    # RISK: MATHER 2021 --------------------------------------------------------
    
    data_mather <- function() {
      dat <- read_csv("data/tables/risk/Mather_2012/mather_2012_table3_5_7.csv",
                      col_types = cols())
      dat_mather <- dat %>% 
        group_by(paper_section, lottery_set, first_author, year_of_publication, title_of_article) %>% 
        summarise( dv_young_sd = sd(dv_young_mean),
                   dv_young_mean = mean(dv_young_mean),
                   dv_old_sd = sd(dv_old_mean),
                   dv_old_mean = mean(dv_old_mean),
                   .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(grepl("1", paper_section) & grepl("Equal-EV sure-gain", lottery_set) ~ 1,
                                       grepl("1", paper_section) & grepl("Unequal-EV sure-gain", lottery_set) ~ 2,
                                       grepl("1", paper_section) & grepl("Equal-EV risky-gain", lottery_set) ~ 3,
                                       grepl("1", paper_section) & grepl("Unequal-EV risky-gain", lottery_set) ~ 4,
                                       grepl("3", paper_section) & grepl("Equal-EV sure-loss", lottery_set) ~ 5,
                                       grepl("3", paper_section) & grepl("Unequal-EV sure-loss", lottery_set) ~ 6,
                                       grepl("4", paper_section) & grepl("Equal EV risky-gain", lottery_set) ~ 9,
                                       grepl("4", paper_section) & grepl("Equal EV risky-loss", lottery_set) ~ 10,
                                       grepl("4", paper_section) & grepl("Equal-EV sure-gain", lottery_set) ~ 7,
                                       grepl("4", paper_section) & grepl("Equal-EV sure-loss", lottery_set) ~ 8),
               dv_description = sprintf("Proportion of participants who selected the risky or riskier option in %s trials.", 
                                        lottery_set),
               dv_units = "Proportion of participants selecting the risky or riskier option") %>% 
        select(-c(lottery_set))
      
      
      return(dat_mather)
      
    }
    
    # RISK: COMBINE PROCESSED TABLE DATA ----------------------------------------------
    
    dat_pref <- bind_rows(data_hess(),
                          data_mather())
  }
  
  
  # NO TABLE DATA FOR TIME OR SOCIAL PREFERENCE
  
  # SAVE OUTPUT -------------------------------------------------------------
  if (preference == "risk") {
    
    write_csv(dat_pref, sprintf("data/tables/%s/processed_table_data_%s.csv", preference, preference)) 
    
    print(
      sprintf("processed_table_data_%s.csv created successfully! Saved in:   data/tables/%s",
              preference, preference)
    )
    
  }
}
