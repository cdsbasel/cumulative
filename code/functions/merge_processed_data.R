
#' `merge_processed_data()` creates "clean_study_data_*.csv", merges and harmonizes the outputs from 'processing_plot_data()', 'processing_raw_data()', 'processing_table_data()' and 'covidence_extraction_format()' into a single data frame.
#' @param prefernece A character string to specify for which econ. preference should data frames be merged Can either be 'risk', 'time' or  'social'
#' @returns A data frame with details of all the studies of the specified econ. preference (e.g., first author, year of publication, sample size) that 
#' is saved as a csv file in a designated folder.
#' @examples
#' merge_processed_data(preference = 'risk')


library(tidyverse)


merge_processed_data <- function(preference) {
  
  calc_prop_female <- function(dat) {
    
    dat <- dat %>%
      mutate(prop_female = case_when(
        grepl("cont",tolower(dv_type_of_comparison)) & is.na(n_female) & is.na(prop_female) & is.na(prop_male) ~ 1 - (n_male/total_n),
        grepl("cont",tolower(dv_type_of_comparison)) & !is.na(n_female)& is.na(prop_female) & is.na(prop_male) ~ n_female/total_n,
        grepl("cont",tolower(dv_type_of_comparison)) & !is.na(prop_female) ~ prop_female,
        grepl("cont",tolower(dv_type_of_comparison)) & !is.na(prop_male) & is.na(prop_female) ~ 1 - prop_male),
        young_prop_female = case_when(
          !grepl("cont",tolower(dv_type_of_comparison)) & is.na(young_n_female) & is.na(young_prop_female) & is.na(young_prop_male) ~ 1 - (young_n_male/young_total_n),
          !grepl("cont",tolower(dv_type_of_comparison)) & !is.na(young_n_female)& is.na(young_prop_female) & is.na(young_prop_male) ~ young_n_female/young_total_n,
          !grepl("cont",tolower(dv_type_of_comparison)) & !is.na(young_prop_female) ~ young_prop_female,
          !grepl("cont",tolower(dv_type_of_comparison)) & !is.na(young_prop_male) & is.na(young_prop_female) ~ 1 - young_prop_male),
        old_prop_female = case_when(
          !grepl("cont",tolower(dv_type_of_comparison)) & is.na(old_n_female) & is.na(old_prop_female) & is.na(old_prop_male) ~ 1 - (old_n_male/old_total_n),
          !grepl("cont",tolower(dv_type_of_comparison)) & !is.na(old_n_female)& is.na(old_prop_female) & is.na(old_prop_male) ~ old_n_female/old_total_n,
          !grepl("cont",tolower(dv_type_of_comparison)) & !is.na(old_prop_female) ~ old_prop_female,
          !grepl("cont",tolower(dv_type_of_comparison)) & !is.na(old_prop_male) & is.na(old_prop_female) ~ 1 - old_prop_male))
    
    return(dat)
    
    
  }
  
  
  
  
  
  
  if (preference == "risk") {
    # RISK: DATA --------------------------------------------------------------------
    
    covidence_dat <- read_csv("data/covidence/risk_ageing_formated.csv", col_types =  cols())
    plot_dat <- read_csv("data/plots/risk/processed_plot_data_risk.csv", col_types = cols())
    table_dat <- read_csv("data/tables/risk/processed_table_data_risk.csv", col_types = cols())
    raw_dat <- read_csv("data/raw_data/risk/processed_raw_data_risk.csv", col_types = cols())
    
    
    # RISK: COMBINE DATA ------------------------------------------------------------
    
    #raw data (> 1 studies)
    basic_vars <-  c("first_author", "year_of_publication","title_of_article","outcome_num")
    
    raw_dat <- raw_dat %>% 
      mutate(temp_label = paste0(first_author, as.character(year_of_publication))) 
    
    covidence_dat <- covidence_dat %>% 
      mutate(temp_label = paste0(first_author, as.character(year_of_publication))) 
    
    
    #process Zilker et al 2020 separately
    raw_dat_sub <- raw_dat %>% 
      filter(temp_label != "Zilker2020") 
    raw_dat_sub <- raw_dat_sub %>% 
      select_if(function(x){!all(is.na(x))}) 
    
    
    r_covidence_dat <- covidence_dat %>% 
      # select in the covidence data the relevant studies
      filter(temp_label %in% unique(raw_dat_sub$temp_label)) %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(raw_dat_sub))))) %>% 
      left_join(raw_dat_sub, by = basic_vars) 
    
    
    #process Zilker et al 2020 separately
    zilker_raw_dat <- raw_dat %>% 
      filter(temp_label == "Zilker2020") %>% 
      mutate(pub_id = unique(covidence_dat$pub_id[covidence_dat$first_author == "Zilker" & covidence_dat$year_of_publication == 2020]),
             dv_summary_stats_in_figure = unique(covidence_dat$dv_summary_stats_in_figure[covidence_dat$first_author == "Zilker" & covidence_dat$year_of_publication == 2020])) 
    
    
    
    r_covidence_dat <- bind_rows(r_covidence_dat, zilker_raw_dat)%>% 
      mutate(temp_label = paste0(first_author, as.character(year_of_publication)))
    
    
    # plot data (> 1 studies)
    basic_vars <-  c("first_author", "year_of_publication","title_of_article","outcome_num")
    
    p_covidence_dat <- covidence_dat %>% 
      mutate(temp_label = paste0(first_author, as.character(year_of_publication))) %>% 
      # select in the covidence data the relevant studies
      filter(temp_label %in% unique(paste0(plot_dat$first_author,as.character(plot_dat$year_of_publication)))) %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(plot_dat))))) %>% 
      left_join(plot_dat, by = basic_vars) %>% 
      mutate(temp_label = paste0(first_author, as.character(year_of_publication)))
    
    
    
    
    
    # table data (>1 studies)
    basic_vars <-  c("first_author", "year_of_publication","outcome_num")
    
    t_covidence_dat <- covidence_dat %>% 
      # select in the covidence data the relevant studies
      filter(first_author %in% unique(table_dat$first_author)) %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(table_dat))))) %>%
      left_join(table_dat, by = basic_vars)%>% 
      mutate(temp_label = paste0(first_author, as.character(year_of_publication)))
    
    
    # RISK: FINAL DATA --------------------------------------------------------------
    
    f_covidence_dat <- covidence_dat %>%
      mutate(temp_label = paste0(first_author, as.character(year_of_publication))) %>% 
      filter(!temp_label %in% unique(c(t_covidence_dat$temp_label,r_covidence_dat$temp_label,p_covidence_dat$temp_label))) %>% 
      bind_rows(t_covidence_dat, p_covidence_dat, r_covidence_dat) %>% 
      select(-temp_label)%>% 
      # adding sample sizes where needed
      mutate(young_total_n = case_when(is.na(young_total_n) & !is.na(young_n_female)  & !is.na(young_n_male)~ young_n_female + young_n_male,
                                       TRUE ~ young_total_n),
             old_total_n = case_when(is.na(old_total_n ) & !is.na(old_n_female)  & !is.na(old_n_male)~ old_n_female + old_n_male,
                                     TRUE ~ old_total_n),
             total_n = case_when(is.na(total_n ) & !is.na(old_total_n)  & !is.na(young_total_n)~ young_total_n + old_total_n,
                                 TRUE ~ total_n)) %>% 
      #adjusting units for certain outcomes
      mutate(dv_old_mean = case_when(first_author %in% c("Mayhorn","RÖnnlund") ~ round((dv_old_mean/100)*old_total_n),
                                     !first_author  %in% c("Mayhorn","RÖnnlund") ~ dv_old_mean),
             dv_young_mean = case_when(first_author  %in% c("Mayhorn","RÖnnlund") ~ round((dv_young_mean/100)*young_total_n),
                                       !first_author  %in% c("Mayhorn","RÖnnlund") ~ dv_young_mean)) %>% 
      rowwise() %>% 
      # standardize labels
      mutate(dv_type_of_comparison = case_when(grepl("sd",tolower(dv_type_of_comparison)) ~ "extreme group (m + sd)",
                                               grepl("both",tolower(dv_type_of_comparison)) ~ "both",
                                               grepl("cont",tolower(dv_type_of_comparison)) ~ "age continuous",
                                               grepl("prop",tolower(dv_type_of_comparison)) ~ "extreme group (ratio)"),
             domain_frame = tolower(domain_frame),
             incentivization = tolower(incentivization),
             source_of_outcome = case_when(grepl("raw|osf|data", tolower(dv_summary_stats_in_figure)) ~ "raw data",
                                           grepl("figure", tolower(dv_summary_stats_in_figure)) ~ "figure",
                                           grepl("table", tolower(dv_summary_stats_in_figure)) ~ "table",
                                           is.na(dv_summary_stats_in_figure) ~ "directly from article"),
             task_type = tolower(task_type),
             task_type = case_when(grepl("descrip", task_type) ~ "description",
                                   grepl("exp", task_type) ~ "experience",
                                   grepl("mix", task_type) ~ "mixed"),
             incentivization = case_when(grepl("inc", incentivization) ~ "incentivized",
                                         TRUE ~ incentivization),
             task_name = str_to_title(task_name))%>% 
      ungroup()
    
    # where possible calculate prop. of female participants
    f_covidence_dat <- calc_prop_female(f_covidence_dat)
    
    
  }
  
  
  
  if (preference == "time") {
    
    
    
    # TIME: DATA --------------------------------------------------------------------
    
    covidence_dat <- read_csv("data/covidence/time_ageing_formated.csv", col_types = cols())
    plot_dat <- read_csv("data/plots/time/processed_plot_data_time.csv", col_types = cols())
    raw_dat <- read_csv("data/raw_data/time/processed_raw_data_time.csv", col_types = cols())
    
    
    # TIME: COMBINE DATA ------------------------------------------------------------
    
    #raw data (> 1 studies)
    basic_vars <-  c("first_author", "year_of_publication","title_of_article","outcome_num")
    
    raw_dat_sub <- raw_dat %>% 
      filter(first_author != "Skylark") %>% 
      select_if(function(x){!all(is.na(x))}) 
    
    
    #skylark is exception because too many conditions to specify in the form
    r_covidence_dat <- covidence_dat %>% 
      # select in the covidence data the relevant studies
      filter(first_author %in% unique(raw_dat_sub$first_author)) %>% 
      filter(first_author != "Skylark") %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(raw_dat_sub))))) %>% 
      left_join(raw_dat_sub, by = basic_vars)
    
    
    #process Skylark et al separately
    skylark_raw_dat <- raw_dat %>% 
      filter(first_author == "Skylark") %>% 
      mutate(pub_id = unique(covidence_dat$pub_id[covidence_dat$first_author == "Skylark"]),
             dv_summary_stats_in_figure = unique(covidence_dat$dv_summary_stats_in_figure[covidence_dat$first_author == "Skylark"])) 
    
    
    r_covidence_dat <- bind_rows(r_covidence_dat, skylark_raw_dat)
    
    
    # plot data (only 1 study)
    basic_vars <-  c("outcome_num")
    
    p_covidence_dat <- covidence_dat %>% 
      # select in the covidence data the relevant studies
      filter(first_author %in% unique(plot_dat$first_author)) %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(plot_dat))))) %>% 
      left_join(plot_dat, by = basic_vars)
    
    
    # covidence data 
    # mutate Löckenhoff separately (reports Kendall's tau) +
    # mutate Schüller separately (reports Spearman's rho) +
    # conversion formula from:
    #  Walker, David A. (2003) "JMASM9: Converting Kendall’s Tau For Correlational Or 
    #  Meta-Analytic Analyses," Journal of Modern Applied Statistical Methods: Vol. 2 : Iss. 2 , Article 26.
    #  DOI: 10.22237/jmasm/1067646360
    #  Rupinski, M. T., & Dunlap, W. P. (1996). Approximating Pearson Product-Moment Correlations 
    #  from Kendall’s Tau and Spearman’s Rho. Educational and Psychological Measurement, 56(3), 
    #  419–429. doi:10.1177/0013164496056003004
    covidence_dat <- covidence_dat %>% 
      mutate(dv_correlation = case_when(first_author == "Löckenhoff" ~  sin(.5*pi*dv_correlation),
                                        first_author == "Schüller" ~  2*sin(dv_correlation*(pi/6)),
                                        TRUE ~ dv_correlation))
    
    
    
    
    # TIME: FINAL DATA --------------------------------------------------------------
    
    f_covidence_dat <- covidence_dat %>% 
      filter(!first_author %in% c(unique(raw_dat$first_author),unique(plot_dat$first_author))) %>% 
      bind_rows(p_covidence_dat, r_covidence_dat)%>% 
      # adding sample sizes where needed
      mutate(young_total_n = case_when(is.na(young_total_n) & !is.na(young_n_female)  & !is.na(young_n_male)~ young_n_female + young_n_male,
                                       TRUE ~ young_total_n),
             old_total_n = case_when(is.na(old_total_n ) & !is.na(old_n_female)  & !is.na(old_n_male)~ old_n_female + old_n_male,
                                     TRUE ~ old_total_n),
             total_n = case_when(is.na(total_n) & !is.na(old_total_n)  & !is.na(young_total_n)~ young_total_n + old_total_n,
                                 TRUE ~ total_n)) %>% 
      rowwise() %>% 
      # standardize comparison labels
      mutate(dv_type_of_comparison = case_when(grepl("sd",tolower(dv_type_of_comparison)) ~ "extreme group (m + sd)",
                                               grepl("both",tolower(dv_type_of_comparison)) ~ "both",
                                               grepl("cont",tolower(dv_type_of_comparison)) ~ "age continuous",
                                               grepl("prop",tolower(dv_type_of_comparison)) ~ "extreme group (ratio)"),
             domain_frame = tolower(domain_frame),
             incentivization = tolower(incentivization),
             source_of_outcome = case_when(grepl("raw|osf|data", tolower(dv_summary_stats_in_figure)) ~ "raw data",
                                           grepl("figure", tolower(dv_summary_stats_in_figure)) ~ "figure",
                                           grepl("table", tolower(dv_summary_stats_in_figure)) ~ "table",
                                           is.na(dv_summary_stats_in_figure) ~ "directly from article"),
             task_type = tolower(task_type),
             task_type = case_when(grepl("descrip", task_type) ~ "description",
                                   grepl("exp", task_type) ~ "experience",
                                   grepl("mix", task_type) ~ "mixed"),
             incentivization = case_when(grepl("inc", incentivization) ~ "incentivized",
                                         TRUE ~ incentivization),
             task_name = case_when(grepl("delay|discount*", tolower(task_name))~ "Delay-Discounting Task",
                                   TRUE ~ str_to_title(task_name)))%>% 
      ungroup()
    
    
    # where possible calculate prop. of female participants
    f_covidence_dat <- calc_prop_female(f_covidence_dat)
    
  }
  
  
  
  
  if (preference == "social") {
    
    
    # SOCIAL: DATA --------------------------------------------------------------------
    
    covidence_dat <- read_csv("data/covidence/social_ageing_formated.csv", col_types = cols())
    plot_dat <- read_csv("data/plots/social/processed_plot_data_social.csv", col_types = cols())
    raw_dat <- read_csv("data/raw_data/social/processed_raw_data_social.csv", col_types = cols())
    
    
    # SOCIAL: COMBINE DATA ------------------------------------------------------------
    
    #raw data (> 1 studies)
    basic_vars <-  c("first_author", "year_of_publication","title_of_article","outcome_num", "dv_units")
    
    r_covidence_dat <- covidence_dat %>% 
      # select in the covidence data the relevant studies
      filter(first_author %in% unique(raw_dat$first_author)) %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(raw_dat))))) %>% 
      left_join(raw_dat, by = basic_vars)
    
    
    # plot data (only 1 study)
    basic_vars <-  c("outcome_num")
    
    p_covidence_dat <- covidence_dat %>% 
      # select in the covidence data the relevant studies
      filter(first_author %in% unique(plot_dat$first_author)) %>% 
      select(all_of(c(basic_vars, setdiff(colnames(covidence_dat), colnames(plot_dat))))) %>% 
      left_join(plot_dat, by = basic_vars)
    
    
    
    # SOCIAL: FINAL DATA --------------------------------------------------------------
    
    f_covidence_dat <- covidence_dat %>% 
      filter(!first_author %in% c(unique(raw_dat$first_author),unique(plot_dat$first_author))) %>% 
      bind_rows(p_covidence_dat, r_covidence_dat) %>% 
      # adding sample sizes where needed
      mutate(young_total_n = case_when(is.na(young_total_n) & !is.na(young_n_female)  & !is.na(young_n_male)~ young_n_female + young_n_male,
                                       TRUE ~ young_total_n),
             old_total_n = case_when(is.na(old_total_n ) & !is.na(old_n_female)  & !is.na(old_n_male)~ old_n_female + old_n_male,
                                     TRUE ~ old_total_n),
             total_n = case_when(is.na(total_n ) & !is.na(old_total_n)  & !is.na(young_total_n)~ young_total_n + old_total_n,
                                 TRUE ~ total_n)) %>% 
      #adjusting units for certain outcomes
      mutate(dv_old_mean = case_when(first_author == "Cobo-Reyes" ~ round((dv_old_mean/100)*old_total_n),
                                     first_author != "Cobo-Reyes" ~ dv_old_mean),
             dv_young_mean = case_when(first_author == "Cobo-Reyes" ~ round((dv_young_mean/100)*young_total_n),
                                       first_author != "Cobo-Reyes" ~ dv_young_mean)) %>% 
      rowwise() %>% 
      # standardize labels
      mutate(dv_type_of_comparison = case_when(grepl("sd",tolower(dv_type_of_comparison)) ~ "extreme group (m + sd)",
                                               grepl("both",tolower(dv_type_of_comparison)) ~ "both",
                                               grepl("cont",tolower(dv_type_of_comparison)) ~ "age continuous",
                                               grepl("prop",tolower(dv_type_of_comparison)) ~ "extreme group (ratio)"),
             domain_frame = tolower(domain_frame),
             incentivization = tolower(incentivization),
             task_type = tolower(task_type),
             source_of_outcome = case_when(grepl("raw|osf|data", tolower(dv_summary_stats_in_figure)) ~ "raw data",
                                           grepl("figure", tolower(dv_summary_stats_in_figure)) ~ "figure",
                                           grepl("table", tolower(dv_summary_stats_in_figure)) ~ "table",
                                           is.na(dv_summary_stats_in_figure) ~ "directly from article"),
             task_type = case_when(grepl("descrip", task_type) ~ "description",
                                   grepl("exp", task_type) ~ "experience",
                                   grepl("mix", task_type) ~ "mixed"),
             incentivization = case_when(grepl("inc", incentivization) ~ "incentivized",
                                         TRUE ~ incentivization),
             task_name = case_when(grepl("dictator", tolower(task_name))~ "Dictator Game",
                                   grepl("discounting", tolower(task_name))~ "Social Discounting Task",
                                   TRUE ~ str_to_title(task_name)))%>% 
      ungroup()
    
    
    # where possible calculate prop. of female participants
    f_covidence_dat <- calc_prop_female(f_covidence_dat)
    
  }
  
  
  
  # SAVE OUTPUT -------------------------------------------------------------
  
  
  
  if (preference != "time") {
    
    write_csv(f_covidence_dat, sprintf("data/summary/%s/clean_study_data_%s.csv", preference, preference)) 
    
    print(
      sprintf("clean_study_data_%s.csv created successfully! Saved in:   data/summary/%s",
              preference, preference)
    )
    
  }
  
  if (preference == "time") {
    
    write_csv(f_covidence_dat, "data/summary/time/clean_study_bagaini_data_time.csv")
    
    print("clean_study_bagaini_data_time.csv created successfully! Saved in:   data/summary/time"
    )
    
  }
  
}


