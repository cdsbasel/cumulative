

#' `compute_es()` creates "effect_sizes_pref.csv", by computing standardized effect sizes using the outcomes 
#' information (e.g., means, SDs, correlations) in 'clean_study_data_*.csv'.
#' @param preference A character string to specify for which econ. preference should effect sizes be calculated. 
#' Can either be 'risk', 'time' or 'social'
#' @returns A data frame with details of all the studies of the specified econ. preference (e.g., first author, 
#' year of publication, sample size) along with the standardized effect size and their respective sampling variance
#' that is saved as a csv file in a designated folder. 
#' @examples
#' compute_es(preference = 'risk')

library(tidyverse) # for data wrangling
library(metafor) # compute effect sizes
library(effectsize )# compute effect sizes


compute_es <- function(preference) {
  
  
  # DATA --------------------------------------------------------------------
  
  filename <- case_when(preference == "time" ~ "data/summary/time/clean_study_bagaini_data_time.csv",
                        TRUE~ sprintf("data/summary/%s/clean_study_data_%s.csv",preference, preference))
  
  
  dat <- read_csv(filename,
                  col_types = cols())
  
  
  
  dat <- dat %>% 
    # adding SDs where needed
    mutate(dv_young_sd = case_when(dv_type_of_comparison %in% c("extreme group (m + sd)") & is.na(dv_young_sd) & 
                                     !is.na(dv_young_se) ~ metaDigitise::se_to_sd(dv_young_se, young_total_n),
                                   TRUE ~ dv_young_sd),
           dv_old_sd = case_when(dv_type_of_comparison %in% c("extreme group (m + sd)") & is.na(dv_old_sd) & 
                                   !is.na(dv_old_se) ~ metaDigitise::se_to_sd(dv_old_se, old_total_n),
                                 TRUE ~ dv_old_sd))
  
  
  
  
  # EFFECT SIZES ----------------------------------------------------------
  
  # means and SDS (young vs. old)
  smd_dat <- dat %>% 
    filter(dv_type_of_comparison %in% c("extreme group (m + sd)"))
  
  
  # add point-biserial correlation and its variance (Soper's 'exact' equation) to dataset
  smd_dat <- escalc(measure = "RPB",
                    m1i = dv_old_mean, m2i = dv_young_mean,
                    sd1i = dv_old_sd, sd2i = dv_young_sd,
                    n1i = old_total_n, n2i = young_total_n,
                    dat = smd_dat,
                    vtype = "LS",
                    replace=FALSE)
  
  smd_dat <- smd_dat %>% 
    # calculate age difference in decades (mean or midpoint in age range)
    mutate(dec_diff = case_when(!is.na(young_mean_age) &!is.na(old_mean_age) ~ .1*(old_mean_age -young_mean_age),
                                TRUE ~ .1*(((old_age_min+old_age_max)/2) - ((young_age_min+young_age_max)/2))),
           cor_type = "RPB",
           study_design = "extreme_group")
  
  
  #####################################################_
  
  # correlations
  cor_dat <- dat %>% 
    filter(dv_type_of_comparison %in% c("age continuous", "both")) # use correlations from extreme group designs if available
  
  # add yi=ri and vi (sampling variances) to dataset
  cor_dat <- escalc(measure = 'COR',
                    ri = dv_correlation, 
                    ni = total_n,
                    vtype = "LS",
                    data = cor_dat)
  
  
  cor_dat <-  cor_dat %>% 
    # calculate age difference in decades
    mutate(dec_diff = case_when(!is.na(age_min) &!is.na(age_max)~ .1*(age_max -age_min)),
           cor_type = "COR",
           study_design = case_when(dv_type_of_comparison == "both" ~"extreme_group",
                                    dv_type_of_comparison == "age continuous" ~ "continuous"))
  
  
  # bind data frames
  es <- bind_rows(smd_dat, cor_dat) %>% 
    select(pub_id, outcome_num, yi, vi, cor_type, study_design, dec_diff) %>% 
    rename(cor_yi = yi,
           cor_vi = vi) %>% 
    full_join(dat, by = c("pub_id", "outcome_num"))  %>% 
    mutate(pref = preference,
           author_extract = "Bagaini",
           es_id = 1:n(),
           # number of participants included in the ES calculation
           n_incl_es = case_when(dv_type_of_comparison == "age continuous" ~ total_n,
                                 # extreme group comparison included old vs. young differences
                                 dv_type_of_comparison != "age continuous" ~ young_total_n + old_total_n))
  
  
  
  
  
  #RISK: MERGE ES ----------------------------------------------------------
  
  if (preference == "risk") {
    
    es_dat <- es %>% 
      # create unique study/sample labels
      group_by(title_of_article) %>% 
      mutate(
        year_of_pub_temp = case_when(
          first_author == "Horn" & grepl("strat", title_of_article) ~ paste0(as.character(year_of_publication),"a"),
          first_author == "Horn" & !grepl("strat", title_of_article) ~ paste0(as.character(year_of_publication),"b"),
          TRUE ~ as.character(year_of_publication)),
        sample_code = tolower(sample_code),
        paper_section = tolower(paper_section),
        study_label = case_when(
          # single-study single-sample papers
          length(unique(sample_code))== 1 & grepl("single", paper_section) ~ paste0(first_author, " (", year_of_pub_temp, ")"),
          # single-study multiple-sample papers
          length(unique(sample_code))!= 1 & grepl("single", paper_section) ~ paste0(first_author, " ", sample_code, " (", year_of_pub_temp, ")"),
          # multiple-study papers (each with a single sample each)
          length(unique(sample_code))== 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")"),
          # multiple-study papers with several samples
          TRUE ~  paste0(first_author, " ", paper_section, " ",sample_code, " (", year_of_pub_temp, ")"))) %>% 
      group_by(study_label) %>% 
      # assign a number to each study
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # reverse effect sizes
      mutate(reversed_es = case_when(study_label %in% c("Westbrook (2012)", "Herman (2018)") ~ 1, 
                                     TRUE ~0 ),
             cor_yi = case_when(reversed_es == 1  ~ cor_yi*-1, 
                                reversed_es == 0 ~cor_yi ))
    
    
  }
  
  # TIME: MERGE ES ----------------------------------------------------------
  
  if (preference == "time") {
    
    es_dat_bagaini <- es %>% 
      # remove loss domain effect sizes
      filter(!domain_frame %in% c("loss", "negative")) %>% 
      # create unique study/sample labels
      group_by(title_of_article) %>% 
      mutate(
        year_of_pub_temp =  as.character(year_of_publication),
        sample_code = tolower(sample_code),
        paper_section = tolower(paper_section),
        study_label = case_when(
          # single-study single-sample papers
          length(unique(sample_code))== 1 & grepl("single", paper_section) ~ paste0(first_author, " (", year_of_pub_temp, ")"),
          # single-study multiple-sample papers
          length(unique(sample_code))!= 1 & grepl("single", paper_section) ~ paste0(first_author, " ", sample_code, " (", year_of_pub_temp, ")"),
          # multiple-study papers (each with a single sample each)
          length(unique(sample_code))== 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")"))) %>% 
      group_by(study_label) %>%  
      # assign a number to each study
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # reverse effect sizes
      mutate(reversed_es = case_when(study_label %in% c("Mok experiment 1 (2020)", "Lamichhane (2020)",
                                                        "Bixter (2019)", "Sparrow (2019)","Fawns-Ritchie (2022)") ~ 1, 
                                     TRUE ~0 ),
             cor_yi = case_when(reversed_es == 1  ~ cor_yi*-1, 
                                reversed_es == 0 ~cor_yi ))
    
    
    es_dat_bagaini <- es_dat_bagaini %>% 
      group_by(study_label) %>% 
      mutate(study_es_id = 1:n()) %>% # create id for effect sizes within each study
      ungroup() %>% 
      select(pref, pub_id, study_id, study_label, study_es_id, es_id, first_author, year_of_publication, title_of_article, paper_section, # pub
             task_name, task_type, domain_frame, incentivization, dv_units, dv_description, # outcome
             sample_code, total_n, young_total_n, old_total_n, n_incl_es, # sample
             prop_female, young_prop_female, old_prop_female,
             age_min,mean_age,age_max, young_age_min, young_mean_age, young_age_max, old_age_min, old_mean_age, old_age_max, 
             dec_diff,
             dv_young_mean, dv_young_sd, dv_old_mean, dv_old_sd, dv_correlation,
             cor_yi, cor_vi, cor_type, study_design, reversed_es, source_of_outcome, # effect size values
             author_extract
      )
    
    
    
    write_csv(es_dat_bagaini, "data/summary/time/effect_sizes_bagaini_time.csv")
    
    print(
      sprintf("effect_sizes_bagaini_time.csv created successfully! Saved in:   data/summary/%s",
              preference) 
    )
    
    
    # obtain Seaman et al ES data
    es_dat_seaman <- read_csv("data/summary/time/effect_sizes_seaman_time.csv",
                              col_types = cols()) %>% 
      mutate(author_extract = "Seaman") %>% 
      group_by(study_label) %>% 
      # assign a number to each study label
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(study_id = study_id + max(es_dat_bagaini$study_id))
    
    
    
    es_dat <- es_dat_bagaini %>% 
      bind_rows(es_dat_seaman) %>% 
      mutate(es_id = 1:n()) %>% 
      group_by(title_of_article) %>% 
      mutate(pub_id = cur_group_id()) %>% 
      ungroup() 
    
  }
  
  
  
  
  # SOCIAL: MERGE ES ------------------------------------------------------
  
  
  if (preference == "social") {
    
    
    # bind data frames
    es_dat <- es %>% 
      # create unique study/sample labels
      group_by(title_of_article) %>% 
      mutate(
        year_of_pub_temp =  as.character(year_of_publication),
        sample_code = tolower(sample_code),
        paper_section = tolower(paper_section),
        study_label = case_when(
          # single-study single-sample papers
          length(unique(sample_code))== 1 & grepl("single", paper_section) ~ paste0(first_author, " (", year_of_pub_temp, ")"),
          # single-study multiple-sample papers
          length(unique(sample_code))!= 1 & grepl("single", paper_section) ~ paste0(first_author, " ", sample_code, " (", year_of_pub_temp, ")"),
          # multiple-study papers (each with a single sample each)
          length(unique(sample_code))== 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")"))) %>% 
      group_by(study_label) %>% 
      # assign a number to each study label
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # no effect sizes to reverse
      mutate(reversed_es = 0)
    
    
    
  }
  
  
  # SAVE OUTPUT ------------------------------------------------------------
  
  # keep certain variables 
  es_dat <- es_dat %>% 
    rename(domain = domain_frame) %>% 
    group_by(study_label) %>% 
    mutate(study_es_id = 1:n()) %>% # create id for effect sizes within each study
    ungroup() %>% 
    select(pref, pub_id, study_id, study_label, study_es_id, es_id, first_author, year_of_publication, title_of_article, paper_section, # pub
           task_name, task_type, domain, incentivization, dv_units, dv_description, # outcome
           sample_code, total_n, young_total_n, old_total_n, n_incl_es, # sample
           prop_female, young_prop_female, old_prop_female,
           age_min,mean_age,age_max, young_age_min, young_mean_age, young_age_max, old_age_min, old_mean_age, old_age_max, 
           dec_diff,
           dv_young_mean, dv_young_sd, dv_old_mean, dv_old_sd, dv_correlation,
           cor_yi, cor_vi, cor_type, study_design, reversed_es, source_of_outcome, # effect size values
           author_extract
    )
  
  
  
  write_csv(es_dat, sprintf("data/summary/%s/effect_sizes_%s.csv", preference, preference))
  
  print(
    sprintf("effect_sizes_%s.csv created successfully! Saved in:   data/summary/%s",
            preference, preference) 
  )
  
  
  
  
  
}
