

#' `compute_es()` creates "effect_sizes_pref.csv", by computing standardized effect sizes using the outcomes 
#' information (e.g., means, SDs, correlations) in 'clean_study_data_*.csv'.
#' @param preference A character string to specify for which econ. preference should effect sizes be calculated. 
#' Can either be 'risk', 'time' or 'social'
#' @returns A data frame with details of all the studies of the specified econ. preference (e.g., first author, 
#' year of publication, sample size) along with the standardized effect size and their respective sampling variance
#' that is saved as a csv file in a designated folder. 
#' @examples
#' compute_es(preference = 'risk')

# library(tidyverse) # for data wrangling
# library(metafor) # compute effect sizes
# library(effectsize )# compute effect sizes
# 

compute_es <- function(preference) {
  
  
  # DATA --------------------------------------------------------------------
  
  filename <- case_when(preference == "time" ~ "data/summary/time/clean_study_bagaini_data_time.csv",
                        TRUE~ sprintf("data/summary/%s/clean_study_data_%s.csv",preference, preference))
  
  
  dat <- read_csv(filename,
                  col_types = cols())
  
  
  context_dat <- read_csv(sprintf("data/summary/%s/study_context_%s.csv",preference, preference), col_types = cols())
  
  
  
  
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
    mutate(age_min = case_when(dv_type_of_comparison == "both" ~ young_age_min,
                               TRUE ~ age_min), # in case age min/max info is unavailable
           age_max = case_when(dv_type_of_comparison == "both" ~ old_age_max,
                               TRUE ~ age_max),   # in case age min/max info is unavailable
           cor_type = "COR",
           study_design = case_when(dv_type_of_comparison == "both" ~"extreme_group",
                                    dv_type_of_comparison == "age continuous" ~ "continuous")) %>% 
    mutate(dec_diff =  .1*(age_max -age_min))
  
  
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
          first_author == "Horn" & grepl("thetic", title_of_article) ~ paste0(as.character(year_of_publication),"a"),
          first_author == "Horn" & !grepl("thetic", title_of_article) ~ paste0(as.character(year_of_publication),"b"),
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
          # multiple-study papers (each with a multiple samples)
          length(unique(sample_code))!= 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " ", sample_code, " (", year_of_pub_temp, ")"))) %>%
      group_by(study_label) %>% 
      # assign a number to each study
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # reverse effect sizes
      rowwise() %>% 
      mutate(reversed_es = case_when(study_label %in% c("Westbrook (2012)", "Herman (2018)", "Sproten experiment 2 (2018)",
                                                        "Albert (2012)", "Fawns-Ritchie (2022)", "Seaman (2016)", 
                                                        "Wood (2016)", "Bickel (2014)"  ) ~ 1,
                                     study_label == "Kurnianingsih (2015)" & domain_frame == "gain" ~ 1, 
                                     study_label == "Samanez-Larkin study 2 (2011)" & grepl("Risk-aversion", dv_description) ~ 1,
                                     TRUE ~ 0 ),
             cor_yi = case_when(reversed_es == 1  ~ cor_yi*-1, 
                                reversed_es == 0 ~cor_yi )) %>% 
      ungroup()
    
    
    # merge with study context data
    es_dat <- es_dat %>% left_join(context_dat, by = c("study_label", "year_of_publication",  "title_of_article" ))
    
    
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
          length(unique(sample_code))== 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")"),
        # multiple-study papers (each with a multiple samples)
        length(unique(sample_code))!= 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " ", sample_code, " (", year_of_pub_temp, ")"))) %>% 
      group_by(study_label) %>%  
      # assign a number to each study
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # reverse effect sizes
      rowwise() %>% 
      mutate(reversed_es = case_when(study_label %in% c("Mok experiment 1 (2020)",
                                                        "Bixter (2019)", 
                                                        "Skylark study 1b (2020)" ,
                                                        "Skylark study 2b (2020)" ,
                                                        "Ciaramelli (2021)", 
                                                        "Halilova (2022)", 
                                                        "Tunney (2022)",
                                                        "Westbrook experiment 2 (2013)",
                                                        "Fiorenzato (2022)") | grepl("Skylark study 1b|Skylark study 2b", study_label) ~ 1, 
                                     study_label == "Löckenhoff (2020)"  & grepl("sequence trend scores", dv_description) ~1,
                                     TRUE ~0 ),
             cor_yi = case_when(reversed_es == 1  ~ cor_yi*-1, 
                                reversed_es == 0 ~cor_yi )) %>% 
      ungroup()
    
    
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
    
    # attach gender information
    dt_gender <- read_csv("data/summary/time/seaman_gender_info.csv", col_types = cols()) %>% 
      select(-c(study_es_id, pref)) %>% 
      distinct(study_label,prop_female, young_prop_female, old_prop_female)%>% 
      mutate(temp_label = str_replace(tolower(study_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(prop_female = if_else(prop_female > 1, prop_female/100, prop_female),
             young_prop_female = if_else(young_prop_female > 1, young_prop_female/100, young_prop_female),
             old_prop_female = if_else(old_prop_female > 1, old_prop_female/100, old_prop_female)) %>% 
      select(-study_label)
    
    es_dat_seaman <- es_dat_seaman %>% 
      mutate(temp_label = str_replace(tolower(study_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      left_join(dt_gender, by = c("temp_label")) %>% 
      select(-temp_label)
    
    
    
    
    
    
    es_dat <- es_dat_bagaini %>% 
      bind_rows(es_dat_seaman) %>% 
      mutate(es_id = 1:n()) %>% 
      group_by(title_of_article) %>% 
      mutate(pub_id = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(temp_label = str_replace(tolower(study_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", ""))
    
    
    
    # merge with study context data
    context_dat <- context_dat %>%
      mutate(temp_label = str_replace(tolower(study_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      mutate(temp_label = str_replace(tolower(temp_label)," ", "")) %>% 
      select(-c(study_label, year_of_publication, author_extract, title_of_article))
    
    es_dat <- es_dat %>% left_join(context_dat, by = c("temp_label"))
    
    
    
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
          length(unique(sample_code))== 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")"),
          # multiple-study papers (each with a multiple samples)
          length(unique(sample_code))!= 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " ", sample_code, " (", year_of_pub_temp, ")"))) %>%
      group_by(study_label) %>% 
      # assign a number to each study label
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # reverse effect sizes
      rowwise() %>% 
      mutate(reversed_es = case_when(study_label == "Gong (2019)" & dv_units ==  "Discount rate" ~ 1, 
                                     TRUE ~0 ),
             cor_yi = case_when(reversed_es == 1  ~ cor_yi*-1, 
                                reversed_es == 0 ~cor_yi )) %>% 
      ungroup()
    
    
    # merge with study context data
    es_dat <- es_dat %>% left_join(context_dat, by = c("study_label", "year_of_publication",  "title_of_article" ))
    
    
    
  }
  
  
  
  
  # EFFORT: MERGE ES ------------------------------------------------------
  
  
  if (preference == "effort") {
    
    
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
          length(unique(sample_code))== 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")"),
          # multiple-study papers (each with a multiple samples)
          length(unique(sample_code))!= 1 & !grepl("single", paper_section) ~ paste0(first_author, " ", paper_section, " ", sample_code, " (", year_of_pub_temp, ")"))) %>%
      group_by(study_label) %>% 
      # assign a number to each study label
      mutate(study_id = cur_group_id()) %>% 
      ungroup() %>% 
      select(-c(year_of_pub_temp)) %>% 
      # reverse effect sizes
      rowwise() %>% 
      mutate(reversed_es = case_when(study_label %in% c("Hess (2021)", "McLaughlin (2021)",
                                                        "Westbrook experiment 2 (2013)", "Westbrook experiment 1 (2013)") ~ 1, 
                                     TRUE ~ 0),
             cor_yi = case_when(reversed_es == 1  ~ cor_yi*-1, 
                                reversed_es == 0 ~ cor_yi )) %>% 
      ungroup()
    
  }
  
  
  
  
  
  
  
  
  
  # SAVE OUTPUT ------------------------------------------------------------
  
  
  if (preference != "effort") {
    
    # keep certain variables 
    es_dat <- es_dat %>% 
      rename(domain = domain_frame) %>% 
      group_by(study_label) %>% 
      mutate(study_es_id = 1:n()) %>% # create id for effect sizes within each study
      ungroup() %>% 
      select(pref, pub_id, study_id, study_label, study_es_id, es_id, first_author, year_of_publication,
             title_of_article, paper_section, study_context, # pub
             task_name, task_type, domain, incentivization, dv_units, dv_description, # outcome
             sample_code, total_n, young_total_n, old_total_n, n_incl_es, # sample
             prop_female, young_prop_female, old_prop_female,
             age_min,mean_age,age_max, young_age_min, young_mean_age, young_age_max, old_age_min, old_mean_age, old_age_max, 
             dec_diff,
             dv_young_mean, dv_young_sd, dv_old_mean, dv_old_sd, dv_correlation,
             cor_yi, cor_vi, cor_type, study_design, reversed_es, source_of_outcome, # effect size values
             author_extract
      )
  }
  
  
  
  if (preference == "effort") {
    
    # keep certain variables 
    es_dat <- es_dat %>% 
      rename(domain = domain_frame) %>% 
      group_by(study_label) %>% 
      mutate(study_es_id = 1:n()) %>% # create id for effect sizes within each study
      ungroup() %>% 
      select(pref, pub_id, study_id, study_label, study_es_id, es_id, first_author, year_of_publication,
             title_of_article, paper_section, study_context, # pub
             task_name, task_type, effort_type, domain, incentivization, dv_units, dv_description, # outcome
             sample_code, total_n, young_total_n, old_total_n, n_incl_es, # sample
             prop_female, young_prop_female, old_prop_female,
             age_min,mean_age,age_max, young_age_min, young_mean_age, young_age_max, old_age_min, old_mean_age, old_age_max, 
             dec_diff,
             dv_young_mean, dv_young_sd, dv_old_mean, dv_old_sd, dv_correlation,
             cor_yi, cor_vi, cor_type, study_design, reversed_es, source_of_outcome, # effect size values
             author_extract
      )
  }
  
  
  
  
  write_csv(es_dat, sprintf("data/summary/%s/effect_sizes_%s.csv", preference, preference))
  
  print(
    sprintf("effect_sizes_%s.csv created successfully! Saved in:   data/summary/%s",
            preference, preference) 
  )
  
  
  
  
  
}
