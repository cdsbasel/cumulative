

#' `processing_raw_data()` reads the available raw data files (in data/raw_data/) of the studies that investigated the specified econ. preference, calculates summary statistics that reflect age effects (e.g., means, SDs or correlations) and formats the output such as to allow further analysis.
#' @param preference A character string to specify for which econ. preference should the raw data file be summarized. Can either be 'risk', 'time' or  'social'
#' @returns A data frame with the summarized information of the raw data  of the studies of the specified econ. preference (e.g., first author, year of publication, sample size)
#' @examples
#' processing_raw_data(preference = 'risk')

# 
# library(tidyverse) # for data wrangling
# library(haven) # for reading certain data files
# library(foreign) # for reading certain data files
# library(readxl) # for reading certain data files
# library(janitor) # for cleaning up column names


processing_raw_data <- function(preference) {
  
  
  
  
  if (preference == "risk") {
    
    
    # RISK FREY 2021 -----------------------------------------------------------
    
    frey_data <- function() {
      load("data/raw_data/risk/Frey_2021/overview.Rdata")
      
      # not using the "_imp" data frame
      dat_frey <- overview %>% 
        select(partid, age, sex,R.dfd2,R.dfd4, R.dfe2, R.dfe4) %>% 
        pivot_longer(-c(partid, age, sex), names_to = "cond", values_to = "risk") %>% 
        group_by(cond) %>% 
        summarise(dv_correlation = cor(risk, age),
                  age_min = min(age),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female =  mean(sex == "female"),
                  .groups = "drop") %>%
        ungroup() %>% 
        mutate(outcome_num = case_when(cond == "R.dfd2" ~ 1,
                                       cond == "R.dfd4" ~ 2,
                                       cond == "R.dfe2" ~ 3,
                                       cond == "R.dfe4" ~ 4)) %>% 
        mutate(first_author = "Frey",
               year_of_publication = "2021",
               title_of_article = "Identifying Robust Correlates of Risk Preference: A Systematic Approach Using Specification Curve Analysis") %>% 
        select(-c(cond))
      
      return(dat_frey)
      
      
    }
    # RISK GAECHTER 2021 -----------------------------------------------------------
    
    gaechter_data <- function() {
      dat <- read_csv("data/raw_data/risk/Gaechter_2021/GJH.csv",
                      col_types = cols())
      
      # Which lotteries did people accept? 1 = reject all, ..., 7 = accept all 
      dat <- dat %>% 
        select(respondent, lotterychoice, age, female) %>% 
        filter(!is.na(lotterychoice) & age %in% c(1,3,6)) %>% 
        mutate(age_group = case_when(age == 1	~	"18-24",
                                     age == 3	~	"35-44",
                                     age == 6	~ "65+")) 
      
      
      
      
      dat_y <- dat %>% 
        filter(age == 1) %>% 
        summarise(dv_young_mean = mean(lotterychoice),
                  dv_young_sd = sd(lotterychoice),
                  young_age_min = 18,
                  young_age_max = 24,
                  young_total_n = n(),
                  young_prop_female = mean(female))
      
      
      dat_m <- dat %>% 
        filter(age == 3) %>% 
        summarise(dv_middle_mean = mean(lotterychoice),
                  dv_middle_sd = sd(lotterychoice),
                  middle_age_min = 35,
                  middle_age_max = 44,
                  middle_total_n = n(),
                  middle_prop_female = mean(female))
      
      
      
      dat_o <- dat %>% 
        filter(age == 6) %>% 
        summarise(dv_old_mean = mean(lotterychoice),
                  dv_old_sd = sd(lotterychoice),
                  old_age_min = 65,
                  old_age_max = NA,
                  old_total_n = n(),
                  old_prop_female = mean(female))
      
      dat_gaechter <- bind_cols(dat_y, dat_m, dat_o) %>% 
        mutate(outcome_num = 1,
               # dv_units = "Lottery choice category",
               first_author = "Gaechter",
               year_of_publication = "2022",
               # dv_description = "Point when participants switch between the risky and the safe option.",
               title_of_article = "Individual-level loss aversion in riskless and risky choices")
      
      return(dat_gaechter)
      
    }
    
    
    # RISK: HORN 2022A ---------------------------------------------------------
    
    horn2022a_data <- function() {
      
      dat <- read.csv2("data/raw_data/risk/Horn_2021/Dataset.Real.Hypothetical.Rewards.HornFreund.csv", header = TRUE, sep = ";", dec = ",")
      
      dat <- dat %>% 
        select(AgeGroup, Age, Case, P.Choices.A.Hyp.High,
               P.Choices.A.Hyp.Low, P.Choices.A.Real.Low, 
               P.Choices.A.Real.High) %>% 
        pivot_longer(P.Choices.A.Hyp.High: P.Choices.A.Real.High, 
                     names_to = "cond", values_to = "safe_choice_prop") %>% 
        mutate(risky_choice_prop = 1 - safe_choice_prop,
               incentive =  case_when(grepl("Hyp", cond) ~ "Hyp", TRUE ~ "Real"),
               stakes =  case_when(grepl("Low", cond) ~ "Low", TRUE ~ "High")) %>% 
        filter(!is.na(risky_choice_prop)) %>% 
        select(-c(cond, safe_choice_prop))
      
      
      
      dat_y <- dat %>% 
        filter(AgeGroup == "Younger.Adults") %>% 
        group_by(incentive, stakes) %>% 
        summarise(dv_young_mean = mean(risky_choice_prop),
                  dv_young_sd = sd(risky_choice_prop),
                  young_age_min = min(Age),
                  young_mean_age = mean(Age),
                  young_sd_age = sd(Age),
                  young_age_max = max(Age),
                  young_total_n = n(),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(incentive == "Hyp" & stakes == "Low"~ 1, 
                                       incentive == "Hyp" & stakes == "High"~ 2, 
                                       incentive == "Real" & stakes == "Low"~ 3, 
                                       incentive == "Real" & stakes == "High"~ 4)) %>% 
        select(-c(incentive, stakes))
      
      
      
      dat_o <- dat %>% 
        filter(AgeGroup == "Older.Adults") %>% 
        group_by(incentive, stakes) %>% 
        summarise(dv_old_mean = mean(risky_choice_prop),
                  dv_old_sd = sd(risky_choice_prop),
                  old_age_min = min(Age),
                  old_mean_age = mean(Age),
                  old_sd_age = sd(Age),
                  old_age_max = max(Age),
                  old_total_n = n(),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(incentive == "Hyp" & stakes == "Low"~ 1, 
                                       incentive == "Hyp" & stakes == "High"~ 2, 
                                       incentive == "Real" & stakes == "Low"~ 3, 
                                       incentive == "Real" & stakes == "High"~ 4)) %>% 
        select(-c(incentive, stakes))
      
      
      dat_cor <- dat %>% 
        group_by(incentive, stakes) %>% 
        summarise(dv_correlation = cor(risky_choice_prop, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(incentive == "Hyp" & stakes == "Low"~ 1, 
                                       incentive == "Hyp" & stakes == "High"~ 2, 
                                       incentive == "Real" & stakes == "Low"~ 3, 
                                       incentive == "Real" & stakes == "High"~ 4)) %>% 
        select(-c(incentive, stakes))
      
      
      
      dat_horn <- dat_y %>% 
        left_join(dat_o, by = "outcome_num")%>% 
        left_join(dat_cor, by = "outcome_num")%>% 
        mutate(first_author = "Horn",
               year_of_publication = "2022",
               title_of_article = "Adult age differences in monetary decisions with real and hypothetical reward")
      
      
      return(dat_horn)
      
    }
    
    # RISK: SEAMAN 2018 -------------------------------------------------------
    
    seaman_data <- function() {
      
      
      dat <- read_csv("data/raw_data/risk/Seaman_2018/skew_la_long.csv",
                      col_types = cols())
      
      # acceptance rate by gamble type (only consider the mixed trials,
      #  as there was just one gain/loss trial for each gamble)
      dat_condA <- dat %>%
        filter(Domain == "Mixed") %>% 
        group_by(ResponseID, Age,Gender, Gamble) %>% 
        summarise(risky = mean(Accept),
                  .groups = "drop") %>% 
        group_by(Gamble) %>% 
        summarise(dv_correlation = cor(risky, Age),
                  age_min = min(Age, na.rm = TRUE),
                  mean_age = mean(Age, na.rm = TRUE),
                  sd_age = sd(Age, na.rm = TRUE),
                  age_max = max(Age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(Gender == "Female"),
                  .groups = "drop") %>%
        ungroup() %>% 
        mutate(outcome_num = case_when(Gamble == "Positive Skew" ~ 1,
                                       Gamble == "Negative Skew" ~ 2,
                                       Gamble == "Symmetric" ~ 3)) %>% 
        mutate(first_author = "Seaman",
               year_of_publication = "2018",
               title_of_article = "Individual Differences in Loss Aversion and Preferences for Skewed Risks Across Adulthood") %>% 
        select(-Gamble)
      
      
      #acceptance rate by domain for gain/loss trial
      dat_condB <- dat %>%
        filter(Domain != "Mixed") %>% 
        group_by(ResponseID, Age,Gender, Domain) %>% 
        summarise(risky = mean(Accept),
                  .groups = "drop") %>% 
        group_by(Domain) %>% 
        summarise(dv_correlation = cor(risky, Age),
                  age_min = min(Age, na.rm = TRUE),
                  mean_age = mean(Age, na.rm = TRUE),
                  sd_age = sd(Age, na.rm = TRUE),
                  age_max = max(Age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(Gender == "Female"),
                  .groups = "drop") %>%
        ungroup() %>% 
        mutate(outcome_num = case_when(Domain == "Gain" ~ 4,
                                       Domain == "Loss" ~ 5)) %>% 
        mutate(first_author = "Seaman",
               year_of_publication = "2018",
               title_of_article = "Individual Differences in Loss Aversion and Preferences for Skewed Risks Across Adulthood") %>% 
        select(-Domain)
      
      
      
      
      dat_seaman <- bind_rows(dat_condA, dat_condB)
      
      
      return(dat_seaman)
    }
    
    # RISK PACHUR 2017 --------------------------------------------------------
    pachur_data <- function() {
      
      # saved separately as a .csv the "Data" tab
      dat <- read_csv("data/raw_data/risk/Pachur_2017/PachurEtAl_Who errs, who dares_Data.csv",
                      col_types = cols())
      
      dat <- dat %>%
        mutate(CV_choice = as.numeric(CV_choice)) %>% 
        filter(!is.na(CV_choice)& !is.na(Age)) %>% 
        group_by(Subject, Age_group, Age, Gender, Domain) %>% 
        summarise(risky = mean(CV_choice),
                  .groups = "drop")
      
      
      dat_y <- dat %>% 
        filter(Age_group == "younger") %>% 
        group_by(Domain) %>% 
        summarise(dv_young_mean = mean(risky),
                  dv_young_sd = sd(risky),
                  young_age_min = min(Age),
                  young_mean_age = mean(Age),
                  young_sd_age = sd(Age),
                  young_age_max = max(Age),
                  young_total_n = n(),
                  young_prop_female = mean(Gender == "female"),
                  .groups = "drop")%>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(Domain == "gains" ~ 1, 
                                       Domain == "losses" ~ 2,
                                       Domain == "mixed" ~ 3))%>% 
        select(-Domain)
      
      
      dat_o <- dat %>% 
        filter(Age_group == "older") %>%
        group_by(Domain) %>%
        summarise(dv_old_mean = mean(risky),
                  dv_old_sd = sd(risky),
                  old_age_min = min(Age),
                  old_mean_age = mean(Age),
                  old_sd_age = sd(Age),
                  old_age_max = max(Age),
                  old_total_n = n(),
                  old_prop_female = mean(Gender == "female"),
                  .groups = "drop")%>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(Domain == "gains" ~ 1, 
                                       Domain == "losses" ~ 2,
                                       Domain == "mixed" ~ 3))%>% 
        select(-Domain)
      
      
      dat_cor <- dat %>% 
        group_by(Domain) %>%
        summarise(dv_correlation = cor(risky, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female = mean(Gender == "female"),
                  .groups = "drop")%>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(Domain == "gains" ~ 1, 
                                       Domain == "losses" ~ 2,
                                       Domain == "mixed" ~ 3)) %>% 
        select(-Domain)
      
      
      dat_pachur <- dat_y %>% 
        left_join(dat_o, by = "outcome_num")%>% 
        left_join(dat_cor, by = "outcome_num")%>% 
        mutate(first_author = "Pachur",
               year_of_publication = "2017",
               title_of_article = "Who Dares, Who Errs? Disentangling Cognitive and Motivational Roots of Age Differences in Decisions Under Risk")
      
      
      return(dat_pachur)
      
    }
    
    
    # RISK: ZILKER 2020 -------------------------------------------------------
    
    zilker2020_data <- function() {
      
      ##### STUDY 1
      load("data/raw_data/risk/Zilker_2020/data_long_study1")
      ##### STUDY 2
      load("data/raw_data/risk/Zilker_2020/data_long_study2")
      
      zilker_outcome_num <-  function(dat, study_num) {
        dat <-  dat %>% 
          mutate(outcome_num = case_when(condition == "1" & domain == "gain" & study_num == 1 ~ 1,
                                         condition == "2" & domain == "gain" & study_num == 1 ~ 2,
                                         condition == "3" & domain == "gain" & study_num == 1 ~ 3,
                                         condition == "1" & domain == "loss" & study_num == 1 ~ 4,
                                         condition == "2" & domain == "loss" & study_num == 1 ~ 5,
                                         condition == "3" & domain == "loss" & study_num == 1 ~ 6,
                                         condition == "B1" & domain == "gain" & study_num == 2 ~ 7,
                                         condition == "B1_zero" & domain == "gain" & study_num == 2 ~ 8,
                                         condition == "B2" & domain == "gain" & study_num == 2 ~ 9,
                                         condition == "B2_zero" & domain == "gain" & study_num == 2 ~ 10,
                                         condition == "B3" & domain == "gain" & study_num == 2 ~ 11,
                                         condition == "B1" & domain == "loss" & study_num == 2 ~ 12,
                                         condition == "B1_zero" & domain == "loss" & study_num == 2 ~ 13,
                                         condition == "B2" & domain == "loss" & study_num == 2 ~ 14,
                                         condition == "B2_zero" & domain == "loss" & study_num == 2 ~ 15,
                                         condition == "B3" & domain == "loss" & study_num == 2 ~ 16)) %>% 
          mutate(condition = case_when(study_num == 1 & condition == 1 ~ "Simple Safe",
                                       study_num == 1 & condition == 2 ~ "Complex Safe",
                                       study_num == 1 & condition == 3 ~ "Risky",
                                       study_num == 2 & condition == "B1"~ "Simple Safe",
                                       study_num == 2 & condition == "B2" ~ "Complex Safe",
                                       study_num == 2 & condition == "B3"~ "Risky",
                                       study_num == 2 & condition == "B1_zero"~ "Simple Safe (zero)",
                                       study_num == 2 & condition == "B2_zero" ~ "Complex Safe (zero)"))
        
        return(dat)
      }
      
      
      zilker_2020_dat <- function(dat, study_num) {
        
        dat <- dat %>% 
          filter(!is.na(higher_CV_choice)) %>% 
          group_by(subject_no, age, agegroup, gender, condition, domain) %>% 
          summarise(risky_choice = mean(higher_CV_choice),
                    .groups = "drop")%>%
          mutate(condition = as.character(condition),
                 gender = as.character(gender)) %>% 
          ungroup()
        
        # young adults
        dat_y <- dat %>% 
          filter(agegroup %in% c("YA", "Younger")) %>% 
          group_by(condition, domain) %>% 
          summarise( dv_young_mean = mean(risky_choice),
                     dv_young_sd = sd(risky_choice),
                     young_age_min = min(age, na.rm = TRUE),
                     young_mean_age = mean(age, na.rm = TRUE),
                     young_sd_age = sd(age, na.rm = TRUE),
                     young_age_max = max(age, na.rm = TRUE),
                     young_total_n = n(),
                     young_prop_female =mean(gender %in% c("2", "w")),
                     .groups = "drop")  %>% 
          ungroup()
        
        # old adults 
        dat_o <- dat %>% 
          filter(agegroup %in% c("OA", "Older")) %>% 
          group_by(condition, domain) %>% 
          summarise( dv_old_mean = mean(risky_choice),
                     dv_old_sd = sd(risky_choice),
                     old_age_min = min(age, na.rm = TRUE),
                     old_mean_age = mean(age, na.rm = TRUE),
                     old_sd_age = sd(age, na.rm = TRUE),
                     old_age_max = max(age, na.rm = TRUE),
                     old_total_n = n(),
                     old_prop_female = mean(gender %in% c("2", "w")),
                     .groups = "drop")  %>% 
          ungroup()
        
        
        #correlation
        dat_cor <- dat %>% 
          group_by(condition, domain) %>% 
          summarise(dv_correlation = cor(risky_choice, age),
                    age_min = min(age, na.rm = TRUE),
                    mean_age = mean(age, na.rm = TRUE),
                    sd_age = sd(age, na.rm = TRUE),
                    age_max = max(age, na.rm = TRUE),
                    total_n = n(),
                    prop_female = mean(gender %in% c("2", "w")),
                    .groups = "drop")  %>% 
          ungroup()
        
        
        dat_y <- zilker_outcome_num(dat_y, study_num)
        dat_o <- zilker_outcome_num(dat_o, study_num)
        dat_cor <- zilker_outcome_num(dat_cor, study_num)
        
        # adding information for all the study
        dat_sum <- dat_y %>% 
          left_join(dat_o, by = c("outcome_num", "condition", "domain")) %>% 
          left_join(dat_cor, by =  c("outcome_num", "condition", "domain")) %>% 
          mutate(first_author = "Zilker",
                 dv_units = "Risky choice",
                 year_of_publication = "2020",
                 dv_description = sprintf("Proportion of Higher CV choice in %s %s trials", domain, condition),
                 domain_frame = domain,
                 title_of_article = "Age Differences in Risk Attitude Are Shaped by Option Complexity",
                 sample_code = "Sample 1",
                 dv_type_of_comparison = "Both",
                 task_name = "Risky Choice task",
                 task_type = "Descriptive",
                 incentivization = "Incentivized",
                 paper_section = sprintf("Study %d", study_num)) %>% 
          select(-c(domain, condition))
        
        
        
        return(dat_sum)
        
      }
      
      ## combine
      dat_zilker2020 <- bind_rows(zilker_2020_dat(dat = data_long, study_num = 1),
                                  zilker_2020_dat(dat = data_long_study2, study_num = 2))
      
      return(dat_zilker2020)
      
    }
    # RISK: ZILKER 2021 -------------------------------------------------------
    
    zilker2021_data <- function(){
      
      ## ISSUES WITH THE FILE (had to change the extension: from .Rda to a .csv file)
      dat <- read.csv("data/raw_data/risk/Zilker_2021/LA_files_matrix.csv",
                      header = TRUE, sep = ";", dec = ",")
      
      dat <- dat %>% 
        filter(EV_B == 0) %>%  # select no distractor trials
        filter(!is.na(higher_SD_choice) & !is.na(age)) %>% 
        group_by(subjid, age, agegroup, gender, typeA, typeB) %>% 
        summarise(risky_choice = mean(higher_SD_choice),
                  .groups = "drop") %>% 
        ungroup()
      
      # by condition
      dat_y <- dat %>% 
        filter(agegroup == "YA") %>% 
        group_by(typeA, typeB) %>% 
        summarise(dv_young_mean = mean(risky_choice),
                  dv_young_sd = sd(risky_choice),
                  young_age_min = min(age, na.rm = TRUE),
                  young_mean_age = mean(age, na.rm = TRUE),
                  young_sd_age = sd(age, na.rm = TRUE),
                  young_age_max = max(age, na.rm = TRUE),
                  young_total_n = n(),
                  young_prop_female =mean(gender == "w"),
                  .groups = "drop")  %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(typeB == "simple safe" ~ 1,
                                       typeB == "complex safe" ~ 2,
                                       typeB == "risky mixed" ~ 3)) %>% 
        select(-c(typeA, typeB))
      
      dat_o <- dat %>% 
        filter(agegroup == "OA") %>% 
        group_by(typeA, typeB) %>% 
        summarise(dv_old_mean = mean(risky_choice),
                  dv_old_sd = sd(risky_choice),
                  old_age_min = min(age, na.rm = TRUE),
                  old_mean_age = mean(age, na.rm = TRUE),
                  old_sd_age = sd(age, na.rm = TRUE),
                  old_age_max = max(age, na.rm = TRUE),
                  old_total_n = n(),
                  old_prop_female = mean(gender == "w"),
                  .groups = "drop")  %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(typeB == "simple safe" ~ 1,
                                       typeB == "complex safe" ~ 2,
                                       typeB == "risky mixed" ~ 3)) %>% 
        select(-c(typeA, typeB))
      
      
      
      
      dat_cor <- dat %>% 
        group_by(typeA, typeB) %>% 
        summarise(dv_correlation = cor(risky_choice, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(gender == "w"),
                  .groups = "drop")  %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(typeB == "simple safe" ~ 1,
                                       typeB == "complex safe" ~ 2,
                                       typeB == "risky mixed" ~ 3)) %>% 
        select(-c(typeA, typeB))
      
      dat_zilker2021 <- dat_y %>% 
        left_join(dat_o, by = "outcome_num") %>% 
        left_join(dat_cor, by = "outcome_num") %>% 
        mutate(first_author = "Zilker",
               year_of_publication = "2021",
               title_of_article = "Does option complexity contribute to the framing effect, loss aversion, and delay discounting in younger and older adults?")
      
      
      return(dat_zilker2021)
    }
    
    
    # RISK: MAMEROW 2016 ------------------------------------------------------
    
    mamerow_data <- function() {
      
      
      load("data/raw_data/risk/Mamerow_2016/gfk.Rdata")
      
      
      dat_mamerow <- gfk %>% 
        select(partid, age, sex, adjpumps16, adjpumps32, expl16, expl32, R_catch, R_normal) %>% 
        pivot_longer(adjpumps16:R_normal, names_to = "outcome", values_to = "risky_choice") %>% 
        filter(!is.na(risky_choice)) %>% 
        group_by(outcome) %>% 
        summarise(dv_correlation = cor(risky_choice, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(sex == 1),
                  .groups = "drop")  %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(outcome == "R_normal" ~ 1,
                                       outcome == "R_catch" ~ 2,
                                       outcome == "adjpumps16" ~ 3,
                                       outcome == "adjpumps32" ~ 4, 
                                       outcome == "expl16" ~ 5,
                                       outcome == "expl32" ~ 6),
               first_author = "Mamerow",
               year_of_publication = "2016",
               title_of_article = "Risk Taking Across the Life Span: A Comparison of Self-Report and Behavioral Measures of Risk Taking") %>% 
        select(-outcome)
      
      
      return(dat_mamerow)
      
    }
    
    
    # RISK: RUTLEDGE 2016 -----------------------------------------------------
    
    rutledge_data <- function() {
      
      dat <- read_csv("data/raw_data/risk/Rutledge_2016/rutledge_data.csv",
                      col_types = cols())
      
      dat_beh <- dat %>% 
        filter(age %in% c(1,3,6,7)) %>% 
        mutate(age_group = case_when(age == 1 ~ "young",
                                     age == 3 ~ "middle",
                                     TRUE ~ "old")) %>% 
        pivot_longer(gainGam:lossGam,
                     names_to = "gamble_type",
                     values_to = "prop_risky") %>% 
        mutate(outcome_num = case_when(grepl("gain", gamble_type) ~ 1,
                                       grepl("loss", gamble_type) ~ 2, 
                                       grepl("mix", gamble_type) ~ 3)) %>% 
        group_by(age_group, gamble_type, outcome_num) %>% 
        summarise(mean = mean(prop_risky),
                  sd = sd(prop_risky),
                  .groups = "drop") %>% 
        pivot_wider(outcome_num,
                    names_from = age_group,
                    values_from = c(mean,sd),
                    names_glue = paste0("dv_","{age_group}_{.value}"))
      
      
      
      
      dat_dem <- dat %>% 
        filter(age %in% c(1,4,6,7)) %>% 
        mutate(age_group = case_when(age == 1 ~ "young",
                                     age == 4 ~ "middle",
                                     TRUE ~ "old")) %>% 
        pivot_longer(gainGam:lossGam,
                     names_to = "gamble_type",
                     values_to = "prop_risky") %>% 
        mutate(outcome_num = case_when(grepl("gain", gamble_type) ~ 1,
                                       grepl("loss", gamble_type) ~ 2, 
                                       grepl("mix", gamble_type) ~ 3)) %>% 
        group_by(age_group, gamble_type, outcome_num) %>% 
        summarise(total_n = n(),
                  prop_female = mean(gender == 1),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(age_min = case_when(age_group == "young" ~ 18,
                                   age_group == "middle" ~ 40,
                                   age_group == "old" ~ 60),
               age_max = case_when(age_group == "young" ~ 25,
                                   age_group == "middle" ~ 49)) %>% 
        pivot_wider(outcome_num,
                    names_from = age_group,
                    values_from = c(total_n, age_min, age_max),
                    names_glue = paste0("{age_group}_{.value}"))
      
      dat_rutledge <- dat_beh %>% 
        left_join(dat_dem, by = "outcome_num") %>% 
        mutate( first_author = "Rutledge",
                year_of_publication = "2016",
                title_of_article = "Risk Taking for Potential Reward Decreases across the Lifespan")
      
      
      return(dat_rutledge)
      
      
    }
    
    
    
    
    # RISK: HORN 2022B ---------------------------------------------------------
    
    horn2022b_data <- function() {
      
      dat <- read.csv2("data/raw_data/risk/Horn_2022/Dataset.PayOnePayAll.csv", header = TRUE, sep = ";", dec = ",")
      
      dat <- dat %>% 
        select(Age.Group, Age, Count, Gender, IncentiveCondition,
               RKgain, RKmixed, RKloss) %>% 
        pivot_longer(RKgain: RKloss, 
                     names_to = "domain", values_to = "risky_choice_prop") %>% 
        mutate(condition = paste0(IncentiveCondition, "_",domain)) %>% 
        filter(!is.na(risky_choice_prop))
      
      
      
      dat_y <- dat %>% 
        filter(Age.Group == "Younger") %>% 
        group_by(condition) %>% 
        summarise(dv_young_mean = mean(risky_choice_prop),
                  dv_young_sd = sd(risky_choice_prop),
                  young_age_min = min(Age),
                  young_mean_age = mean(Age),
                  young_sd_age = sd(Age),
                  young_age_max = max(Age),
                  young_total_n = n(),
                  young_prop_female = mean(Gender == "female"),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(grepl("All.*gain", condition)~ 1,
                                       grepl("One.*gain", condition)~ 2,
                                       grepl("All.*mix", condition)~ 3,
                                       grepl("One.*mix", condition)~ 4,
                                       grepl("All.*loss", condition)~ 5,
                                       grepl("One.*loss", condition)~ 6)) %>% 
        select(-c(condition))
      
      
      dat_o <- dat %>% 
        filter(Age.Group == "Older") %>% 
        group_by(condition) %>% 
        summarise(dv_old_mean = mean(risky_choice_prop),
                  dv_old_sd = sd(risky_choice_prop),
                  old_age_min = min(Age),
                  old_mean_age = mean(Age),
                  old_sd_age = sd(Age),
                  old_age_max = max(Age),
                  old_total_n = n(),
                  old_prop_female = mean(Gender == "female"),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(grepl("All.*gain", condition)~ 1,
                                       grepl("One.*gain", condition)~ 2,
                                       grepl("All.*mix", condition)~ 3,
                                       grepl("One.*mix", condition)~ 4,
                                       grepl("All.*loss", condition)~ 5,
                                       grepl("One.*loss", condition)~ 6)) %>% 
        select(-c(condition))
      
      
      dat_cor <- dat %>% 
        group_by(condition) %>% 
        summarise(dv_correlation = cor(risky_choice_prop, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female = mean(Gender == "female"),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(grepl("All.*gain", condition)~ 1,
                                       grepl("One.*gain", condition)~ 2,
                                       grepl("All.*mix", condition)~ 3,
                                       grepl("One.*mix", condition)~ 4,
                                       grepl("All.*loss", condition)~ 5,
                                       grepl("One.*loss", condition)~ 6)) %>% 
        select(-c(condition))
      
      
      dat_horn <- dat_y %>% 
        left_join(dat_o, by = "outcome_num")%>% 
        left_join(dat_cor, by = "outcome_num")%>% 
        mutate(first_author = "Horn",
               year_of_publication = "2022",
               title_of_article = "Pay One or Pay All? The Role of Incentive Schemes in Decision Making Across Adulthood")
      
      
      return(dat_horn)
      
    }
    
    
    
    
    
    
    
    
    # RISK: RODRIGUES ---------------------------------------------------------
    rodrigues_data <- function() {
      dat <- read_xlsx("data/raw_data/risk/Rodrigues_2022/Data_Rodrigues_Ruthenberg_Mussel_Hewig_2021.xlsx")
      
      
      dat <- dat %>% 
        select(`gain only not exploded`, `mixed not exploded`,
               `count explosions gain only`, `count explosions mixed`,
               age, gender) %>% 
        mutate(id = 1:n()) %>% 
        rename(gain_adjpumps = `gain only not exploded`,
               mixed_adjpumps = `mixed not exploded`,
               gain_explosions =  `count explosions gain only`,
               mixed_explosions = `count explosions mixed`) %>% 
        pivot_longer(cols  = -c(id, gender, age), names_to = "metric", values_to = "risk")
      
      
      rodrigues_dat <- dat %>% 
        group_by(metric) %>% 
        summarise(dv_correlation = cor(risk, age),
                  age_min = min(age),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = mean(gender == 4),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(grepl("gain_adjpumps", metric)~ 1,
                                       grepl("mixed_adjpumps", metric)~ 2,
                                       grepl("gain_explosions", metric)~ 3,
                                       grepl("mixed_explosions", metric)~ 4)) %>% 
        select(-c(metric)) %>% 
        mutate(first_author = "Rodrigues",
               year_of_publication = "2022",
               title_of_article = "Never mind losing the pound... still got the penny! The influence of trait greed on risky decision behavior in a mixed and gain only BART")
      
      
      return(rodrigues_dat)
      
    }
    
    
    # RISK: SEAMAN 2016 ---------------------------------------------------------
    seaman2016_data <- function() {
      dat <- read_csv("data/raw_data/risk/Seaman_2016/all_sublevel_data.csv", 
                      col_types = cols())
      
      dat <- dat %>% filter(task == "Probability_money")
      
      seaman_dat  <- dat %>% 
        filter(!is.na(Age) & !is.na(k)) %>% 
        summarise(dv_correlation = cor(k, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female =  mean(Sex == "F"),
                  .groups = "drop")%>% 
        ungroup()  %>% 
        mutate(outcome_num = 1,
               first_author = "Seaman",
               year_of_publication = "2016",
               title_of_article = "Adult age differences in decision making across domains: Increased discounting of social and health-related rewards")
      
      
      return(seaman_dat)
    }
    
    
    
    # RISK: THRAILKILL 2022 -----------------------------------------------------
    
    thrailkill_data <- function() {
      
      dat <- read_xlsx("data/raw_data/risk/Thrailkill_2022/Thrailkill et al_Data_online.xlsx")
      
      dat_thrailkill <- dat %>% 
        mutate(age = Age_inmonths/12) %>% 
        filter(!is.na(age) & !is.na(MGT_mean) & !is.na(Gender_M_F_O)) %>% 
        summarise(dv_correlation = cor(MGT_mean, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(Gender_M_F_O == 1),
                  .groups = "drop")%>% 
        mutate(outcome_num = 1,
               # dv_units = "Risky choice",
               first_author = "Thrailkill",
               year_of_publication = "2022",
               title_of_article = "Loss aversion and risk for cigarette smoking and other substance use")
      
      
      return(dat_thrailkill)
      
      
    }
    
    
    
    
    
    # RISK: COMBINE RAW DATA --------------------------------------------------------
    
    dat_pref <- bind_rows(frey_data(),
                          gaechter_data(),
                          horn2022a_data(),
                          seaman_data(),
                          pachur_data(),
                          zilker2020_data(),
                          zilker2021_data(),
                          rutledge_data(),
                          mamerow_data(),
                          horn2022b_data(),
                          seaman2016_data(),
                          thrailkill_data(),
                          rodrigues_data())
    
    
  }  
  
  
  
  if (preference == "time") {
    
    # TIME: CIARAMELLI 2021 --------------------------------------------------------
    
    ciaramelli_data <- function() { 
      
      # transformed xls data (Data DD sheet) into csv
      dat <- read_csv("data/raw_data/time/Ciaramelli_2021/ciaramelli_datadd.csv", col_types = cols())
      dat <- clean_names(dat)
      
      dat_ciaramelli <- dat %>%
        select(code,group, age, sex, auc_td_small,
               auc_td_large, auc_cd_small, auc_cd_large) %>% 
        filter(group == 1) %>%  # healthy controls
        pivot_longer(auc_td_small:auc_cd_large,
                     names_to = "outcome", values_to = "auc") %>% 
        filter(grepl("td", outcome)) %>%  # only standard condition
        mutate(auc = -1* as.numeric(auc), # reverse code such that greater values == greater discounting
               age = as.numeric(age)) %>% 
        group_by(outcome) %>% 
        summarise(dv_correlation = cor(age, auc),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_min = min(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = sum(sex == "f")/n(),
                  dv_units = "Area Under the Curve",
                  .groups = "drop") %>% 
        mutate(first_author = "Ciaramelli",
               year_of_publication = "2021",
               title_of_article = "The role of ventromedial prefrontal cortex in reward valuation and future thinking during intertemporal choice",
               outcome_num = 
                 case_when(grepl("small", outcome) ~ 1, TRUE ~ 2))  %>% 
        select(-outcome)
      
      return(dat_ciaramelli)
      
    }
    
    # TIME: ZILKER 2021 -------------------------------------------------------
    
    zilker_data <- function() {
      
      # Had to change file extension, from .Rda to .csv
      dat <- read.csv("data/raw_data/time/Zilker_2021/DD_files_matrix.csv",
                      header = TRUE, sep = ";", dec = ",")
      
      dat <- dat %>% 
        mutate(choice_ss = case_when(choice == "SS" ~  1,
                                     TRUE ~ 0)) %>% 
        filter(!is.na(choice_ss) & 
                 !is.na(age) & 
                 !grepl("attention", condition))  # exclude attention checks
      
      
      
      # by condition
      dat_cond_y <- dat %>% 
        group_by(subjid, age, agegroup, gender, condition) %>% 
        summarise(ss = mean(choice_ss),
                  .groups = "drop") %>% 
        ungroup() %>% 
        filter(agegroup == "YA") %>% 
        group_by(condition) %>% 
        summarise(young_mean_age = mean(age),
                  young_sd_age = sd(age),
                  young_age_min = min(age),
                  young_age_max = max(age),
                  young_total_n = n(),
                  young_prop_female = sum(gender == "w")/n(),
                  dv_young_mean = mean(ss),
                  dv_young_sd = sd(ss),
                  .groups = "drop") 
      
      dat_cond_o <- dat %>% 
        group_by(subjid, age, agegroup, gender, condition) %>% 
        summarise(ss = mean(choice_ss),
                  .groups = "drop") %>% 
        ungroup() %>% 
        filter(agegroup == "OA") %>% 
        group_by(condition) %>% 
        summarise(old_mean_age = mean(age),
                  old_sd_age = sd(age),
                  old_age_min = min(age),
                  old_age_max = max(age),
                  old_total_n = n(),
                  old_prop_female = sum(gender == "w")/n(),
                  dv_old_mean = mean(ss),
                  dv_old_sd = sd(ss),
                  .groups = "drop") 
      
      
      
      dat_cond_correl <- dat %>% 
        group_by(subjid, age, gender, condition) %>% 
        summarise(ss = mean(choice_ss),
                  .groups = "drop") %>% 
        ungroup() %>% 
        group_by(condition) %>% 
        summarise(mean_age = mean(age),
                  sd_age = sd(age),
                  age_min = min(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = sum(gender == "w")/n(),
                  dv_correlation = cor(ss, age),
                  .groups = "drop") 
      
      
      dat_cond <- dat_cond_o %>% 
        left_join(dat_cond_y, by = "condition") %>% 
        left_join(dat_cond_correl, by = "condition")%>% 
        mutate(first_author = "Zilker",
               year_of_publication = "2021",
               dv_units = "SS Choices",
               title_of_article = "Does option complexity contribute to the framing effet, loss aversion, and delay discounting in younger and older adults?",
               outcome_num = case_when(condition == "C1" ~ 1,
                                       condition == "C2" ~ 2,
                                       condition == "C3" ~ 3
               ))
      
      dat_zilker <-  dat_cond %>% select(-condition)
      
      return(dat_zilker)
      
    }
    
    
    
    # TIME: SEAMAN 2021 -------------------------------------------------------
    
    seaman_data <- function()  {
      
      # Primary sample
      dat <- read_csv("data/raw_data/time/Seaman_2021/tdsd_s1_data.csv",
                      col_types = cols())
      
      dat_primary <- dat %>% 
        select(ID,Q3, Q5, Q80:Q121) %>% # sugject ID, sex, age, and TD task choices
        rename(age = Q5,
               sex = Q3) %>% 
        pivot_longer(Q80:Q121,
                     names_to = "trial", values_to = "choice") %>%
        group_by(ID, age, sex) %>% 
        summarise(ss = mean(choice),
                  .groups = "drop") %>% 
        ungroup() %>% 
        summarise(mean_age = mean(age),
                  sd_age = sd(age),
                  age_min = min(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = mean(sex),
                  dv_correlation = cor(ss, age),
                  outcome_num = 1)
      
      
      
      # Replication sample
      dat <- read_csv("data/raw_data/time/Seaman_2021/tdsd_s2_data.csv",
                      col_types = cols())
      
      dat_replic <- dat %>% 
        select(ID,Q3, Q5, Q80:Q121) %>% # sugject ID, sex, age, and TD task choices
        rename(age = Q5,
               sex = Q3) %>% 
        pivot_longer(Q80:Q121,
                     names_to = "trial", values_to = "choice") %>%
        group_by(ID, age, sex) %>% 
        summarise(ss = mean(choice),
                  .groups = "drop") %>% 
        ungroup() %>% 
        summarise(mean_age = mean(age),
                  sd_age = sd(age),
                  age_min = min(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = mean(sex),
                  dv_correlation = cor(ss, age),
                  outcome_num = 2)
      
      
      dat_seaman <- bind_rows(dat_replic, dat_primary) %>% 
        mutate(first_author = "Seaman",
               year_of_publication = "2021",
               dv_units = "SS choices",
               title_of_article = "Decision Making across Adulthood during Physical Distancing")
      
      
      return(dat_seaman)
      
    }
    
    
    
    # TIME: SKYLARK -----------------------------------------------------------
    
    skylark_data <- function() {
      
      # study 1A
      
      dat1A <- read_csv("data/raw_data/time/Skylark_2021/Study_1A_data.csv", col_types = cols())
      
      #data contains some (extreme) outliers, ideally would need to use non-parametric test, but for the comparison of other texts, we use pearson
      
      dat1A <- dat1A %>% 
        group_by(cond) %>% 
        summarise(dv_correlation = cor(estimate, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(gender == "Female"),
                  .groups = "drop")%>% 
        mutate(paper_section = "Study 1A",
               sample_code = sprintf("Sample %d",
                                     1:n()),
               dv_units = "Expected reward",
               dv_description = sprintf("Expected reward after %s",
                                        as.character(cond))) %>% 
        select(-cond)
      
      
      
      # study 1B
      dat1B <- read_csv("data/raw_data/time/Skylark_2021/Study_1B_data.csv", col_types = cols())
      
      #data contains some (extreme) outliers, ideally would need to use non-parametric test, but for the comparison of other texts, we use pearson
      
      dat1B <- dat1B %>% 
        group_by(cond_num) %>% 
        summarise(dv_correlation = cor(estimate, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(gender == "Female"),
                  .groups = "drop")%>% 
        mutate(paper_section = "Study 1B",
               sample_code = sprintf("Sample %d",
                                     1:n()),
               dv_units = "Number of days",
               dv_description = sprintf("Number of days associated to an outcome of $%s",
                                        as.character(cond_num))) %>% 
        select(-cond_num)
      
      
      # study 2A
      
      dat2A <- read_csv("data/raw_data/time/Skylark_2021/Study_2A_data.csv", col_types = cols())
      
      
      dat2A <- dat2A %>% 
        filter(comp_fail == 0) %>% # only select respondent who passed the attention checks
        group_by(cond_num) %>% 
        summarise(dv_correlation = cor(estimate, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(gender == "Female"),
                  .groups = "drop") %>% 
        mutate(paper_section = "Study 2A",
               sample_code = sprintf("Sample %d",
                                     1:n()),
               dv_units = "Expected reward",
               dv_description = sprintf("Expected reward after %s day(s)",
                                        as.character(cond_num))) %>% 
        select(-cond_num)
      
      
      
      
      
      # study 2B
      
      dat2B <- read_csv("data/raw_data/time/Skylark_2021/Study_2B_data.csv", col_types = cols())
      
      
      dat2B <- dat2B %>% 
        filter(comp_fail == 0) %>% # only select respondent who passed the attention checks
        group_by(cond_num) %>% 
        summarise(dv_correlation = cor(estimate, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(gender == "Female"),
                  .groups = "drop") %>% 
        mutate(paper_section = "Study 2B",
               sample_code = sprintf("Sample %d",
                                     1:n()),
               dv_units = "Number of days",
               dv_description = sprintf("Number of days associated to an outcome of £%s",
                                        as.character(cond_num))) %>% 
        select(-cond_num)
      
      
      
      dat_skylark <- bind_rows(dat1A, dat1B, dat2A, dat2B) %>% 
        mutate(first_author = "Skylark",
               year_of_publication = "2020",
               title_of_article = "The delay-reward heuristic: What do people expect in intertemporal choice tasks?",
               dv_type_of_comparison = "Age continuous",
               task_name = "Intertemporal choice task",
               task_type = "Descriptive",
               domain_frame = "gain",
               incentivization = "Hypothetical",
               outcome_num = 1:n()) 
      
      return(dat_skylark)
      
    }    
    
    
    # TIME: MARTIN 2019 -------------------------------------------------------
    
    
    
    martin_data <- function() {
      
      
      dat <- read_xlsx("data/raw_data/time/Martin_2019/martin_data.xlsx")
      
      dat_martin <- dat %>% 
        ungroup() %>% 
        summarise(mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_min = min(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female = 1 - mean(`Gender (male)`),
                  dv_correlation = cor(nSSchoices, Age), 
                  outcome_num = 1) %>% 
        mutate(first_author = "Martin",
               year_of_publication = "2019",
               dv_units = "SS choices",
               title_of_article = "The appropriate response of Spanish Gitanos: short-run orientation beyond current socio-economic status")
      
      return(dat_martin)
      
      
      
    }    
    
    
    
    
    # TIME: VEILLARD 2020 -----------------------------------------------------
    
    veillard_data <- function() {
      
      dat <- read_csv("data/raw_data/time/Veillard_2020/all_cleaned_scored.csv", col_types = cols())
      
      dat_veillard <- dat %>% 
        summarise(dv_correlation = cor(logk_money, age),
                  age_min = min(age),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female =  mean(sex == "female"),
                  .groups = "drop")%>% 
        mutate(outcome_num = 1,
               dv_units = "Discount rate",
               first_author = "Veillard",
               year_of_publication = "2020",
               title_of_article = "Temporal discounting does not influence body mass index")
      
      
      return(dat_veillard)
      
      
    }
    
    # TIME: HALILOVA 2022 -----------------------------------------------------
    
    halilova_data <- function() {
      
      dat <- read_csv("data/raw_data/time/Halilova_2022/DecisionMaking_Vaccination_data.csv", col_types = cols())
      
      dat_halilova <- dat %>% 
        filter(!is.na(age) & !is.na(AUC_reward_L) &!is.na(gender)) %>% # 510 rows with missing AUC data
        summarise(dv_correlation = cor(AUC_reward_L, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(gender == "female"),
                  .groups = "drop")%>% 
        mutate(outcome_num = 1,
               dv_units = "Area under the curve",
               first_author = "Halilova",
               year_of_publication = "2022",
               title_of_article = "Short‑sighted decision‑making by those not vaccinated against COVID‑19")
      
      
      return(dat_halilova)
      
      
    }
    
    
    
    
    # TIME: THRAILKILL 2022 -----------------------------------------------------
    
    thrailkill_data <- function() {
      
      dat <- read_xlsx("data/raw_data/time/Thrailkill_2022/Thrailkill et al_Data_online.xlsx")
      
      dat_thrailkill <- dat %>% 
        mutate(age = Age_inmonths/12) %>% 
        filter(!is.na(age) & !is.na(Discounting_lnK) & !is.na(Gender_M_F_O)) %>% 
        summarise(dv_correlation = cor(Discounting_lnK, age),
                  age_min = min(age, na.rm = TRUE),
                  mean_age = mean(age, na.rm = TRUE),
                  sd_age = sd(age, na.rm = TRUE),
                  age_max = max(age, na.rm = TRUE),
                  total_n = n(),
                  prop_female =  mean(Gender_M_F_O == 1),
                  .groups = "drop")%>% 
        mutate(outcome_num = 1,
               dv_units = "Discouting Rate",
               first_author = "Thrailkill",
               year_of_publication = "2022",
               title_of_article = "Loss aversion and risk for cigarette smoking and other substance use ")
      
      
      return(dat_thrailkill)
      
      
    }
    
    
    # TIME: SEAMAN 2016 ---------------------------------------------------------
    seaman2016_data <- function() {
      dat <- read_csv("data/raw_data/time/Seaman_2016/all_sublevel_data.csv", 
                      col_types = cols())
      
      dat <- dat %>% filter(task == "Time_money")
      
      seaman_dat  <- dat %>% 
        filter(!is.na(Age) & !is.na(k)) %>% 
        summarise(dv_correlation = cor(k, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female =  mean(Sex == "F"),
                  .groups = "drop")%>% 
        ungroup()  %>% 
        mutate(outcome_num = 1,
               dv_units = "Discouting Rate",
               first_author = "Seaman",
               year_of_publication = "2016",
               title_of_article = "Adult age differences in decision making across domains: Increased discounting of social and health-related rewards")
      
      
      return(seaman_dat)
    }
    
    
    
    
    
    
    
    # TIME: COMBINE RAW DATA ----------------------------------------------
    
    dat_pref <- bind_rows(zilker_data(),
                          ciaramelli_data(),
                          seaman_data(),
                          seaman2016_data(),
                          martin_data(),
                          veillard_data(),
                          skylark_data(),
                          halilova_data(),
                          thrailkill_data())
    
    
    
  }
  
  
  
  if (preference == "social") {
    
    
    
    # SOCIAL: HELLMANN 2021 -------------------------------------------------
    
    data_hellmann <- function() {
      
      ## STUDY 1
      dat1 <- read_dta("data/raw_data/social/Hellmann_2021/Dataforanalysis_Prosocial behavior during the COVID19 pandemic_Study1.dta")
      
      dat1_sum <- dat1 %>%
        filter(!is.na(female) & !is.na(age)) %>% 
        select(participant, age, female,SVOangle_T1, SVOangle_T2, dg_) %>% 
        group_by(participant, age, female) %>% 
        summarise(svo_t1 = mean(SVOangle_T1),
                  svo_t2 = mean(SVOangle_T2),
                  dg = sum(dg_), # sum amount donate across trials
                  .groups = "drop") %>% 
        pivot_longer(svo_t1:dg, names_to = "outcome", values_to = "resp") %>% 
        filter(!is.na(resp)) %>% 
        group_by(outcome) %>% 
        summarise(dv_correlation = cor(resp, age),
                  age_min = min(age),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = mean(female),
                  .groups = "drop") %>% 
        mutate(outcome_num = case_when(outcome == "dg" ~ 2,
                                       outcome == "svo_t2" ~ 1,
                                       TRUE ~ 4),
               dv_units = case_when(outcome == "dg" ~ "Amount given",
                                    TRUE ~ "SVO angle")) %>% 
        select(-outcome)
      
      ## STUDY 2
      dat2 <- read_dta("data/raw_data/social/Hellmann_2021/Dataforanalysis_Prosocial behavior during the COVID19 pandemic_Study2.dta")
      
      dat2_sum <- dat2 %>% 
        select(participant, age,female, dg_) %>% 
        filter(!is.na(dg_) & !is.na(female) & !is.na(age)) %>% 
        group_by(participant, age,female) %>% 
        summarise(dg = sum(dg_),
                  .groups = "drop") %>% 
        pivot_longer(dg, names_to = "outcome", values_to = "resp") %>% 
        group_by(outcome) %>% 
        summarise(dv_correlation = cor(resp, age),
                  age_min = min(age),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = mean(female),
                  outcome_num = 3,
                  dv_units = "Amount given",
                  .groups = "drop") %>% 
        select(-outcome)
      
      
      dat_hellmann <- bind_rows(dat1_sum, dat2_sum) %>% 
        mutate(first_author = "Hellmann",
               year_of_publication = "2021",
               title_of_article = "Prosocial behavior during the COVID-19 pandemic in Germany. The role of responsibility and vulnerability")
      
      return(dat_hellmann)
      
    }
    
    #SOCIAL: KETTNER 2016 --------------------------------------------------
    
    data_kettner <- function() {
      
      
      dat <- readxl::read_xlsx("data/raw_data/social/Kettner_2016/Final_Dataset_Kettner&Waichman.xlsx")
      
      # Treatment code.
      #1=GRM5 (Give Real Mature 5 Euros),
      #2=GHM5 (Give Hypothetical Mature 5 Euros),
      #3=TRM5 (Take Real Mature 5 Euros),
      #4=THM5 (Take Hypothetical Mature 5 Euros),
      #5=GRM20 (Give Real Mature 20 Euros), X Exclude because only Older Adults took part
      #6=GHM20 (Give Hypothetical Mature 20 Euros), X Exclude because only Older Adults took part
      #7=TRM20 (Take Real Mature 20 Euros), X Exclude because only Older Adults took part
      #8=THM20 (Take Hypothetical Mature 20 Euros), X Exclude because only Older Adults took part
      #9=GRI5 (Give Real Inexperienced students 5 Euros),
      #10=GHI5 (Give Hypothetical Inexperienced students 5 Euros),
      #11=TRI5 (Take Real Inexperienced students 5 Euros),
      #12=THI5 (Take Hypothetical Inexperienced students 5 Euros),
      #13=GRE5 (Give Real Experienced students 5 Euro),
      #14=GHE5 (Give Hypothetical Experienced students 5 Euros),
      #15=TRE5 (Take Real Experienced students 5 Euro),
      #16=THE5 (Take Hypothetical Experienced students 5 Euros).
      
      
      
      
      dat <- dat %>% 
        filter(!treatment_cat_complete %in% c(5:8)) %>% #
        mutate(cond_code = case_when(treatment_cat_complete == 1~ "GRM5",
                                     treatment_cat_complete == 2~ "GHM5",
                                     treatment_cat_complete == 3~ "TRM5",
                                     treatment_cat_complete == 4~ "THM5",
                                     treatment_cat_complete == 9~ "GRI5",
                                     treatment_cat_complete == 10~ "GHI5",
                                     treatment_cat_complete == 11~ "TRI5",
                                     treatment_cat_complete == 12~ "THI5",
                                     treatment_cat_complete == 13~ "GRE5",
                                     treatment_cat_complete == 14~ "GHE5",
                                     treatment_cat_complete == 15~ "TRE5",
                                     treatment_cat_complete == 16~ "THE5")) %>% 
        mutate(age_group = case_when(grepl("M", cond_code) ~ "OA",
                                     TRUE ~ "YA"),
               frame = case_when(grepl("G", cond_code) ~ "give",
                                 TRUE ~ "take"),
               incentive = case_when(grepl("H", cond_code) ~ "hyp",
                                     TRUE ~"real")) 
      
      dat_sum_y <- dat %>% 
        filter(age_group == "YA") %>% 
        group_by(frame,incentive) %>% 
        summarise(dv_young_mean = mean(perc_other),
                  dv_young_sd = sd(perc_other),
                  young_age_min = min(age),
                  young_mean_age = mean(age),
                  young_sd_age = sd(age),
                  young_age_max = max(age),
                  young_total_n = n(),
                  young_prop_female = mean(female),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(incentive == "hyp" & frame == "take" ~ 1,
                                       incentive == "hyp" & frame == "give" ~ 2,
                                       incentive == "real" & frame == "take" ~ 3,
                                       incentive == "real" & frame == "give" ~ 4)) %>% 
        select(-c(frame, incentive))
      
      
      dat_sum_o <- dat %>% 
        filter(age_group == "OA") %>% 
        group_by(frame,incentive) %>% 
        summarise(dv_old_mean = mean(perc_other),
                  dv_old_sd = sd(perc_other),
                  old_age_min = min(age),
                  old_mean_age = mean(age),
                  old_sd_age = sd(age),
                  old_age_max = max(age),
                  old_total_n = n(),
                  old_prop_female = mean(female),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(incentive == "hyp" & frame == "take" ~ 1,
                                       incentive == "hyp" & frame == "give" ~ 2,
                                       incentive == "real" & frame == "take" ~ 3,
                                       incentive == "real" & frame == "give" ~ 4)) %>% 
        select(-c(frame, incentive))
      
      
      
      
      dat_sum_cor <- dat %>% 
        group_by(frame,incentive) %>% 
        summarise(dv_correlation = cor(perc_other, age),
                  age_min = min(age),
                  mean_age = mean(age),
                  sd_age = sd(age),
                  age_max = max(age),
                  total_n = n(),
                  prop_female = mean(female),
                  .groups = "drop")%>% 
        ungroup() %>% 
        mutate(outcome_num = case_when(incentive == "hyp" & frame == "take" ~ 1,
                                       incentive == "hyp" & frame == "give" ~ 2,
                                       incentive == "real" & frame == "take" ~ 3,
                                       incentive == "real" & frame == "give" ~ 4)) %>% 
        select(-c(frame, incentive))
      
      
      dat_kettner <- dat_sum_o %>% 
        left_join(dat_sum_y, by = "outcome_num") %>% 
        left_join(dat_sum_cor, by = "outcome_num") %>% 
        mutate(dv_units = "Percent given",
               first_author = "Kettner",
               year_of_publication = "2016",
               title_of_article = "Old age and prosocial behavior: Social preferences or experimental confounds?")
      
      
      return(dat_kettner)
      
    }
    
    
    # SOCIAL: LONG 2017 -----------------------------------------------------
    
    data_long <- function() {
      
      ## Survey 1
      dat1 <- read.dta("data/raw_data/social/Long_2017/2013-Survey-Data.dta", convert.factors = TRUE)
      
      dat1 <- dat1 %>% 
        select(CaseID, age, ppgender, v8_c_1:v8_c_10) %>% 
        pivot_longer(v8_c_1:v8_c_10, names_to = "outcome", values_to = "resp") %>% 
        filter(resp >= 0) %>% # invalid response (?)
        mutate(resp = 10 - resp) %>%  # participants are aksed to enter how many tickets to give themselves
        #"Enter the number (between 0 and 10) in the blue box that you would like to be given to you. "
        # calculate for each ID the mean response
        group_by(CaseID,age,ppgender) %>% 
        summarise(given = mean(resp),
                  .groups = "drop") %>% 
        ungroup()
      
      dat1_sum_y <- dat1 %>% 
        filter(age == "Age 18-29") %>% 
        summarise(dv_young_mean = mean(given),
                  dv_young_sd = sd(given),
                  young_age_min = 18,
                  young_age_max = 29,
                  young_total_n = n(),
                  young_prop_female = mean(ppgender == "Female")) 
      
      dat1_sum_m <- dat1 %>% 
        filter(age == "Age 40-49") %>% 
        summarise(dv_middle_mean = mean(given),
                  dv_middle_sd = sd(given),
                  middle_age_min = 40,
                  middle_age_max = 49,
                  middle_total_n = n(),
                  middle_prop_female = mean(ppgender == "Female"))  
      
      dat1_sum_o <- dat1 %>% 
        filter(age %in% c("Age 70-79", "Age 80 or older")) %>% 
        summarise(dv_old_mean = mean(given),
                  dv_old_sd = sd(given),
                  old_age_min = 70,
                  old_age_max = NA,
                  old_total_n = n(),
                  old_prop_female =  mean(ppgender == "Female")) 
      
      dat1_sum <- bind_cols(dat1_sum_y, dat1_sum_m, dat1_sum_o) %>% 
        mutate(outcome_num = 1,
               dv_units = "Scratch off tickets given",
               first_author = "Long",
               year_of_publication = "2017",
               title_of_article = "Altruism by age and social proximity")
      
      
      
      ## Survey 2
      dat2 <- haven::read_dta("data/raw_data/social/Long_2017/2015-Survey-Data.dta")
      
      # NOT CLEAR ON HOW TO READ THE VARIABLE HEADERS TO CALC THE DV CORRECTLY
      
      
      dat_long <- dat1_sum
      
      return(dat_long)
      
    }
    
    
    
    # SOCIAL: OLESZKIEWICZ 2020 -----------------------------------------------
    
    data_oleszkiewicz <- function() {
      
      dat <- read_xlsx("data/raw_data/social/Oleszkiewicz_2020/data_s2.xlsx")
      
      # from tables 1 & 2
      female_prop <- tibble(n = c(74,100,99,97),
              prct_female = c(50,53,46.5,52.6))
      
      
      
        
        
      dat_oleszkiewicz <- dat %>% 
        filter(!is.na(Offer) &
                 !is.na(Age)) %>% 
        summarise(dv_correlation = cor(Offer, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  .groups = "drop") %>% 
        ungroup() %>% 
        mutate(outcome_num = 1,
               prop_female = weighted.mean(female_prop$prct_female,female_prop$n)/100,
               dv_units = "Amount given",
               first_author = "Oleszkiewicz",
               year_of_publication = "2020",
               title_of_article = "Sensory impairment reduces money sharing in the Dictator Game regardless of the recipient’s sensory status")
      
      
      return(dat_oleszkiewicz)
      
    }
    
    
    # SOCIAL: COMBINE RAW DATA-------------------------------------------------------------------------
    
    
    dat_pref <- bind_rows(data_hellmann(),
                          data_kettner(),
                          data_oleszkiewicz(),
                          data_long())
    
    
  }
  
  
  
  if (preference == "effort") { 
    
    # EFFORT: LOCKWOOD 2021 ---------------------------------------------------
    
    lockwood_data <- function() {
      
      dat_k <- read_xlsx("data/raw_data/effort/Lockwood_2021/k_values_2k1b.xlsx")
      dat_dem <- read_xlsx("data/raw_data/effort/Lockwood_2021/lme_data_PM_young_old_final.xlsx", sheet = "Sheet2")
      
      
      dat_lockwood_cor <- dat_k %>% 
        left_join(dat_dem, by = "ID") %>% 
        filter(!is.na(Age)) %>% 
        summarise(dv_correlation = cor(self_k, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female =  mean(Gender == 2),
                  .groups = "drop")%>% 
        ungroup() 
      
      
      dat_lockwood_y <- dat_k %>% 
        left_join(dat_dem, by = "ID") %>% 
        filter(!is.na(Age)) %>%  
        filter(group == "young") %>% 
        summarise(dv_young_mean = mean(self_k),
                  dv_young_sd = sd(self_k),
                  young_age_min = min(Age),
                  young_mean_age = mean(Age),
                  young_sd_age = sd(Age),
                  young_age_max = max(Age),
                  young_total_n = n(),
                  young_prop_female = mean(Gender == 2),
                  .groups = "drop") %>% 
        ungroup()
      
      
      dat_lockwood_o <- dat_k %>% 
        left_join(dat_dem, by = "ID")%>% 
        filter(!is.na(Age)) %>% 
        filter(group == "old") %>% 
        summarise(dv_old_mean = mean(self_k),
                  dv_old_sd = sd(self_k),
                  old_age_min = min(Age),
                  old_mean_age = mean(Age),
                  old_sd_age = sd(Age),
                  old_age_max = max(Age),
                  old_total_n = n(),
                  old_prop_female = mean(Gender == 2),
                  .groups = "drop") %>% 
        ungroup()
      
      
      
      dat_lockwood <- bind_cols(dat_lockwood_cor, dat_lockwood_o, dat_lockwood_y)%>% 
        mutate(outcome_num = 1,
               dv_units = "Discounting rate",
               first_author = "Lockwood",
               year_of_publication = "2021",
               title_of_article = "Aging Increases Prosocial Motivation for Effort")
      
      
      return(dat_lockwood)
      
      
    }
    
    
    
    # EFFORT: MCLAUGHLIN 2021 ---------------------------------------------------
    
    mclaughlin_data <- function() {
      
      dat_auc <- read_csv("data/raw_data/effort/McLaughlin_2021/LED-P2_AUC.csv", col_types =  cols())
      dat_dem <- read_xlsx("data/raw_data/effort/McLaughlin_2021/LED_Demographics.xlsx")
      dat_dem <- dat_dem %>% rename(Participant = Number)  %>% 
        mutate(Age = as.numeric(Age))
      dat_auc <- dat_auc %>% mutate(Participant = as.numeric(Participant),
                                    AUC = as.numeric(AUC))
      
      
      dat_mclaughlin_cor <- dat_auc %>% 
        left_join(dat_dem, by = "Participant") %>% 
        filter(!is.na(Age) & !is.infinite(AUC)) %>% 
        summarise(dv_correlation = cor(AUC, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female =  mean(Sex == "Female"),
                  .groups = "drop")%>% 
        ungroup() 
      
      
      dat_mclaughlin_y <- dat_auc %>% 
        left_join(dat_dem, by = "Participant") %>% 
        filter(!is.na(Age) & !is.infinite(AUC)) %>% 
        filter(AgeGroup == "YA") %>% 
        summarise(dv_young_mean = mean(AUC),
                  dv_young_sd = sd(AUC),
                  young_age_min = min(Age),
                  young_mean_age = mean(Age),
                  young_sd_age = sd(Age),
                  young_age_max = max(Age),
                  young_total_n = n(),
                  young_prop_female = mean(Sex == "Female"),
                  .groups = "drop") %>% 
        ungroup()
      
      
      dat_mclaughlin_o <- dat_auc %>% 
        left_join(dat_dem, by = "Participant") %>% 
        filter(!is.na(Age) & !is.infinite(AUC)) %>% 
        filter(AgeGroup == "OA") %>% 
        summarise(dv_old_mean = mean(AUC),
                  dv_old_sd = sd(AUC),
                  old_age_min = min(Age),
                  old_mean_age = mean(Age),
                  old_sd_age = sd(Age),
                  old_age_max = max(Age),
                  old_total_n = n(),
                  old_prop_female = mean(Sex == "Female"),
                  .groups = "drop") %>% 
        ungroup()
      
      
      dat_mclaughlin <- bind_cols(dat_mclaughlin_cor, dat_mclaughlin_o, dat_mclaughlin_y)%>% 
        mutate(outcome_num = 1,
               dv_units = "Area Under the Curve",
               first_author = "McLaughlin",
               year_of_publication = "2021",
               title_of_article = "Measuring the Subjective Cost of Listening Effort Using a Discounting Task")
      
      
      return(dat_mclaughlin)
      
      
    }
    
    
    
    # EFFORT: SEAMAN 2016 ---------------------------------------------------
    
    seaman_data <- function() {
      
      
      
      dat <- read_csv("data/raw_data/effort/Seaman_2016/all_sublevel_data.csv", col_types =  cols())
      dat <- dat %>% filter(task == "Effort_money")
      
      seaman_dat  <- dat %>% 
        filter(!is.na(Age) & !is.na(k)) %>% 
        summarise(dv_correlation = cor(k, Age),
                  age_min = min(Age),
                  mean_age = mean(Age),
                  sd_age = sd(Age),
                  age_max = max(Age),
                  total_n = n(),
                  prop_female =  mean(Sex == "F"),
                  .groups = "drop")%>% 
        ungroup()  %>% 
        mutate(outcome_num = 1,
               dv_units = "Discounting Rate",
               first_author = "Seaman",
               year_of_publication = "2016",
               title_of_article = "Adult Age Differences in Decision Making Across Domains: Increased Discounting of Social and Health-Related Rewards")
      
      
      return(seaman_dat)
    }
    
    
    # EFFORT: CHEN 2020 ---------------------------------------------------
    
    chen_data <- function() {
      
      
      dat <- read_csv("data/raw_data/effort/Chen_2020/chen_2020_indifpoint_dat.csv", col_types =  cols())
      
      dat_chen_y <- dat %>% 
        filter(age_group == "ya") %>% 
        group_by(force_lvl, cond_domain) %>% 
        summarise(dv_young_mean = mean(indif_point),
                  dv_young_sd = sd(indif_point),
                  young_mean_age = 23.1,
                  young_sd_age = 4.56,
                  young_total_n = n(),
                  .groups = "drop") %>% 
        ungroup() %>% 
        arrange(desc(cond_domain),force_lvl) %>% 
        mutate(outcome_num = 1:12) %>% 
        select(-c(cond_domain,force_lvl))
      
      
      dat_chen_o <- dat %>% 
        filter(age_group == "oa") %>% 
        group_by(force_lvl, cond_domain) %>% 
        summarise(dv_old_mean = mean(indif_point),
                  dv_old_sd = sd(indif_point),
                  old_mean_age = 69,
                  old_sd_age = 4.54,
                  old_total_n = n(),
                  .groups = "drop") %>% 
        ungroup() %>% 
        arrange(desc(cond_domain),force_lvl) %>% 
        mutate(outcome_num = 1:12) %>% 
        select(-c(cond_domain,force_lvl))
      
      
      dat_chen <- left_join(dat_chen_o, dat_chen_y, 
                            by = "outcome_num")%>% 
        mutate(dv_units = "Indifference Point",
               first_author = "Chen",
               year_of_publication = "2020",
               title_of_article = "Dopamine-Dependent Loss Aversion during Effort-Based Decision-Making")
      
      
      return(dat_chen)
    }
    
    
    
    
    
    
    # EFFORT: COMBINE RAW DATA ----------------------------------------------
    
    dat_pref <- bind_rows(lockwood_data(),
                          mclaughlin_data(),
                          seaman_data(),
                          chen_data())
    
    
    
  }
  
  
  
  
  # SAVE OUTPUT -------------------------------------------------------------
  
  
  write_csv(dat_pref, sprintf("data/raw_data/%s/processed_raw_data_%s.csv", 
                              preference, preference))
  
  print(
    sprintf("processed_raw_data_%s.csv created successfully! Saved in:   data/raw_data/%s",
            preference, preference) 
  )
  
  
  
  
  
}
