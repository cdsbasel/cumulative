

#' `processing_plot_data()` creates "processed_plot_data_*.csv", by first reading the data files 
#' containing summary statistics extracted from plots of the studies that investigated the 
#' specified econ. preference, and formats the output such as to allow further analysis. 
#' Summary statistics were first extracted from plots using the {metaDigitise} package (Pick, Nakagawa, & Noble, 2018).
#' See here for tutorial: https://cran.r-project.org/web/packages/metaDigitise/vignettes/metaDigitise.html
#' @param preference A character string to specify for which econ. preference should the 
#' raw data file be summarized. Can either be 'risk', 'time' or  'social'
#' @returns A data frame with the summarized information of the studies of the specified 
#' econ. preference (e.g., first author, year of publication, sample size) that 
#' is saved as a csv file in a designated folder.
#' @examples
#' processing_plot_data(preference = 'risk')


library(tidyverse) # for data wrangling
library(janitor) # for cleaning up column names



processing_plot_data <- function(preference) {
  
  
  if (preference == "risk") {
    
    # RISK: CAVANAGH 2012 -------------------------------------------------------
    
    data_cavanagh <- function() {
      
      dat <- read_csv("data/plots/risk/Cavanagh_2012/Cavanagh_2012_plots.csv",
                      col_types = cols())
      
      dat_cavanagh <- dat %>% 
        mutate(outcome_num = case_when(variable == "avg. pumps" & grepl("low", group_id) ~ 1,
                                       variable == "avg. pumps" & grepl("high", group_id) ~ 2,
                                       variable == "explosions" & grepl("low", group_id) ~ 3,
                                       variable == "explosions" & grepl("high", group_id) ~ 4))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Cavanagh",
               year_of_publication = "2012",
               title_of_article = "Individual differences in risky decision-making among seniors reflect increased reward sensitivity")
      
      return(dat_cavanagh)
      
    }
    
    # RISK: DROR 1998 -------------------------------------------------------------------
    
    data_dror <- function(){
      
      dat <- read_csv("data/plots/risk/Dror_1998/Dror_1998_plots.csv",
                      col_types = cols())
      
      dat_dror <- dat %>% 
        mutate(outcome_num = case_when(grepl("no", group_id) ~ 1,
                                       grepl("low", group_id) ~ 2,
                                       grepl("med", group_id) ~ 3,
                                       grepl("young_high|old_high", group_id) ~ 4,
                                       grepl("very_high", group_id) ~ 5,
                                       grepl("inf", group_id) ~ 6))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Dror",
               year_of_publication = "1998",
               title_of_article = "Age Differences in Decision Making: To Take a Risk or Not?")
      
      return(dat_dror)
      
      
    }
    
    
    
    
    # RISK: MIKELS 2009 --------------------------------------------------------------------
    
    data_mikels <- function() {
      
      
      dat <- read_csv("data/plots/risk/Mikels_2009/Mikels_2009_plots.csv",
                      col_types = cols())
      
      
      dat_mikels <- dat %>% 
        mutate(outcome_num = case_when(grepl("gain", group_id) ~ 1,
                                       grepl("loss", group_id) ~ 2))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_old_mean: dv_young_sd) %>% 
        mutate(first_author = "Mikels",
               year_of_publication = "2009",
               title_of_article = "Monetary Losses Do Not Loom Large in Later Life: Age Differences in the Framing Effect")
      
      return(dat_mikels)
      
      
    }
    
    
    
    
    
    # RISK: SAMANEZ-LARKIN 2011 -------------------------------------------------------------------
    
    data_samanez_larkin <- function() {
      
      dat <- read_csv("data/plots/risk/Samanez_Larkin_2011/Samanez_Larkin_2011_plots.csv",
                      col_types = cols())
      
      
      
      dat_samanez_larkin <- dat %>% 
        mutate(outcome_num = case_when(grepl("ram", group_id) ~ 2,
                                       grepl("rsm", group_id) ~ 1,
                                       grepl("cm", group_id) ~ 3))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Samanez-Larkin",
               year_of_publication = "2011",
               title_of_article = "Expected value information improves financial risk taking across the adult life span")
      
      return(dat_samanez_larkin)
      
      
    }
    
    
    
    
    # RISK: SPANIOL 2012 --------------------------------------------------------------------
    
    data_spaniol <- function() {
      
      dat <- read_csv("data/plots/risk/Spaniol_2012/Spaniol_2012_plots.csv",
                      col_types = cols())
      
      dat_spaniol <- dat %>% 
        mutate(outcome_num = case_when(grepl("gain", filename) & grepl("10", group_id) ~ 1,
                                       grepl("gain", filename) & grepl("20", group_id) ~ 2,
                                       grepl("gain", filename) & grepl("80", group_id) ~ 3,
                                       grepl("gain", filename) & grepl("90", group_id) ~ 4,
                                       grepl("loss", filename) & grepl("10", group_id) ~ 5,
                                       grepl("loss", filename) & grepl("20", group_id) ~ 6,
                                       grepl("loss", filename) & grepl("80", group_id) ~ 7,
                                       grepl("loss", filename) & grepl("90", group_id) ~ 8))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Spaniol",
               year_of_publication = "2012",
               title_of_article = "Decisions from experience: adaptive information search and choice in younger and older adults")
      
      return(dat_spaniol)
      
      
    }
    
    
    
    
    # RISK: TYMULA 2013 --------------------------------------------------------------------
    
    data_tymula <- function() {
      
      dat <- read_csv("data/plots/risk/Tymula_2013/Tymula_2013_plots.csv",
                      col_types = cols())
      dat_tymula <- dat %>% 
        mutate(outcome_num = case_when(grepl("gain", group_id) ~ 1,
                                       grepl("loss", group_id) ~ 2))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]],
               group_id = case_when(group_id == "y" ~ "young",
                                    group_id == "m" ~ "middle",
                                    group_id == "o" ~ "old")) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Tymula",
               year_of_publication = "2013",
               title_of_article = "Like cognitive function, decision making across the lifespan shows profound age-related changes")
      
      return(dat_tymula)
      
    }
    
    
    
    
    # RISK: WELLER 2011--------------------------------------------------------------------
    
    
    
    data_weller <- function() {
      
      dat <- read_csv("data/plots/risk/Weller_2011/Weller_2011_plots.csv",
                      col_types = cols())
      
      dat_weller <- dat %>%
        filter(!grepl("1", filename)) %>%  # remove overall choices for comparison
        filter(!grepl("25_44", group_id)) %>%  # remove one age group from comparison
        mutate(outcome_num = case_when(grepl("gain_ra", group_id) ~ 1,
                                       grepl("gain_rd", group_id) ~ 2,
                                       grepl("loss_ra", group_id) ~ 3,
                                       grepl("loss_rd", group_id) ~ 4))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]],
               group_id = case_when(group_id == "y" ~ "young",
                                    group_id == "m" ~ "middle",
                                    group_id == "o" ~ "old")) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Weller",
               year_of_publication = "2011",
               title_of_article = "Trajectory of Risky Decision Making for Potential Gains and Losses From Ages 5 to 85")
      
      return(dat_weller)
      
    }
    
    
    
    
    # RISK: WESTBROOK 2012 --------------------------------------------------------------------
    
    data_westbrook <- function() {
      
      dat <- read_csv("data/plots/risk/Westbrook_2012/Westbrook_2012_plots.csv",
                      col_types = cols())
      
      dat_westbrook <- dat %>%
        mutate(outcome_num = 1)%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_old_mean: dv_young_sd) %>% 
        mutate(first_author = "Westbrook",
               year_of_publication = "2012",
               title_of_article = "Strategic insight and age-related goal-neglect influence risky decision-making")
      
      return(dat_westbrook)
      
    }
    
    
    
    
    # RISK: WILSON 2018 --------------------------------------------------------------------
    
    data_wilson <- function() {
      
      dat <- read_csv("data/plots/risk/Wilson_2018/Wilson_2018_plots.csv",
                      col_types = cols())
      
      dat_wilson <- dat %>%
        mutate(outcome_num = case_when(grepl("gain",group_id) ~ 1,
                                       grepl("loss",group_id) ~ 2))%>% 
        rowwise() %>% 
        mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
        ungroup() %>%
        pivot_wider(outcome_num,
                    names_from = group_id,
                    values_from = c(mean,se,sd),
                    names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
        select(outcome_num, dv_young_mean: dv_old_sd) %>% 
        mutate(first_author = "Wilson",
               year_of_publication = "2018",
               title_of_article = "Age-differences in cognitive flexibility when overcoming a preexisting bias through feedback")
      
      return(dat_wilson)
      
    }
    
    
    
    
    
    # RISK: COMBINE PROCESSED PLOT DATA ----------------------------------------------
    
    dat_pref <-  bind_rows(data_cavanagh(),
                           data_dror(),
                           data_mikels(),
                           data_samanez_larkin(),
                           data_spaniol(),
                           data_tymula(),
                           data_weller(),
                           data_westbrook(),
                           data_wilson())
    
  }
  
  
  
  
  if (preference == "time") {
    
    
    # TIME: SPARROW 2019 ---------------------------------------------------
    
    dat <- read_csv("data/plots/time/Sparrow_2019/Sparrow_2019_plots.csv",
                    col_types = cols())
    
    
    dat_g <- dat %>% 
      filter(grepl("gain", group_id)) %>% # from the gain domain
      rowwise() %>% 
      mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
      ungroup() %>% 
      pivot_wider(names_from = group_id,
                  values_from = c(mean,se,sd),
                  names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
      select(dv_young_mean: dv_old_sd) %>% 
      mutate(outcome_num = 1)
    
    
    
    
    dat_l <- dat %>% 
      filter(!grepl("gain", group_id)) %>% # from the loss domain
      rowwise() %>% 
      mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
      ungroup() %>% 
      pivot_wider(names_from = group_id,
                  values_from = c(mean,se,sd),
                  names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
      select(dv_young_mean: dv_old_sd) %>% 
      mutate(outcome_num = 2)
    
    
    
    dat_sparrow <- bind_rows(dat_g, dat_l) %>% 
      mutate(dv_units = "Reward Index",
             first_author = "Sparrow",
             year_of_publication = "2019",
             title_of_article = "Acute stress and altruism in younger and older adults")
    
    
    
    # TIME: COMBINE PROCESSED PLOT DATA ----------------------------------------------
    
    dat_pref <- dat_sparrow
    
    
  }
  
  
  
  if (preference == "social") {
    
    
    
    # SOCIAL: ROSI 2019 ---------------------------------------------------
    
    dat <- read_csv("data/plots/social/Rosi_2019/Rosi_2019_plots.csv",
                    col_types = cols())
    
    
    dat_rosi <- dat %>% 
      rowwise() %>% 
      mutate(group_id = unlist(str_split(group_id, "_"))[[1]]) %>% 
      ungroup() %>% 
      pivot_wider(names_from = group_id,
                  values_from = c(mean,se,sd),
                  names_glue = paste0("dv_","{group_id}_{.value}")) %>% 
      select(dv_old_mean: dv_young_sd) %>% 
      mutate(outcome_num = 1,
             dv_units = "Percent given",
             first_author = "Rosi",
             year_of_publication = "2019",
             title_of_article = "Prosocial behavior in aging: which factors can explain age-related differences in social-economic decision making?")
    
    
    
    
    
    
    
    # SOCIAL: COMBINE PROCESSED PLOT DATA ----------------------------------------------
    
    dat_pref <- dat_rosi
    
    
  }
  
  
  
  # SAVE OUTPUT -------------------------------------------------------------
  
  write_csv(dat_pref, sprintf("data/plots/%s/processed_plot_data_%s.csv", preference, preference)) 
  
  print(
    sprintf("processed_plot_data_%s.csv created successfully! Saved in:   data/plots/%s",
            preference, preference)
  )
  
}