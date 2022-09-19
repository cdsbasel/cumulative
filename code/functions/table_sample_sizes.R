
#' `table_sample_sizes()` creates table with an overview of the sample sizes by study, 
#' @param dat data frame containing study information
#' @returns a data frame with a summary information.
#' @examples
#' table_es_pref_overview(dat = read_csv("data/summary/risk/effect_sizes_risk.csv",col_types = cols()))

# library(tidyverse)

table_sample_sizes <- function(dat) {
  
  
  dt <- dat %>%  
    mutate(pub_study = paste0(first_author, "_", title_of_article, "_",paper_section)) %>% 
    distinct(study_label, n_incl_es, sample_code, year_of_publication,pub_study, pref) %>% 
    group_by(study_label, sample_code, pref) %>%  
    # within studies even if a sample completed the same task, some data points 
    # might go missing, so we select the highest number recorded within a same sample
    filter(n_incl_es == max(n_incl_es)) %>% 
    ungroup() %>% 
    # summing within studies (independent) subsamples
    group_by(pub_study,year_of_publication, pref) %>% 
    summarise(n_incl_es = sum(n_incl_es)) %>%  
    ungroup()
  
  
  
  return(dt)
  
  
  
}