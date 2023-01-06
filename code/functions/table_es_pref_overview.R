
#' `table_es_pref_overview()` creates table with an overview of the effect sizes by econ. preference, 
#' with the number of participants, number of effect sizes, number of studies and publication year range
#' @param dat data frame containing effect sizes
#' @returns a data frame with a summary information.
#' @examples
#' table_es_pref_overview(dat = read_csv("data/summary/risk/effect_sizes_risk.csv",col_types = cols()))

#library(tidyverse)  # for data wrangling 




table_es_pref_overview <- function(dat) {

  dat <- dat %>%  
    mutate(pub_study = paste0(title_of_article, tolower(paper_section)))
  
  
  dat_n <- dat %>% 
    distinct(study_label, n_incl_es, sample_code,pub_study) %>% 
    group_by(study_label, sample_code) %>%  
    # within studies even if a sample completed the same task, some data points 
    # might go missing, so we select the highest number recorded within a same sample
    filter(n_incl_es == max(n_incl_es)) %>% 
    ungroup()
  
  # for risk there is an exception, in Sproten experiment 2 = experiment 1 subsample
  
  t <-  tibble(
    preference = unique(dat$pref),
    n_publications =  length(unique(paste0(dat$title_of_article, dat$first_author))),
    n_studies = length(unique(dat$pub_study)),
    n_es = nrow(dat),
    n_participants = sum(dat_n$n_incl_es),
    pub_range = paste0(as.character(min(dat$year_of_publication))," - ", as.character(max(dat$year_of_publication))))

  
  return(t)
  
}
