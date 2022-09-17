
#' `table_egger()` creates table of Egger's regression test results.
#' @param pcurve output of dmetar::pcurve(). Contains main results of the pcurve analysis
#' @returns a data frame with a summary of the p-curve analysis
#' @examples
#' table_egger(egger = read_rds("output/social/pcurve_social.rds"))

library(tidyverse)  # for data wrangling 

table_egger <- function(egger) {
  
  
  tab <- egger %>%
    mutate_if(is.numeric, round, 3)  %>% 
    mutate_if(is.numeric, as.character)  %>% 
    filter(moderator != "intrcpt") %>% 
    mutate(precision = precision_indc,
           est = estimate,
           se = se,
           tval = tval,
           pval = pval,
           ci95 = paste0("[", ci_lb, ", ", ci_ub,"]"))  %>%
    select(precision, est,se,tval, pval, ci95) 

  
  return(tab)
  
  
}
