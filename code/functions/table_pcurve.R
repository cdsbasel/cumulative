
#' `table_pcurve()` creates table of p-curve results.
#' @param pcurve output of dmetar::pcurve(). Contains main results of the pcurve analysis
#' @returns a data frame with a summary of the p-curve analysis
#' @examples
#' table_pcurve(pcurve = read_rds("output/social/pcurve_social.rds"))

library(tidyverse)  # for data wrangling 

table_pcurve <- function(pcurve) {
  
  
  # rest of pcurve results
  tab <- as.data.frame(pcurve$pcurveResults) %>% 
    rownames_to_column(var = "test_name") %>%
    mutate(kFull = paste0(as.character(pcurve$kAnalyzed),
                          "(", as.character(100*round(pcurve$kAnalyzed/pcurve$kInput,3)), "%)"),
           kHalf = paste0(as.character(pcurve$kp0.25),
                          "(", as.character(100*round(pcurve$kp0.25/pcurve$kInput,3)), "%)")) 
                
  
  return(tab)
  
  
}
