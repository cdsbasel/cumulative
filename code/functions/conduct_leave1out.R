
#' `conduct_leave1out()` computes a pooled effect size by fitting a multilevel meta-analytic model repetitively by 
#' removing one study.
#' @param dat A data frame containing effect sizes (i.e., "effect_sizes_pref.csv")
#' @param rho value of the correlation of the sampling errors within clusters
#' @param rve logical value to apply or not robust variance estimation
#' @returns a data frame with summary information of the pooled estimates
#' @examples
#' conduct_leave1out(dat = risk_es, rho = .5, rve = TRUE)




# library(tidyverse) # for data wrangling
# library(metafor) # compute effect sizes
# 

conduct_leave1out <- function(dat, rho, rve) {
  
  # get the mlma function
  source("code/functions/conduct_mlma.R")
  
  # object to store output
  m <- NULL
  
  for (i in 0:max(dat$study_id)) {
    
    # omit one study
    sdat <- dat %>% filter(study_id != i)
    
    # study being omitted
    omit_study <- paste0("-",unique(dat$study_label[dat$study_id == i]))
    year_of_publication  <- unique(dat$year_of_publication[dat$study_id == i])
    
    
    if (i == 0) {
      omit_study <- "Overall"
      year_of_publication <- NULL
    }

    
    # fit multilevel model
    res <- conduct_mlma(dat = sdat, rho = rho)
    
    # robust variance estimation
    if (isTRUE(rve)) {
      res <- robust(res, cluster = study_id, clubSandwich = TRUE)
    }
    
    
    
    #create output
    m <- bind_rows(m,
                   tibble(   preference = unique(dat$pref),
                             slab = omit_study,
                             year_of_publication = year_of_publication,
                             estimate = res$beta[1],
                             se = res$se,
                             zval = res$tval,
                             df = res$ddf,
                             pval = res$pval,
                             ci.lb = res$ci.lb,
                             ci.ub = res$ci.ub,
                             Q = res$QE,
                             Qp = res$QEp))
    
    
  } # study_id for loop
  
  
  return(m)
}
