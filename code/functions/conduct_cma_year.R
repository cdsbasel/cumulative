
#' `conduct_cma_year()` computes a pooled effect size using the information from "effect_sizes_pref.csv" by 
#' fitting a multilevel meta-analytic model repetitively by adding the effect size(s) of each publication year. 
#' @param dat A data frame containing effect sizes (i.e., "effect_sizes_pref.csv")
#' @param rho value of the correlation of the sampling errors within clusters
#' @param rve logical value to apply or not robust variance estimation
#' @returns a data frame with summary information of the pooled estimates
#' @examples
#' conduct_cma_year(dat = risk_es, rho = .5, rve = TRUE)


library(tidyverse) # for data wrangling
library(metafor) # compute effect sizes


conduct_cma_year <- function(dat, rho, rve) {
  
  # get the mlma function
  source("code/functions/conduct_mlma.R")
  
  # object to store output
  m <- NULL
  
  #publication years
  pub_years <- sort(unique(dat$year_of_publication))
  
  for (i in 1:length(pub_years)) {
    
    # select subset of effect sizes
    sdat <- dat %>% filter(year_of_publication %in% c(pub_years[1]:pub_years[i]))
    
    # year being added
    added_year <- if_else(i == 1, as.character(pub_years[i]), 
                          paste0("+ ",as.character(pub_years[i])))
    
    
    # if first year, no need multilevel model: simply aggregate effect sizes
    #  and fit EE model to easily extract values of interest
    if (i == 1) {
      
      # transform data into an escalc object
      sdat <- escalc(yi = cor_yi, vi = cor_vi, data = sdat)
      
      # aggregate ES
      agg <- aggregate(sdat,
                       cluster=study_id,
                       rho=rho)
      
      res <- rma(yi = yi,
                 vi = vi,
                 data = agg, 
                 method="EE", 
                 slab= study_label)
      
    }
    
    
    # subsequent years 
    if (i != 1) {
      
      # fit multilevel model
      res <- conduct_mlma(dat = sdat, rho = rho)
      
      # robust variance estimation
      if (isTRUE(rve)) {
        res <- robust(res, cluster = study_id, clubSandwich = TRUE)
      }
      
    }
    
    
    #create output
    m <- bind_rows(m,
                   tibble(preference = unique(dat$pref),
                          slab = pub_years[i],
                          studies_added = length(unique(dat$study_label[dat$year_of_publication == pub_years[i]])),
                          es_added = nrow(dat[dat$year_of_publication == pub_years[i],]), # number of ESs added
                          rve = rve,
                          estimate = res$beta[1],
                          se = res$se,
                          zval = res$zval,
                          pval = res$pval,
                          ci.lb = res$ci.lb,
                          ci.ub = res$ci.ub,
                          sigma_2_1 = ifelse(i != 1,res$sigma2[1], 0),
                          sigma_2_2 = ifelse(i != 1,res$sigma2[2], 0)))
    
    
    
    
  }# end year for loop
  
  return(m)
} 




