


# DESCRIPTION -------------------------------------------------------------
# This script performs a series of multilevel meta-analytic regressions for 
# different values of rho (i.e., correlations within studies) and applying 
# robust variance estimation using the metafor package
# For each econ. preference,two data frames are saved:
# 1. With all the estimates. 
# 2. Estimates for rho set at .5 (for the manuscript).


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(metafor)



# RISK --------------------------------------------------------------------

# get data
dat <- read_csv("data/summary/risk/effect_sizes_risk.csv",col_types = cols())

# transform data into an escalc object
dat <- escalc(yi = cor_yi, vi = cor_vi, data = dat )
df <- NULL
rho_vec <- seq(from = .1, to = .9, by = .1)


for (rho in rho_vec) {
  
  # create approx. V matrix assuming that the effect sizes within studies are correlated with a correlation coefficient of value 'rho'
  V_mat <- vcalc(vi = vi,
                 cluster = study_id,
                 obs = es_id,
                 data = dat,
                 rho = rho)
  
  m <- list()
  
  
  # fit multilevel models using approximate V matrix
  ## age range in decades as moderator
  ### some studies are removed due to missing age information
  m[[1]] <- rma.mv(yi ~ 1 + dec_diff,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[1]] <- robust(m[[1]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## ES metric as moderator
  m[[2]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator
  m[[3]] <- rma.mv(yi ~ 0 + study_design,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## incentivization as moderator
  m[[4]] <- rma.mv(yi ~ 0 + incentivization,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  ##  domain as moderator
  m[[5]] <- rma.mv(yi ~ 0 + domain,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  ## task type as moderator
  datb <- dat %>% filter(task_type != "mixed")  # exclude three outcomes
  datb$study_id <- as.factor(datb$study_id)
  datb$es_id <- as.factor(datb$es_id)
  
  V_matb <- vcalc(vi = vi,
                  cluster = study_id,
                  obs = es_id,
                  data = datb, 
                  rho = rho)
  
  m[[6]] <- rma.mv(yi ~ 0 + task_type,
                   V = V_matb,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=datb,
                   digits=4)
  
  m[[6]] <- robust(m[[6]], cluster = study_id, clubSandwich = TRUE)
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(moderator = rownames(res$b),
                     estimate = res$b[,1],
                     se = res$se,
                     tval = res$zval,
                     ci_lb = res$ci.lb,
                     ci_ub = res$ci.ub,
                     pval = res$pval)
    
    
    meta_reg_df <- bind_rows(meta_reg_df,res_df)
    
    
    
  }
  
  meta_reg_df <- meta_reg_df %>% mutate(rho = rho)
  
  df <- df %>% bind_rows(meta_reg_df) %>% 
    mutate_if(is.numeric, round, 3)  %>% 
    write_csv("output/risk/meta_reg_risk.csv")
  
}

df_format <- df %>%
  mutate_if(is.numeric, as.character)  %>% 
  filter(moderator != "intrcpt") %>% 
  filter(rho == .5) %>%
  mutate(est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/risk/meta_reg_risk_format.csv")



# TIME --------------------------------------------------------------------

# get data
dat <- read_csv("data/summary/time/effect_sizes_time.csv",col_types = cols())

# transform data into an escalc object
dat <- escalc(yi = cor_yi, vi = cor_vi, data = dat )
df <- NULL
rho_vec <- seq(from = .1, to = .9, by = .1)


for (rho in rho_vec) {
  
  # create approx. V matrix assuming that the effect sizes within studies are correlated with a correlation coefficient of value 'rho'
  V_mat <- vcalc(vi = vi,
                 cluster = study_id,
                 obs = es_id,
                 data = dat,
                 rho = rho)
  
  m <- list()
  
  
  # fit multilevel models using approximate V matrix
  ## age range in decades as moderator
  ### some studies are removed due to missing age information
  m[[1]] <- rma.mv(yi ~ 1 + dec_diff,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[1]] <- robust(m[[1]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## ES metric as moderator
  m[[2]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator
  m[[3]] <- rma.mv(yi ~ 0 + study_design,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## incentivization as moderator
  m[[4]] <- rma.mv(yi ~ 0 + incentivization,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(moderator = rownames(res$b),
                     estimate = res$b[,1],
                     se = res$se,
                     tval = res$zval,
                     ci_lb = res$ci.lb,
                     ci_ub = res$ci.ub,
                     pval = res$pval)
    
    meta_reg_df <- bind_rows(meta_reg_df,res_df)
    
    
    
  }
  
  meta_reg_df <- meta_reg_df %>% mutate(rho = rho)
  
  df <- df %>% bind_rows(meta_reg_df) %>% 
    mutate_if(is.numeric, round, 3)  %>% 
    write_csv("output/time/meta_reg_time.csv")
  
}

df_format <- df %>%
  mutate_if(is.numeric, as.character)  %>% 
  filter(moderator != "intrcpt") %>% 
  filter(rho == .5) %>%
  mutate(est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/time/meta_reg_time_format.csv")



# SOCIAL --------------------------------------------------------------------

# get data
dat <- read_csv("data/summary/social/effect_sizes_social.csv",col_types = cols())

# transform data into an escalc object
dat <- escalc(yi = cor_yi, vi = cor_vi, data = dat )
df <- NULL
rho_vec <- seq(from = .1, to = .9, by = .1)


for (rho in rho_vec) {
  
  # create approx. V matrix assuming that the effect sizes within studies are correlated with a correlation coefficient of value 'rho'
  V_mat <- vcalc(vi = vi,
                 cluster = study_id,
                 obs = es_id,
                 data = dat,
                 rho = rho)
  
  m <- list()
  
  
  # fit multilevel models using approximate V matrix
  ## age range in decades as moderator
  ### some studies are removed due to missing age information
  m[[1]] <- rma.mv(yi ~ 1 + dec_diff,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[1]] <- robust(m[[1]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## ES metric as moderator
  m[[2]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator
  m[[3]] <- rma.mv(yi ~ 0 + study_design,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  # ## incentivization as moderator (NOT APPLICABLE)
  # m[[4]] <- rma.mv(yi ~ 0 + incentivization,
  #                  V = V_mat,
  #                  random = ~ 1 | study_id/es_id, # random effect of study and estimate
  #                  data=dat,
  #                  digits=4)
  # 
  # m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(moderator = rownames(res$b),
                     estimate = res$b[,1],
                     se = res$se,
                     tval = res$zval,
                     ci_lb = res$ci.lb,
                     ci_ub = res$ci.ub,
                     pval = res$pval)
    
    
    meta_reg_df <- bind_rows(meta_reg_df,res_df)
    
    
    
  }
  
  meta_reg_df <- meta_reg_df %>% mutate(rho = rho)
  
  df <- df %>% bind_rows(meta_reg_df) %>% 
    mutate_if(is.numeric, round, 3)  %>% 
    write_csv("output/social/meta_reg_social.csv")
  
}

df_format <- df %>%
  mutate_if(is.numeric, as.character)  %>% 
  filter(moderator != "intrcpt") %>% 
  filter(rho == .5) %>%
  mutate(est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/social/meta_reg_social_format.csv")

# clear environment
rm(list = ls()) 
