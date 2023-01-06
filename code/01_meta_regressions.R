


# DESCRIPTION -------------------------------------------------------------
# This script performs a series of multilevel meta-analytic regressions for 
# different values of rho (i.e., correlations within studies) and applying 
# robust variance estimation using the metafor package
# For each econ. preference,two data frames are saved:
# 1. With all the estimates. 
# 2. Estimates for rho set at .5 (for the manuscript).


# PACKAGES ----------------------------------------------------------------

library(tidyverse) # for data wrangling
library(metafor) # for meta-regressions

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
  
  ### for extreme group design studies we calculate average prop. of females in the sample (weighted average).
  ###  some studies are removed due to missing gender information
  dat <- dat %>% 
    rowwise() %>% 
    mutate(prop_female_gen = 
             case_when(is.na(prop_female) & !is.na(young_prop_female) & !is.na(old_prop_female) ~ ((young_total_n/n_incl_es)*young_prop_female)+((old_total_n/n_incl_es)*old_prop_female),
                       !is.na(prop_female) ~ prop_female)) %>% 
    ungroup()
  
  m[[2]] <- rma.mv(yi ~ 1 + prop_female_gen,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  
  
  ## ES metric as moderator
  m[[3]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator
  m[[4]] <- rma.mv(yi ~ 0 + study_design,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## incentivization as moderator
  
  ## remove "unknown" incentivization

  datb <- dat %>% filter(incentivization != "unknown")  # exclude when we are not certain if choice were incetivzed or not
  datb$study_id <- as.factor(datb$study_id)
  datb$es_id <- as.factor(datb$es_id)
  
  V_matb <- vcalc(vi = vi,
                  cluster = study_id,
                  obs = es_id,
                  data = datb, 
                  rho = rho)
  
  
  m[[5]] <- rma.mv(yi ~ 0 + incentivization,
                   V = V_matb,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=datb,
                   digits=4)
  
  m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  
  ##  domain as moderator
  m[[6]] <- rma.mv(yi ~ 0 + domain,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[6]] <- robust(m[[6]], cluster = study_id, clubSandwich = TRUE)
  
  ## task type as moderator
  datb <- dat %>% filter(task_type != "mixed")  # exclude three outcomes
  datb$study_id <- as.factor(datb$study_id)
  datb$es_id <- as.factor(datb$es_id)
  
  V_matb <- vcalc(vi = vi,
                  cluster = study_id,
                  obs = es_id,
                  data = datb, 
                  rho = rho)
  
  m[[7]] <- rma.mv(yi ~ 0 + task_type,
                   V = V_matb,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=datb,
                   digits=4)
  
  m[[7]] <- robust(m[[7]], cluster = study_id, clubSandwich = TRUE)
  
  ## study context as moderator
  m[[8]] <- rma.mv(yi ~ 0 + study_context,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[8]] <- robust(m[[8]], cluster = study_id, clubSandwich = TRUE)
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(reg_num = i,
                     moderator = rownames(res$b),
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
  mutate(reg_num = reg_num,
         est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/risk/meta_reg_risk_format.csv")

print("Meta-regression results with risk pref. effect sizes saved in output/risk/")

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
  
  ### for extreme group design studies we calculate average prop. of females in the sample (weighted average).
  ###  some studies are removed due to missing gender information
  dat <- dat %>% 
    rowwise() %>% 
    mutate(prop_female_gen = 
             case_when(is.na(prop_female) & !is.na(young_prop_female) & !is.na(old_prop_female) ~ ((young_total_n/n_incl_es)*young_prop_female)+((old_total_n/n_incl_es)*old_prop_female),
                       !is.na(prop_female) ~ prop_female)) %>% 
    ungroup()
  
  m[[2]] <- rma.mv(yi ~ 1 + prop_female_gen,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  
  
  ## ES metric as moderator
  m[[3]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator
  m[[4]] <- rma.mv(yi ~ 0 + study_design,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## incentivization as moderator
  datb <- dat %>% filter(incentivization != "unknown")  # exclude when we are not certain if choice were incetivzed or not
  datb$study_id <- as.factor(datb$study_id)
  datb$es_id <- as.factor(datb$es_id)
  
  V_matb <- vcalc(vi = vi,
                  cluster = study_id,
                  obs = es_id,
                  data = datb, 
                  rho = rho)
  
  
  m[[5]] <- rma.mv(yi ~ 0 + incentivization,
                   V = V_matb,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=datb,
                   digits=4)
  
  m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study context as moderator
  m[[6]] <- rma.mv(yi ~ 0 + study_context,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[6]] <- robust(m[[6]], cluster = study_id, clubSandwich = TRUE)
  
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(reg_num = i,
                     moderator = rownames(res$b),
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
  mutate(reg_num = reg_num,
         est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/time/meta_reg_time_format.csv")

print("Meta-regression results with time pref. effect sizes saved in output/time/")

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
  
  
  ### for extreme group design studies we calculate average prop. of females in the sample (weighted average).
  ###  some studies are removed due to missing gender information
  dat <- dat %>% 
    rowwise() %>% 
    mutate(prop_female_gen = 
             case_when(is.na(prop_female) & !is.na(young_prop_female) & !is.na(old_prop_female) ~ ((young_total_n/n_incl_es)*young_prop_female)+((old_total_n/n_incl_es)*old_prop_female),
                       !is.na(prop_female) ~ prop_female)) %>% 
    ungroup()
  
  m[[2]] <- rma.mv(yi ~ 1 + prop_female_gen,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## ES metric as moderator
  m[[3]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator
  m[[4]] <- rma.mv(yi ~ 0 + study_design,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## incentivization as moderator
  datb <- dat %>% filter(incentivization != "unknown")  # exclude when we are not certain if choice were incetivzed or not
  datb$study_id <- as.factor(datb$study_id)
  datb$es_id <- as.factor(datb$es_id)
  
  V_matb <- vcalc(vi = vi,
                  cluster = study_id,
                  obs = es_id,
                  data = datb, 
                  rho = rho)
  
  
  m[[5]] <- rma.mv(yi ~ 0 + incentivization,
                   V = V_matb,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=datb,
                   digits=4)

  m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  ## study context as moderator
  m[[6]] <- rma.mv(yi ~ 0 + study_context,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[6]] <- robust(m[[6]], cluster = study_id, clubSandwich = TRUE)
  
  
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(reg_num = i,
                     moderator = rownames(res$b),
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
  mutate(reg_num = reg_num,
         est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/social/meta_reg_social_format.csv")


print("Meta-regression results with social pref. effect sizes saved in output/social/")






# EFFORT --------------------------------------------------------------------

# get data
dat <- read_csv("data/summary/effort/effect_sizes_effort.csv",col_types = cols())

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
  
  
  ### for extreme group design studies we calculate average prop. of females in the sample (weighted average).
  ###  some studies are removed due to missing gender information
  dat <- dat %>% 
    rowwise() %>% 
    mutate(prop_female_gen = 
             case_when(is.na(prop_female) & !is.na(young_prop_female) & !is.na(old_prop_female) ~ ((young_total_n/n_incl_es)*young_prop_female)+((old_total_n/n_incl_es)*old_prop_female),
                       !is.na(prop_female) ~ prop_female)) %>% 
    ungroup()
  
  m[[2]] <- rma.mv(yi ~ 1 + prop_female_gen,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[2]] <- robust(m[[2]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## ES metric as moderator
  m[[3]] <- rma.mv(yi ~ 0 + cor_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[3]] <- robust(m[[3]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## Effort type as moderator
  m[[4]] <- rma.mv(yi ~ 0 + effort_type,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## Domain type as moderator
  m[[5]] <- rma.mv(yi ~ 0 + domain,
                   V = V_mat,
                   random = ~ 1 | study_id/es_id, # random effect of study and estimate
                   data=dat,
                   digits=4)
  
  m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  
  ## study design as moderator (NOT APPLICABLE)
  # m[[4]] <- rma.mv(yi ~ 0 + study_design,
  #                  V = V_mat,
  #                  random = ~ 1 | study_id/es_id, # random effect of study and estimate
  #                  data=dat,
  #                  digits=4)
  # 
  # m[[4]] <- robust(m[[4]], cluster = study_id, clubSandwich = TRUE)
  
  
  #  ## incentivization as moderator (NOT APPLICABLE)
  # m[[5]] <- rma.mv(yi ~ 0 + incentivization,
  #                  V = V_mat,
  #                  random = ~ 1 | study_id/es_id, # random effect of study and estimate
  #                  data=dat,
  #                  digits=4)
  # 
  # m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  # study context as moderator (NOT APPLICABLE)
  # m[[5]] <- rma.mv(yi ~ 0 + study_context,
  #                  V = V_mat,
  #                  random = ~ 1 | study_id/es_id, # random effect of study and estimate
  #                  data=dat,
  #                  digits=4)
  # 
  # m[[5]] <- robust(m[[5]], cluster = study_id, clubSandwich = TRUE)
  
  
  
  # expand the list of models in a tidy format
  meta_reg_df <- NULL
  for (i in 1:length(m)) {
    
    res <- m[[i]]
    # could also use broom::tidy()
    res_df <- tibble(reg_num = i,
                     moderator = rownames(res$b),
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
    write_csv("output/effort/meta_reg_effort.csv")
  
}

df_format <- df %>%
  mutate_if(is.numeric, as.character)  %>% 
  filter(moderator != "intrcpt") %>% 
  filter(rho == .5) %>%
  mutate(reg_num = reg_num,
         est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(moderator, est,se,tval, pval, ci95) %>% 
  write_csv("output/effort/meta_reg_effort_format.csv")


print("Meta-regression results with effort pref. effect sizes saved in output/effort/")


