
library(tidyverse)

# DATA FORMATTING ---------------------------------------------------------

dat <- read_csv("citation/es_cit_dat.csv")

es_cit_dat <- dat %>% 
  filter(no_link == FALSE)  %>% 
  mutate(year_in_print = year-year_of_publication) %>%  # cited before publication
  group_by(first_author, year_of_pub_chr,year_of_publication, preference,year_in_print,yi, n_incl_es) %>% 
  summarise(cit_count = sum(cit_count)) %>% 
  mutate(preference = str_to_title(preference),
         study_label = paste0(first_author, " (", year_of_pub_chr, ")")) %>% 
  ungroup() %>% 
  group_by(study_label) %>% 
  # some papers have been cited as a pre-print/working paper
  mutate(corrected_year_in_print = year_in_print - min(year_in_print),
         corrected_year_of_publication = year_of_publication - corrected_year_in_print)


# overall mean/median yearly citations for each publication
dat_pub <- es_cit_dat %>% 
  group_by(preference, study_label, yi, n_incl_es,corrected_year_of_publication) %>% 
  summarise(cit_median = median(cit_count),
            cit_mean = mean(cit_count)) %>% 
  mutate(n_incl_es_log = log10(n_incl_es),
         # some papers have not been cited
         cit_median_log = if_else(cit_median ==0,log10((cit_median+1)), log10(cit_median))) %>% 
  rename(year_of_publication = corrected_year_of_publication) %>% 
  mutate(num_dec_available = (2022-year_of_publication)/10)

# LM CITATIONS & YI -------------------------------------------------------

fit <- NULL

fit_risk <- lm(cit_median_log ~ 1 + yi + num_dec_available,
               data =  filter(dat_pub, preference == "Risk"))
fit[[1]] <- fit_risk

fit_time <- lm(cit_median_log ~ 1 + yi + num_dec_available,
               data =  filter(dat_pub, preference == "Time"))
fit[[2]] <- fit_time

fit_social <-  lm(cit_median_log ~ 1 + yi + num_dec_available,
                  data =  filter(dat_pub, preference == "Social"))
fit[[3]] <-fit_social

fit_effort <-  lm(cit_median_log ~ 1 + yi + num_dec_available,
                  data =  filter(dat_pub, preference == "Effort"))
fit[[4]] <- fit_effort

prefs <- c("risk", "time", "social", "effort")


# expand the list of models in a tidy format
meta_reg_df <- NULL
for (i in 1:length(fit)) {
  
  pref <- prefs[i]
  
  res <- fit[[i]]
  res_s <- summary(res)
  # could also use broom::tidy()
  res_df <- tibble(pref = pref,
                   moderator = rownames(res_s$coefficients),
                   estimate = res_s$coefficients[,1],
                   se = res_s$coefficients[,2],
                   tval = res_s$coefficients[,3],
                   ci_lb = confint(res)[,1],
                   ci_ub = confint(res)[,2],
                   pval = res_s$coefficients[,4])
  
  
  meta_reg_df <- bind_rows(meta_reg_df,res_df)
  
  
  
}


df <-  bind_rows(meta_reg_df) %>%
  mutate_if(is.numeric, round, 3)  %>%
  write_csv("citation/cit_yi_lm.csv")


df_format <- df %>%
  mutate_if(is.numeric, as.character)  %>% 
  mutate(est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(pref, moderator, est,se,tval, pval, ci95) %>% 
  write_csv("citation/reg_cit_yi_format.csv")



print("Regression results with citation counts and effect cizes saved in citation/")

# LM CITATIONS & SAMPLE SIZE ----------------------------------------------------------------

fit <- NULL

fit_risk <- lm(cit_median_log ~ 1 + n_incl_es_log + num_dec_available,
               data =  filter(dat_pub, preference == "Risk"))
fit[[1]] <- fit_risk

fit_time <- lm(cit_median_log ~ 1 + n_incl_es_log + num_dec_available,
               data =  filter(dat_pub, preference == "Time"))
fit[[2]] <- fit_time

fit_social <-  lm(cit_median_log ~ 1 + n_incl_es_log + num_dec_available,
                  data =  filter(dat_pub, preference == "Social"))
fit[[3]] <- fit_social

fit_effort <-  lm(cit_median_log ~ 1 + n_incl_es_log + num_dec_available,
                  data =  filter(dat_pub, preference == "Effort"))
fit[[4]] <- fit_effort

prefs <- c("risk", "time", "social", "effort")


# expand the list of models in a tidy format
meta_reg_df <- NULL
for (i in 1:length(fit)) {
  
  pref <- prefs[i]
  
  res <- fit[[i]]
  res_s <- summary(res)
  # could also use broom::tidy()
  res_df <- tibble(pref = pref,
                   moderator = rownames(res_s$coefficients),
                   estimate = as.vector(res_s$coefficients[,1]),
                   se = as.vector(res_s$coefficients[,2]),
                   tval = as.vector(res_s$coefficients[,3]),
                   ci_lb = as.vector(confint(res)[,1]),
                   ci_ub = as.vector(confint(res)[,2]),
                   pval = as.vector(res_s$coefficients[,4]))
  
  
  meta_reg_df <- bind_rows(meta_reg_df,res_df)
  
  
  
}


df <- meta_reg_df %>%
  mutate_if(is.numeric, round, 3)  %>%
  write_csv("citation/cit_n_lm.csv")



df_format <- df %>%
  mutate_if(is.numeric, as.character)  %>% 
  mutate(est = estimate,
         se = se,
         tval = tval,
         pval = pval,
         ci95 = paste0(" [", ci_lb, ", ", ci_ub,"]"))  %>%
  select(pref, moderator, est,se,tval, pval, ci95) %>% 
  write_csv("citation/reg_cit_n_format.csv")


print("Regression results with citation counts and sample cizes saved in citation/")








