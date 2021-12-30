
# DESCRIPTION -------------------------------------------------------------

# starting to analyse smthg 


# TO DO -------------------------------------------------------------------

# Need to sort "risk":
# eliminate duplicate studies between Mata and Best.
# also make sure how certain studies are designed when aggregating the effect sizes 
# sort the gain/loss and pos/neg domain effec sizes
# update study list


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(metafor)


# READ DATA ---------------------------------------------------------------


cma_file <- "data/cma_data.csv"

col_specs <- cols(
  pref = col_character(),
  ma_origin = col_character(),
  study = col_character(),
  year = col_character(),
  n = col_double(),
  n_young = col_double(),
  n_old = col_double(),
  mean_age_young = col_character(),
  mean_age_old = col_character(),
  mean_edu_younger = col_double(),
  mean_edu_older = col_double(),
  task = col_character(),
  task_scen = col_character(),
  task_stak = col_character(),
  beh_task = col_double(),
  fin_task = col_double(),
  g = col_double(),
  se = col_double(),
  g_pos_fram = col_double(),
  g_neg_fram = col_double(),
  se_pos_fram = col_double(),
  se_neg_fram = col_double(),
  w_fix = col_double(),
  w_rang = col_double(),
  g_gain_dom = col_double(),
  g_loss_dom = col_double(),
  se_gain_dom = col_double(),
  se_loss_dom = col_double()
)



cma_data <- read_csv(cma_file, col_types = col_specs)


# MA BY STUDY : TIME  --------------------------------------------------------------

cma_t <- cma_data %>% filter(pref == "time") %>% 
  # not ideal, but unpublished results get assigned the year 2021
  mutate(year = if_else(year == "unpublished", 2021, as.numeric(year)))

# ARE THESE NEXT STEPS CORRECT? I checked Kendra's code on OSF, and yoU used the metacor package + 
# I am not sure if I should use "zcor" as a measure, because otherwise the values are not converted back 


### Aggregate Multiple Effect Sizes or Outcomes Within Studies
# NA

### calculate correlations and corresponding sampling variances
cma_t <- escalc(measure = "COR", ri = g, ni = n, data = cma_t) 

# fitting a random-effects meta-analysis model  
rma_model <-  rma(yi = yi, vi = vi, data = cma_t, 
                  slab=paste0(study,", ",year, "   (", as.character(n), ")"))



### forest plot
png("figures/ma_time.png", height = 30, width = 20, units = "cm", res = 600)
forest(rma_model, cex=0.75, header="Author(s) and Year (Sample size)")
dev.off()

# CMA BY STUDY : RISK --------------------------------------------------------------

cma_r <- cma_data %>% 
  filter(pref == "risk" & !task_scen %in% c("Mort", "Var"))  %>% # only take into account monetary tasks
  mutate(n = case_when(is.na(n) ~ n_young + n_old,
                       TRUE ~ n),
         study = paste0(study,", ",year))

## Fairly messy, will need to fix this
cma_ra<- cma_r %>%  select(c(pref:n_old), se, g)  %>% filter(!is.na(g)) 
cma_rb<- cma_r %>%  select(c(pref:n_old), se_neg_fram, g_neg_fram)  %>% filter(!is.na(g_neg_fram)) %>%
  rename(se = se_neg_fram, g = g_neg_fram)
cma_rc<- cma_r %>%  select(c(pref:n_old), se_pos_fram, g_pos_fram)  %>% filter(!is.na(g_pos_fram)) %>%
  rename(se = se_pos_fram, g = g_pos_fram)
cma_rd<- cma_r %>%  select(c(pref:n_old), se_loss_dom, g_loss_dom)  %>% filter(!is.na(g_loss_dom)) %>%
  rename(se = se_loss_dom, g = g_loss_dom)
cma_re<- cma_r %>%  select(c(pref:n_old), se_gain_dom, g_gain_dom)  %>% filter(!is.na(g_gain_dom))%>%
  rename(se = se_gain_dom, g = g_gain_dom)

cma_r <- bind_rows(cma_ra, cma_rb, cma_rc, cma_rd, cma_re) 
# what do we do about the pos/neg framing and gain/loss domain? Should we aggregate these?


cma_r <- escalc(yi = g, sei = se, data = cma_r)

## Aggregate Multiple Effect Sizes or Outcomes Within Studies (assuming independent samples, not ideal...)
cma_r <- aggregate.escalc(cma_r, cluster = study, struct="ID")


rma_model <-  rma(yi, vi , data = cma_r, 
                  slab=paste0(study,"   (", as.character(n), ")"))


### forest plot
png("figures/ma_risk.png", height = 30, width = 20, units = "cm", res = 600)
forest(rma_model, cex=0.75, header="Author(s) and Year (Sample size)")
dev.off()



# CMA BY STUDY : ALTRUISM --------------------------------------------------------------

cma_a <- cma_data %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1) %>% #  need to make sure we are selecting the correct studies with taks of interest
  mutate(year = as.numeric(year),
         n = if_else(is.na(n), n_young + n_old, n))


cma_a <- escalc(yi = g, sei = se, data = cma_a)

## Aggregate Multiple Effect Sizes or Outcomes Within Studies (assuming independent samples, not ideal...)
# NA

rma_model <-  rma(yi = yi, vi = vi, data = cma_a, 
                  slab=paste0(study,", ",year, "   (", as.character(n), ")"))


### forest plot
png("figures/ma_altruism.png", height = 20, width = 15, units = "cm", res = 600)
forest(rma_model, cex=0.75, header="Author(s) and Year (Sample size)")
dev.off()




