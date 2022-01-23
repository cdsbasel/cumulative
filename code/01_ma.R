
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
library(data.table)


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
  w_fix = col_double(),
  w_rang = col_double(),
  g_gain_dom = col_double(),
  g_loss_dom = col_double()
)



cma_data <- read_csv(cma_file, col_types = col_specs)


# MA : TIME  --------------------------------------------------------------

cma_t <- cma_data %>% filter(pref == "time") %>% 
  # adding date for unpublished results 
  mutate(year = case_when(year == "unpublished" & study == "Lempert" ~ "2018",
                          year == "unpublished" & study == "Sisso" ~ "2017",
                          year == "unpublished" & study == "Li (study 2)" ~ "2020",
                          TRUE ~ year),
         year = as.numeric(year))


# calculating corresponding sampling variances
# vtype = "AV" leads to comparable results, even "narrower" CIs and lower ESs
cma_t <- escalc(measure = "COR", ri = g, ni = n, data = cma_t, vtype = "LS") 

# fitting a random-effects meta-analysis model  
rma_model <-  rma(yi = yi,
                  vi = vi,
                  data = cma_t, 
                  method = "REML",
                  slab=paste0(study,", ",year, "   (", as.character(n), ")"))




write_rds(rma_model, file = "output/ma_time.rds")

# save default cumulative forest plot
png("figures/ma_time_def.png", height = 25, width = 15, units = "cm", res = 600)
forest(rma_model, cex=0.75, header="Author(s) and Year (Sample size)", order = "obs")
dev.off()


# MA : RISK --------------------------------------------------------------

cma_r <- cma_data %>% 
  # only take into account monetary tasks (for now just using Best and Charness)
  filter(pref == "risk" & ma_origin == "Best_Charness (2015)v2" & !task_scen %in% c("Mortality", "Variable") )  %>% #
  pivot_longer(c(g_pos_fram, g_neg_fram), names_to = "g_type", values_to = "g_val") %>% 
  filter(!is.na(g_val)) %>% 
  mutate(g_type = case_when(g_type %in% c("g_pos_fram") ~ "g_gain_dom",
                            g_type %in% c("g_neg_fram") ~ "g_loss_dom",
                            g_type %in% c("g") ~ "g_dk_dom",
                            TRUE ~ g_type),
         n = case_when(is.na(n) ~ n_young + n_old,
                       TRUE ~ n),
         study = paste0(study,", ",year)) %>% 
  # calculating se since not given in the MAs 
  # (https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d)
  mutate(se = sqrt((n_young + n_old)/(n_young*n_old)) + ((g_val^2)/(2*(n_young + n_old)))) %>% 
  select(study, g_val, g_type, year, task_stak, se, n) 



# no need to aggregate ESs (1 per (sub)study)
cma_r <- escalc(yi = g_val, sei = se, data = cma_r)

# separating ESs into gains and losses
cma_r_loss <- cma_r %>% filter(g_type == "g_loss_dom")
cma_r_gain <- cma_r %>% filter(g_type == "g_gain_dom")

rma_model_g <-  rma(yi = yi,
                    vi = vi,
                    data = cma_r_gain, 
                    method = "REML",
                    slab=paste0(study,"   (", as.character(n), ")"))


rma_model_l <-  rma(yi = yi,
                    vi = vi,
                    data = cma_r_loss, 
                    method = "REML",
                    slab=paste0(study,"   (", as.character(n), ")"))



write_rds(rma_model_g, file = "output/ma_risk_g.rds")


write_rds(rma_model_l, file = "output/ma_risk_l.rds")

### cumulative forest plot
png("figures/ma_risk_l_def.png", height = 30, width = 20, units = "cm", res = 600)
forest(rma_model_l, cex=0.75, header="Author(s) and Year (Sample size)", order = "obs")
dev.off()

png("figures/ma_risk_g_def.png", height = 30, width = 20, units = "cm", res = 600)
forest(rma_model_g, cex=0.75, header="Author(s) and Year (Sample size)", order = "obs")
dev.off()


# MA : ALTRUISM --------------------------------------------------------------

# select studies with task of interest (i.e., lab based financial/monetary tasks; no real-world donations) 
cma_a <- cma_data %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1 & !study %in% c("Freund: Exp 4", "Sze")) %>% 
  mutate(year = as.numeric(year),
         n = if_else(is.na(n), n_young + n_old, n))

# calculating corresponding sampling variances (i.e., se^2)
cma_a <- escalc(yi = g, sei = se, data = cma_a)


# fitting a random-effects meta-analysis model  
rma_model <-  rma(yi = yi,
                  vi = vi,
                  data = cma_a, 
                  method = "REML",
                  slab=paste0(study,", ",year, "   (", as.character(n), ")"))


write_rds(rma_model, file = "output/ma_altrusim.rds")

### cumulative forest plot
png("figures/ma_altruism_def.png", height = 20, width = 15, units = "cm", res = 600)
forest(rma_model, cex=0.75, header="Author(s) and Year (Sample size)", order = "obs")
dev.off()



