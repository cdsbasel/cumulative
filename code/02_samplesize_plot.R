
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
  w_fix = col_double(),
  w_rang = col_double(),
  g_gain_dom = col_double(),
  g_loss_dom = col_double()
)



cma_data <- read_csv(cma_file, col_types = col_specs)


# CMA BY STUDY : TIME  --------------------------------------------------------------

cma_t <- cma_data %>% filter(pref == "time") %>% 
  # adding date for unpublished results 
  mutate(year = case_when(year == "unpublished" & study == "Lempert" ~ "2018",
                          year == "unpublished" & study == "Sisso" ~ "2017",
                          year == "unpublished" & study == "Li (study 2)" ~ "2020",
                          TRUE ~ year),
         year = as.numeric(year))

cma_t %>%
  filter(n < 1000) %>% 
  group_by(year) %>% 
  summarise(n = mean(n)) %>% 
  ggplot() +
geom_col(aes(x = year, y = n))

# CMA BY STUDY : RISK --------------------------------------------------------------

cma_r <- cma_data %>% 
  # only take into account monetary tasks
  filter(pref == "risk" & !task_scen %in% c("Mort", "Var"))  %>% 
  
  # classify ESs into a gain, or loss or dk domain
  mutate(g = case_when(!is.na(g) & c(!is.na(g_gain_dom) | !is.na(g_loss_dom)) ~ NA_real_,
                       TRUE ~ g)) %>% 
  pivot_longer(c(g, g_pos_fram, g_neg_fram, g_gain_dom, g_loss_dom), names_to = "g_type", values_to = "g_val") %>% 
  filter(!is.na(g_val)) %>% 
  mutate(g_type = case_when(g_type %in% c("g_pos_fram") ~ "g_gain_dom",
                            g_type %in% c("g_neg_fram") ~ "g_loss_dom",
                            g_type %in% c("g") ~ "g_dk_dom",
                            TRUE ~ g_type),
         n = case_when(is.na(n) ~ n_young + n_old,
                       TRUE ~ n),
         study = paste0(study,", ",year)) %>% 
  select(study, g_val, g_type, year, n) %>% 
  # removing duplicates
  distinct(study, g_type, n,year, .keep_all = T) %>% 
  filter(study != "Weller et al.") # not sure about this one....


cma_r %>%
  filter(n < 1000) %>% 
  group_by(year) %>% 
  summarise(n = mean(n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = year, y = n))


# CMA BY STUDY : ALTRUISM --------------------------------------------------------------

# select studies with task of interest (i.e., lab based financial/monetary tasks; no real-world donations) 
cma_a <- cma_data %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1 & !study %in% c("Freund: Exp 4", "Sze")) %>% 
  mutate(year = as.numeric(year),
         n = if_else(is.na(n), n_young + n_old, n))

cma_a %>%
  filter(n < 1000) %>% 
  group_by(year) %>% 
  summarise(n = mean(n)) %>% 
  ggplot() +
  geom_col(aes(x = year, y = n))


