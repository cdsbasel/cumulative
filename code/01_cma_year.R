
# DESCRIPTION -------------------------------------------------------------

# starting to analyse smthg 

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(metafor)


# READ DATA ---------------------------------------------------------------


cma_file <- "cma_data.csv"

col_specs <- cols(
  pref = col_character(),
  ma_origin = col_character(),
  study = col_character(),
  year = col_character(),
  task = col_character(),
  n_young = col_double(),
  n_old = col_double(),
  n = col_double(),
  mean_age_young = col_character(),
  mean_age_old = col_character(),
  mean_edu_younger = col_double(),
  mean_edu_older = col_double(),
  g = col_double(),
  g_gain_dom = col_double(),
  g_loss_dom = col_double(),
  task_scen = col_character(),
  task_stak = col_character(),
  g_pos_fram = col_double(),
  g_neg_fram = col_double(),
  w_fix = col_double(),
  w_rang = col_double(),
  task_type = col_character(),
  task_inc = col_character(),
  task_anon = col_character()
)



cma_data <- read_csv(cma_file, col_types = col_specs)


# CMA : TIME (test) --------------------------------------------------------------

cma_t <- cma_data %>% filter(pref == "time") %>% 
  
  # not ideal, but unpublished results get assigned ther year 2021
  mutate(year = if_else(year == "unpublished", 2021, as.numeric(year)))

# ARE THESE NEXT STEPS CORRECT? I checked Kendra's code on OSF, and yoU used the metacor package + 
# I am not sure if I should use "zcor" as a measure, because otherwise the values are not converted back 

### calculate correlations and corresponding sampling variances
cma_ <- escalc(measure = "COR", ri = g, ni = n, data = cma_t) 

# fitting a random-effects meta-analysis model  
rma_model <-  rma(yi = yi, vi = vi, data = cma_, 
                  slab=paste0(study,", ",year, "   (", as.character(n), ")"))

### cumulative meta-analysis (in the order of publication year)
tmp_y <- cumul(rma_model, order = order(cma_$year))

### cumulative forest plot
forest(tmp_y, cex=0.75, header="Author(s) and Year (Sample size)")



