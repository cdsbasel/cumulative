
# DESCRIPTION -------------------------------------------------------------

# starting to analyse smthg 


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(metafor)
library(data.table)


# FUNCTIONS ---------------------------------------------------------------

# from https://osf.io/gs6hk/ ton create correlation / covariance matrices of outcomes
cor_matrix <- function(x, r, v = rep(1, length(x)), na.rm = FALSE) {
  # creat correlation / covariance matrices of outcomes
  mat <- diag(v)
  se <- sqrt(v)
  se[is.na(se)] <- 0
  if (length(x) > 1L) {
    for (i in 2:nrow(mat)) {
      for (j in 1:(i-1)) {
        if (x[i] == x[j]) {
          mat[i, j] <- mat[j, i] <- r * se[i] * se[j]
        }
      }
    }
  }
  dimnames(mat) <- list(1:nrow(mat), 1:ncol(mat))
  if (na.rm) {
    keep <- !is.na(diag(mat))
    mat <- mat[keep, keep]
  }
  mat
}


# READ DATA ---------------------------------------------------------------


ef_file <- "data/ef_data.csv"

col_specs <- cols(
  row_num = col_double(),
  study = col_character(),
  year = col_double(),
  Citation = col_character(),
  n_1 = col_double(),
  n_2 = col_double(),
  mean_age_1 = col_character(),
  mean_age_2 = col_character(),
  domain = col_character(),
  Miyake = col_character(),
  stask = col_character(),
  Task.Description = col_character(),
  DV = col_character(),
  Log.transformed. = col_character(),
  g = col_double(),
  var.g = col_double(),
  se.g = col_double()
)



ef_data <- read_csv(ef_file, col_types =  col_specs)


# MA: UPDATING  --------------------------------------------------------------


dat <- ef_data %>% filter(domain == "updating") %>%  mutate(row_num=1:n())
dat <- dat %>% filter(var.g < 3) # extremely high variance for some of the ESs (related to RT), seem to be on a different scale

#In some of the primary studies, multiple outcomes were reported.
# In order to make use of all available information and to avoid
# potential bias by selecting only one outcome per study,
# all reported outcomes of each study were included in the meta-analysis.
# Assuming different outcomes of the same study to be independent is likely
# invalid as they refer to the same treatment and control group.
# Thus, outcomes reported by the same study were explicitly modeled as correlated.
# In the absence of any reported correlations in the primary studies,
# correlations were set to r = 0.7.
 
V <- cor_matrix(dat$study, r = 0.7, v = dat$var.g)


# fitting a three-level meta-analysis model 
rma_model <-  rma.mv(yi = g,
                  V = V,
                  data = dat, 
                  method = "REML",
                  random = ~ 1 | study/row_num, 
                  slab = study)


write_rds(rma_model, file = "output/ma_ef_updating.rds")


# MA: SHIFTING  --------------------------------------------------------------


dat <- ef_data %>% filter(domain == "shifting") %>%  mutate(row_num=1:n())


#In some of the primary studies, multiple outcomes were reported.
# In order to make use of all available information and to avoid
# potential bias by selecting only one outcome per study,
# all reported outcomes of each study were included in the meta-analysis.
# Assuming different outcomes of the same study to be independent is likely
# invalid as they refer to the same treatment and control group.
# Thus, outcomes reported by the same study were explicitly modeled as correlated.
# In the absence of any reported correlations in the primary studies,
# correlations were set to r = 0.7.

V <- cor_matrix(dat$study, r = 0.7, v = dat$var.g)


# fitting a three-level meta-analysis model 
rma_model <-  rma.mv(yi = g,
                     V = V,
                     data = dat, 
                     method = "REML",
                     random = ~ 1 | study/row_num, 
                     slab = study)

write_rds(rma_model, file = "output/ma_ef_shifting.rds")


# MA: SPEED  --------------------------------------------------------------


dat <- ef_data %>% filter(domain == "processing speed") %>%  mutate(row_num=1:n())


#In some of the primary studies, multiple outcomes were reported.
# In order to make use of all available information and to avoid
# potential bias by selecting only one outcome per study,
# all reported outcomes of each study were included in the meta-analysis.
# Assuming different outcomes of the same study to be independent is likely
# invalid as they refer to the same treatment and control group.
# Thus, outcomes reported by the same study were explicitly modeled as correlated.
# In the absence of any reported correlations in the primary studies,
# correlations were set to r = 0.7.

V <- cor_matrix(dat$study, r = 0.7, v = dat$var.g)


# fitting a three-level meta-analysis model 
rma_model <-  rma.mv(yi = g,
                     V = V,
                     data = dat, 
                     method = "REML",
                     random = ~ 1 | study/row_num, 
                     slab = study)

write_rds(rma_model, file = "output/ma_ef_procspeed.rds")











# MA: INHIBITION  --------------------------------------------------------------


dat <- ef_data %>% filter(domain == "inhibition") %>%  mutate(row_num=1:n())


#In some of the primary studies, multiple outcomes were reported.
# In order to make use of all available information and to avoid
# potential bias by selecting only one outcome per study,
# all reported outcomes of each study were included in the meta-analysis.
# Assuming different outcomes of the same study to be independent is likely
# invalid as they refer to the same treatment and control group.
# Thus, outcomes reported by the same study were explicitly modeled as correlated.
# In the absence of any reported correlations in the primary studies,
# correlations were set to r = 0.7.

V <- cor_matrix(dat$study, r = 0.7, v = dat$var.g)


# fitting a three-level meta-analysis model 
rma_model <-  rma.mv(yi = g,
                     V = V,
                     data = dat, 
                     method = "REML",
                     random = ~ 1 | study/row_num, 
                     slab = study)

write_rds(rma_model, file = "output/ma_ef_inhibition.rds")


