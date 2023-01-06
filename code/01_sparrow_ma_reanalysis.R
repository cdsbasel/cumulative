

# DESCRIPTION -------------------------------------------------------------
# This scripts reads, computes and prints effect sizes (Hedge's g and correlation) using 
# the Sparrow et al. (2021) data set (corrected) downloaded from https://osf.io/9hacs

# PACKAGES ----------------------------------------------------------------


library(metafor) # compute effect sizes + rma
library(janitor) # clean var names
library(esc) # compute effect sizes
library(readxl) # open xlsx file
library(tidyverse)# for data wrangling



# DATA --------------------------------------------------------------------

dt <- read_xlsx("data/summary/social/sparrow_effect_size_altruism.xlsx") 

# SPARROW ET AL 2021: HEDGE'S G-----------------------------------------------------------------

# Means and SDs calculate Hedge's g
dtA <- dt %>% filter(!is.na(`Older Mean`))
dtA <- clean_names(dtA)
dtA <- escalc("SMD",
              m1i = older_mean,
              sd1i = older_std_dev,
              n1i = older_sample_size,
              m2i = younger_mean,
              sd2i = younger_std_dev,
              n2i = younger_sample_size,
              data = dtA)


# Chi-squared test (Freund (Exp. 4), 2014) calculate Hedge's g
dtB <- dt %>% filter(is.na(`Older Mean`))
dtB <- clean_names(dtB)
dtB$yi <- esc_chisq(chisq = dtB$chi_squared, totaln =  dtB$total_n, es.type = "g")$es[1] # get es
dtB$vi <- esc_chisq(chisq = dtB$chi_squared, totaln =  dtB$total_n,  es.type = "g")$var[1] # get var

# put data together
dtC <- bind_rows(dtA,dtB)

### pooling ES

# more or less replicate the published effect
reported_rma <- rma(yi = abs(yi), # Gong et al., 2019 should be negative
                     vi = vi,
                     data = dtC)


print(list("Reported overall effect size",reported_rma ))

# we get a fairly lower estimate than in Sparrow et al., 
corrected_rma <- rma(yi = yi,
                    vi = vi,
                    data = dtC)


print(list("Corrected overall effect size",corrected_rma ))

# SPARROW ET AL 2021: CORRELATION -----------------------------------------------------------------

dtA <- dt %>% filter(!is.na(`Older Mean`))
dtA <- janitor::clean_names(dtA)
dtA <- escalc("RPB",
              m1i = older_mean,
              sd1i = older_std_dev,
              n1i = older_sample_size,
              m2i = younger_mean,
              sd2i = younger_std_dev,
              n2i = younger_sample_size,
              data = dtA)

# Chi-squared test (Freund (Exp. 4), 2014) calculate correlation
dtB <- dt %>% filter(is.na(`Older Mean`))
dtB <- clean_names(dtB)
dtB$yi <- esc_chisq(chisq = dtB$chi_squared, totaln =  dtB$total_n, es.type = "r")$es[1] # get es
dtB$vi <- esc_chisq(chisq = dtB$chi_squared, totaln =  dtB$total_n,  es.type = "r")$var[1] # get var

# put data together
dtC <- bind_rows(dtA,dtB)

# converted estimate (published effect)
conv_rep_rma <- rma(yi = abs(yi),
                     vi = vi,
                     data = dtC)

# converted estimate
conv_corr_rma <- rma(yi = yi,
                     vi = vi,
                     data = dtC)


print(list("Corrected & Converted overall effect size",conv_corr_rma ))



