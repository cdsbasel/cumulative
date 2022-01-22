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
         study = paste0(study, " (", year, ")"),
         year = as.numeric(year))


# calculating corresponding sampling variances
# vtype = "AV" leads to comparable results, even "narrower" CIs and lower ESs
cma_t <- escalc(measure = "COR", ri = g, ni = n, data = cma_t, vtype = "LS") 

cma_t <- summary(cma_t)
cma_t <- cma_t %>% arrange(yi) %>% mutate(order_ = 1:n())

ma_t <- read_rds(file = "output/ma_time.rds")



cma_t <- bind_rows(cma_t,
                   data.frame(study = "Overall", yi = ma_t$beta[1], ci.ub = ma_t$ci.ub, ci.lb = ma_t$ci.lb, order_ = nrow(cma_t)+1))

#setting up the basic plot
p <- cma_t %>% 
  ggplot(aes(y= reorder(study,-order_), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80", size = 0.5) +
  geom_point(shape = c(rep(15,41),18), size = c(rep(1,41),3.5), color = "grey15")+ 
  geom_errorbarh(height=.2, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "cor") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15", face=c("bold",rep("plain", 41)),
                                   size=c(10, rep(6, 41))))
p


ggsave(filename = "figures/ma_time.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)

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
  # calculating se since not given in the MAs 
  # (https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d)
  mutate(se = sqrt((n_young + n_old)/(n_young*n_old)) + ((g_val^2)/(2*(n_young + n_old)))) %>% 
  select(study, g_val, g_type, year, se, n) %>% 
  # removing duplicates
  distinct(study, g_type, n,year, .keep_all = T) %>% 
  filter(study != "Weller et al.") # not sure about this one....



# no need to aggregate ESs (1 per (sub)study)
cma_r <- escalc(yi = g_val, sei = se, data = cma_r)

# separating ESs into gains and losses
cma_r_loss <- cma_r %>% filter(g_type == "g_loss_dom")
cma_r_gain <- cma_r %>% filter(g_type == "g_gain_dom")

## aggregate by year (assuming independent samples...)
cma_r_loss <- aggregate.escalc(cma_r_loss, cluster=year, struct="ID")

## aggregate by year (assuming independent samples...)
cma_r_gain <- aggregate.escalc(cma_r_gain, cluster=year, struct="ID")



rma_model_g <-  rma(yi = yi,
                    vi = vi,
                    data = cma_r_gain, 
                    slab = as.character(year))


rma_model_l <-  rma(yi = yi,
                    vi = vi,
                    data = cma_r_loss, 
                    slab = as.character(year))



### cumulative meta-analysis (in the order of publication year)
risk_y_g <- cumul(rma_model_g, order = cma_r_gain$year)
write_rds(risk_y_g, file = "output/cma_year_risk_g.rds")

risk_y_l <- cumul(rma_model_l, order = cma_r_loss$year)
write_rds(risk_y_l, file = "output/cma_year_risk_l.rds")

### cumulative forest plot
png("figures/cma_risk_l_year_def.png", height = 30, width = 20, units = "cm", res = 600)
forest(risk_y_l, cex=0.75, header="Year of Publication")
dev.off()

png("figures/cma_risk_g_year_def.png", height = 30, width = 20, units = "cm", res = 600)
forest(risk_y_g, cex=0.75, header="Year of Publication")
dev.off()


# CMA BY STUDY : ALTRUISM --------------------------------------------------------------

# select studies with task of interest (i.e., lab based financial/monetary tasks; no real-world donations) 
cma_a <- cma_data %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1 & !study %in% c("Freund: Exp 4", "Sze")) %>% 
  mutate(year = as.numeric(year),
         study = paste0(study, " (", year, ")"),
         n = if_else(is.na(n), n_young + n_old, n))

# calculating corresponding sampling variances (i.e., se^2)
cma_a <- escalc(yi = g, sei = se, data = cma_a)

cma_a <- summary(cma_a)
cma_a <- cma_a %>% arrange(yi) %>% mutate(order_ = 1:n())

ma_a <- read_rds(file = "output/ma_altrusim.rds")



cma_a <- bind_rows(cma_a,
                   data.frame(study = "Overall", yi = ma_a$beta[1], ci.ub = ma_a$ci.ub, ci.lb = ma_a$ci.lb, order_ = nrow(cma_a)+1))

#setting up the basic plot
p <- cma_a %>% 
  ggplot(aes(y= reorder(study,-order_), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80", size = 0.5) +
  geom_point(shape = c(rep(15,9),18), size = c(rep(1.5,9),4), color = "grey15")+ 
  geom_errorbarh(height=.2, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "cor") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15", face=c("bold",rep("plain", 9)),
                                   size=c(12, rep(8, 9))))
p


ggsave(filename = "figures/ma_altruism.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)





