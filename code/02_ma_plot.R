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


# MA BY STUDY : TIME  --------------------------------------------------------------

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
cma_t <- cma_t %>% arrange(year) %>% mutate(order_ = 1:n())

ma_t <- read_rds(file = "output/ma_time.rds")



cma_t <- bind_rows(cma_t,
                   data.frame(study = "Overall", yi = ma_t$beta[1], ci.ub = ma_t$ci.ub, ci.lb = ma_t$ci.lb, order_ = nrow(cma_t)+1))

#setting up the basic plot
p <- cma_t %>% 
  ggplot(aes(y= reorder(study,-order_), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = c(rep(21,41),18), size = c(rep(2,41),3.5), color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "cor") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = c("Barlow ExtraBold",rep("Barlow Medium", 41)),
                                   size=c(10, rep(6, 41)))) +
  scale_x_continuous(limits = c(-1.5, 1), breaks = seq(-1.5,1,by = 0.5))+
  geom_text(size = 3, aes(
    x = ma_t$beta[1] - 0.75,
    y = "Overall"),
    family = "Barlow Bold", color = "grey15", 
    label = paste0(as.character(round(ma_t$beta[1],2)),"   [", as.character(round(ma_t$ci.lb,2)), ", ", as.character(round(ma_t$ci.ub,2)),"]"))
p


ggsave(filename = "figures/ma_time.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)

# MA BY STUDY : RISK GAINS --------------------------------------------------------------

cma_r <- cma_data %>% 
  # only take into account monetary tasks (for now just using Best and Charness)
  filter(pref == "risk" & ma_origin == "Best_Charness (2015)v2" & !task_scen %in% c("Mortality", "Variable") )  %>% #
  pivot_longer(c(g_pos_fram, g_neg_fram), names_to = "g_type", values_to = "g_val") %>% 
  filter(!is.na(g_val)) %>% 
  mutate(g_type = case_when(g_type %in% c("g_pos_fram") ~ "g_gain_dom",
                            g_type %in% c("g_neg_fram") ~ "g_loss_dom",
                            g_type %in% c("g") ~ "g_dk_dom",
                            TRUE ~ g_type),
         study = case_when(study %like% "Sprot" ~ "Sprotem et al.",
                           study %like% "Tymu" ~ "Tymula et al.",
                           study %like% "Well" ~ "Weller et al.",
                           TRUE ~ study),
         n = case_when(is.na(n) ~ n_young + n_old,
                       TRUE ~ n),
         study = paste0(study,", ",year)) %>% 
  # calculating se since not given in the MAs 
  # (https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d)
  mutate(se = sqrt((n_young + n_old)/(n_young*n_old)) + ((g_val^2)/(2*(n_young + n_old)))) %>% 
  select(study, g_val, g_type, year, task_stak, se, n) 



# no need to aggregate ESs (1 per (sub)study)
cma_r <- escalc(yi = g_val, sei = se, data = cma_r)


cma_r_gain <- cma_r %>% filter(g_type == "g_gain_dom")


ma_r_gain <- read_rds(file = "output/ma_risk_g.rds")

cma_r_gain <- summary(cma_r_gain)

cma_r_gain <- cma_r_gain %>% arrange(year) %>% mutate(order_ = 1:n())

cma_r_gain <- bind_rows(cma_r_gain,
                        data.frame(study = "Overall",
                                   yi = ma_r_gain$beta[1],
                                   ci.ub = ma_r_gain$ci.ub,
                                   ci.lb = ma_r_gain$ci.lb,
                                   order_ = nrow(cma_r_gain)+1))


#setting up the basic plot
p <-  ggplot(data = cma_r_gain, aes(y= reorder(study,-order_), x= yi, xmin= ci.lb, xmax=ci.ub)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = c(rep(21,17),18), size = c(rep(3,17),4.5), color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = c("Barlow ExtraBold",rep("Barlow Medium", 41)),
                                   size=c(11, rep(8, 17)))) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2,2,by = 1)) +
  geom_text(size = 3, aes(
    x = ma_r_gain$beta[1] + 1.3,
    y = "Overall"),
    family = "Barlow Bold", color = "grey15", 
    label = paste0(as.character(round(ma_r_gain$beta[1],2)),"   [", as.character(round(ma_r_gain$ci.lb,2)), ", ", as.character(round(ma_r_gain$ci.ub,2)),"]"))
p



ggsave(filename = "figures/ma_risk_g.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)



# MA BY STUDY : RISK LOSS --------------------------------------------------------------

cma_r <- cma_data %>% 
  # only take into account monetary tasks (for now just using Best and Charness)
  filter(pref == "risk" & ma_origin == "Best_Charness (2015)v2" & !task_scen %in% c("Mortality", "Variable") )  %>% #
  pivot_longer(c(g_pos_fram, g_neg_fram), names_to = "g_type", values_to = "g_val") %>% 
  filter(!is.na(g_val)) %>% 
  mutate(g_type = case_when(g_type %in% c("g_pos_fram") ~ "g_gain_dom",
                            g_type %in% c("g_neg_fram") ~ "g_loss_dom",
                            g_type %in% c("g") ~ "g_dk_dom",
                            TRUE ~ g_type),
         study = case_when(study %like% "Sprot" ~ "Sprotem et al.",
                           study %like% "Tymu" ~ "Tymula et al.",
                           study %like% "Well" ~ "Weller et al.",
                           TRUE ~ study),
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

ma_r_loss <- read_rds(file = "output/ma_risk_l.rds")

cma_r_loss <- summary(cma_r_loss)
cma_r_loss <- cma_r_loss %>% arrange(year) %>% mutate(order_ = 1:n())

cma_r_loss <- bind_rows(cma_r_loss,
                        data.frame(study = "Overall", yi = ma_r_loss$beta[1], ci.ub = ma_r_loss$ci.ub, ci.lb = ma_r_loss$ci.lb, order_ = nrow(cma_r_loss)+1))

#setting up the basic plot
p <- cma_r_loss %>% 
  ggplot(aes(y= reorder(study,-order_), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = c(rep(21,14),18), size = c(rep(3,14),4.5), color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  scale_x_continuous(limits = c(-1.5, 2.5), breaks = seq(-1,2,by = 1))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = c("Barlow ExtraBold",rep("Barlow Medium", 41)),
                                   size=c(11, rep(8, 14)))) +
  geom_text(size = 3.5, aes(
    x = ma_r_loss$beta[1] + 1.25,
    y = "Overall"),
    family = "Barlow Bold", color = "grey15", 
    label = paste0(as.character(round(ma_r_loss$beta[1],2)),"   [", as.character(round(ma_r_loss$ci.lb,2)), ", ", as.character(round(ma_r_loss$ci.ub,2)),"]"))
p


ggsave(filename = "figures/ma_risk_l.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)


# MA BY STUDY : ALTRUISM --------------------------------------------------------------

# select studies with task of interest (i.e., lab based financial/monetary tasks; no real-world donations) 
cma_a <- cma_data %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1 & !study %in% c("Freund: Exp 4", "Sze")) %>% 
  mutate(year = as.numeric(year),
         study = paste0(study, " (", year, ")"),
         n = if_else(is.na(n), n_young + n_old, n))

# calculating corresponding sampling variances (i.e., se^2)
cma_a <- escalc(yi = g, sei = se, data = cma_a)

cma_a <- summary(cma_a)
cma_a <- cma_a %>% arrange(year) %>% mutate(order_ = 1:n())

ma_a <- read_rds(file = "output/ma_altrusim.rds")



cma_a <- bind_rows(cma_a,
                   data.frame(study = "Overall", yi = ma_a$beta[1], ci.ub = ma_a$ci.ub, ci.lb = ma_a$ci.lb, order_ = nrow(cma_a)+1))

#setting up the basic plot
p <- cma_a %>% 
  ggplot(aes(y= reorder(study,-order_), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = c(rep(21,9),18), size = c(rep(3,9),4.5), color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = c("Barlow ExtraBold",rep("Barlow Medium", 41)),
                                   size=c(12, rep(9, 9)))) +
  scale_x_continuous(limits = c(-1, 3), breaks = seq(-1,3,by = 1))+
  geom_text(size = 3.5, aes(
    x = ma_a$beta[1] + 1.25,
    y = "Overall"),
    family = "Barlow Bold", color = "grey15", 
    label = paste0(as.character(round(ma_a$beta[1],2)),"   [", as.character(round(ma_a$ci.lb,2)), ", ", as.character(round(ma_a$ci.ub,2)),"]"))
p


ggsave(filename = "figures/ma_altruism.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)





