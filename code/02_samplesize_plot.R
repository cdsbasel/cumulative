
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


# DATA  --------------------------------------------------------------

cma_t <- cma_data %>% filter(pref == "time") %>% 
  # adding date for unpublished results 
  mutate(year = case_when(year == "unpublished" & study == "Lempert" ~ "2018",
                          year == "unpublished" & study == "Sisso" ~ "2017",
                          year == "unpublished" & study == "Li (study 2)" ~ "2020",
                          TRUE ~ year),
         pref == " time ",
         es = metafor::transf.rtoz(g),
         year = as.numeric(year)) %>% 
  select(pref, n, year, es) 


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
         pref = " risk ",
         es = g_val,
         study = paste0(study,", ",year),
         year = as.numeric(year)) %>% 
  select(pref, n, year, es) 



# select studies with task of interest (i.e., lab based financial/monetary tasks; no real-world donations) 
cma_a <- cma_data %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1 & !study %in% c("Freund: Exp 4", "Sze")) %>% 
  mutate(year = as.numeric(year),
         es = g,
         n = if_else(is.na(n), n_young + n_old, n)) %>% 
  select(pref, n, year, es) 


dat <- bind_rows(cma_a, cma_t, cma_r) %>% mutate(es = abs(es)) 



p <- dat %>% ggplot(aes(x = as.character(year), y = n, color = toupper(pref), fill = toupper(pref), size = es)) + 
  theme_minimal() +
  geom_jitter(shape = 21,stroke = 1, height = 0, width = 0.5) + 
  scale_colour_manual(values = c("#3C9AB2", "#F22300", "#E1AF00"))+
  scale_fill_manual(values =alpha(c("#3C9AB2","#F22300", "#E1AF00"),0.4)) +
  scale_y_log10() + 
  annotation_logticks(sides = "l") +
  facet_grid(.~toupper(pref)) +
  # scale_x_continuous() +
  scale_size_continuous(range = c(0.05, 8), breaks = c(0.2, 0.5, 0.8), labels = c("0.2", "0.5", "0.8")) +
  labs(fill = "PREFERENCE", size = "EFFECT SIZE", x = "YEAR", y = "SAMPLE SIZE    (log10)",
       caption = sprintf("Num of effect sizes = %d", nrow(dat)),
       title = toupper("Sample Sizes & Effect Sizes of Studies by Year of Publication"))+
  theme(legend.position = "top",
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(t = 10, b = 5), size = 20),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 11, angle = 45),
        strip.text  = element_text(size = 15, family = "Barlow Bold"),
        plot.caption = element_text(size = 10, family = "Barlow"),
        legend.text = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(hjust = 0.5, size = 11),
        title = element_text(family = "Barlow Bold"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        legend.margin = margin(b = 10, t = 5, r = 30),
        text = element_text(family = "Barlow")) +
  guides(color = FALSE,
          size = guide_legend(label.position = "bottom", title.position = "left"),
          fill = FALSE) #guide_legend(label.position = "bottom", title.position = "bottom", override.aes = list(size = 4, alpha = 0.9, stroke = 0)))

p

ggsave(filename = "figures/sample_size.png", plot = p, width = 34, height = 19, units = "cm", dpi = 600)

