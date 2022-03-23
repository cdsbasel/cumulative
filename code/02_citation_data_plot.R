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



# READ DATA ---------------------------------------------------------------
col_specs <- cols(
  pref = col_character(),
  study = col_character(),
  year_pub = col_character(),
  g = col_double(),
  cite_p_year = col_double(),
  n_yr = col_double()
)



es_cit_dat <- read_csv(file = "data/es_cit_dat.csv", col_types = col_specs)

es_cit_dat <- es_cit_dat %>% 
  filter(!is.na(cite_p_year)) %>% 
  mutate(g = abs(g))

# PLOTTING ----------------------------------------------------------------


es_cit_dat %>% 
  ggplot(aes(x = n_yr, y = cite_p_year, size = g, color = pref)) +
  geom_point(alpha = .75) +
  theme_minimal() +
  labs(color = "PREFERENCE", size = "EFFECT SIZE", x = "NUM OF YEARS BEING CITED", y = "CITATIONS/YEAR",
       caption = sprintf("Num of effect sizes = %d", nrow(es_cit_dat)),
       title = toupper("Citation Rates & Effect Sizes of Studies"))+
  theme(legend.position = "top",
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(t = 10, b = 5), size = 20),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        strip.text  = element_text(size = 15, family = "Barlow Bold"),
        plot.caption = element_text(size = 10, family = "Barlow"),
        legend.text = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(hjust = 0.5, size = 11),
        title = element_text(family = "Barlow Bold"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        legend.margin = margin(b = 10, t = 5, r = 30),
        text = element_text(family = "Barlow")) +
  # scale_size_continuous(range = c(.5, 5), breaks = c(0.2, 0.5, 0.8, 1, 1.5), labels = c("0.2", "0.5", "0.8", "1.0", "1.5"))+
  scale_color_manual(values = c("#2a9d8f", "#e76f51")) +
  guides(size = guide_legend(label.position = "top", title.position = "bottom"),
         color = guide_legend(label.position = "top", title.position = "bottom", override.aes = list(size = 4))) #guide_legend(label.position = "bottom", title.position = "bottom", override.aes = list(size = 4, alpha = 0.9, stroke = 0)))


es_cit_dat %>% 
  ggplot(aes(x = cite_p_year, y = g, color = pref)) +
  geom_point(alpha = .75, size = 3) +
  theme_minimal() +
  labs(color = "PREFERENCE", y = "EFFECT SIZE", x = "CITATIONS/YEAR",
       caption = sprintf("Num of effect sizes = %d", nrow(es_cit_dat)),
       title = toupper("Citation Rates & Effect Sizes of Studies"))+
  theme(legend.position = "top",
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(t = 10, b = 5), size = 20),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        strip.text  = element_text(size = 15, family = "Barlow Bold"),
        plot.caption = element_text(size = 10, family = "Barlow"),
        legend.text = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(hjust = 0.5, size = 11),
        title = element_text(family = "Barlow Bold"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        legend.margin = margin(b = 10, t = 5, r = 30),
        text = element_text(family = "Barlow")) +
  # scale_size_continuous(range = c(.5, 5), breaks = c(0.2, 0.5, 0.8, 1, 1.5), labels = c("0.2", "0.5", "0.8", "1.0", "1.5"))+
  scale_color_manual(values = c("#2a9d8f", "#e76f51")) +
  guides(size = guide_legend(label.position = "top", title.position = "bottom"),
         color = guide_legend(label.position = "top", title.position = "bottom", override.aes = list(size = 4))) #guide_legend(label.position = "bottom", title.position = "bottom", override.aes = list(size = 4, alpha = 0.9, stroke = 0)))


es_cit_dat %>% 
  ggplot(aes(x = n_yr, y = g, color = pref)) +
  geom_point(alpha = .75, size = 3) +
  theme_minimal() +
  labs(color = "PREFERENCE", y = "EFFECT SIZE", x = "NUM OF YEARS BEING CITED",
       caption = sprintf("Num of effect sizes = %d", nrow(es_cit_dat)),
       title = toupper("Citation Rates & Effect Sizes of Studies"))+
  theme(legend.position = "top",
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(t = 10, b = 5), size = 20),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        strip.text  = element_text(size = 15, family = "Barlow Bold"),
        plot.caption = element_text(size = 10, family = "Barlow"),
        legend.text = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(hjust = 0.5, size = 11),
        title = element_text(family = "Barlow Bold"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        legend.margin = margin(b = 10, t = 5, r = 30),
        text = element_text(family = "Barlow")) +
  # scale_size_continuous(range = c(.5, 5), breaks = c(0.2, 0.5, 0.8, 1, 1.5), labels = c("0.2", "0.5", "0.8", "1.0", "1.5"))+
  scale_color_manual(values = c("#2a9d8f", "#e76f51")) +
  guides(size = guide_legend(label.position = "top", title.position = "bottom"),
         color = guide_legend(label.position = "top", title.position = "bottom", override.aes = list(size = 4))) #guide_legend(label.position = "bottom", title.position = "bottom", override.aes = list(size = 4, alpha = 0.9, stroke = 0)))


