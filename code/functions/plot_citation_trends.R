
#library(tidyverse)

plot_citation_trends <- function(data) {


dat <- data[complete.cases(data),]


### DATA formatting
es_cit_dat <- dat %>% 
  filter(no_link == FALSE)  %>% 
  mutate(year_in_print = year-year_of_publication) %>%  # cited before publication
  group_by(first_author, year_of_pub_chr,year_of_publication, preference,year_in_print,yi, n_incl_es) %>% 
  summarise(cit_count = sum(cit_count)) %>% 
  mutate(preference = str_to_title(preference),
         study_label = paste0(first_author, " (", year_of_pub_chr, ")")) %>% 
  ungroup() %>% 
  group_by(study_label) %>% 
  # some papers have been cited as a pre-print/working paper
  mutate(corrected_year_in_print = year_in_print - min(year_in_print),
         corrected_year_of_publication = year_of_publication - corrected_year_in_print)

# overall mean/median yearly citations for each publication
es_cit_dat_pub <- es_cit_dat %>% 
  group_by(preference, study_label, yi, n_incl_es) %>% 
  summarise(cit_median = median(cit_count),
            cit_mean = mean(cit_count)) %>% 
  mutate(n_incl_es_log = log10(n_incl_es),
         cit_median_log = log10(cit_median))



# mean/median citations by year for each preference
es_cit_dat_pref_yr <- es_cit_dat %>% 
  group_by(preference,corrected_year_in_print) %>% 
  summarise(cit_count = median(cit_count))

# overall mean/median yearly citations  for each preference
es_cit_dat_pref <- es_cit_dat %>% 
  group_by(preference) %>% 
  summarise(cit_median = median(cit_count),
            cit_mean = mean(cit_count))

label_plots <- tibble(preference = c("Risk", "Time", "Social", "Effort"),
                      y = es_cit_dat_pref$cit_mean[es_cit_dat_pref$preference == "Effort"] + 3,
                      x = 29.5,
                      label = c("","","", "Mean")) %>% 
  bind_rows(tibble(preference = c("Risk", "Time", "Social", "Effort"),
                   y = es_cit_dat_pref$cit_median[es_cit_dat_pref$preference == "Effort"] - 3,
                   x = 29.5,
                   label = c("","","", "Median")))

es_cit_dat$preference <- factor(es_cit_dat$preference, levels = c("Risk", "Time", "Social", "Effort"))
es_cit_dat_pref_yr$preference <- factor(es_cit_dat_pref_yr$preference, levels = c("Risk", "Time", "Social", "Effort"))
es_cit_dat_pref$preference <- factor(es_cit_dat_pref$preference, levels = c("Risk", "Time", "Social", "Effort"))
label_plots$preference <- factor(label_plots$preference, levels = c("Risk", "Time", "Social", "Effort"))
es_cit_dat_pub$preference <- factor(es_cit_dat_pub$preference, levels = c("Risk", "Time", "Social", "Effort"))

# yearly citations over time
pA <- ggplot(data = es_cit_dat, aes(x = corrected_year_in_print, y = (1+cit_count), color = preference)) +
  geom_hline(data = es_cit_dat_pref, aes(yintercept = cit_mean), color = "grey40", linetype = "dashed", size = 1) +
  geom_hline(data = es_cit_dat_pref, aes(yintercept = cit_median), color = "grey70", linetype = "dashed", size = 1) +
  geom_point(size = 1, alpha = .25, color = "grey50") + facet_grid(.~preference) +
  geom_point(data = es_cit_dat_pref_yr, aes(x = corrected_year_in_print, y = (1+cit_count), color = preference), shape = "-", size = .01, stroke = 14) +
  geom_text(data = label_plots, aes(x = x, y = y, label = label), size = 3,hjust = 1, family = "Arial", fontface = "italic", color = rep(c("grey40","grey70"), each = 4)) +
  scale_y_log10(expand = c(0.01,0.01), limits = c(1,110)) +theme_minimal() + 
  scale_x_continuous(breaks = seq(0,30,10), expand = c(0.01,0.01))+
  coord_cartesian(xlim = c(0,30)) +
  labs(y = "Yearly Citations (log10)",
       x = "Number of Years in Print",
       # caption = sprintf("Data based on %d publications", 
       #                   nrow(es_cit_dat_pub)),
       tag = "A")+
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.tag.position = c(-.01,1),
        plot.tag = element_text(size = 15, family = "Arial", face = "bold"),
        plot.title =  element_blank(),
        axis.title.y = element_text(size = 9, margin = margin(r = 1)),
        axis.title.x = element_text(size = 9, margin = margin(t = 2 )),
        axis.text.x = element_text(size = 9),
        strip.text  = element_text(size = 9, family = "Arial", face = "bold"),
        panel.spacing.x = unit(.5, "cm"),
        panel.grid.minor.x = element_blank(),
        plot.caption =  element_blank(),
        legend.text = element_text(hjust = 0.5, size = 9),
        legend.title = element_text(hjust = 0.5, size = 9),
        panel.background = element_rect(fill = NA, color = "grey50", size = .7),
        title = element_text(family = "Arial"),
        legend.spacing.x = unit(0.5, "cm"),
        plot.margin = margin(t = 0, r = 0, l = 10, b = 20),
        legend.spacing.y = unit(0.2, "cm"),
        legend.margin = margin(b = 10, t = 2 , r = 30),
        text = element_text(family = "Arial")) +
  scale_color_manual(values = c("#2AB7CA","#4D5382", "#D5573B", "#9F7131")) 



# effect size by cit rate
pB <- es_cit_dat_pub %>% 
  ggplot(aes(y = cit_median, x = yi, color = preference)) +
  geom_smooth(method = "lm",   fullrange = TRUE, formula = "y ~ 1 + x") +
  # geom_line(data = df_pred_yi, aes(y = pred, x = yi, color = preference))+
  # geom_vline(xintercept = 0, size = .8, color = "grey30", linetype = "dashed") +
  geom_point(alpha = .75, size = 2) +
  theme_minimal() +
  facet_grid(.~preference, scales = "free_x") +
  # scale_x_continuous(limits = c(-.75,.75), expand = c(0,0), breaks = seq(-.75,.75,.25)) +
  scale_y_log10(expand = c(0.01,0.01)) +
  scale_x_continuous(expand = c(0.01,0.01)) +
  coord_cartesian(ylim  = c(1,80))  +
  # scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,.5)) +
  labs(color = "PREFERENCE", x = "Aggregated Effect Size", y = "Median Yearly Citation (log10)",
       # caption = sprintf("Data based on %d publications",nrow(es_cit_dat_pub)),
       tag = "B")+
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.tag.position = c(-.01,1.05),
        plot.tag = element_text(size = 15, family = "Arial", face = "bold"),
        plot.title =  element_blank(),
        axis.title.y = element_text(size = 9, margin = margin(r = 1)),
        axis.title.x = element_text(size = 9, margin = margin(t = 2 )),
        axis.text.x = element_text(size = 9),
        strip.text  = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(.5, "cm"),
        plot.caption = element_blank(),
        legend.text = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(hjust = 0.5, size = 11),
        panel.background = element_rect(fill = NA, color = "grey50", size = .7),
        title = element_text(family = "Arial"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        plot.margin = margin(t = 0, r = 0, l = 10, b = 20),
        legend.margin = margin(b = 10, t = 2 , r = 30),
        text = element_text(family = "Arial"))+
  scale_color_manual(values = c("#2AB7CA","#4D5382", "#D5573B", "#9F7131")) 






# sample size by cit rate
pC <- es_cit_dat_pub %>% 
  ggplot(aes(y = cit_median, x = n_incl_es, color = preference)) +
  geom_smooth(method = "lm",   fullrange = TRUE, formula = "y ~ 1 + x") +
  geom_point(alpha = .75, size = 2) +
  theme_minimal() +
  facet_grid(.~preference, scales = "free_x") +
  scale_x_log10(expand = c(0.01,0.01)) +
  # scale_x_log10(breaks = c(10,100,1000, 10000), limits = c(10,50000),expand = c(0.01,0.01)) +
  scale_y_log10(expand = c(0.01,0.01)) +
  coord_cartesian(ylim  = c(1,80))  +
  # scale_y_continuous(limits = c(-1,1), breaks = seq(-1,1,.5)) +
  labs(color = "PREFERENCE", x = "Sample Size (log10)", y = "Median Yearly Citation (log10)",
       # caption = sprintf("Data based on %d publications", nrow(es_cit_dat_pub)),
       tag = "C")+
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.tag.position = c(-0.01,1.05),
        panel.spacing.x = unit(.5, "cm"),
        plot.tag = element_text(size = 15, family = "Arial", face = "bold"),
        plot.title =  element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 9, margin = margin(r = 1)),
        axis.title.x = element_text(size = 9, margin = margin(t = 2 )),
        axis.text.x = element_text(size = 9),
        strip.text  = element_blank(),
        plot.caption = element_text(size = 10, family = "Arial"),
        legend.text = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(hjust = 0.5, size = 11),
        panel.background = element_rect(fill = NA, color = "grey50", size = .7),
        title = element_text(family = "Arial"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm"),
        plot.margin = margin(b = 0, r = 0, l = 10, t = 0),
        legend.margin = margin(b = 10, t = 2 , r = 30),
        text = element_text(family = "Arial"))+
  scale_color_manual(values = c("#2AB7CA","#4D5382", "#D5573B", "#9F7131")) 


p <- pA/pB/pC

return(p)

}



