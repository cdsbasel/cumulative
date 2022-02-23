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


dat <- ef_data %>% filter(domain == "updating")
dat <- dat %>% filter(var.g < 3) # extremely high variance for some of the ESs (related to RT), seem to be on a different scale

dat <- escalc(yi = g, vi = var.g, data = dat) 
# dat <- aggregate.escalc(dat, cluster = study, struct = "CS", rho = .7)

dat <- summary(dat)

dat <- dat %>% 
  group_by(study) %>% 
  mutate(study_lab = paste0(study, "(", LETTERS[1:n()], ")")) %>% 
  ungroup() %>% 
  arrange(year,study_lab) %>%
  mutate(row_num=1:n())

ma_res <- read_rds(file = "output/ma_ef_updating.rds")

dat <- bind_rows(dat,
                   data.frame(study_lab = "Overall", yi = ma_res$beta[1], ci.ub = ma_res$ci.ub, ci.lb = ma_res$ci.lb, row_num = nrow(dat)+1))


p <- dat %>% 
  ggplot(aes(y= reorder(study_lab,-row_num), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.25) +
  geom_point(shape = c(rep(21,482),18), size = c(rep(0.7,482),2.5), color = "grey15", stroke = 0.35, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = c("Barlow ExtraBold",rep("Barlow Medium", 482)),
                                   size=c(6, rep(3.5, 482)))) +
  coord_cartesian(xlim = c(-5, 10), clip = "off") +
  geom_text(size = 2, aes(
    x = ma_res$beta[1] - 2.5,
    y = "Overall"),
    family = "Barlow Bold", color = "grey15", 
    label = paste0(as.character(round(ma_res$beta[1],2)),"   [", as.character(round(ma_res$ci.lb,2)), ", ", as.character(round(ma_res$ci.ub,2)),"]"))
p


ggsave(filename = "figures/ma_ef_updating.png", plot = p, width = 10, height = 60, units = "cm", dpi = 600)


p <- dat %>% 
  ggplot(aes(y= reorder(study_lab,-row_num), x= yi, xmin=ci.lb, xmax=ci.ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.25) +
  geom_point(shape = c(rep(21,482),18), size = c(rep(0.7,482),2.5), color = "grey30", stroke = 0.35, fill = "grey30")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_blank()) +
  coord_cartesian(xlim = c(-5, 10), clip = "off") +
  geom_text(size = 2, aes(
    x = ma_res$beta[1] - 2.5,
    y = "Overall"),
    family = "Barlow Bold", color = "grey15", 
    label = paste0(as.character(round(ma_res$beta[1],2)),"   [", as.character(round(ma_res$ci.lb,2)), ", ", as.character(round(ma_res$ci.ub,2)),"]"))
p

ggsave(filename = "figures/ma_ef_updating_simple.png", plot = p, width = 10, height = 20, units = "cm", dpi = 600)

