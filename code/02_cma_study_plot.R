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


# CMA BY STUDY : TIME  --------------------------------------------------------------

cma_t <- read_rds(file = "output/cma_study_time.rds")
cma_t <- as.data.frame(cma_t)
cma_t <- cma_t %>% mutate(study = rownames(cma_t),
                          order_ = 1:n(),
                          study = case_when(order_ != 1 ~ paste0("+ ", study),
                                            TRUE ~ study))


#setting up the basic plot
p <- cma_t %>% 
  ggplot(aes(y= reorder(study, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = 21, size = 2, color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "cor") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = "Barlow Medium",
                                   size= 6)) +
  scale_x_continuous(limits = c(-1.5, 1), breaks = seq(-1.5,1,by = 0.5))
p


ggsave(filename = "figures/cma_study_time.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)




# CMA BY STUDY : ALTRUISM  --------------------------------------------------------------

cma_a <- read_rds(file = "output/cma_study_altrusim.rds")
cma_a <- as.data.frame(cma_a)
cma_a <- cma_a %>% mutate(study = rownames(cma_a),
                          order_ = 1:n(),
                          study = case_when(order_ != 1 ~ paste0("+ ", study),
                                            TRUE ~ study))


#setting up the basic plot
p <- cma_a %>% 
  ggplot(aes(y= reorder(study, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = 21, size = 3, color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = "Barlow Medium",
                                   size= 10)) +
  scale_x_continuous(limits = c(-1, 3), breaks = seq(-1,3,by = 1))
  p


ggsave(filename = "figures/cma_study_altruism.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)


# CMA BY STUDY : RISK  GAINS  --------------------------------------------------------------

cma_r <- read_rds(file = "output/cma_study_risk_g.rds")
cma_r <- as.data.frame(cma_r)
cma_r <- cma_r %>% mutate(study = rownames(cma_r),
                          order_ = 1:n(),
                          study = case_when(order_ != 1 ~ paste0("+ ", study),
                                            TRUE ~ study))


#setting up the basic plot
p <- cma_r %>% 
  ggplot(aes(y= reorder(study, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = 21, size = 3, color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = "Barlow Medium",
                                   size= 9)) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2,2,by = 1))
p


ggsave(filename = "figures/cma_study_risk_g.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)



# CMA BY STUDY : RISK  LOSS  --------------------------------------------------------------

cma_r <- read_rds(file = "output/cma_study_risk_l.rds")
cma_r <- as.data.frame(cma_r)
cma_r <- cma_r %>% mutate(study = rownames(cma_r),
                          order_ = 1:n(),
                          study = case_when(order_ != 1 ~ paste0("+ ", study),
                                            TRUE ~ study))


#setting up the basic plot
p <- cma_r %>% 
  ggplot(aes(y= reorder(study, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height=0, color = "grey15", size = 0.75) +
  geom_point(shape = 21, size = 3, color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = "Barlow Medium",
                                   size= 9)) +
  scale_x_continuous(limits = c(-1.5, 2.5), breaks = seq(-1,2,by = 1)) 

p


ggsave(filename = "figures/cma_study_risk_l.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)

