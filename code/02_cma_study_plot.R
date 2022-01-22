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
                          order_ = 1:n())


#setting up the basic plot
p <- cma_t %>% 
  ggplot(aes(y= reorder(study, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80", size = 0.5) +
  geom_point(shape = 15, size = 1, color = "grey15")+ 
  geom_errorbarh(height=.2, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "cor") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15",
                                   size= 6))
p


ggsave(filename = "figures/cma_study_time.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)
