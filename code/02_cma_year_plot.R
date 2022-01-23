# DESCRIPTION -------------------------------------------------------------

# starting to analyse smthg 


# TO DO -------------------------------------------------------------------

# Need to sort "risk":
# eliminate duplicate studies between Mata and Best.
# also make sure how certain studies are designed when aggregating the effect sizes 
# sort the gain/loss and pos/neg domain effec sizes
# update year list


# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(metafor)


# CMA BY year : TIME  --------------------------------------------------------------

cma_t <- read_rds(file = "output/cma_year_time.rds")
cma_t <- as.data.frame(cma_t)
cma_t <- cma_t %>% mutate(year = rownames(cma_t),
                          order_ = 1:n(),
                          year = case_when(order_ != 1 ~ paste0("+ ", year),
                                            TRUE ~ year))


#setting up the basic plot
p <- cma_t %>% 
  ggplot(aes(y= reorder(year, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_point(shape = 15, size = 2, color = "grey15")+ 
  geom_errorbarh(height=.5, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "cor") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15",
                                   size= 8)) +
  scale_x_continuous(limits = c(-1.5, 1), breaks = seq(-1.5,1,by = 0.5))
  
p


ggsave(filename = "figures/cma_year_time.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)




# CMA BY year : ALTRUISM  --------------------------------------------------------------

cma_a <- read_rds(file = "output/cma_year_altrusim.rds")
cma_a <- as.data.frame(cma_a)
cma_a <- cma_a %>% mutate(year = rownames(cma_a),
                          order_ = 1:n(),
                          year = case_when(order_ != 1 ~ paste0("+ ", year),
                                            TRUE ~ year))


#setting up the basic plot
p <- cma_a %>% 
  ggplot(aes(y= reorder(year, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_point(shape = 15, size = 3, color = "grey15")+ 
  geom_errorbarh(height=.5, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15",
                                   size= 9)) +
  scale_x_continuous(limits = c(-1, 3), breaks = seq(-1,3,by = 0.5))
  
p


ggsave(filename = "figures/cma_year_altruism.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)


# CMA BY year : RISK  GAINS  --------------------------------------------------------------

cma_r <- read_rds(file = "output/cma_year_risk_g.rds")
cma_r <- as.data.frame(cma_r)
cma_r <- cma_r %>% mutate(year = rownames(cma_r),
                          order_ = 1:n(),
                          year = case_when(order_ != 1 ~ paste0("+ ", year),
                                            TRUE ~ year))


#setting up the basic plot
p <- cma_r %>% 
  ggplot(aes(y= reorder(year, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_point(shape = 15, size = 2.5, color = "grey15")+ 
  geom_errorbarh(height=.5, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15",
                                   size= 9)) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2,2,by = 0.5)) 
p


ggsave(filename = "figures/cma_year_risk_g.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)



# CMA BY year : RISK  LOSS  --------------------------------------------------------------

cma_r <- read_rds(file = "output/cma_year_risk_l.rds")
cma_r <- as.data.frame(cma_r)
cma_r <- cma_r %>% mutate(year = rownames(cma_r),
                          order_ = 1:n(),
                          year = case_when(order_ != 1 ~ paste0("+ ", year),
                                            TRUE ~ year))


#setting up the basic plot
p <- cma_r %>% 
  ggplot(aes(y= reorder(year, -order_), x= estimate, xmin=ci.lb, xmax=ci.ub)) + 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_point(shape = 15, size = 2.5, color = "grey15")+ 
  geom_errorbarh(height=.5, color = "grey15") +
  theme_minimal() +
  labs(y = "", x = "g") +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Barlow", color = "grey15"),
        axis.text.y = element_text(color = "grey15",
                                   size= 9)) +
  scale_x_continuous(limits = c(-1.5, 2.5), breaks = seq(-1.5,2.5,by = 0.5)) 

p


ggsave(filename = "figures/cma_year_risk_l.png", plot = p, width = 10, height = 13.5, units = "cm", dpi = 600)

