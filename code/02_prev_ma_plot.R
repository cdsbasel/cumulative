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



# DATA --------------------------------------------------------------------



ma_data <- tibble(pref = c("Risk (Gain)", "Risk (Loss)", "Time", "Atruism"),
                  order = c(1:4),
                  g = c(-.25, -.02, -.07, .61),
                  lb = c(-.33, -.1, -.17, .47),
                  ub = c(-.18, .06, .04, .75))


# PLOT  --------------------------------------------------------------

#setting up the basic plot
p <- ma_data %>% 
  ggplot(aes(y= fct_reorder(pref, -order), x= g, xmin= lb, xmax=ub))+ 
  #this adds the effect sizes to the plot
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.5) +
  geom_errorbarh(height= 0, color = "grey15", size = 0.75) +
  geom_point(shape = 21, size = 3, color = "grey15", stroke = 1, fill = "#e9edc9")+ 
  theme_minimal() +
  labs(y = "", x = "Effect Size (g)",
       caption = "Effect size estimates - Risk (Gain & Loss): Best & Charness (2015); Time: Seaman et al. (2020); Altruism: Sparrow et al., (2021)") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "grey15",
                                   family = "Barlow"),
        plot.caption = element_text(color = "grey15",
                                    family = "Barlow Italic",
                                    size = 8,
                                    margin = margin(t = 10)),
        axis.title.x = element_text(color = "grey15",
                                    family = "Barlow ExtraBold"),
        axis.text.y = element_text(color = "grey15",
                                   family = "Barlow ExtraBold",
                                   size=10,
                                   hjust = 0)) +
  scale_x_continuous(limits = c(-.5, 1), breaks = seq(-.5,1,by = 0.25))


p
ggsave(filename = "figures/prev_ma_overview.png", plot = p, width = 30, height = 8, units = "cm", dpi = 600)

