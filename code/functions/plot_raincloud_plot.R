
#' `plot_raincloud_plot()` creates a raincloud plot of effect sizes. Adapted from Allen et al, (2021) and
#' Scherer (2021).
#' 
#' Allen M, Poggiali D, Whitaker K et al. Raincloud plots: a multi-platform tool 
#' for robust data visualization [version 2; peer review: 2 approved]. Wellcome 
#' Open Res 2021, 4:63 (https://doi.org/10.12688/wellcomeopenres.15191.2)
#' Cédric Scherer (2021). Visualizing distributions with raincloud plots (and how to create them with ggplot2)
#' https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
#' 
#' @param dat data frame containing effect sizes
#' @returns a ggplot object
#' @examples
#' plot_raincloud_plot(dat = read_csv("data/summary/risk/effect_sizes_risk.csv",col_types = cols()))


# library(tidyverse)  # for plotting



plot_raincloud_plot <- function(dat) {
  
  
  # choosing color palette
  col_pal <- case_when(sum(grepl("risk", unique(dat$pref))) == 1 ~rev(c("#2AB7CA","#4D5382", "#D5573B")),
                       sum(grepl("risk", unique(dat$pref))) == 3 ~c("#4D5382","#4D5382", "#4D5382"))
  
  
  dat$pref <- str_to_title(dat$pref)
  
  
  if (sum(grepl("risk", tolower(unique(dat$pref)))) == 1) {
    
    risk_lab <- unique(dat$pref)[grepl("risk", tolower(unique(dat$pref)))]
    time_lab <- unique(dat$pref)[grepl("time", tolower(unique(dat$pref)))]
    social_lab <- unique(dat$pref)[grepl("social", tolower(unique(dat$pref)))]
    
    dat$pref <- factor(dat$pref, levels= rev(c(risk_lab,time_lab,social_lab)))
    
  }
  
  
  
  p <- dat %>% 
    ggplot(aes(y = pref, x = val, color = pref, fill = pref)) + 
    ggdist::stat_halfeye(
      adjust = .5,
      shape = 18,
      alpha = .5,
      width = .6,
      .width = 0,
      justification = -.1,
      point_colour = NA) +
    geom_jitter(
      shape = 1,
      height = 0.1,
      width = 0,
      stroke = 0.2,
      size = 1.5,
      alpha = 1
    ) +
    geom_boxplot(
      width = .15,
      alpha = .25,
      size = 0.6,
      outlier.shape = NA
    ) +
    theme_void() +
    labs(y = "", x = "effect size (cor)") +
    scale_color_manual(values = col_pal) +
    scale_fill_manual(values = col_pal) +
    scale_y_discrete(expand = c(0,0)) +
    theme(axis.text.x  = element_text(family = "Arial", size = 9, colour = "grey20", margin = margin(t = 10, b = 10)),
          plot.caption  = element_text(family = "Arial", size = 8, colour = "grey20", margin = margin(t = 1, b = 1, r = 5)),
          axis.text.y = element_text(family = "Arial", face = "bold", size = 10, margin = margin(l = 10)),
          axis.title.x = element_text(family = "Arial",face = "bold", size = 8,colour = "grey20",margin = margin(b = 10)),
          # panel.grid.major.x  = element_line(color = "grey90", size = 0.5, linetype = "solid"),
          # panel.grid.minor.x  = element_line(color = "grey90", size = 0.5, linetype = "solid"),
          axis.ticks.length = unit(0, "cm"),
          panel.grid = element_line(color = "grey80", size = .25),
          legend.position = "none") 
  

  
  return(p)
}


