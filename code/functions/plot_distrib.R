#' `plot_distrib()` creates a plot to show how the values of a certain variable are distributed.
#' Plots the raw value, with the median and the 66% and 95% CIs.
#' 
#' @param dat data frame containing values to plot
#' @returns a ggplot object
#' @examples
#' plot_distrib(dat = dt)


# library(tidyverse)  # for plotting


plot_distrib <- function(dat) {
  # choosing color palette
  if (sum(grepl("risk", tolower(unique(dat$pref)))) == 1) {
    col_pal <- rev(c("#2AB7CA","#4D5382", "#D5573B", "#9F7131"))
    
  } else {
    col_pal <-c("#2AB7CA","#2AB7CA", "#2AB7CA")}
  
  
  dat$pref <- str_to_title(dat$pref)
  
  dat_sum <- dat %>%
    group_by(pref) %>% 
    summarise(m = median(val),
              ul95 = quantile(val,.975),
              ll95 = quantile(val,.025),
              ul66 = quantile(val,.83),
              ll66 = quantile(val,.17))
  
  
  
  if (sum(grepl("risk", tolower(unique(dat$pref)))) == 1) {
    
    risk_lab <- unique(dat$pref)[grepl("risk", tolower(unique(dat$pref)))]
    time_lab <- unique(dat$pref)[grepl("time", tolower(unique(dat$pref)))]
    social_lab <- unique(dat$pref)[grepl("social", tolower(unique(dat$pref)))]
    effort_lab <- unique(dat$pref)[grepl("effort", tolower(unique(dat$pref)))]
    
    dat_sum$pref <- factor(dat_sum$pref, levels= (c(risk_lab,time_lab,social_lab,effort_lab)))
    
    dat$pref <- factor(dat$pref, levels= (c(risk_lab,time_lab,social_lab,effort_lab)))
    
  }
  
  

  
  
  p <- ggplot() + 
    geom_point(data = dat, aes(y = pref, x = val),color = "grey70",
               alpha = .35, shape="\U2014", size = 6) +
    geom_errorbarh(data = dat_sum, aes(xmin = ll95, xmax = ul95, y = pref, color = pref), height = 0, size = .5) +
    geom_errorbarh(data = dat_sum, aes(xmin = ll66, xmax = ul66, y = pref, color = pref), height = 0, size = 1) +
    geom_point(data = dat_sum, aes(y = pref, x = m, color = pref),size = 2) +
    theme_void() +
    coord_flip() +
    # scale_y_discrete(expand = c(.075,.075))+
    labs(y = "", x = "") +
    scale_color_manual(values = col_pal) +
    scale_fill_manual(values = col_pal) +
    theme(axis.text.x  = element_text(family = "Arial", size = 9, face = "bold", colour = "grey20", margin = margin(t = 10, b = 10)),
          plot.caption  = element_text(family = "Arial", size = 8, colour = "grey20", margin = margin(t = 1, b = 1, r = 5)),
          axis.title.y = element_text(family = "Arial", size = 9, face = "bold", colour = "grey20", margin = margin(l = 10), angle = 90),
          axis.text.y = element_text(family = "Arial", size = 8,colour = "grey20",margin = margin(r = 5)),
          # panel.grid.major.x  = element_line(color = "grey90", size = 0.5, linetype = "solid"),
          # panel.grid.minor.x  = element_line(color = "grey90", size = 0.5, linetype = "solid"),
          axis.ticks.length = unit(0, "cm"),
          panel.background = element_rect(fill = NA, color = "grey50", size = .5),
          panel.grid.minor = element_blank(),
          plot.margin = margin(0),
          panel.grid.major = element_line(color = "grey80", size = .25),
          legend.position = "none") 
  
  
  return(p)
  
  
}
