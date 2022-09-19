
#' `plot_pcurve()` creates a p-curve plot. Adapted from https://dmetar.protectlab.org/reference/pcurve
#' @param pcurve output of dmetar::pcurve(). Contains main results of the pcurve analysis
#' @returns a ggplot object
#' @examples
#' plot_pcurve(pcurve = read_rds("output/social/pcurve_social.rds"))


# library(tidyverse)  # for data wrangling and plotting 


plot_pcurve <- function(pcurve) {
  
  # Extract data to put into ggplot. 
  p_dat <- pcurve$PlotData 
  p_dat <- janitor::clean_names(p_dat) # clean up names
  p_dat <- pivot_longer(p_dat, cols = -p_value,
                        names_to = "curve", values_to = "percent") %>% 
    mutate(percent = round(percent, digits =1))
  
  # Generate new p_curve plot from p_curve model results
  p <-  ggplot(p_dat, aes(x = p_value,  y = percent, color = curve, linetype = curve)) + 
    geom_line(size = .75) + 
    geom_point(data = filter(p_dat,  curve == "observed_blue"),size = 2) + 
    geom_label(data = filter(p_dat,  curve == "observed_blue"), 
               aes(label = paste(percent, "%", sep = ""), y = percent + 2, x = p_value + .0025), 
               size = 3, color = "Black", family = "Arial") + 
    theme_minimal() + 
    labs(y = "Percentage of test results",
         x = "p-value") + 
    scale_color_manual(values=c('grey40', 'indianred3', "grey10"), 
                       name = "", 
                       labels = c("Curve under null of 0% power", 
                                  "Observed p-curve", 
                                  "Curve under null of 30% power")) +
    theme(legend.position = c(.90, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.key.width = unit(1.5, "cm"),
          panel.grid = element_blank(),
          axis.line = element_line(color = "grey50", size = .2),
          text = element_text(family = "Arial", size = 10),
          title = element_text(family = "Arial", size = 10, face = "bold")) +
    scale_linetype_manual(values=c("dotted", "solid", "dashed")) + 
    guides(linetype = "none", 
           color = guide_legend(override.aes = list(shape = NA, size = 1,
                                                    linetype = c("dotted", "solid", "dashed")))) +
    scale_y_continuous(limits = c(-1,100), breaks = seq(0,100,25)) + 
    scale_x_continuous(limits = c(.009,.055), breaks = seq(0.01,0.05,.01)) +
    coord_cartesian(expand = FALSE)
  
  return(p)
  
  
}
