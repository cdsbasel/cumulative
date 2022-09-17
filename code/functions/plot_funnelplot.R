
#' `plot_funnelplot()` creates a plot showing the link between inverse sample size and effect size, . 
#' Adapted from code in https://stats.stackexchange.com/a/195333
#' @param m rma.mv metafor object. Fitted multi-level meta-analytic model
#' @returns a ggplot object
#' @examples
#' plot_funnelplot(m = read_rds("output/risk/mlma_risk.rds"), ref = 0)


library(tidyverse)  # for plotting
library(patchwork) # for combining plots

plot_funnelplot <- function(m) {
  
  dat <- m$data 
  

 # plotting other plot with inverse sample size and effect size 
 p <- ggplot() +
    labs(y = 'Inverse Sample Size', x = 'Effect Size', fill = "", linetype = "")+
    # add effect size estimates
    geom_point(data = dat, aes(y = (1/n_incl_es), x = yi), shape = 21, fill = "white", color = "grey20", stroke = .5, size = 1.5, alpha = 1) +
     # pooled estimate lines
    scale_y_reverse()+
    geom_vline(aes(xintercept = m$b[1]), linetype = 'dashed', size = .25) +
    theme_minimal() +
    theme(text = element_text(family = "Arial", size = 9),
          title = element_text(family = "Arial", face = "bold", size = 9),
          legend.margin = margin(-0.75,0,0,0, unit="cm"),
          # panel.background = element_rect(fill = "grey95"),
          panel.border  = element_rect(fill = NA, color = "black")) +
    guides(fill = guide_legend(override.aes = list(color = "black", size = .25)))
  

 
 
  return(p)
}

