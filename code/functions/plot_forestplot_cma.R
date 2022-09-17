
#' `plot_forestplot_cma()` creates a forest plot of the cumulative meta-analytic estimates (study level)
#' @param m a data frame with summary information of the pooled estimates
#' @returns a ggplot object
#' @examples
#' plot_forestplot_cma(m = read_rds("output/risk/cma_study_risk.rds"))


# PACKAGES ----------------------------------------------------------------


library(tidyverse)
library(metafor)
library(data.table)


# READ DATA ---------------------------------------------------------------

plot_forestplot_cma <- function(m) {
  
  preference <- unique(m$preference)
  ## colors
  fill_col <- case_when(preference == "risk" ~ "#2AB7CA",
                        preference == "social" ~ "#D5573B",
                        preference == "time" ~ "#4D5382")
  
  
  
  ## data
  # plot data
  dat <- m %>% 
    mutate(y = -(1:n())) 
  
  
  ## create text to add as labels on plot
  
  # study names
  study_text <-  dat %>% 
    mutate(label = slab) %>% 
    select(y, label) %>%
    mutate(x = -2)
  
  # estimate values
  estim_text <- dat %>%
    mutate(label = as.character(format(round(estimate, 3), nsmall = 3))) %>% 
    select(y, label) %>%
    mutate(x = 2)
  
  ci_text <- dat %>% 
    mutate(label = paste0("[", as.character(format(round(ci.lb, 3), nsmall = 3)),", ",
                          as.character(format(round(ci.ub, 3), nsmall = 3)),
                          "]" )) %>% 
    select(y, label) %>%
    mutate(x = 2.35)
  
  # headers
  title_text <- tibble(x = unique(c(study_text$x, estim_text$x)),
                       y = 0.5,
                       label = c("Study", "Cumulative Estimate [95% CI]"))
  
  
  # edit points  in that that are too wide to plot
  dat <- dat %>% 
    mutate( add_arrow_lb = ifelse(ci.lb < -3.5, 1, 0),
            add_arrow_ub = ifelse(ci.ub > 3.5, 1, 0),
            ci.lb = ifelse(ci.lb < -1.5, -1.9, ci.lb),
            ci.ub = ifelse(ci.ub > 1.5, 1.9, ci.ub))
  
  
  
  ## plot
  p <- ggplot()+ 
    # add titles and labels
    geom_text(data = study_text, aes(x = x, y = y, label = label), 
              vjust = .5, hjust = 1,
              color = "grey15", family = "Arial", size = 2) +
    geom_text(data = estim_text, aes(x = x, y = y, label = label),
              vjust = .5, hjust = 0,
              color = "grey15", family = "Arial", size = 2) +
    geom_text(data = ci_text, aes(x = x, y = y, label = label),
              vjust = .5, hjust = 0,
              color = "grey15", family = "Arial", size = 2) +
    geom_text(data = title_text, aes(x = x, y = y, label = label), 
              hjust = c(1,0), color = "grey15", family = "Arial",
              size = 3, fontface = "bold" ) +
    # add lines
    geom_segment(data = dat, aes(x = -3.5, y = -.25, xend = 3.5, yend = -.25), 
                 linetype = "solid", color = "grey15", size = 0.5) +
    geom_segment(data = dat, aes(x = 0, y = min(y)-3, xend = 0, yend = -.25),
                 linetype = "dashed", color = "grey50", size = 0.25) +
    geom_segment(data = dat, aes(x = -3.5, y = min(y)-1.25, xend = 3.5, yend = min(y)-1.25), 
                 linetype = "solid", color = "grey15", size = 0.5) +
    # add effect sizes & 95 Ci
    geom_errorbarh(data = dat, aes(y= y, xmin=ci.lb, xmax=ci.ub),
                   height = 0.25, color = "grey15", size = 0.5) +
    geom_point(data = dat, aes(y= y, x= estimate),
               shape = 21, size = 1, color = "grey15", stroke = .75, fill = fill_col)+ 
    # add arrows for wide error bars
    geom_segment(data = filter(dat, add_arrow_ub == 1), aes(x = ci.lb, y = y, xend = ci.ub, yend = y),
                 arrow = arrow(length = unit(0.01, "npc"), ends = "both")) +
    theme_minimal() +
    labs(x = "Cumulative Effect Size") +
    theme(panel.grid = element_blank(),
          plot.title.position = "plot",
          axis.text.x = element_text(color = "grey15",
                                     family = "Arial",
                                     size = 9),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(color = "grey15",
                                      family = "Arial",
                                      face = "bold",
                                      size = 9,
                                      margin = margin(t = 3))) +
    scale_x_continuous(breaks = seq(-1.5,1.5,.5)) +
    coord_cartesian(expand = FALSE, xlim = c(-3.5,3.5), ylim =c(min(dat$y)-3, 1))
  
  
  
  return(p)
  
}



