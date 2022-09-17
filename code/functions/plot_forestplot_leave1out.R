
#' `plot_forestplot_cma()` creates a forest plot of the pooled estimates 
#' (study level) when omitting a certain study. Ordered by effect size magnitude.
#' @param m a data frame with summary information of the pooled estimates
#' @returns a ggplot object
#' @examples
#' plot_forestplot_leave1out(m = read_rds("output/risk/leave1out_study_risk.rds")



# PACKAGES ----------------------------------------------------------------


library(tidyverse)

# READ DATA ---------------------------------------------------------------

plot_forestplot_leave1out <- function(m) {
  
  
  dat <- m %>% 
    filter(slab != "Overall") %>% 
    arrange(estimate) %>% 
    mutate(y = -(1:n()))
  
  overall <- m %>% 
    filter(slab == "Overall") %>% 
    mutate(y = min(dat$y))
  
  preference = unique(dat$preference)
  
  ## colors
  fill_col <- case_when(preference == "risk" ~ "#2AB7CA",
                        preference == "social" ~ "#D5573B",
                        preference == "time" ~ "#4D5382")
  
  
  
  ## create text to add as labels on plot
  
  # study names
  study_text <-  dat %>% 
    mutate(label =  slab) %>% 
    select(y, label) %>%
    mutate(x = min(dat$ci.lb)-.1)
  
  # estimate values
  estim_text <- dat %>%
    mutate(label = as.character(format(round(estimate, 3), nsmall = 3))) %>% 
    select(y, label) %>%
    mutate(x = max(dat$ci.ub)+.1)
  
  ci_text <- dat %>% 
    mutate(label = paste0("[", as.character(format(round(ci.lb, 3), nsmall = 3)),", ",
                          as.character(format(round(ci.ub, 3), nsmall = 3)),
                          "]" )) %>% 
    select(y, label) %>%
    mutate(x = max(dat$ci.ub)+.15)
  
  # headers
  title_text <- tibble(x = unique(c(study_text$x, estim_text$x)),
                       y = 0.5,
                       label = c("Omitted Study", "Estimate [95% CI]"))
  
  
  
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
    # estimate 95%ci lines
    geom_rect(data = overall, aes(xmin = ci.lb, xmax = ci.ub,  ymin = 0, ymax = y),
              fill = fill_col, alpha = .25) +
    geom_segment(data = overall, aes(x = estimate, y = y-3, xend = estimate, yend = -.25),
                 linetype = "dashed", color = "grey50", size = 0.5) +
    # add lines
    geom_segment(data = dat, aes(x = min(ci.lb)-.5, y = -.25, xend =max(ci.ub)+.5, yend = -.25), 
                 linetype = "solid", color = "grey15", size = 0.5) +
    geom_segment(data = dat, aes(x =  min(ci.lb)-.5, y = min(y)-1.25, xend = max(ci.ub)+.5, yend = min(y)-1.25), 
                 linetype = "solid", color = "grey15", size = 0.5) +
    # add effect sizes & 95 Ci
    geom_errorbarh(data = dat, aes(y= y, xmin=ci.lb, xmax=ci.ub),
                   height = 0.25, color = "grey15", size = 0.5) +
    geom_point(data = dat, aes(y= y, x= estimate),
               shape = 21, size = 1, color = "grey15", stroke = .75, fill = fill_col)+ 
    theme_minimal() +
    labs(x = "Pooled estimate") +
    theme(panel.grid = element_blank(),
          plot.title.position = "plot",
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(color = "grey15",
                                      family = "Arial",
                                      face = "bold",
                                      size = 9,
                                      margin = margin(t = 3))) +
    coord_cartesian(expand = FALSE, ylim =c(min(dat$y)-3, 1))
  
  
  
  if(preference != "social") {
    p <- p + scale_x_continuous(breaks = c(-.1,0,.1)) +  
      theme(axis.text.x = element_text(color = "grey15",
                                       family = "Arial",
                                       size = 8))
  } 
  
  if(preference == "social") {
    p <- p + scale_x_continuous(breaks = c(-.2,0,.2))  +  
      theme(axis.text.x = element_text(color = "grey15",
                                       family = "Arial",
                                       size = 8))
  }
  
  
  return(p)
  
}



