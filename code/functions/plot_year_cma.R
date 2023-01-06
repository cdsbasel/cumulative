

#' `plot_year_cma()` creates a series of forest plots of year-level cumulative 
#' estimates (for each econ. preference)
#' @param m_list a vector of the files names of the data framewith summary 
#' information of the pooled estimates.
#' @returns a ggplot object
#' @examples
#' plot_year_cma(m_list = c("output/risk/cma_year_risk.rds", "output/time/cma_year_time.rds", "output/social/cma_year_social.rds")))



# library(tidyverse)


plot_year_cma <- function(m_list) {
  
  dat <- NULL
  
  
  
  for (m_name in m_list) {
    
    m <- read_rds(m_name)
    dat <- bind_rows(dat, m)
  }
  
  
  if (sum(grepl("risk", tolower(unique(dat$preference)))) == 1) {
    dat$preference = factor(dat$preference, levels=c('risk','time','social', 'effort'))
  }
  

  # choosing color palette
  if (sum(grepl("risk", tolower(unique(dat$preference)))) == 1) {
    col_pal <- c("#2AB7CA","#4D5382", "#D5573B", "#9F7131")
    
  } else {
    col_pal <-c("#2AB7CA","#2AB7CA", "#2AB7CA")}
  
  
  p <-  dat %>% ggplot()+ 
    # add effect sizes & 95 Ci
    geom_errorbar(aes(x= slab, ymin=ci.lb, ymax=ci.ub, color = preference),
                  width = 0.3, size = 0.5) +
    geom_point(aes(x= slab, y= estimate, color = preference),
               shape = 21, size = 1, stroke = .75) +
    # add lines
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "grey50", size = 0.25) +
    geom_hline(yintercept = setdiff(seq(-.5,.5,.25),0),
               linetype = "solid", color = "grey80", size = 0.2) +
    theme_minimal() +
    scale_color_manual(values = col_pal) +
    labs(title = "Cumulative Effect Sizes",
         x = "Year of Publication") +
    theme(panel.grid = element_blank(),
          panel.spacing = unit(.75, "lines"),
          plot.title = element_text(color = "grey15",
                                    family = "Arial",
                                    face = "bold",
                                    size = 9,
                                    hjust = 0.5,
                                    margin=margin(t = 0, b = 5, r = 0, l = 0)),
          legend.position = "none",
          strip.text = element_text(hjust =0),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(color = "grey15",
                                     family = "Arial",
                                     size = 8),
          panel.border = element_rect(fill = NA, color = "grey")) +
    scale_y_continuous(expand = c(0,0), limits =  c(-.825,.825), oob = scales::squish) +
    scale_x_continuous(breaks = seq(1995,2020,5), expand = c(0,0)) +
    coord_cartesian(clip = "off", xlim = c(1993,2023)) +
    facet_wrap(.~preference, ncol = 1)
  
  
  return(p)
}


