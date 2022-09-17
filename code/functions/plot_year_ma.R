

#' `plot_year_ma()` creates a series of forest plots of year-level estimates 
#' (for each econ. preference) using the fitted multi-level meta-analytic model.
#' @param m_list a vector of the files names of the fitted multi-level meta-analytic model.
#' @returns a ggplot object
#' @examples
#' plot_year_ma(m_list = c("output/risk/mlma_risk.rds","output/time/mlma_time.rds","output/social/mlma_social.rds"))



library(tidyverse) # for data wrangling and plotting 


plot_year_ma <- function(m_list, rho) {
  
  l <- NULL
  dat <- NULL
  
  for (m_name in m_list) {
    
    m <- read_rds(m_name)
    
    agg_yr <- aggregate(m$data,
                        cluster=year_of_publication, # year-level estimates
                        V=vcov(m, type="obs"), #returns the marginal variance-covariance matrix of the observed effect sizes or outcomes
                        addk=TRUE) # count
    
    
    l <- bind_rows(l, agg_yr)
    
    
    
  }
  
  
  
  l <- l %>%
    mutate(estimate = yi,
           ci.ub = yi + (1.96*sqrt(vi)),
           ci.lb = yi - (1.96*sqrt(vi)),
           slab = year_of_publication)
  
  
  if (sum(grepl("risk", tolower(unique(l$pref)))) == 1) {
    l$pref = factor(l$pref, levels=c('risk','time','social'))
    
  }
  
  # choosing color palette
  col_pal <- case_when(sum(grepl("risk", tolower(unique(l$pref)))) == 1 ~c("#2AB7CA","#4D5382", "#D5573B"),
                       sum(grepl("risk", tolower(unique(l$pref)))) != 1 ~c("#2AB7CA","#2AB7CA", "#2AB7CA"))
  
  
  p <- l %>% 
    ggplot()+ 
    # add effect sizes & 95 Ci
    geom_errorbar(aes(x= slab, ymin=ci.lb, ymax=ci.ub, color = pref),
                  width = 0.3, size = 0.5) +
    geom_point(aes(x= slab, y= estimate, color = pref),
               shape = 21, size = 1, stroke = .75) +
    # add 0-intercept line + other lines
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "grey50", size = 0.25) +
    geom_hline(yintercept = setdiff(seq(-.5,.5,.25),0),
               linetype = "solid", color = "grey80", size = 0.2) +
    theme_minimal() +
    scale_color_manual(values = col_pal) +
    scale_fill_manual(values = col_pal) +
    labs(title = "Effect Sizes by Year", y = "",
         x = "Year of Publication") +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          plot.title = element_text(color = "grey15",
                                    family = "Arial",
                                    face = "bold",
                                    size = 9,
                                    hjust = 0.5,
                                    margin=margin(t = 0, b = 5, r = 0, l = 0)),
          strip.text = element_text(hjust = 0,
                                    color = "grey15",
                                    family = "Arial",
                                    size = 8,
                                    face = "bold.italic"),
          axis.text = element_text(color = "grey15",
                                   family = "Arial",
                                   size = 8),
          axis.title = element_text(color = "grey15",
                                    family = "Arial",
                                    face = "bold",
                                    size = 8),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10)),
          panel.border = element_rect(fill = NA, color = "grey")) +
    scale_y_continuous(expand = c(0,0), limits =  c(-.825,.825),breaks = seq(-.5,.5,.25), oob = scales::squish) +
    scale_x_continuous(breaks = seq(1995,2020,5), expand = c(0,0)) +
    coord_cartesian(clip = "off", xlim = c(1993,2023)) +
    facet_wrap(.~pref, ncol = 1)
  
  return(p)
}


