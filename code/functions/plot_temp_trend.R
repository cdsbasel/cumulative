


#' `plot_temp_trend()` creates a series of scatter plots of the relation between year 
#' of publication and effect size (for each econ. preference) 
#' @param m_list a vector of the files names of the fitted multi-level meta regression model (year of publication a predictor).
#' @returns a ggplot object
#' @examples
#' plot_temp_trend(m_list = c("output/risk/temp_trend_risk.rds", "output/time/temp_trend_time.rds", "output/social/temp_trend_social.rds"))

# 
# 
# library(tidyverse) # for plotting a data wrangling
# library(metafor) # model predictions



plot_temp_trend <- function(m_list) {
  
  # get data
  dat <- NULL
  df <- NULL
  
  
  # reading data and getting model predictions
  for (m_name in m_list) {
    
    m <- read_rds(m_name)
    n_es <- as.character(nrow(m$data))
    m$data$pref <- paste0(str_to_title(gsub("_", " ", m$data$pref)), " (k = ", n_es, ")") # add number of ess for each pref
 
    dat <- bind_rows(dat, m$data)

    sdat <-  m$data

    #  predictions
    df <- bind_rows(df, 
                    bind_cols(sdat,
      as.data.frame(predict.rma(m))))
                                                
  }   
  
  
  # direction of effect sizes label
  es_dir_txt <- tibble( x= rep(c(1986),length(unique(dat$pref))*2),
                        y = rep(c(-.35, .35), each = length(unique(dat$pref))),
                        pref = rep(unique(dat$pref),2), 
                        label=rep(c("Decreases\nwith age","Increases\nwith age"),each = length(unique(dat$pref))))
  

  # choosing color palette
  col_pal <- case_when(sum(grepl("risk", tolower(unique(dat$pref)))) == 1 ~c("#2AB7CA","#4D5382", "#D5573B"),
                       sum(grepl("risk", tolower(unique(dat$pref)))) != 1 ~c("#2AB7CA","#2AB7CA", "#2AB7CA"))
  
  
  if (sum(grepl("risk", tolower(unique(dat$pref)))) == 1) {
    
  risk_lab <- unique(dat$pref)[grepl("risk", tolower(unique(dat$pref)))]
  time_lab <- unique(dat$pref)[grepl("time", tolower(unique(dat$pref)))]
  social_lab <- unique(dat$pref)[grepl("social", tolower(unique(dat$pref)))]
  
  dat$pref <- factor(dat$pref, levels=c(risk_lab,time_lab,social_lab))
  
  df$pref <- factor(df$pref, levels=c(risk_lab,time_lab,social_lab))
  
  es_dir_txt$pref <- factor(es_dir_txt$pref, levels=c(risk_lab,time_lab,social_lab))
  
  }
  
  # plotting predictions 
  p <- ggplot() +
    # add 0-intercept line + other lines
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "grey50", size = 0.25) +
    geom_hline(yintercept = setdiff(seq(-.5,.5,.25),0),
               linetype = "solid", color = "grey80", size = 0.2) +
    # add raw es points and predictions
    geom_jitter(data = dat, aes(y = cor_yi, x = year_of_publication, color = pref),
                height = 0, width = .25, alpha = .2, size = 1.25)+
    geom_ribbon(data = df, 
                aes(x = year_of_publication, ymin = ci.lb, ymax = ci.ub, fill = pref), alpha = .3) +
    geom_line(data = df, 
              aes(x = year_of_publication, y = pred, color = pref), size = .75) +
    # add effect size direction label
    geom_text(data = es_dir_txt, 
              aes(x = x, y = y, label = label), 
              angle = 90, size = 1.5, color = "grey50",
              lineheight = .75,
              fontface = "italic") + 
    scale_color_manual(values = col_pal) +
    scale_fill_manual(values = col_pal) +
    theme_minimal() +
    labs(y = "", x = "Year of Publication",
         title = "Individual Effect Sizes") +
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
          axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(r = 5)),
          # axis.title = element_text(color = "grey15",
          #                           family = "Arial",
          #                           face = "bold",
          #                           size = 8,
          #                           margin = margin(t = 3)),
          panel.border = element_rect(fill = NA, color = "grey")) +
    scale_y_continuous(expand = c(0,0), limits =  c(-.825,.825),breaks = seq(-.5,.5,.25), oob = scales::squish) +
    scale_x_continuous(breaks = seq(1995,2020,5), expand = c(0,0)) +
    coord_cartesian(clip = "off", xlim = c(1993,2023)) +
    facet_wrap(.~pref, ncol = 1)
  
  return(p)
  
  
}



