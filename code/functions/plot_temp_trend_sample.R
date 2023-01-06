


#' `plot_temp_trend_sample()` creates a series of scatter plots of the relation between year 
#' of publication and effect size (for each econ. preference) 
#' @param m_list a vector of the files names of the fitted multi-level meta regression model (year of publication a predictor).
#' @returns a ggplot object
#' @examples
#' plot_temp_trend(m_list = c("output/risk/temp_trend_risk.rds", "output/time/temp_trend_time.rds", "output/social/temp_trend_social.rds"))



# library(tidyverse) # for plotting a data wrangling



plot_temp_trend_sample <- function(m_list) {
  
  # get data
  dat <- NULL
  df_text <- NULL
  df_pred <- NULL
  
  # reading data and getting model predictions
  for (m_name in m_list) {
    
    m <- read_rds(m_name)
    model <- broom::tidy(m)
    sdat <- m$data
    n_es <- as.character(nrow(sdat))
    sdat$pref <- str_to_title(gsub("_", " ", sdat$pref))
    
    
    
    dat <- bind_rows(dat, sdat)
    
    
    #  predictions
    df_pred <- bind_rows(df_pred, 
                    bind_cols(sdat,
                              tibble(pred_sample = predict(m, interval = "confidence")[,1],
                                     lwr_sample = predict(m, interval = "confidence")[,2],
                                     upr_sample = predict(m, interval = "confidence")[,3])))
    
    
    #  model values
    df_text <- bind_rows(df_text,
                         tibble(pref = unique(sdat$pref),
                                txt_val = paste0("b = ", as.character(round(model$estimate[2],3)), "\n ",
                                                 "p = ", as.character(round(model$p.value[2],3))),
                                y = 5000,
                                x = 2000))
    
    
  }   
  
  
  # choosing color palette

    col_pal <- c("#2AB7CA","#4D5382", "#D5573B", "#9F7131")


    risk_lab <- unique(dat$pref)[grepl("risk", tolower(unique(dat$pref)))]
    time_lab <- unique(dat$pref)[grepl("time", tolower(unique(dat$pref)))]
    social_lab <- unique(dat$pref)[grepl("social", tolower(unique(dat$pref)))]
    effort_lab <- unique(dat$pref)[grepl("effort", tolower(unique(dat$pref)))]
    
    
    dat$pref <- factor(dat$pref, levels=c(risk_lab,time_lab,social_lab,effort_lab))
    
    # df$pref <- factor(df$pref, levels=c(risk_lab,time_lab,social_lab,effort_lab))
    df_pred$pref <- factor(df_pred$pref, levels=c(risk_lab,time_lab,social_lab,effort_lab))
    df_text$pref <- factor(df_text$pref, levels=c(risk_lab,time_lab,social_lab,effort_lab))
    

  
  
  
  # plotting predictions 
  p <- ggplot() +
    # add raw sample points and predictions
    geom_jitter(dat = dat, aes(y = n_incl_es, x = 2022+(dec_in_print*10), color = pref),
                height = 0, width = .25, alpha = .3, size = 1.75)+
    # geom_smooth(dat = dat, aes(y = n_incl_es, x = 2022+(dec_in_print*10), color = pref), method = stats::lm, formula = y ~x) +
    # model predictions
    geom_ribbon(data = df_pred, 
                aes(x = 2022+(dec_in_print*10), ymin = 10^lwr_sample, ymax = 10^upr_sample, fill = pref), alpha = .3) +
    geom_line(data = df_pred, 
              aes(x = 2022+(dec_in_print*10), y = 10^pred_sample, color = pref), size = .75) +
    geom_text(data = df_text, aes(x = x, y = y, label = txt_val), family = "Arial", fontface = "italic", size = 3, hjust = 0) +
    scale_color_manual(values = col_pal) +
    scale_fill_manual(values = col_pal) +
    theme_minimal() +
    labs(y = "Sample Size (log10)", x = "Year of Publication") +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          panel.spacing = unit(1.1, "lines"),
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
          axis.title.x = element_text(hjust = 0.5,
                                      color = "grey15",
                                      family = "Arial",
                                      size = 9,
                                      face = "bold"),
          axis.title.y = element_text(hjust = 0.5,
                                      color = "grey15",
                                      family = "Arial",
                                      size = 9,
                                      face = "bold"),
          # axis.title = element_text(color = "grey15",
          #                           family = "Arial",
          #                           face = "bold",
          #                           size = 8,
          #                           margin = margin(t = 3)),
          panel.border = element_rect(fill = NA, color = "grey")) +
    scale_y_log10() +
    scale_x_continuous(breaks = seq(1990,2020,5), expand = c(0,0), limits = c(1993,2023)) +
    facet_wrap(.~pref, nrow = 1)
  
  return(p)
  
  
}



