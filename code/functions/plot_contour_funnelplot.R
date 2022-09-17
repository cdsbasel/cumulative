
#' `plot_contour_funnelplot()` creates a contour-enhanced funnel plot with SEs and effect sizes. 
#' Adapted from code in https://stats.stackexchange.com/a/195333
#' @param m rma.mv metafor object. Fitted multi-level meta-analytic model
#' @param ref value at which the plot is centered 
#' @returns a ggplot object
#' @examples
#' plot_contour_funnelplot(m = read_rds("output/risk/mlma_risk.rds"), ref = 0)


library(tidyverse)  # for plotting
library(patchwork) # for combining plots

plot_contour_funnelplot <- function(m, ref) {
  
  dat <- m$data 
  
  # create df with CIs
  ci_df <- tibble(
    estimate = m$b[1],
    refline = ref,
    se_seq = seq(0,max(sqrt(dat$vi)+.01),.0001), # create sequence of se values
    # 90% CI region
    ll90 = refline-(1.645*se_seq),
    ul90 = refline+(1.645*se_seq),
    # 95% CI region
    ll95 = refline-(1.96*se_seq),
    ul95 = refline+(1.96*se_seq),
    # 99% CI region 
    ll99 = refline-(2.576*se_seq),
    ul99 = refline+(2.576*se_seq),
    # estime 95% CI region)
    est_ll95 = estimate-(1.96*se_seq),
    est_ul95 = estimate+(1.96*se_seq))
  
# plotting contour-enhanced funnel plot
  p <- ggplot() +
    labs(y = 'Standard Error', x = 'Effect Size', fill = "", linetype = "")+
    geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = -Inf, xmax = Inf, fill = "p < 1%"), alpha = 1) +
    # draw 99% CI region
    geom_ribbon(data = ci_df, aes(y = se_seq, xmin = ul95, xmax = ul99, fill = "1% < p < 5%"), alpha = 1) +
    geom_ribbon(data = ci_df, aes(y = se_seq, xmin = ll95, xmax = ll99, fill = "1% < p < 5%"),  alpha = 1) +
    # draw 95% CI region
    geom_ribbon(data = ci_df, aes(y = se_seq, xmin = ul90, xmax = ul95,  fill = "5% < p < 10%"), alpha = 1) +
    geom_ribbon(data = ci_df, aes(y = se_seq, xmin = ll90, xmax = ll95,  fill = "5% < p < 10%"), alpha = 1) +
    # draw 90% CI region
    geom_ribbon(data = ci_df, aes(y = se_seq, xmin = ll90, xmax = ul90,  fill = "10% < p"), alpha = 1) +
    scale_fill_manual(breaks=c('10% < p', '5% < p < 10%', '1% < p < 5%', "p < 1%"),
                      values=c('10% < p'='white', '5% < p < 10%'='grey55', '1% < p < 5%'='grey75', "p < 1%" = "grey95"))+
    # add effect size estimates
    geom_point(data = dat, aes(y = sqrt(vi), x = yi), shape = 21, fill = "white", color = "grey20", stroke = .5, size = 1.5, alpha = 1) +
    # pooled estimate lines
    geom_vline(data = ci_df, aes(xintercept = estimate), linetype = 'dashed', size = .25) +
    geom_line(data = ci_df, aes(y = se_seq, x = est_ll95, linetype = 'Estimate (95% CI)'), size = .25) +
    geom_line(data = ci_df, aes(y = se_seq, x = est_ul95, linetype = 'Estimate (95% CI)'), size = .25) +
    scale_linetype_manual(breaks=c('Estimate (95% CI)'),
                          values=c('Estimate (95% CI)'='dashed'))+
    scale_y_reverse(expand = c(0,0))+
    theme_minimal() +
    theme(text = element_text(family = "Arial", size = 9),
          title = element_text(family = "Arial", face = "bold", size = 9),
          legend.margin = margin(-0.75,0,0,0, unit="cm"),
          # panel.background = element_rect(fill = "grey95"),
          panel.border  = element_rect(fill = NA, color = "black")) +
    guides(fill = guide_legend(override.aes = list(color = "black", size = .25)))

  return(p)
}

