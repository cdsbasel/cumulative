

#' `compute_es_seaman_time()` creates "effect_sizes_seaman_time.csv", by computing standardized 
#' effect sizes using the outcomes information (e.g., means, SDs, correlations) in 
#' 'clean_study_seaman_data_time.csv.csv'. Script adapted from Seaman et al. (2022)
#' 
#' Seaman, K. L., Abiodun, S. J., Fenn, Z., Samanez-Larkin, G. R., & Mata, R. (2022). 
#' Temporal discounting across adulthood: A systematic review and meta-analysis. 
#' Psychology and Aging, 37(1), 111–124. https://doi.org/10.1037/pag0000634
#' 
#' @param none
#' @returns A data frame with details of all the studies of the specified econ. preference (e.g., first author, 
#' year of publication, sample size) along with the standardized effect size and their respective sampling variance
#' that is saved as a csv file in a designated folder. 
#' @examples
#' compute_es_seaman_time()


# library(tidyverse) # for data wrangling
# library(metafor) # for effect size calculations
# library(esc) # for effect size calculations



compute_es_seaman_time <- function(){
  
  reverse_es <- function(df, studyid) {
    x = df[which(df$Study.Identifier == studyid),] # pull out study of interest
    x$yi <- x$yi * -1 # reverse effect size
    dt <- df[-which(df$Study.Identifier == studyid),] # remove old effect sizes from df
    rbind(dt, x) # add new effectsizes to df
  }

  
  dt <- read_csv("data/summary/time/clean_study_seaman_data_time.csv", col_types = cols())
  
  dt <- dt %>% 
    filter(!grepl("Middle", Intervention)) %>%   # remove middle-aged comparisons
    # remove studies with data collected in fMRI
    filter(!grepl("Saman|Seaman 2018|Stoeckel 2013|Eppinger 2012|Sheffer",Study.Identifier )) %>% 
    filter(!grepl("Session 2",condition)) # Eppinger 2018 Session 2 
  # make interaction term for study + conditions
  dt$conditionID <- interaction(dt$Study.Identifier, dt$condition)
  
  
  
  # CALC EFFECT SIZES: SMD --------------------------------------------------
  
  # Calculate effect sizes for extreme group designs
  dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
  dm <- dm[!is.na(dm$sd),] # non missing standard dev
  dm <- dm[c(1:2, 6, 8:15, 17:18, 21)]
  
  
  dm <- pivot_wider(dm, id_cols = colnames(dm[c(1, 3:6, 14)]), names_from = 'Intervention', 
                    values_from = c('mean', 'sd', 'n', 'age_mean', 'age_range', 'age_sd'))

  # add point biserial correlation and its variance to dataset
  dm <- escalc(measure = 'RPB',
               m1i = mean_Older, sd1i = sd_Older, n1i = n_Older, 
               m2i = mean_Younger, sd2i = sd_Younger, n2i = n_Younger,
               data = dm, replace=FALSE)
  
  dm$cor_type <- "RPB"
  dm$study_design <- 'extreme_group'
  
  # remove unneccesary columns
  dm$mean_Older <- NULL; dm$mean_Younger <- NULL; dm$sd_Older <- NULL; dm$sd_Younger <- NULL; dm$conditionID <- NULL
  dm$cohens_d <- NULL; dm$var_cohens_d <- NULL; dm$a <- NULL; dm$r <-NULL; dm$var_r <- NULL
  
  # effect per decade ##
  #dm$adj_effect_size <- dm$effect_size * 10 # calculate effect per year and then multiply by 10 for decade
  
  # Reversals  #
  dm <- reverse_es(dm, 'Garza 2016')
  dm <- reverse_es(dm, 'Li 2013')
  # dm <- reverse_es(dm, 'Sparrow 2019')
  dm <- reverse_es(dm, 'Sparrow 2018 Study 1')
  dm <- reverse_es(dm, 'Sparrow 2018 Study 2')
  dm <- reverse_es(dm, 'Whelan 2009') # not in orignal script

  
  dat_smd <- dm
  
  
  
  # CALC EFFECT SIZES: STATS -------------------------------------------------
  
  dm <- dt[which(dt$Design == 'extreme group'),] # pull out means
  ds <- dm[is.na(dm$sd),] # pull out no sd files
  
  ds <- ds[c(1:2, 6, 8:10, 12:15, 19:20)]
  
  ds <- pivot_wider(ds, 
                    id_cols = colnames(ds[c(1, 3:6, 11:12)]), 
                    names_from = 'Intervention', 
                    values_from = c('n', 'age_mean', 'age_range', 'age_sd'))
  
  
  # calculate effect size from t vals ##
  ds <- ds[!is.na(ds$tvalue),]
  ds <- ds %>%  
    rowwise() %>% 
    mutate(yi = esc_t(t = tvalue, grp1n = n_Older, grp2n = n_Younger, es.type = "r")$es[[1]], # point biserial cor
           vi = esc_t(t = tvalue, grp1n = n_Older, grp2n = n_Younger, es.type = "r")$var[[1]]) %>% # point biserial cor variance
    ungroup()
  ds$cor_type <- "RPB"
  ds$study_design <- 'extreme_group'
  
  # remove unnecessary columns
  ds$tvalue <- NULL
  ds$Fvalue <- NULL
  ds$df <- NULL
  
  
  # Reversals ##
  ds <- reverse_es(ds, 'Green 1994')
  
  dat_stats <- ds
  
  
  # CALC EFFECT SIZES: CONT. -------------------------------------------------
  
  
  # Calculate effect sizes for continuous age designs
  dc <- dt[which(dt$Design == 'continuous age'),] # pull out correlational studies
  dc <- dc[!is.na(dc$correlation),] # remove incomplete studies
  
  #not applicable here
  #dc <- escalc(measure = 'ZCOR', ri = correlation, ni = n, data = dc, var.names = c('fishers_z', 'var_fishers_z'))
  
  
  # mutate Gollner separately (reports Kendall's tau) +
  # mutate Löckenhoff + Johnson + Reimers separately (report Spearman's rho) +
  # conversion formula from:
  #  Walker, David A. (2003) "JMASM9: Converting Kendall’s Tau For Correlational Or 
  #  Meta-Analytic Analyses," Journal of Modern Applied Statistical Methods: Vol. 2 : Iss. 2 , Article 26.
  #  DOI: 10.22237/jmasm/1067646360
  #  Rupinski, M. T., & Dunlap, W. P. (1996). Approximating Pearson Product-Moment Correlations 
  #  from Kendall’s Tau and Spearman’s Rho. Educational and Psychological Measurement, 56(3), 
  #  419–429. doi:10.1177/0013164496056003004
  dc <- dc %>% 
    mutate(correlation = case_when(grepl( "Löckenhoff|Johnson|Reimers" , Study.Identifier) ~  2*sin(correlation*(pi/6)),
                                   grepl( "Gollner"  , Study.Identifier) ~  sin(.5*pi*correlation),
                                      TRUE ~ correlation))
  


  
  
  dc <- escalc(measure = 'COR',
               ri = correlation, 
               ni = n,
               data = dc)
  
  dc$cor_type <- "COR"
  dc$study_design <- "continuous"
  # remove unnecessary columns
  dc <- dc[c(1, 6, 8:13, 14:15, 22:25)]
  
  # Reversals ##
  dc <- reverse_es(dc, 'Löckenhoff 2011')
  dc <- reverse_es(dc, 'Hampton 2018')
  dc <- reverse_es(dc, 'Wolfe 2017')
  
  
  dat_cont <- dc
  
  
  # COMBINE ROWS ------------------------------------------------------------
  
  # format data frame
  dat_seaman <- 
    bind_rows(dat_smd, dat_stats, dat_cont) %>% 
    filter(Study.Identifier != "Sparrow 2019") %>% # remove duplicated study also recently extracted
    # renaming variables and creating variables to be consistent with the other dataframes
    rename(cor_yi = yi,
           cor_vi = vi,
           first_author = Study.Identifier,
           dv_type_of_comparison = Design,
           old_mean_age = age_mean_Older,
           young_mean_age = age_mean_Younger,
           old_sd_age = age_sd_Older,
           young_sd_age = age_sd_Younger,
           mean_age = age_mean,
           sd_age = age_sd,
           total_n = n,
           old_total_n = n_Older,
           young_total_n = n_Younger,
           dv_units = Measure,
           incentivization = Incentive) %>% 
    mutate(first_author = gsub("-","",first_author),
           n_incl_es = case_when(!is.na(total_n) ~ total_n,
                                 TRUE ~ young_total_n + old_total_n),
           pref = "time",
           reversed_es = case_when(first_author %in% c('Garza 2016', 'Li 2013',
                                                      'Sparrow 2018 Study 1',
                                                      'Sparrow 2018 Study 2', 'Green 1994', "Whelan 2009",
                                                      'Löckenhoff 2011','Hampton 2018','Wolfe 2017') ~ 1,
                                  TRUE ~0)) %>% 
    separate(age_range_Younger, c("young_age_min", "young_age_max"), "-") %>% # cannot separate one row
    separate(age_range_Older, c("old_age_min", "old_age_max"), "-") %>% # cannot separate one row
    separate(age_range, c("age_min", "age_max"), "-") %>% 
    mutate(young_age_min = as.numeric(young_age_min),
           young_age_max = as.numeric(young_age_max),
           old_age_min = as.numeric(old_age_min),
           old_age_max = as.numeric(old_age_max),
           age_min = as.numeric(age_min),
           age_max = as.numeric(age_max),
           dec_diff =  case_when(grepl("extreme",dv_type_of_comparison) ~  .1*(old_mean_age -young_mean_age),
                                 TRUE ~  .1*(age_max -age_min))) %>% 
    filter(!grepl("unpub",first_author)) %>% # remove unpublished data
    rowwise() %>% 
    mutate(year_of_publication = parse_number(first_author),
           source = "Seaman",
           domain_frame = "gain",
           task_type = "description",
           first_author = gsub("[1900-9999]{4}","",first_author),
           sample_code = case_when(grepl("Sample", first_author) ~ unlist(str_split(first_author, "  ",2))[2],
                                   TRUE ~ "Sample 1"),
           paper_section = case_when(grepl("Study", first_author) ~ unlist(str_split(first_author, "  ",2))[2],
                                     TRUE ~ "Single"),
           first_author = case_when(grepl("Sample|Study", first_author) ~ unlist(str_split(first_author, "  ",2))[1],
                                    TRUE ~ first_author),
           dv_description = paste0("Magnitude.of.Time.Delay: ", Magnitude.of.Time.Delay, ". Condition: ", condition),
           dv_type_of_comparison = case_when(dv_type_of_comparison == "extreme group" & grepl("Saman|Green|Jimura",first_author) ~ "extreme group (stats.)",
                                             dv_type_of_comparison == "extreme group" & !grepl("Saman|Green|Jimura",first_author) ~ "extreme group (m + sd)",
                                             dv_type_of_comparison != "extreme group" ~ "age continuous")) %>% 
    ungroup() %>%
    #adding a publication id
    group_by(first_author, year_of_publication) %>% 
    mutate(pub_id = cur_group_id()) %>% 
    ungroup() %>% 
    select(-c(Magnitude.of.Time.Delay, condition)) %>% 
    # create unique study/sample labels
    group_by(first_author, year_of_publication) %>% 
    mutate(
      year_of_pub_temp =  as.character(year_of_publication),
      incentivization = tolower(incentivization),
      incentivization = case_when(grepl("real", incentivization) ~ "incentivized",
                                  TRUE ~ incentivization),
      study_label = case_when(
        # single-study single-sample papers
        length(unique(sample_code))== 1 & grepl("Single", paper_section) ~ paste0(first_author, " (", year_of_pub_temp, ")"),
        # single-study multiple-sample papers
        length(unique(sample_code))!= 1 & grepl("Single", paper_section) ~ paste0(first_author, " ", sample_code, " (", year_of_pub_temp, ")"),
        # multiple-study papers (each with a single sample each)
        length(unique(sample_code))== 1 & !grepl("Single", paper_section) ~ paste0(first_author, " ", paper_section, " (", year_of_pub_temp, ")")),
      # NO TITLE OF ARTICLES IN THE DATA
      title_of_article = paste0(first_author, as.character(year_of_publication))) %>% 
    ungroup() %>% 
    select(-year_of_pub_temp) 
  
  

  
  
  write_csv(dat_seaman, "data/summary/time/effect_sizes_seaman_time.csv")
  
  print("effect_sizes_seaman_time.csv created successfully! Saved in:   data/summary/time")
  
}
