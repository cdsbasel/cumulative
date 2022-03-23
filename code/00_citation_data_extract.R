
# DESCRIPTION -------------------------------------------------------------


# This script reads the sheets in the excel workbook that contains the effect
#  sizes that were manually extracted from the different meta-analyses that
#  rersults form our serch. It compiles these it into a singles csv file

# PACKAGES ----------------------------------------------------------------

library(tidyverse)
library(scholar)


# FUNCTIONS ---------------------------------------------------------------


get_google_ids <- function(data) {
  
  
  data <- data %>% select(Author, Title, `Publication Year`, Notes) %>% 
    rowwise() %>% 
    mutate(google_id = str_match(Notes, "view=\\s*(.*?)\\s*</p>")[2]) %>% 
    ungroup() %>% 
    separate(google_id, c("autid","pubid"), sep = "([:])")
  
  return(data)
}


get_citations <- function(data) {
  
  for (CurrRef in 1:nrow(data)) {
    cit <- NA
    
    print(data$Author[CurrRef])
    
    try(
      cit <- get_article_cite_history(id = data$autid[CurrRef], article = data$pubid[CurrRef])
    ) 
    
    cit <- data.frame(cit)
    cit <- cit %>% mutate(autid = data$autid[CurrRef])
    
    
    data$cita[CurrRef] <- list(data.frame(cit))
    
    if(sum(is.na(cit)) == 0) {print("ok")} else {print("not ok")}
    
  }
  return(data)
}



# READING CSV REF FILES -------------------------------------------------------

time_ref <- read_csv("data/time_ageing_ref.csv")
altr_ref <- read_csv("data/altruism_ageing_ref.csv")
cma_dat <- read_csv("data/cma_data.csv")


# GET CITATIONS DATA ------------------------------------------------------


data_time <- get_google_ids(data = time_ref)
data_time <- get_citations(data = data_time)

data_altr <- get_google_ids(data = altr_ref)
data_altr <- get_citations(data = data_altr)

cit_altr <- bind_rows(data_altr$cita) %>% 
  left_join(data_altr, by = c("pubid", "autid")) %>% 
  # filter(!is.na(pubid)) %>% 
  select(-c(cit, cita))

cit_time <- bind_rows(data_time$cita) %>% 
  left_join(data_time, by = c("pubid", "autid") ) %>% 
  # filter(!is.na(pubid)) %>% 
  select(-c(cit, cita))


# CALC INDECES ------------------------------------------------------------


cit_altr_sum <- cit_altr %>%
  group_by(across(pubid:Notes)) %>%
  summarise(cite_p_year = mean(cites), n_yr = n()) %>%
  rowwise() %>%
  mutate(autid2 = str_split_fixed(Author, ",",2)[1]) %>%
  ungroup() %>% 
  rename(year_pub = `Publication Year`) %>% 
  mutate(year_pub = as.character(year_pub))

cit_time_sum <- cit_time %>% 
  group_by(across(pubid:Notes)) %>%
  summarise(cite_p_year = mean(cites), n_yr = n()) %>% 
  rowwise() %>%
  mutate(autid2 = str_split_fixed(Author, ",",2)[1]) %>% 
  ungroup() %>% 
  rename(year_pub = `Publication Year`) %>% 
  mutate(year_pub = as.character(year_pub)) %>% 
  mutate(year_pub = if_else(is.na(year_pub), "unpublished", year_pub))



# MATCH WITH ES DATA ------------------------------------------------------

# altruism
# had to fix some year of pub manually (Beadle + Pornpattananangkul) as these were wrong in the raw data...
# 1/11 missing row....can be fixed
cma_alt <- cma_dat %>% 
  filter(pref == "altruism" & beh_task == 1 & fin_task == 1) %>% 
  rowwise %>% 
  mutate(autid2 = str_split_fixed(study, ":",2)[1],
         year_pub = year) %>% 
  ungroup()


es_cit_alt <- cma_alt %>% left_join(cit_altr_sum, by = c("autid2", "year_pub"))%>% 
  select(pref, study, year_pub,g, cite_p_year, n_yr)


# time
# had to fix some year of pub manually as these were wrong in the raw data (Halfmann 2016 does not exist)...
# 13/40 missing rows....some of which can be fixed
cma_time <- cma_dat %>% 
  filter(pref == "time") %>% 
  rowwise() %>% 
  mutate(autid2 = str_split_fixed(study, " ",2)[1],
         year_pub = year) %>% 
  ungroup()


es_cit_time <- cma_time %>% left_join(cit_time_sum, by = c("autid2", "year_pub")) %>% 
  select(pref, study, year_pub,g, cite_p_year, n_yr)

es_cit_dat <- bind_rows(es_cit_time, es_cit_alt) 

# SAVE RDS DATA -----------------------------------------------------------

write_csv(es_cit_dat, file = "data/es_cit_dat.csv")

