
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
    separate(google_id, c("author_id","article_id"), sep = "([:])")
  
  return(data)
}


get_citations <- function(data) {
  
  for (CurrRef in 1:nrow(data)) {
    cit <- NULL
    
    try(
      cit <- get_article_cite_history(id = data$author_id[CurrRef], article = data$article_id[CurrRef])
    ) 
    
    data$cita[CurrRef] <- list(cit)
    
    
  }
  return(data)
}



# READING CSV REF FILES -------------------------------------------------------

time_ref <- read_csv("data/time_ageing_ref.csv")
altr_ref <- read_csv("data/altruism_ageing_ref.csv")



# GET CITATIONS DATA ------------------------------------------------------


data_time <- get_google_ids(data = time_ref)
data_time <- get_citations(data = data_time)

data_altr <- get_google_ids(data = altr_ref)
data_altr <- get_citations(data = data_altr)



# SAVE RDS DATA -----------------------------------------------------------

####

