library(tidyverse)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

pumpkins <- pumpkins %>% 
  mutate(weight_lbs = str_remove(weight_lbs, ","),
         weight_lbs = as.numeric(weight_lbs)) %>% 
  drop_na(weight_lbs)
  
pumpkins <- pumpkins %>% separate(id, c("year", "variety"), "-",remove = F)

pumpkins <- pumpkins %>% mutate(pollinator_father2 = str_split(pollinator_father, pattern = " ", n = 2, simplify = T)[,1],
                                 pollinator_father2 = str_to_lower(pollinator_father2))

pumpkins <- pumpkins %>% mutate(pollinator_father2 = case_when(str_detect(pollinator_father2, "self") ~ "self",
                                                            str_detect(pollinator_father2, "open") ~ "open",
                                                            str_detect(pollinator_father2, "sib") ~ "sib",
                                                            is.na(pollinator_father2) ~ "no_data",
                                                            TRUE ~ "controlled"))
