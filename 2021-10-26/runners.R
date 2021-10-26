library(tidyverse)

# ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

ultra_rankings <-  read_csv('https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv', col_types = c("ddccdcc"))
ultra_rankings$Time <- parse_time(ultra_rankings$Time)

colnames(ultra_rankings)[1] <- "race_year_id"

str(race)
str(ultra_rankings)

race_ar <- race %>%  
  filter(distance > 0) %>% 
  mutate(net_elev = elevation_gain + elevation_loss ) %>% 
  arrange(distance, net_elev) %>% 
  mutate(ordered = row_number())

test <- ultra_rankings %>% 
  left_join(race_ar, "race_year_id") %>% 
  filter(!is.na(Time),
         str_detect(participation, regex("Solo", ignore_case = T))) %>%  
  mutate(time = as.numeric(Time)/60/60)

ggplot(test, aes(x = ordered, y = time, col = Gender )) + geom_point(alpha = .7) 
  
test %>% filter(distance > 150) %>% 
ggplot( aes(x = ordered, y = time, col = gender )) + geom_point(alpha = .7) 

test %>% filter(distance < 150) %>% 
  ggplot( aes(x = race_year_id, y = time, col = gender )) + geom_point(alpha = .7) 


sub_test <- test %>% group_by(race_year_id, Gender) %>% summarise_at("time", mean)
sub_test <- sub_test %>% left_join(race_ar, "race_year_id")

sub_test %>% filter(distance > 150,!is.na(Gender)) %>% 
ggplot( aes(x =  distance, y = time, col = Gender )) + geom_point(alpha = .7) + geom_smooth()

# Idee ist: vielleicht alle besten% über die Jahre oder Veranstaltungen als Flächen darstellen
