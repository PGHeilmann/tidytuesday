# Get the Data
setwd("C:/Users/sf8642/Desktop/Daten/tidytuesday/")

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

library(nberwp)
library(tidyverse)
dir.create("2021")
dir.create("2021/2021-09-28")

papers %>% 
  write_csv("2021/2021-09-28/papers.csv")

authors %>% 
  write_csv("2021/2021-09-28/authors.csv")

programs %>% 
  write_csv("2021/2021-09-28/programs.csv")

paper_authors %>% 
  write_csv('2021/2021-09-28/paper_authors.csv')

paper_programs %>% 
  write_csv("2021/2021-09-28/paper_programs.csv")

joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 

joined_df$month.a <- month.abb[joined_df$month]

joined_df <- joined_df %>%
  arrange( year, month) %>% 
  mutate(Y_M = paste(month.a, year),
         ym_fac = as.factor(year):as.factor(month))

joined_df$sum <- 1

joined_df <- joined_df %>% 
  group_by(name, ym_fac) %>% 
  mutate(cumsum = cumsum(sum)) %>%
  ungroup(ym_fac) %>%
  mutate(sum = sum(sum)) %>% 
  filter(sum > 2) %>% 
  ungroup()

joined_df[1:1000,] %>% 
  ggplot(aes(reorder(name, -cumsum), cumsum)) +
  geom_bar(stat = "identity", alpha = 0.66) + coord_flip() +
  transition_states(Y_M, transition_length = 1, state_length = 1) +
  #view_follow(fixed_y=TRUE) +
  ease_aes('linear')
  

ff <- joined_df %>% 
  group_by(name) %>% 
  mutate(cumsum = cumsum(sum)) %>%
  group_by(name, ym_fac) %>%
  mutate(cumsum_m = max(cumsum)) %>%
  ungroup()

ff <- ff %>% 
  group_by(name) %>% 
  mutate(sum = sum(sum)) %>% 
  filter(sum > 2) %>% 
  ungroup()

test <- ff[order(ff$sum,decreasing = T),]

test <- test[!duplicated(test$sum),]

my.names <- test$name[1:20]

test <- ff[ff$name %in% my.names,]
as.data.frame(test[test$author == "w5605.1",])[c("name","sum","cumsum","cumsum_m", "ym_fac")]

df <- data.frame(name = sort(unique(test$name)),
                 year = NA,
                 cumsum = 0,
                 order = 0)

all_list <- list()
for (i in 1:length(levels(test$ym_fac))) {
    
  df$year <- levels(test$ym_fac)[i]
  sb <- subset(test, ym_fac == levels(test$ym_fac)[i])
  if(!is.na(sb[1,1])){
  sb <- subset(sb, !duplicated(name))
  sb <- sb[order(sb$name),]
  idx <- df$name %in% sb$name
  df$cumsum[idx] <- sb$cumsum_m
  }
  df$order[order(df$cumsum, decreasing = T)] <- 1:20 
  all_list[[i]] <- df
}

test_f <- do.call(rbind, all_list)
test_f$year <- factor(test_f$year, levels = unique(test_f$year))

t <- test_f %>% 
  ggplot(aes(order, cumsum)) +
  geom_bar(stat = "identity", alpha = 0.66) + coord_flip(ylim = c(0,300),clip = "off") + scale_x_reverse() +
  geom_text(aes(x = order, y = -10, label = name), hjust = 1 ) +  labs(x = "", y = "") +
  theme(plot.title = element_text( size = 32),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_line(colour = "grey"),
        axis.ticks.y = element_blank(), 
        axis.text.x  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_manual(frames = year) +
  #transition_states(year, 1,1)+
  labs(title = '{current_frame}') +
  #view_follow(fixed_y=TRUE) +
  ease_aes('linear')

leng <-length(unique(test_f$year))

animate(t, nframes = 400, fps = 5)



test_f[test_f$year == "1975:2",] %>% 
  ggplot(aes(order, cumsum)) +
  geom_bar(stat = "identity", alpha = 0.66) + coord_flip(ylim = c(0,300), clip = "off") + scale_x_reverse() +
  geom_text(aes(x = order, y = -10, label = name), hjust = 1 ) +  labs(x = "", y = "") +
  theme(plot.title = element_text( size = 32),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_line(colour = "grey"),
        axis.ticks.y = element_blank(), 
        axis.text  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm"))


test_f[test_f$year == "2020:1",]
test_f[test_f$year == "2020:1","cumsum"]
ggg <-order(test_f[test_f$year == "2020:1","cumsum"], decreasing = T)
test_f[test_f$year == "2020:1","cumsum"][ggg]
ggg;test_f[test_f$year == "2020:1","order"]
test_f[test_f$year == "2020:1",]
