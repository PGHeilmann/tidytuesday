# Get the Data
setwd("C:/Users/sf8642/Desktop/Daten/tidytuesday/")

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

library(tidyverse)
library(gganimate)

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

library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)
colors <- brewer.pal(n =12, name = "Dark2")

set.seed(2)
col.idx <- sample(1:8,20,replace = T)

df <- data.frame(name = sort(unique(test$name)),
                 year = NA,
                 cumsum = 0,
                 order = 0,
                 c_alph = colors[col.idx])

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
  df_temp <- df
  
  df_temp[df$order > 10,"c_alph"] <- alpha(df[df$order > 10,"c_alph"],0)
  
  all_list[[i]] <- df_temp
}

test_f <- do.call(rbind, all_list)
test_f$year <- factor(test_f$year, levels = unique(test_f$year))

t <- test_f %>% 
  ggplot(aes(order, cumsum, fill = c_alph, color = c_alph)) + 
  scale_fill_identity() + scale_color_identity() +
  geom_bar(stat = "identity") + coord_flip(ylim = c(0,300), xlim =c(10,1), clip = "off") + scale_x_reverse() +
  geom_text(aes(x = order, y = -10, label = name), hjust = 1 ) +  labs(x = "", y = "") + guides(fill = "none") +
  theme(plot.title = element_text( size = 32),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_line(colour = "grey"),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 2, state_length = 1) +
  #transition_manual(frames = year) +
  #transition_states(year, 1,1)+
  #labs(title = '{current_frame}') +
# view_follow() +
  ease_aes('linear')

leng <-length(unique(test_f$year))

animate(t, nframes = leng*2,detail = 8, fps = 20, end_pause = 20)

library(ggstance)

df.full[df.full$year == "May 1999",] %>% 
  ggplot(aes(y = order, x = cumsum, fill = c_alph, color = c_alph)) + 
  scale_fill_identity() + scale_color_identity() +
  geom_colh() +
  coord_cartesian(ylim =c(10,1), clip = "off") +
  geom_text(aes(y = order, x = -10, label = name), hjust = 1 ) +  labs(x = "", y = "") + guides(fill = "none") +
  theme(plot.title = element_text( size = 32),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_line(colour = "grey"),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm"))


library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)
colors <- brewer.pal(n =12, name = "Set2")

set.seed(2)
col.idx <- sample(1:8,20,replace = T)

#################################

joined_df <-
  left_join(papers, paper_authors) %>%
  left_join(authors) %>%
  mutate(month.a = month.abb[month]) %>%
  arrange(year, month) %>%
  mutate(Y_M = factor(paste(month.a, year)),
         Y_M = factor(Y_M, levels = unique(Y_M)),
         sum = 1)

joined_df <- joined_df %>% 
  group_by(name) %>% 
  mutate(cumsum = cumsum(sum)) %>%
  group_by(name, Y_M) %>%
  mutate(cumsum_m = max(cumsum)) %>%
  group_by(name) %>% 
  mutate(sum = sum(sum)) %>% 
  filter(sum > 2) %>% 
  ungroup()

library(RColorBrewer)
brewer.pal.info

colors <- brewer.pal(n =12, name = "Paired")
n.names <- length(unique(joined_df$name))
set.seed(2)
col.idx <- sample(1:8,n.names,replace = T)

df <- data.frame(name = sort(unique(joined_df$name)),
                 year = NA,
                 cumsum = 0,
                 order = 0,
                 c_alph = colors[col.idx])

all_list <- list()
for (i in 1:length(levels(joined_df$Y_M))) {
  df$year <- levels(joined_df$Y_M)[i]
  sb <- subset(joined_df, Y_M == levels(joined_df$Y_M)[i])
  if (!is.na(sb[1, 1])) {
    sb <- subset(sb,!duplicated(name))
    sb <- sb[order(sb$name), ]
    idx <- df$name %in% sb$name
    df$cumsum[idx] <- sb$cumsum_m
  }
  df$order[order(df$cumsum, decreasing = T)] <- 1:n.names
  df_temp <- df
  
  df_temp[df$order > 10, "c_alph"] <-
    alpha(df[df$order > 10, "c_alph"], 0)
  df_temp[df$order <= 10, "c_alph"] <-
    alpha(df[df$order <= 10, "c_alph"], 0.8)
  
  df_temp <- df_temp[df$order <= 20, ]
  
  all_list[[i]] <- df_temp
}

df.full <- do.call(rbind, all_list)
df.full$year <- factor(df.full$year, levels = unique(df.full$year))

t <- df.full %>% 
  ggplot(aes(y = order, x = cumsum, fill = c_alph, color = c_alph)) + 
  scale_fill_identity() + scale_color_identity() +
  geom_colh() +
  coord_cartesian(ylim =c(10,1), clip = "off") +
  geom_text(aes(y = order, x = -10, label = name), hjust = 1 ) +  labs(x = "", y = "") + guides(fill = "none") +
  theme(plot.title = element_text( size = 32),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major.x = element_line(colour = "grey"),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(), 
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 2, state_length = 1) +
  #transition_manual(frames = year) +
  #transition_states(year, 1,1)+
  labs(title = '{closest_state}') +
  exit_fly(x_loc = 0, y_loc = 0) + enter_fly(x_loc = 0, y_loc = 0) +
  view_follow(fixed_y = c(10,1), fixed_x = c(-20,NA)) +
  ease_aes('linear')

leng <-length(unique(df.full$year))

animate(t, nframes = leng*3,detail = 8, fps = 20, end_pause = 40)

