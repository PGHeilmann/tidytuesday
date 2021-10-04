library(tidyverse)
library(gganimate)
library(ggstance)
library(RColorBrewer)

# Getting the data, see original tidytuesday repo for this
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# Create a new DF containing the year:month combination
# containing all papers and authors
joined_df <-
  left_join(papers, paper_authors) %>%
  left_join(authors) %>%
  mutate(month.a = month.abb[month]) %>%
  arrange(year, month) %>% # Sort chronologically
  mutate(Y_M = factor(paste(month.a, year)),
         Y_M = factor(Y_M, levels = unique(Y_M)), # Reorganize factor levels chronologically
         sum = 1) # We will use this to add up each paper

# Here we calculate the cummulative sum of released papers
# for each author. I used the original sum column which contains only 1, so
# that the cummulative sum is always increased by 1. Then I replace the sum column
# with the overall sum and remove everyone who did not release more than 1 paper
joined_df <- joined_df %>% 
  group_by(name) %>% 
  mutate(cumsum = cumsum(sum)) %>%
  group_by(name, Y_M) %>%
  mutate(cumsum_m = max(cumsum)) %>%
  group_by(name) %>% 
  mutate(sum = sum(sum)) %>% 
  filter(sum > 2) %>% 
  ungroup()

# Color blind friendly color scheme
# Every author gets a color
colors <- brewer.pal(n =12, name = "Paired")  # Colorblind-friendly
n.names <- length(unique(joined_df$name))
set.seed(123)
colors <- colors[-11]                         # Yellow remove because it is hard to see on white bg
col.idx <- sample(1:11,n.names,replace = T)


# This data frame will be used in the loop for every unique year:month combination
# DFs will be added to a list to create a uniform data structure for every combination
df <- data.frame(name = sort(unique(joined_df$name)), # Author names
                 year = NA,                           # Year:month
                 cumsum = 0,                          # Sum of paper until this month
                 order = 1:n.names,                   # Rank in published papers
                 c_alph = colors[col.idx])            # Colors (never change)


all_list <- list()
for (i in 1:length(levels(joined_df$Y_M))) {
  # Current year:month for the animation
  df$year <- levels(joined_df$Y_M)[i]
  
  # Subset with data for this year:month
  sb <- subset(joined_df, Y_M == levels(joined_df$Y_M)[i])
  
  # Only change data if papers were published this year:month
  if (!is.na(sb[1, 1])) {
    sb <- subset(sb,!duplicated(name))  # Remove duplicate authors
    sb <- sb[order(sb$name), ]          # Order Alphabetically
    idx <- df$name %in% sb$name         # Get index of authors in df to update
    df$cumsum[idx] <- sb$cumsum_m       # Update cumsum for this year:month
    
    df$order[order(df$cumsum, decreasing = T)] <- 1:n.names # Update rank
  }
  
  df_temp <- df                         
  df_temp <- df_temp[df$order <= 15, ]  # Only keep top 10 authors
  
  all_list[[i]] <- df_temp
}

# Bind all DFs together
df.full <- do.call(rbind, all_list)
# Correct sorting of year:month
df.full$year <- factor(df.full$year, levels = unique(df.full$year))
# Create new column with year only for display
df.full <- df.full %>% separate(year, into = c(NA,"year_num"), sep = " ",remove = F)

t <- df.full %>%
  ggplot(
    aes(y = order, x = cumsum, fill = c_alph, color = c_alph)
    ) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_colh(alpha = .8) +             # Horizontal columns from ggstance
  coord_cartesian(clip = "off") +
  labs(
    x = "Published Articles",
    y = "",
    title = "Hall of Fame: Most Publications",
    subtitle = "(Co-)Authorship of papers published by the NBER",
    caption = " Data: National Bureau of Economic Research (NBER) | R package 'nberwp'"
  ) +
  guides(fill = "none", color = "none") +
  geom_text(
    aes(cumsum, order, label = as.character(cumsum)),
    hjust = -0.2
    ) +
  geom_text(
    aes(0, order, label = name),
    hjust = 1.1 , fontface = 'bold'
    ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(colour = "grey"),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    plot.margin = margin(3, 3, 3, 6, "cm")
  ) +
  transition_states(year, transition_length = 2, state_length = 0) +
  geom_text(
    aes(x = max(cumsum),y = 8,label = year_num),
    hjust = -.5, alpha = 0.4, col = "gray", size = 8
  ) +
  view_follow(fixed_y = c(15, 1)) +
  exit_fade() + enter_fade() +
  ease_aes('linear')

# one frame for every year:month
leng <-length(unique(df.full$year))

fin <- animate(t, nframes = leng, detail = 10, fps = 10,  width = 700, height = 700, rewind = FALSE )
anim_save(animation = fin,filename = "NBER_barbplotrace.gif")

