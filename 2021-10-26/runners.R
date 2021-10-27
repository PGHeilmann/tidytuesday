library(tidyverse)

# ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

ultra_rankings <-  read_csv('https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv', col_types = c("ddccdcc"))
ultra_rankings$Time <- parse_time(ultra_rankings$Time)

colnames(ultra_rankings)[1] <- "race_year_id"

str(race)
str(ultra_rankings)

table(round(race$distance))
hist(round(race$distance), nclass = 50)

race_ar <- race %>%  
  filter(distance > 150 & distance <= 175) %>% 
  mutate(net_elev = elevation_gain + elevation_loss ) %>% 
  arrange(distance, net_elev) %>% 
  mutate(ordered = row_number())

table(round(race_ar$distance))
hist(round(race_ar$distance), nclass = 50)

race_ar$distance <- round(race_ar$distance)

rank_and_race <- ultra_rankings %>% 
  left_join(race_ar, "race_year_id") %>% 
  filter(!is.na(Time),
         str_detect(participation, regex("Solo", ignore_case = T))) %>%  
  mutate(time = as.numeric(Time)/60/60)

race_Qs <- 
rank_and_race %>%
  group_by(race_year_id) %>% 
  summarise_at("Rank", max) %>% 
  filter(Rank > 100) %>% 
  mutate(top10 = 10,
         q10   = round(Rank*.1),
         q25   = round(Rank*.25),
         q50   = round(Rank*.50),
         q75   = round(Rank*.75),
         q90   = round(Rank*.90))

rank_and_race <- rank_and_race %>% 
  select(c("race_year_id","Rank","time", "date", "distance")) %>% 
  left_join(race_Qs, by = "race_year_id") %>% 
  drop_na()

T10 <- rank_and_race %>% filter(Rank.x <= top10) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "T10")
Q10 <- rank_and_race %>% filter(Rank.x > top10 & Rank.x <= q10) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "Q10")
Q25 <- rank_and_race %>% filter(Rank.x > q10 & Rank.x <= q25) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "Q25")
Q50 <- rank_and_race %>% filter(Rank.x > q25 & Rank.x <= q50) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "Q50")
Q75 <- rank_and_race %>% filter(Rank.x > q50 & Rank.x <= q75) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "Q75")
Q90 <- rank_and_race %>% filter(Rank.x > q75 & Rank.x <= q90) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "Q90")
Qmax <- rank_and_race %>% filter(Rank.x > q90) %>% group_by(distance) %>% summarise_at("time",mean) %>% mutate(Type = "Qmax")

full.df <- rbind(Qmax,Q90,Q75,Q50,Q25,Q10,T10)
full.df$Type <- fct_inorder(full.df$Type)

ymin <- full.df %>% group_by(Type) %>% filter(distance == 175) %>% select("time")

ymin$time
ymin$y <- seq(32,14,-3)

background <- tibble(x = seq(155,174,1),
                     xend = seq(156,175,1),
                     y = rep(58,20),
                     co = rep(c("grey20","grey30"),10))
background.txt <- tibble(x = seq(155,175,2),
                         y = rep(58,11))

library(RColorBrewer);library(colorspace)
cols1 <- colorRampPalette(c("brown","grey90","gold"))(7)
cols2 <- colorRampPalette(c("brown","gold"))(7)
dcols <- lighten(cols2,amount = .1)

p <- 
ggplot(full.df, aes(x = distance, y = time, fill = Type)) +   
  geom_rect(data = background,inherit.aes = F, aes( xmin=x,xmax=xend,ymin=0,ymax=y, group = co), fill = background$co) +
  geom_segment(aes(x=155,xend=155,y= 0,yend= 58), col = "grey30", lty = 1, lwd = 1) +
  geom_text(data = background.txt,inherit.aes = F, aes(x=x,y=y, label = x), col = "grey70", vjust = 0,hjust = 0.5, nudge_y = .5) +
  geom_text(aes(x=178,y= 58, label = "Distance in km"), col = "grey70", hjust = 0.5, nudge_y = .5, size = 6, fontface = "bold") +
  
  geom_segment(aes(x=153,xend=174,y=48,yend=48), col = "grey90", lty = 2,lwd = 1) +
  stat_smooth(geom = 'area', method = 'loess', span = 1/10, alpha = .9) + 
  stat_smooth(geom = 'line', method = 'loess', span = 1/10, aes(col = Type), lwd = 1, lty = 1) + 

  geom_curve(data = ymin, inherit.aes = F,aes(x = 175, y = ymin$time, xend = 178, yend = ymin$y),curvature = .2, col=dcols, lwd = 1) +
  geom_point(data = ymin, inherit.aes = F,aes(x = 178, y = ymin$y), col=dcols, size = 2) +
  geom_text( data = ymin, inherit.aes = F,aes(x = 178, y = ymin$y, label = rev(c("Top 10","Q10","Q25","Q50","Q75","Q90","Qmax"))),
            col=dcols, fontface = "bold", hjust = 0, nudge_x = .5, size = 6) +
  
  geom_segment(aes(x=153,xend=174,y= 0,yend= 0), col = "grey90", lty = 2, lwd = 1) +
  geom_segment(aes(x=153,xend=174,y=36,yend=36), col = "grey90", lty = 2) +
  geom_segment(aes(x=153,xend=174,y=24,yend=24), col = "grey90", lty = 2) +
  geom_segment(aes(x=153,xend=174,y=12,yend=12), col = "grey90", lty = 2) +
  
  geom_text(aes(x=153,y=48, label = "48 hours"), col = "grey90", vjust = 0,hjust = 0, nudge_y = .5) +
  geom_text(aes(x=153,y=36, label = "36 hours"), col = "grey90", vjust = 0,hjust = 0, nudge_y = .5) +
  geom_text(aes(x=153,y=24, label = "24 hours"), col = "grey90", vjust = 0,hjust = 0, nudge_y = .5) +
  geom_text(aes(x=153,y=12, label = "12 hours"), col = "grey90", vjust = 0,hjust = 0, nudge_y = .5) +
  geom_text(aes(x=153,y=0, label = "0"), col = "grey90", vjust = 0, hjust = 0, nudge_y = .5) +
  
  xlim(c(150,180)) + # ylim(c(0,70)) +
  scale_fill_manual(values = cols2) +
  scale_color_manual(values = dcols) +
  theme_void() + 
   theme(legend.position = "none",
         plot.background = element_rect(fill = "grey20", colour = NA ) )


library(patchwork)
library(ggtext)

f <-
p + plot_annotation(title = "How long does it take different runners to complete an ultra running event?",
                    subtitle = "Ultra running describes all races longer than a marathon (42 km). <br>
                    Over 300 different events with 21 different running distances were analysed in respect to the average time required to finish the race.<br>
                    Runners were split into different groups according to their finishing rank. <br> <br>
                    <b> Example:</b> <br>
                    <b style='color:#FFDB51;'>Top 10</b> depicts the average time required to finish a race for ranks 1 to 10 over all races of the same distance.<br>
                    <b style='color:#F6C021;'>Q10</b> is the average time required to finish for the best ranking 10%, excluding the <b style='color:#FFDB51;'>Top 10</b>.<br>
                    <b style='color:#EAA528;'>Q25</b> is the average time required to finish for the best ranking 25%, excluding the <b style='color:#F6C021;'>Q10</b> and the <b style='color:#FFDB51;'>Top 10</b> and so on. <br>",
                    caption = "Graphic by Philipp Heilmann | Source: Benjamin Nowak/International Trail Running Association (ITRA)", 
                    theme = theme(plot.title = element_markdown(size = 18, hjust = 0, face = "bold", color = "grey90"),
                                  plot.subtitle = element_markdown(size = 14, hjust = 0, color = "grey90"),
                                  plot.caption  = element_markdown(size = 14, color = "grey90"),
                                  plot.background = element_rect(fill = "grey20"),
                                  plot.margin = margin(1,1,1,1,"cm")
                                  ))

ggsave(filename = "runners.png",f, dpi = 200, height = 12, width = 16)

