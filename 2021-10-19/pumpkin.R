library(tidyverse)
library(countrycode)
library(ggtext)
library(patchwork)

# Get the data
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

# Data wrangling
pumpkins <- pumpkins %>% 
  mutate(weight_lbs = str_remove(weight_lbs, ","),
         weight_lbs = as.numeric(weight_lbs)) %>% 
  drop_na(weight_lbs)

pumpkins <- pumpkins %>% separate(id, c("year", "variety"), "-",remove = F)

# Remove countries with low participation
pumpkins <- pumpkins %>% group_by(country,variety) %>% filter( n() > 5)

# country:variety means
pumpkins <- pumpkins %>% group_by(country,variety) %>% summarise_at("weight_lbs", mean)

# Add continents
pumpkins$continent <- countrycode(pumpkins$country,"country.name.en",destination = "continent")

# Check Availability
table(pumpkins$country)
xtabs(~ country + variety, data = pumpkins)

# Each Variety for subsets
varieties <- unique(pumpkins$variety)

# Colors for continents
color.c <- c("Americas" = "#f23939","Asia" = "#78b21b","Europe" = "#1457c4", "Oceania" = "#f0ae14")

#### First plot
varieties_F <- pumpkins %>% filter(variety == varieties[1]) %>% arrange(weight_lbs) # Subset

# Text for y axis
axis_annot_text1  <- data.frame(y = c(25,50,75,100), x = rep(15,4))

# Curves for y axis
axis_annot_curve1  <- data.frame(y = c(25,50,75,100),
                                 x = rep(12,4),
                                 xend = rep(16,4))

# Annotate countries to the bars
annot_countries1 <- data.frame(countries = varieties_F$country,
                               x = 0.5:11.5,
                               y = varieties_F$weight_lbs,
                               angle = c(rep(440,8),rep(260, 4)))

p1 <-
  varieties_F %>% 
  ggplot(aes(x =0.5:11.5, y = weight_lbs, fill = continent)) + labs(title = "Field Pumpkin") +
  geom_segment(data = axis_annot_curve1, inherit.aes = F,
               aes( x = x, xend = xend,
                    y = y, yend = y)) +
  geom_text(data = axis_annot_text1,inherit.aes = F, aes( x = x, y = y, label = y), nudge_x = seq(-.1,-.4,-.1), nudge_y = 7.5, size = 3) +
  geom_text(data = annot_countries1,inherit.aes = F, aes( x = x, y = y + 50, label = countries, angle = angle - (22.5*(x-.5))), size = 3.5) +
  geom_col(width = 1, color = "white") + 
  coord_polar(clip = "off") +
  scale_x_continuous(limits = c(0,16)) +
  scale_fill_manual(values = color.c[c("Americas","Europe")],) +
  scale_y_continuous(breaks = seq(0,100,25)) +
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = yarrr::transparent("#f5ae00",.8),
                                       colour = "NA")) 


#### Second plot
varieties_L <- pumpkins %>% filter(variety == varieties[2]) %>% arrange(weight_lbs)

axis_annot_text2  <- data.frame(y = c(25,50,75,100), 
                                x = rep(13,4))

axis_annot_curve2  <- data.frame(y = c(25,50,75,100),
                                 x = rep(10,4),
                                 xend = rep(13.333,4))

annot_countries2 <- data.frame(countries = varieties_L$country,
                               x = .5:9.5,
                               y = varieties_L$weight_lbs,
                               angle = c(rep(435,7),rep(255, 3)))
p2 <-
  varieties_L %>% 
  ggplot(aes(x = .5:9.5, y = weight_lbs, fill = continent)) + labs(title = "Long Gourd") +
  geom_segment(data = axis_annot_curve2, inherit.aes = F,
               aes( x = x, xend = xend,
                    y = y, yend = y)) +
  geom_text(data = axis_annot_text2,inherit.aes = F, aes( x = x, y = y, label = y),nudge_x = seq(-.1,-.4,-.1), nudge_y = 7.5, size = 3) +
  geom_text(data = annot_countries2,inherit.aes = F, aes( x = x, y = pmin(y+50,160), label = countries, angle = angle - (27*(x-.5)) ), size = 3.5) +
  geom_col(width = 1, color = "white") + 
  coord_polar(clip = "off") +
  scale_x_continuous(limits = c(0,13.333)) +
  scale_fill_manual(values = color.c[c("Americas","Europe")],) +
  scale_y_continuous(breaks = seq(0,100,25)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = yarrr::transparent("#4ecc15",.8),
                                       colour = "NA")
  ) 


#### Third plot
varieties_P <- pumpkins %>% filter(variety == varieties[3]) %>% arrange(weight_lbs)
axis_annot_text3  <- data.frame(y = seq(250,1000,250), 
                                x = rep(23,4))

axis_annot_curve3  <- data.frame(y = seq(250,1000,250),
                                 x = rep(18,4),
                                 xend = rep(24,4))

annot_countries3 <- data.frame(countries = varieties_P$country,
                               x = 0.5:17.5,
                               y = varieties_P$weight_lbs,
                               angle = c(rep(440,12),rep(260, 6)))
p3 <-
  varieties_P %>% 
  ggplot(aes(x =0.5:17.5, y = weight_lbs, fill = continent)) + labs(title = "Giant Pumpkin") +
  geom_segment(data = axis_annot_curve3, inherit.aes = F,
               aes( x = x, xend = xend,
                    y = y, yend = y)) +
  geom_text(data = axis_annot_text3,inherit.aes = F, aes( x = x, y = y, label = y), nudge_x = seq(-.1,-.4,-.1), nudge_y = 75, size = 3) +
  geom_text(data = annot_countries3,inherit.aes = F, aes( x = x, y = y+500 , label = countries, angle = angle - (15*(x-.5)) ), size = 3.5) +
  geom_col(width = 1, color = "white") + 
  coord_polar(clip = "off") +
  scale_x_continuous(limits = c(0,24)) +
  scale_fill_manual(values = color.c) +
  scale_y_continuous(breaks = seq(0,1000,250)) +
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = yarrr::transparent("darkorange",.9),
                                       colour = "NA")
  ) 


#### Fourth plot
varieties_S <- pumpkins %>% filter(variety == varieties[4]) %>% arrange(weight_lbs)
axis_annot_text4  <- data.frame(y = seq(200,800,200), 
                                x = rep(18,4))

axis_annot_curve4  <- data.frame(y = seq(200,800,200),
                                 x = rep(14,4),
                                 xend = rep(18.6,4))

annot_countries4 <- data.frame(countries = varieties_S$country,
                               x = 0.5:13.5,
                               y = varieties_S$weight_lbs,
                               angle = c(rep(440,9),rep(260, 5)))
p4 <-
  varieties_S %>% 
  ggplot(aes(x =0.5:13.5, y = weight_lbs, fill = continent)) + labs(title = "Giant Squash") +
  geom_segment(data = axis_annot_curve4, inherit.aes = F,
               aes( x = x, xend = xend,
                    y = y, yend = y)) +
  geom_text(data = axis_annot_text4,inherit.aes = F, aes( x = x, y = y, label = y), nudge_x = seq(-.1,-.4,-.1), nudge_y = 60, size = 3) +
  geom_text(data = annot_countries4,inherit.aes = F, aes( x = x, y = y+400 , label = countries, angle = angle - (19.28*(x-.5)) ), size = 3.5) +
  geom_col(width = 1, color = "white") + 
  coord_polar(clip = "off") +
  scale_x_continuous(limits = c(0,18.666)) +
  scale_fill_manual(values = color.c[c("Americas","Asia","Europe")]) +
  scale_y_continuous(breaks = seq(0,600,200)) +
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = yarrr::transparent("orange",.9),
                                       colour = "NA")
  ) 


#### Fifth plot
varieties_T <- pumpkins %>% filter(variety == varieties[5]) %>% arrange(weight_lbs)
axis_annot_text5  <- data.frame(y = seq(1,3,1), 
                                x = rep(17,3))

axis_annot_curve5  <- data.frame(y = seq(1,3,1),
                                 x = rep(13,3),
                                 xend = rep(17.3,3))

annot_countries5 <- data.frame(countries = varieties_T$country,
                               x = 0.5:12.5,
                               y = varieties_T$weight_lbs,
                               angle = c(rep(440,8),rep(260, 5)))
p5 <-
  varieties_T %>% 
  ggplot(aes(x =0.5:12.5, y = weight_lbs, fill = continent)) + labs(title = "Tomato") +
  geom_segment(data = axis_annot_curve5, inherit.aes = F,
               aes( x = x, xend = xend,
                    y = y, yend = y)) +
  geom_text(data = axis_annot_text5,inherit.aes = F, aes( x = x, y = y, label = y), nudge_x = seq(-.1,-.3,-.1), nudge_y = .3, size = 3) +
  geom_text(data = annot_countries5,inherit.aes = F, aes( x = x, y = y+2 , label = countries, angle = angle - (20.77*(x-.5)) ), size = 3.5) +
  geom_col(width = 1, color = "white") + 
  coord_polar(clip = "off") +
  scale_x_continuous(limits = c(0,17.333)) +
  scale_fill_manual(values = color.c[c("Americas","Europe")]) +
  scale_y_continuous(breaks = seq(1,3,1)) +
  theme_void() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = yarrr::transparent("red",.9),
                                       colour = "NA")
  ) 


#### Sixth plot
varieties_W <- pumpkins %>% filter(variety == varieties[6]) %>% arrange(weight_lbs)
axis_annot_text6  <- data.frame(y = seq(50,150,50), 
                                x = rep(13,3))

axis_annot_curve6  <- data.frame(y = seq(50,150,50),
                                 x = rep(10,3),
                                 xend = rep(13.3,3))

annot_countries6 <- data.frame(countries = varieties_W$country,
                               x = 0.5:9.5,
                               y = varieties_W$weight_lbs,
                               angle = c(rep(440,7),rep(260, 3)))
p6 <- 
  varieties_W %>% 
  ggplot(aes(x =0.5:9.5, y = weight_lbs, fill = continent)) + labs(title = "Giant Watermelon") +
  geom_segment(data = axis_annot_curve6, inherit.aes = F,
               aes( x = x, xend = xend,
                    y = y, yend = y)) +
  geom_text(data = axis_annot_text6,inherit.aes = F, aes( x = x, y = y, label = y), nudge_x = seq(-.1,-.3,-.1), nudge_y = 20, size = 3) +
  geom_text(data = annot_countries6,inherit.aes = F, aes( x = x, y = pmax(110,y+60) , label = countries, angle = angle - (27*(x-.5)) ), size = 3.5) +
  geom_col(width = 1, color = "white") + 
  coord_polar(clip = "off") +
  scale_x_continuous(limits = c(0,13.333)) +
  scale_fill_manual(values = color.c[c("Americas","Europe")]) +
  scale_y_continuous(breaks = seq(50,150,50)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = yarrr::transparent("green",.9),
                                       colour = "NA")
  ) 

#### Final plot
f <-
  (p1+p2+p3)/(p4+p5+p6) + 
  plot_annotation(title = "<b style='color:#eaa311;'>Great Pumpkin Commonwealth:</b> Country Averages",
                  subtitle = "The Great Pumpkin Commonwealth (GPC) provides data on the weight/length of Field Pumpkins, Giant Pumpkins, Giant Squashs, Giant Watermelons,<br>
                                        Long Gourds and Tomatos from growers' competitions all throughout the world.
                                        Participants came from countries all over 
                                        <b style='color:#f23939;'>North America</b> , <b style='color:#78b21b;'>Asia</b>, <b style='color:#1457c4;'>Europe</b> and <b style='color:#f0ae14;'>Oceania</b>.<br> 
                                        <br>
                                        Countries with only 5 or less growers for a specific plant were removed for this plant.<br>
                                        Weights for all crops was meassured in pounds (lbs.), For Long Gourd, only length in inches (in.) was meassured <br>",
                  caption = "by **Philipp Heilmann** | Source: Great Pumpkin Commonwealth",
                  theme = theme(plot.title = element_markdown(size = 20, hjust = 0,
                                                              face = "bold"),
                                plot.subtitle = element_markdown(size = 13, hjust = 0),
                                plot.caption  = element_markdown(size = 13),
                                plot.background = element_rect(fill = yarrr::transparent("cornsilk",.6)),
                                plot.margin = margin(t = 0.5,r = 0.5,b = 1,l = 2, unit = "cm")))



ggsave(filename = "pumpkin.png",f, dpi = 200, height = 12, width = 16)
