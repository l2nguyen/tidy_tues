library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv") %>%
  mutate(min_playtime = ifelse(min_playtime == 0, 5, min_playtime),
         max_playtime = ifelse(max_playtime == 0, 5, max_playtime))

board_games %>%
  mutate(avg_playtime_hrs =
           round(((min_playtime + max_playtime) / 2)/ 60,
                 digits = 2)) %>%
  mutate(playtime_cats = cut(avg_playtime_hrs,
                             breaks = c(0, 0.49, 1.99, 4.99, 9.99,
                                        max(avg_playtime_hrs)),
                             labels = c("<30 minutes", "30 mins to 2 hrs",
                                        "<2 hrs to 5 hrs","<5 hrs to 10 hrs",
                                        "10+ hrs"))) %>%
  ggplot(aes(x = playtime_cats, y = average_rating, fill = playtime_cats)) +
  geom_violin() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(x = "Average Playtime",
       y = "Average User Rating",
       title = "Average Board Game Rating by Length of Playtime",
       caption = "SOURCE: BOARD GAME GEEK") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(size = 7, margin = margin(t = 10), color = "grey20")
    )
