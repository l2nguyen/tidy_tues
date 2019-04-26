library(tidyverse)
library(lubridate)
library(ggridges)

# load data
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv",
                              # do not load synopsis and background columns
                              col_types = cols(
                                synopsis = col_skip(),
                                background = col_skip()
                              ))

tv_anime <- tidy_anime %>%
  # keep only necessary variables
  select(name, type, animeID, episodes, start_date,
         rating, score, scored_by, favorites) %>%
  filter(!is.na(animeID)) %>%
  # remove duplicates
  distinct(animeID, .keep_all = TRUE) %>%
  # keep only tv animes scored by at 1000 people
  filter(type=="TV",
         scored_by>1000) %>%
  mutate(rating = factor(rating),
         start_date = ymd(start_date),
         score_bins = cut(score, breaks = c(1,4.99,7.99,10), labels=c("low", "average", "high"))
         )

tv_anime %>%
  # reorder factors by frequency for graphing
  mutate(rating = fct_rev(fct_infreq(rating))) %>%
  ggplot(aes(x = rating, fill=score_bins)) +
  geom_bar(stat="count") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "TV Anime by Age Rating",
    caption = "Source: Tam Nguyen and MyAnimeList.net via Kaggle"
  ) +
  theme(
    plot.title = element_text(size = 14, face="bold", hjust=0),
    plot.caption = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
