library(tidyverse)
library(ggridges)
library(hrbrthemes)

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
         start_date = lubridate::ymd(start_date),
         # Make scores into categorical variable
         score_bins = cut(score,
                          breaks = c(1,4.99,7.99,10),
                          labels=c("Low (5 and below)", "Average (5-8)", "High (8 and above)"))
         )

# TV Anime by age rating
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

# Score versus people scored, facted by rating
ggplot(tv_anime, aes(x=score, y=scored_by, fill=rating)) +
  geom_point(alpha=0.5) +
  facet_wrap(.~rating) +
  scale_y_log10()

#  Number of rating for each score bins
# Posted on Twitter
ggplot(tv_anime, aes(x=scored_by, y=score_bins, fill=score_bins)) +
  geom_density_ridges() +
  scale_x_log10() +
  theme_ipsum() +
  labs(
    title = "Are higher rated anime TV shows rated by more people?",
    x = "Number of users that scored",
    y = "Scores",
    caption = "Source: Tam Nguyen and MyAnimeList.net via Kaggle"
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme(
    legend.position = "none",
    axis.title = element_text(size=12, face="bold"),
    plot.caption = element_text(hjust=0, face = "italic")
  )
