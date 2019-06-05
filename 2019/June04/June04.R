library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

# Get top 5 countries based on number of observations in data
ramen_ratings %>%
  count(brand) %>%
  top_n(10) %>%
  pull(brand) -> top10brands

plot_data <- filter(ramen_ratings, brand %in% top10brands)

# Custom color palette
custom_col = brewer.pal(4, "OrRd")
custom_col = colorRampPalette(custom_col)(10)

ggplot(plot_data,
       aes(x = brand, y = stars, fill=brand)) +
  geom_violin(alpha = 0.6) +
  geom_hline(yintercept=5, alpha=0.7) +
  scale_fill_manual(values = custom_col) +
  theme_few() +
  theme(legend.position = "NA",
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.subtitle = element_text(size=11, face="italic", color = "gray50"),
        axis.title = element_text(size=12, face="bold"),
        plot.caption = element_text(size=8, hjust=0, face = "italic", color = "gray50")) +
  labs(
       x = "Brand",
       y = "Rating",
       title = "Ramen ratings by brand",
       subtitle = "The distribution of ratings for the 10 brands with the most observations in the data. 5 is the best rating for the ramen.",
       caption = "Source: Ramen Rater | @lena2nguyen")
