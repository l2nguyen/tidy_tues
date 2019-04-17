library(tidyverse)
library(ggthemes)

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

max_women <- women_research %>%
  group_by(country) %>%
  filter(percent_women==max(percent_women)) %>%
  arrange(percent_women) %>%
  ungroup() %>%
  mutate(country = factor(country))

women_research_fct <- mutate(women_research,
                             field = factor(field)) %>%
  filter(field!="Women inventores")

# Make plot
ggplot(women_research_fct,
       aes(x = country, y = percent_women)) +
  # Line to the maximum point
  geom_segment(data = max_women, aes(x = fct_reorder(country, percent_women),
                                     xend = fct_reorder(country, percent_women),
                                     y = 0, yend = percent_women), color="gray40") +
  # Line at 50%
  geom_hline(yintercept = 0.5, color = "red", size = 0.5, alpha = 0.6) +
  # point for fields
  geom_point(aes(color=field), size=3, alpha=0.8) +
  geom_hline(yintercept = 0, color="gray20") +
  coord_flip() +
  scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6), limits = c(0,0.6),
                     labels = scales::percent_format(accuracy=1)) +
  scale_color_brewer(palette = "Set1") +
  theme_fivethirtyeight() +
  labs(title = "Still a man's world",
       subtitle = "Women among researchers with papers published, 2011-2015, % of total in:",
       caption = "Source: Economist") +
  theme(
    plot.title = element_text(size=14, face = "bold", hjust=0),
    plot.subtitle = element_text(size=10, face="italic", hjust=0),
    plot.caption = element_text(size = 8, hjust = 0, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_text(face="bold"),
    axis.line.x.bottom = element_line(colour="black"),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.margin = margin(0,0,0,0),
    legend.background = element_blank(),
    legend.direction = "horizontal"
  )
