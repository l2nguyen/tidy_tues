library(tidyverse)
library(ggthemes)

brexit <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv")

brexit_manip <- brexit %>%
  gather(key = "response", value = "percent", -date) %>%
  mutate(date = lubridate::dmy(date),
         response = recode(response,
                           "percent_responding_right" ="Right",
                           "percent_responding_wrong" = "Wrong")
         )

ggplot(brexit_manip, aes(x=date, y=percent, col=response)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se=FALSE) +
  geom_text(data = filter(brexit_manip, date==max(date)),
             aes(label=response),
             nudge_y = -0.5) +
  scale_color_manual(values = c("#2554C7","#B31423")) +
  theme_economist() +
    scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(breaks=seq(38, 50, by=2),
                     limits = c(38,50)) +
  labs(
    title = "Bremorse",
    subtitle = "In hindsight, do you think it was right or wrong to vote to leave the EU?",
    y = "% responding",
    caption = "Source: Economist"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, hjust=0),
    plot.caption = element_text(hjust = 0, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(size=9, face="italic", angle = 0,
                                     margin= margin(0,-55,0,0)
                                     ),
    axis.ticks.length = unit(0.2, "cm")
    )