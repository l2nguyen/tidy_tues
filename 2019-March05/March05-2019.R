library(tidyverse)

earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv")

earnings_temp <- earnings_female %>% mutate(group_temp = group) %>%
    filter(group != "Total, 16 years and older") %>%
    mutate(proportion = percent / 100)

ggplot(data = earnings_temp, aes(x = Year, y = proportion)) +
geom_line(data = earnings_temp %>%
    select(-group), aes(group = group_temp),
    color = "grey75", size = 0.5, alpha = 0.5) +
    geom_line(aes(color = group), color = "#5dbfb8", size = 1) +
    labs(
        y = "Women's wages as percent of men's wages",
        x = "Year",
        title = "Changes in the Salary of Women by Age Group (1979-2011)") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme_light() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            axis.title = element_text(face = "bold"),
            panel.grid = element_blank()) +
    facet_wrap(. ~ group)
