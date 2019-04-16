library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)

# read in data
data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/marijuana_search_rates.csv")

# Marijuana was legalized in both CO and WA on November 6, 2012
# So setting legalization date to end of December 2012
legalization <- as.Date("2012-12-31")

# Make graph for Washington and Colorado
data %>% filter(state %in% c("CO", "WA")) %>%
  ggplot(aes(x = quarter, y = search_rate, colour = driver_race)) +
  geom_line() +
  geom_vline(xintercept = legalization, linetype="dashed",
               color = "gray50") +
  scale_color_manual(name = "Driver's Race",
                       values=c("#CD5C5C", "#E69F00", "#56B4E9")) +
  labs(x = "Year",
      y = "Search Rate",
      caption = "SOURCE: STANFORD OPEN POLICING PROJECT") +
  scale_x_date(date_breaks = "1 year", date_labels="%Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~state) +
  theme_light() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank())

# Graph other states in the that did not legalize marijuana in 2012 elections
data %>% filter(state %in% c("AZ", "CA", "FL", "MA",
                             "MT", "NC", "OH", "RI",
                             "SC", "TX", "VT", "WI")) %>%
  ggplot(aes(x = quarter, y = search_rate, colour = driver_race)) +
  geom_line() +
  geom_vline(xintercept = legalization, linetype="dashed",
             color = "gray50") +
  scale_color_manual(name = "Driver's Race",
                     values=c("#CD5C5C", "#E69F00", "#56B4E9")) +
  labs(x = "Year",
       y = "Search Rate",
       caption = "SOURCE: STANFORD OPEN POLICING PROJECT") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~state, scales="free") +
  theme_light() +
  theme(
    axis.title = element_text(face = "bold"),
    panel.grid = element_blank())
