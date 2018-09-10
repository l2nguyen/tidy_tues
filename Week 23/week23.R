library("tidyverse")
library("shiny")

data_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-04/fastfood_calories.csv"

food <- read_csv(data_url) %>% select(-X1, -salad)

str(food)
head(food)

