library("tidyverse")
library("readxl")
library("WDI")
library("here")

#-- Load coffee data
starbucks <- readxl::read_xlsx(here("week6_coffee_chains.xlsx")) %>%
  # only interested in Starbucks
  filter(Brand == "Starbucks") %>%
  # Keep only relevant columns
  select(City, Country, Longitude, Latitude)

head(starbucks)

#-- Get population data for 2016
# Note: This uses population data from the World Bank

# Find WDI indicator for total population
WDIsearch(string = "population, total", field = "name", short = TRUE, cache = NULL)

# Get population data for every country for 2016
pop_2016 <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2016, end = 2016, extra = TRUE) %>%
  # WDI data has region aggregate, keep only country data
  filter(region != "Aggregates") %>%
  # Keep only columns we want
  select(iso2c, country, SP.POP.TOTL, region, income) %>%
  rename(population = SP.POP.TOTL)

str(pop_2016)

#--- Make data frame with Starbucks per capita for every country
sb_percap <- starbucks %>%
  # Get total number of Starbucks in every country
  group_by(Country) %>%
  summarise(n_stores = n()) %>%
  # join with population data
  left_join(pop_2016, by = c("Country" = "iso2c")) %>%
  # calculate store for each million person
  mutate(store_pmil = round(n_stores/(population/1000000), digits = 1)) %>%
  # Two country column is confusing, drop the iso2c code "Country" column
  select(-Country) %>%
  arrange(desc(store_pmil))

ggplot(head(sb_percap, 20), aes(x = reorder(country, store_pmil), y = store_pmil)) +
  geom_bar(stat = "identity", fill = "#00704a") +
  geom_text(aes(label = store_pmil),
            hjust = 1.2, col = "white") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 55)) +
  labs(title = "Starbucks locations adjusted for population",
       x = "Stores per million people",
       y = "Country",
       caption = "Source: Kaggle and World Bank")

