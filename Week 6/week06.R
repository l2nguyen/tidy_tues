library("tidyverse")
library("WDI")
library("here")
library("countrycode")

#-- Load starbucks data downloaded from kaggle
starbucks <- read_csv(here("directory.csv")) %>%
  # Keep only relevant columns
  select(City, Country, Longitude, Latitude)

head(starbucks)

# Count number of starbucks in each city
countries <- starbucks %>%
  # Get total number of Starbucks in every country
  group_by(Country) %>%
  summarise(n_stores = n()) %>%
  rename(iso2c = Country) %>%
  # Replace iso2c country code with country name
  mutate(Country = countrycode(iso2c, "iso2c", "country.name")) %>%
  arrange(desc(n_stores))

#-- Plot 1: Number of starbucks in each country for top 10
p1 <- ggplot(head(countries, 10), aes(x = reorder(Country, n_stores), y = n_stores)) +
  geom_bar(stat = "identity", fill = "#00704a") +
  geom_text(aes(label = scales::comma(n_stores)),
            nudge_y = 600) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 15000), expand = c(0,0)) +
  labs(title = "Total Number of Starbucks Locations in Each Country",
       x = "Country",
       y = "Total Starbucks locations",
       caption = "Source: Kaggle and World Bank") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 7, margin = margin(t = 10), color = "grey20"),
    axis.ticks.y = element_blank())

p1

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
sb_percap <- countries %>%
  # join with population data
  left_join(pop_2016, by = "iso2c") %>%
  # calculate store for each million person
  mutate(store_pmil = round(n_stores/(population/1000000), digits = 1)) %>%
  # Two country column is confusing, drop the iso2c code "Country" column
  select(-Country) %>%
  arrange(desc(store_pmil))

# --- Plot 2: plot number of starbucks location, adjusted for population
p2 <- ggplot(head(sb_percap, 15), aes(x = reorder(country, store_pmil), y = store_pmil)) +
  geom_bar(stat = "identity", fill = "#00704a") +
  geom_text(aes(label = store_pmil),
            nudge_y = -2, col = "white") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 60), expand = c(0,0)) +
  labs(title = "Starbucks Locations, Adjusted for Population",
       x = "Country",
       y = "Stores per million people",
       caption = "Source: Kaggle and World Bank") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),
    axis.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 7, margin = margin(t = 10), color = "grey20"),
    axis.ticks.y = element_blank())

p2
