library(tidyverse)
library(plotly)
library(wbstats) # package to interact with WDI API
library(httr)
library(jsonlite)

# load data
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# get population and GDP data from World Bank
wdi_raw <- wb(country="countries_only",
              indicator=c('NY.GDP.PCAP.KD', 'SP.POP.TOTL'),
              # start/end year based on student ratio dataset
              startdate=2012, enddate=2017, return_wide = TRUE)

#--- Get basic data for countries
# NOTE: Mostly interested in income data
country_url <- "http://api.worldbank.org/v2/countries/"

country <- GET(country_url,
               query = list(per_page = 1000, format = 'json'))

# Prettify nested JSON data from API
country_data <- fromJSON(content(country, "text"), flatten = TRUE)
country_data <- country_data[[2]] %>%
  select(id, incomeLevel.value, region.value) %>%
  rename(income_level = incomeLevel.value, region = region.value) %>%
  # keep only country data, drop region level aggregate data
  filter(region != "Aggregates")

#-------------- DATA WRANGLING --------------------#

# clean up dataset to make it easier to chart
ratio_clean <- student_ratio %>%
  mutate(indicator = as.factor(indicator)) %>%
  select(-edulit_ind, -starts_with("flag"))

# Construct dataset with all country data from World Bank
wdi_clean <- wdi_raw %>%
  # change date from character to numeric
  mutate(year = as.numeric(date)) %>%
  # rename variables from indicator name
  rename(gdp_per_cap = NY.GDP.PCAP.KD, pop = SP.POP.TOTL) %>%
  # drop obs missing gdp_per_cap or population
  filter(!is.na(gdp_per_cap) & !is.na(pop)) %>%
  # join with country data
  left_join(country_data, by = c('iso3c' = 'id')) %>%
  # keep only necessary variables
  select(-country, -iso2c)

ratio_joined <- ratio_clean %>%
  # join with student ratio data
  left_join(wdi_clean, by = c('country_code' = 'iso3c', 'year')) %>%
  filter(!is.na(gdp_per_cap) & !is.na(student_ratio))

primary <- ratio_joined %>%
  filter(indicator=="Primary Education") %>%
  filter(year == 2016)

plot_ly(data = primary, x = ~gdp_per_cap,
        y = ~student_ratio,
        color = ~region,
        type = 'scatter',
        mode = 'markers',
        hoverinfo = 'text',
        text = ~paste('Country: ', country,
                      '<br /> Student to teacher ratio: ', student_ratio,
                      '<br /> GDP per capita: ', gdp_per_cap)) %>%
  layout(xaxis = list(title = 'GDP Per Capita', type = "log"),
         yaxis = list(title = 'Student to Teacher Ratio'))
