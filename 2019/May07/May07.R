library(tidyverse)
library(plotly)
library(wbstats) # package to interact with WDI API
library(httr)
library(jsonlite)
library(RColorBrewer)

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
  filter(!is.na(gdp_per_cap) & !is.na(student_ratio)) %>%
  mutate(gdp_per_cap = round(gdp_per_cap, digits = 0),
         student_ratio = round(student_ratio, digits = 1))

#-------------- PLOT --------------------#

# make dataset of primary for latest year
primary <- ratio_joined %>%
  filter(indicator=="Primary Education") %>%
  filter(year == 2016)

# scale down bubble sizes
slope <- 8e-6
primary$size <- sqrt(primary$pop * slope)

# set colors
pal <- brewer.pal(6, name = "Set1")

plot_ly(data = primary, x = ~gdp_per_cap, y = ~student_ratio,
        color = ~region, size = ~size,
        type = 'scatter', mode = 'markers',
        colors = pal,
        sizes = c(min(primary$size), max(primary$size)),
        hoverinfo = 'text',
        text = ~paste0('Country: ', country,
                      '<br>Student to teacher ratio: ', student_ratio,
                      '<br>GDP per capita: ', "$", gdp_per_cap),
        marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.5,
                      line = list(color = 'rgb(0, 0, 0)',
                                  width = 1))
        ) %>%
  layout(title = list(text = "<i>Primary School Student Teacher Ratio by GDP Per Capita for 2016</i>", size = 14),
         xaxis = list(title = 'GDP per capita (constant 2010 US$)',
                      type = "log",
                      gridcolor = 'rgb(255, 255, 255)',
                      zerolinewidth = 1),
         yaxis = list(title = 'Student to teacher ratio (Primary School)',
                      gridcolor = 'rgb(255, 255, 255)',
                      zerolinewidth = 1,
                      gridwith = 2,
                      range = c(0, 95)),
         legend = list(orientation = 'h', y = -0.15),
         annotations = list(x = 1, y = -0.1,
                            text = "Source: UNESCO & World Bank",
                            showarrow = F,
                            xref = 'paper', x = 0,
                            yref = 'paper', y = -0.3,
                            font=list(size=8)),
         paper_bgcolor = 'rgb(240, 240, 240)',
         plot_bgcolor = 'rgb(240, 240, 240)'
         )
