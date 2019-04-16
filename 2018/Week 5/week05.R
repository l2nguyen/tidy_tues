library("tidyverse")
library("ineq")
library("sp")
library("here")

data <- read_csv(here("acs2015_county_data.csv"), col_names = TRUE)

