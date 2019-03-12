library(tidyverse)
library(here)
library(readxl)
library(stringr)

# Load data set
deaths <- read_xlsx(here("global_mortality.xlsx"))

# Clean up column names
names(deaths) <- str_trim(
  str_remove_all(names(deaths), "[[:punct:]]")
)
