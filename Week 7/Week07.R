library("tidyverse")
library("here")

data <- read_csv(here("StarWars.csv"), col_names=TRUE)

#---- CLEAN DATA ------#
# clean up column names
colnames(data) <- paste(colnames(data), data[1,])
colnames(data) <- gsub("^X[0-9]", "", colnames(data))
colnames(data) <- gsub("Response$", "", colnames(data))

# remove first column
data <- data[-1,]

str(data)