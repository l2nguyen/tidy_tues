library("tidyverse")
library("here")

data <- read_csv(here("StarWars.csv"), col_names = TRUE)

#---- CLEAN DATA ------#
# clean up column names
colnames(data) <- paste(colnames(data), data[1,])
colnames(data) <- gsub("^X[0-9][0-9]? ", "", colnames(data))
colnames(data) <- gsub(" Response$", "", colnames(data))

# Manual cleaning up of column names
names(data)[4] <- "Star Wars: Episode I The Phantom Menace"
names(data)[10] <- "Star Wars: Episode I  The Phantom Menace"
names(data)[16] <- "Hans Solo"
names(data)[32] <- "Fan of the Expanded Universe"
names(data)[33] <- "Star trek fan"
names(data)[10:15] <- paste(names(data)[10:15], "Rank")

# drop first column
data <- data[-1,]

str(data)

# Change character columns into factor for easier data manipulation
data %>% mutate_if(is.character, as.factor) -> data



