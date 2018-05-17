library("tidyverse")
library("here")
library("ggridges")
library("RColorBrewer")

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
colnames(data) <- gsub("^Star Wars: ", "", colnames(data))
names(data)[4:9] <- paste(names(data)[10:15], "Watched")
names(data)[10:15] <- paste(names(data)[10:15], "Rank")

# drop first row
data <- data[-1,]

str(data)

# Change character columns into factor for easier data manipulation
data %>%
# filter only for people that have seen any star wars movie
  filter(.[[2]] == "Yes") %>%
  # Change all character data into factors
  mutate_if(is.character, as_factor) -> data

# reshape data to get ranking
data %>%
  select(Age, ends_with("Rank")) %>%
  mutate(Age = factor(Age, levels = c("18-29", "30-44", "45-60", "> 60"))) %>%
  filter(!is.na(Age)) %>%
  gather(key = "movie", value = "rank", contains("Episode")) %>%
  filter(rank == 1) %>%
  group_by(Age, movie) %>%
  summarise(count = n()) %>%
  mutate(perc = round((count/sum(count) * 100), digits = 1)) -> age_fave

p1 <- ggplot(data = age_fave, aes(x = Age, y = perc, group = movie, fill = movie)) +
  geom_bar(stat = "identity")

p1 + scale_fill_brewer(palette = "RdYlBu")
