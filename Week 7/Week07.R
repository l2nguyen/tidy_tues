library("tidyverse")
library("here")
library("RColorBrewer")
library("waffle")

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
  mutate(perc = round(count/sum(count) * 100, digits = 0)) -> age_fave

# stacked bar chart
p1 <- ggplot(data = age_fave, aes(x = Age, y = perc, group = movie, fill = movie)) +
  geom_bar(stat = "identity")

p1 + scale_fill_brewer(palette = "RdYlBu")

# make colors for waffle
cols <- brewer.pal(6, "RdBu")

age_fave %>%
  select(-count) %>%
  spread(movie, perc) -> wide_age

# Make vectors for waffle charts
age18_29 <- unlist(wide_age[1, 2:7])
age30_44 <- unlist(wide_age[2, 2:7])
age45_60 <- unlist(wide_age[3, 2:7])
age61_up <- unlist(wide_age[4, 2:7])

# Make waffle graph for each group
w1 <-  waffle(age18_29, rows = 5, size = 0.5, colors = cols, pad = 1) +
  labs(title = "Favorite Star Wars Movie by Age Groups",
       subtitle = "Age 18-29") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0))

w2 <-  waffle(age30_44, rows = 5, size = 0.5, colors = cols, pad = 1) +
  labs(subtitle = "Age 30-44")

w3 <-  waffle(age45_60, rows = 5, size = 0.5, colors = cols, pad = 1) +
  labs(subtitle = "Age 45-60")

w4 <-  waffle(age61_up, rows = 5, size = 0.5, colors = cols, pad = 1, xlab = "1 Square = 1 Percent") +
  labs(subtitle = "Age 61 and older",
       caption = "Source: Surveymonkey audience") +
  theme(plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey20"),
        axis.title = element_text(size = 10, face = "bold"))

iron(w1, w2, w3, w4)

