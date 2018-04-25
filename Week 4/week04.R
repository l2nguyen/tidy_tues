library("tidyverse")
library("here")

salaries <- read_csv(here("week4_australian_salary.csv"), col_names = TRUE)

salaries$occupation <- str_replace_all(salaries$occupation, "<96>", "-")

# Top 15 highest paying job for men (assuming it is highest paying jobs)
highest_paying <- salaries %>%
  filter(gender == "Male") %>%
  filter(individuals > 15) %>%
  top_n(wt = average_taxable_income, n = 20) %>%
  select(occupation)

top_15 <- salaries %>%
  filter(occupation %in% highest_paying$occupation) %>%
  select(-c(X1, gender_rank))

pay_gap <- salaries %>%
  # filter only those occupations with over 15 observations
  filter(individuals > 15) %>%
  # Drop first two columns
  select(-c(X1, individuals, gender_rank)) %>%
  # Transform from long to wide with a column each for male and female income
  spread(key = gender, value = average_taxable_income) %>%
  # keep occupations with only enough observations for both
  filter(!is.na(Male) & !is.na(Female)) %>%
  # find the pay gap in dollars and percentage of male salary
  mutate(pay_gap_perc = round((Female/Male * 100), digits = 1))

# Filter to only include jobs where women are paid less than 50% of men
top_diff <- filter(pay_gap, pay_gap_perc < 50)

# Top 10 paying jobs where pay is equal
equal_jobs <- filter(pay_gap, pay_gap_perc >= 100) %>%
  top_n(wt = Female, 10) %>%
  select(occupation)

# Filter top paying equal jobs
top_equal <- salaries %>%
  filter(occupation %in% equal_jobs$occupation) %>%
  select(-c(X1, gender_rank))

# filter original dataset to include occupations with top pay gap difference
highlight <- filter(top_15, occupation %in% top_diff$occupation)

# Graph about gender pay gap in highest paying jobs
p <- ggplot(top_15, aes(x = average_taxable_income/1000,
                        y = fct_reorder(occupation, average_taxable_income))) +
  geom_line(aes(group = occupation), alpha = 0.5) +
  geom_point(aes(color = gender), size = 1.25, alpha = 0.5) +
  geom_line(data = highlight, aes(group = occupation)) +
  geom_point(data = highlight, aes(color = gender), size = 2) +
  theme_minimal() +
  scale_x_continuous(limits = c(100, 600)) +
  scale_color_manual(values = c("#7BCEF8", "#233FBB")) +
  labs(title = "Gender Pay Gap for Top 15 Highest Paying Jobs",
       y = "Occupation",
       x = "Average Gross Annual Salary \n (Thousands of A$)",
       caption = "Source: Australian Tax Data") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 6, margin = margin(t = 10), color = "grey30"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-5,-10,-10,-10),
    legend.background = element_blank(),
    legend.direction = "horizontal"
    )

p

# Graph about jobs where women are paid equally or more
p2 <- ggplot(top_equal, aes(x = average_taxable_income/1000,
                            y = fct_reorder(occupation, average_taxable_income))) +
  geom_line(aes(group = occupation), alpha = 0.8) +
  geom_point(aes(color = gender), size = 1.25, alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = c("#7BCEF8", "#233FBB")) +
  labs(title = "Top 10 Highest Paid Jobs that have Gender Pay Equality",
       y = "Occupation",
       x = "Average Gross Annual Salary \n (Thousands of A$)",
       caption = "Source: Australian Tax Data") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 6, margin = margin(t = 10), color = "grey30"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-5,-10,-10,-10),
    legend.background = element_blank(),
    legend.direction = "horizontal"
  )

p2
