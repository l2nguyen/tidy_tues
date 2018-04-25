library("tidyverse")
library("here")

salaries <- read_csv(here("week4_australian_salary.csv"), col_names = TRUE)

# Top 15 highest paying job for men (assuming it is highest paying jobs)
highest_paying <- salaries %>%
  filter(gender == "Male") %>%
  filter(individuals > 20) %>%
  top_n(wt = average_taxable_income, n = 20) %>%
  select(occupation)

top_15 <- salaries %>%
  filter(occupation %in% highest_paying$occupation) %>%
  select(-c(X1, individuals, gender_rank))

top_diff <- salaries %>%
  # filter only those occupations with over 50 observations
  filter(individuals > 20) %>%
  # Drop first two columns
  select(-c(X1, individuals, gender_rank)) %>%
  # Transform from long to wide with a column each for male and female income
  spread(key = gender, value = average_taxable_income) %>%
  # keep occupations with only enough observations for both
  filter(!is.na(Male) & !is.na(Female)) %>%
  # find the pay gap in dollars and percentage of male salary
  mutate(pay_gap_perc = round((Female/Male * 100), digits = 1)) %>%
  # Filter to only include jobs where women are paid less than 70% of men
  filter(pay_gap_perc < 70)

# filter original dataset to include occupations with top 20 pay gap difference
highlight <- filter(top_15, occupation %in% top_diff$occupation)

p <- ggplot(top_15, aes(x = average_taxable_income/1000, y = occupation)) +
  geom_line(aes(group = occupation), alpha = 0.5) +
  geom_point(aes(color = gender), size = 1.25, alpha = 0.5) +
  geom_line(data = highlight, aes(group = occupation)) +
  geom_point(data = highlight, aes(color = gender), size = 2) +
  theme_minimal() +
  scale_x_continuous(limits = c(150, 600)) +
  scale_color_manual(values = c("#7BCEF8", "#233FBB")) +
  labs(title = "Gender Pay Gap for Top 15 Highest Paying Jobs",
       y = "Occupation",
       x = "Average Gross Annual Salary \n (Thousands of A$)",
       caption = "Source: Australian Tax Data") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.4),
    axis.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 6, margin = margin(t = 10), color = "grey30"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.direction = "horizontal"
    )


p
