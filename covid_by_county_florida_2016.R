library(ggplot2)

source(here::here("cleaning_data.R"))

# Graphing Data -----------------------------------------------------------

# graphing data from picture of histogram and bar chart look at ggpubr::ggscatterhist()

# Cases
graph_data_cases <-
  main_data_cases %>%
  mutate(party = as.factor(party)) %>%
  filter(case_count > 0)


# Filter for cases above 0 to help focus on important data
norm_case_data <-
  norm_case_data %>%
  filter(normalized_cases > 0)

# Refactor to correct colors
norm_case_data$party <- factor(
  norm_case_data$party,
  levels = c("republican", "democrat")
)

# Graph scatter plot
norm_case_data %>%
  ggplot(aes(x = date, y = normalized_cases, group = county, color = party)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Normalized cases over time",
    subtitle = "Cases/Population of County = Normalized",
    x = "Date",
    y = "Normalized Cases"
  )
# + facet_wrap(~party)

# Deaths final clean up and graph -----------------------------------------

# deaths normalized graph
death_norm_data <-
  death_norm_data %>%
  mutate(party = as.factor(party)) %>%
  filter(death_count > 0)

# Re factor Party
death_norm_data$party <- factor(
  death_norm_data$party,
  levels = c("republican", "democrat")
)

death_norm_data %>%
  ggplot(aes(x = date, y = deaths_normalized, group = county, color = party)) +
  geom_line() +
  geom_point()

# ggplot; color by party


lm()

?lm


# Notes -------------------------------------------------------------------

# Numbers from cases are just low numbers; mobility isn't standardized.
ggplot(norm_case_data, aes(x = date)) +
  geom_point(aes(y = mobility, color = "mobility")) +
  geom_point(aes(y = normalized_cases, color = "normalized_cases"))

# Filters for democrat
dem_norm_case_data <-
  norm_case_data %>%
  filter(party == "democrat")

# Filters for Hillsborough & gets rid of NA's in mobility
hills_case_data <-
  dem_norm_case_data %>%
  filter(county == "Hillsborough") %>%
  filter(!is.na(mobility))

# How to fit a loess regression to a plot
hills_case_data %>%
  ggplot(aes(x = date, y = normalized_cases)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE)

# The relationship between mobility and cases in Hillsborough
library(patchwork)

p1 <- hills_case_data %>%
  ggplot(aes(x = normalized_cases, y = mobility)) +
  geom_point() +
  geom_smooth(method = "loess")

p2 <- hills_case_data %>%
  ggplot(aes(x = normalized_cases, y = mobility)) +
  geom_point() +
  geom_smooth(method = "loess") +
  coord_flip()

p3 <- p1 + p2
p3

# styler:::style_active_file() for styling code

# add pretty labels (x, y , title, subtitle)
# experiment with different types of graphs (x and y axis)
# get a story together about the jumps
# Keep track of where you get the data
# population data: https://www.florida-demographics.com/counties_by_population
