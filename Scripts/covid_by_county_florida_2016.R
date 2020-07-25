library(ggplot2)
library(ggthemes)
library(dplyr)

source(here::here("Scripts","cleaning_data.R"))


# Create scaled data ------------------------------------------------------

norm_case_data <-
  norm_case_data %>%
  mutate(scaled_cases = scale(case_count),
         norm_scale_cases = scale(normalized_cases),
         mobility_scaled = scale(mobility)) %>%
  clean_names()

death_norm_data <-
  death_norm_data %>%
  mutate(scaled_deaths = scale(death_count),
         norm_scale_deaths = scale(deaths_normalized),
         mobility_scaled = scale(mobility)) %>%
  clean_names()


# cases_deaths_covid_party_mobilty <- 
#   norm_case_data %>%
#   left_join(death_norm_data) 
# 
# write.csv(cases_deaths_covid_party_mobilty, "Covid_mobility_party_and_more.csv")

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
  geom_point(alpha = .025) +
  labs(
    title = "Normalized cases over time",
    subtitle = "Cases/Population of County = Normalized",
    x = "Date",
    y = "Normalized Cases"
  )+ 
  facet_wrap(~party) +
  theme_fivethirtyeight()

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

# Graph scatter plot
death_norm_data %>%
  ggplot(aes(x = date, y = deaths_normalized, group = county, color = party)) +
  geom_line() +
  geom_point(alpha = .025) +
  labs(
    title = "Normalized Deaths over time",
    subtitle = "Deaths/Population of County = Normalized",
    x = "Date",
    y = "Normalized Deaths"
  )+ 
  facet_wrap(~party) + 
  theme_fivethirtyeight()



# Graph Scaled Data -------------------------------------------------------
norm_case_data %>% 
  ggplot(aes(x = date, y = norm_scale_cases, group = county, color = party)) +
  geom_point(alpha = .25) +
  geom_line(alpha = 1) +
  geom_point(aes(y = mobility_scaled, color = "Mobility"), alpha = .1)+
  facet_wrap(~party)+
  labs(
    title = "Scaled/Normalized cases and scaled mobility over time",
    subtitle = "By Party",
    x = "Date",
    y = "Scaled/Normalized Cases and Scaled Mobility"
  ) +
  theme_fivethirtyeight()

norm_case_data %>% 
  ggplot(aes(x = date, y = norm_scale_cases, group = county, color = party)) +
  geom_point() +
  geom_line(alpha = 1) +
  geom_point(aes(y = mobility_scaled, color = "Mobility"), alpha = .25)+
  facet_wrap(~county)+
  labs(
    title = "Normalized/Scaled cases and scaled mobility over time",
    subtitle = "By County",
    x = "Date",
    y = "Scaled Cases and Mobility"
  ) 

death_norm_data %>%
  ggplot(aes(x = date, y = norm_scale_deaths, group = county, color = party)) +
  geom_point(alpha = .5) +
  geom_line(alpha = .75) +
  geom_point(aes(y = mobility_scaled, color = "Mobility"), alpha = .2)+
  facet_wrap(~party)+
  labs(
    title = "Normalized and Scaled deaths and scaled mobility over time",
    subtitle = "By County",
    x = "Date",
    y = "Scaled and Normalized Deaths and Mobility"
  ) +
  theme_fivethirtyeight()








# # Notes -------------------------------------------------------------------
# 
# # Numbers from cases are just low numbers; mobility isn't standardized.
# 
# norm_case_data_dems <- 
#   norm_case_data %>%
#   filter(party == "democratic") %>%
#   group_by(county) %>%
#   summarise(value = max(case_count))
#   
#   
# ?geom_point
# 
# ?sec_axis
# # Filters for democrat
# dem_norm_case_data <-
#   norm_case_data %>%
#   filter(party == "democrat")
# 
# # Filters for Hillsborough & gets rid of NA's in mobility
# hills_case_data <-
#   dem_norm_case_data %>%
#   filter(county == "Hillsborough") %>%
#   filter(!is.na(mobility))
# 
# # How to fit a loess regression to a plot
# # hills_case_data %>%
# #   ggplot(aes(x = date, y = normalized_cases)) +
# #   geom_line() +
# #   geom_smooth(method = "loess", se = FALSE)
# 
# # The relationship between mobility and cases in Hillsborough
# library(patchwork)
# 
# 
# # Cases Graphs ------------------------------------------------------------
# 
# p1 <- hills_case_data %>%
#   ggplot(aes(x = normalized_cases, y = mobility)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   labs(
#     title = "Hillsborough county",
#     subtitle = "Cases and Mobility Loess Regression",
#     x = "Cases per 100,000",
#     y = "Mobility"
#   )
# 
# p2 <- hills_case_data %>%
#   ggplot(aes(x = normalized_cases, y = mobility)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   coord_flip()+
#   labs(
#     title = "Hillsborough county",
#     subtitle = "Cases and Mobility Loess Regression",
#     x = "Cases per 100,000",
#     y = "Mobility"
#   )
# 
# p3 <- p1 + p2
# p3
# 
# 
# 
# # Republican county cases example -----------------------------------------------
# 
# repub_norm_data <- 
#   norm_case_data %>%
#   filter(party == "republican")
# 
# santa_rosa_data <-
#   repub_norm_data %>%
#   filter(county == "Santa Rosa") %>%
#   filter(!is.na(mobility))
# 
# r1 <- 
#   santa_rosa_data %>%
#   ggplot(aes(x = normalized_cases, y = mobility)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   labs(
#     title = "Santa Rosa county",
#     subtitle = "Cases and Mobility Loess Regression",
#     x = "Cases per 100,000",
#     y = "Mobility"
#   )
# 
# r2 <- 
#   santa_rosa_data %>%
#   ggplot(aes(x = normalized_cases, y = mobility)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   coord_flip()+
#   labs(
#     title = "Santa Rosa county",
#     subtitle = "Cases and Mobility Loess Regression",
#     x = "Cases per 100,000, Flipped Axis",
#     y = "Mobility"
#   )
# 
# r3 <- r1 + r2
# 
# r3


# styler:::style_active_file() for styling code

# add pretty labels (x, y , title, subtitle)
# experiment with different types of graphs (x and y axis)
# get a story together about the jumps
# Keep track of where you get the data
# population data: https://www.florida-demographics.com/counties_by_population
