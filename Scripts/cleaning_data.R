# Import libraries --------------------------------------------------------
library(stats)
library(janitor)
library(dplyr)
library(tidyverse)
library(lubridate)

# Reading Data ------------------------------------------------------------

# Florida election results
election_data <-
  read.csv(here::here("data","florida_county_election_results_2016.csv"))

pop_data_raw <-
  read.csv(here::here("data","population_county_data.csv"))
# %>%
# rename(Rank = "ï..Rank")

mobility_data_raw <-
  read.csv(here::here("data","applemobilitytrends-2020-07-15.csv"))

# WHO COVID Date
covid_cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid_deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"


# Read Data to CSV
deaths <- read.csv(url(covid_deaths_url), stringsAsFactors = FALSE)
cases <- read.csv(url(covid_cases_url), stringsAsFactors = FALSE)


# Cleaning Cases Data -----------------------------------------------------

cases <-
  cases %>%
  filter(Country_Region == "US" & Province_State == "Florida") %>%
  rename(County = Admin2) %>%
  select(County, X1.22.20:ncol(cases)) %>%
  filter(!County %in% c(
    "Out of FL",
    "Unassigned"
  ))

cases <- pivot_longer(cases, cols = X1.22.20:ncol(cases), names_to = "Date", values_to = "case_count")


# Remove X from date column
cases$Date <- gsub("X", "", cases$Date)

# set Date column to Date
cases$Date <- as.Date(cases$Date, format = "%m.%d.%y")

# Cleaning Deaths Data ----------------------------------------------------

deaths <-
  deaths %>%
  filter(Country_Region == "US" & Province_State == "Florida") %>%
  rename(County = Admin2) %>%
  select(County, X1.22.20:ncol(deaths)) %>%
  filter(!County %in% c(
    "Out of FL",
    "Unassigned"
  ))

# Pivot data
deaths <- pivot_longer(deaths, cols = X1.22.20:ncol(deaths), names_to = "Date", values_to = "death_count")

# Remove X from date column
deaths$Date <- gsub("X", "", deaths$Date)

# Change data type to Date of Date column
deaths$Date <- as.Date(deaths$Date, format = "%m.%d.%y")

# Cleaning Population Data ------------------------------------------------

pop_data_clean <-
  pop_data_raw %>%
  mutate(County = gsub(" County", "", County)) %>%
  clean_names()


# Cleaning Mobility Data --------------------------------------------------

# Select sub region florida only
mobility_data_clean <-
  mobility_data_raw %>%
  filter(sub.region == "Florida") %>%
  clean_names()

# Select only Florida counties
mobility_data_clean <-
  mobility_data_clean %>%
  filter(geo_type == "county") %>%
  clean_names()

# Remove County string from county, select correct columns
mobility_data_clean <-
  mobility_data_clean %>%
  mutate(region = gsub(" County", "", region)) %>%
  select(region, x2020_01_13:ncol(mobility_data_clean)) %>%
  clean_names()

# Pivot table to match formats
mobility_data_clean <-
  pivot_longer(mobility_data_clean,
    cols = x2020_01_13:ncol(mobility_data_clean),
    names_to = "Date",
    values_to = "Mobility"
  )

# Remove X character from date string
mobility_data_clean <-
  mobility_data_clean %>%
  mutate(Date = gsub("x", "", Date)) %>%
  clean_names()

# Change date column from char to date
mobility_data_clean <-
  mobility_data_clean %>%
  mutate(date = as.Date(date, format = "%Y_%m_%d"))


# Joining Election Data with COVID Data -----------------------------------

str(election_data)
election_data <-
  election_data %>%
  rename(County = "ï..Vote.by.county")

main_data_cases <-
  cases %>%
  left_join(election_data, by = "County") %>%
  clean_names()

main_data_deaths <-
  deaths %>%
  left_join(election_data, by = "County") %>%
  clean_names()

# Joining Mobility Data ---------------------------------------------------

mobility_data_clean <-
  mobility_data_clean %>%
  rename(county = region) %>%
  clean_names()

main_data_cases <-
  main_data_cases %>%
  left_join(mobility_data_clean, by = c("county", "date")) %>%
  clean_names()

main_data_deaths <-
  main_data_deaths %>%
  left_join(mobility_data_clean, by = c("county", "date")) %>%
  clean_names()


# Normalizing Counts by Pop -----------------------------------------------

norm_case_data <-
  main_data_cases %>%
  inner_join(pop_data_clean, by = "county") %>%
  mutate(population = as.integer(gsub(",", "", population))) %>% # Removes `,` and makes int
  mutate(normalized_cases = case_count / population) %>%
  clean_names()

death_norm_data <-
  main_data_deaths %>%
  inner_join(pop_data_clean, by = "county") %>%
  mutate(population = as.integer((gsub(",", "", population)))) %>%
  mutate(deaths_normalized = death_count / population) %>%
  clean_names()

