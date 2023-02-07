# Load packages ----
library(tidyverse)
library(janitor)
library(prophet)
library(vroom)

# Read data ----
jhu <- vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# Keep relevant variables ----
jhu <- jhu[,c(5,7, 12:ncol(jhu))]

jhu %>% 
  pivot_longer(
    data = ., cols = 3:ncol(jhu),
    names_to = "week",
    values_to = "count"
  ) %>%
  mutate(
    date = as.Date(week, format="%m/%d/%y"),
    FIPS = stringr::str_pad(FIPS, side="left", width=5, pad="0")
  )  %>%
  janitor::clean_names() -> df
rm(jhu)
gc()

# read census regions ----
vroom::vroom("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv") %>%
  janitor::clean_names() -> regions

# merge census regions ----
df %>%
  left_join(
    regions, by=c("province_state" = "state")
  ) %>%
  select(
    -c(state_code)
    ) %>%
  rename(
    cumulative_cases_by_county = "count",
    state = "province_state"
    ) %>%
  group_by(fips) %>%
  arrange(date) %>%
  mutate(
    new_cases_by_county = cumulative_cases_by_county - lag(cumulative_cases_by_county),
    ) %>%
  replace_na(new_cases_by_county, 0) %>%
  ungroup() %>%
  filter(
    state %in% state.name
    ) %>%
  arrange(
    fips, date
    )-> df
rm(regions)
gc()








