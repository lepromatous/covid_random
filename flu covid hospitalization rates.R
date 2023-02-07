### ### ### ### ### ### ### ### ### ###
# ----------- get flu data ---------- #
### ### ### ### ### ### ### ### ### ###
library(tidyverse)
library(RSocrata)
library(tidyr)
library(MMWRweek)
library(qcensus)
library(zoo)
library(tsibble)


# https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html
flu <- readr::read_csv("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid_random/FluSurveillance_Custom_Download_Data.csv")
flu %>%
  janitor::clean_names() %>%
  filter(
    age_category == "< 18",
    sex_category == "Overall",
    race_category == "Overall"
  ) -> flu

# change to date
flu$date <- MMWRweek::MMWRweek2Date(flu$mmwr_year, flu$mmwr_week)


### ### ### ### ### ### ### ### ### ###
# ----------- get covid data ---------- #
### ### ### ### ### ### ### ### ### ###
covid <- readr::read_csv("/Users/timothywiemken/Downloads/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries.csv")
covid %>%
  filter(
    !state %in% c("AS", "VA", "VI") ## remove not states - keeps DC
  ) -> covid
covid %>%
  group_by(date) %>%
  summarize(
    total_covid_peds  = sum(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid, na.rm=T),
    pop = qcensus::qcensus(2022, 0:17),
    peds_covid_rate = total_covid_peds / pop *100000
    ) -> covid

# filter COVID data to week start that matches flu week
table(flu$date)
min(covid$date)
covid %>%
  filter(
    date >= "2020-01-05"
  ) -> covid

# sum covid to get weekly data that matches flu. 
covid %>%
  arrange(date) %>%
  mutate(
    wkyr = factor(paste0(lubridate::year(date), "-", lubridate::epiweek(date)),
                  levels = unique(paste0(lubridate::year(date), "-", lubridate::epiweek(date))))
  ) %>%
  group_by(wkyr) %>%
  summarise(
    total_covid_peds = sum(total_covid_peds)
    ) %>% 
  ungroup() -> covid

total_covid <- covid %>% pull(total_covid_peds)
date <- seq.Date(as.Date("2020-01-05"), length.out = length(total_covid), by = "1 week")
pop <- 74280423
covid_rate <-total_covid / 74280423 *100000
covid <- data.frame(date, total_covid, covid_rate)

### ### ### ### ### ### ### ### ### ###### ### ### 
# ----------- merge flu and  covid data ---------- #
### ### ### ### ### ### ### ### ### ###### ### ###
df <- merge(flu, covid, by = "date", all.y=T)


### ### ### ### ### ### ### ### ### ###
# plot 
### ### ### ### ### ### ### ### ### ###

ggplot() +
  geom_line(data=flu, aes(x=date, y = as.numeric(cumulative_rate)), color = "#0095FF") +
  geom_line(data=covid, aes(x=date, y = as.numeric(covid_rate)), color = "#67BB6E") +
  labs(
    x = "",
    y = "Hospitalization Rate Per 100,000 \n"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line("black", size = 0.2),
    axis.text.x = element_text(angle =0)
  ) +
  geom_rect(
    aes(xmin=as.Date("2012-01-01") , xmax=as.Date("2012-06-18"), ymin=39.7, ymax=40), fill = "#0095FF"
  ) +
  geom_rect(
    aes(xmin=as.Date("2012-01-01") , xmax=as.Date("2012-06-18"), ymin=37.8, ymax=38.1), fill = "#67BB6E"
  ) +
  annotate(
    "text",
    x = as.Date("2012-10-18"),
    y=40,
    label = "Flu", size = 3
  ) +
  annotate(
    "text",
    x = as.Date("2012-12-18"),
    y=38,
    label = "COVID", size = 3
  ) +
  ggtitle("Pediatric Hospitalization Rates Per 100,000 Population, Influenza vs COVID-19 \n")




