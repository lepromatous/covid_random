library(tidyverse)
library(vroom)
library(janitor)
library(tidyquant)
library(plotly)

jhu <- vroom::vroom("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")


jhu <- jhu[,c(7, 12:ncol(jhu))]

jhu %>%
  pivot_longer(
    data = ., cols = 2:ncol(jhu),
    names_to = "week",
    values_to = "count"
  ) %>%
  mutate(
    date = as.Date(week, format="%m/%d/%y")
  ) %>%
  janitor::clean_names() %>%
  group_by(province_state, date) %>%
  summarise(
    cases = sum(count, na.rm=T)
  ) %>%
  ungroup() %>%
  group_by(province_state) %>%
  arrange(date) %>%
  mutate(
    new_cases = cases - lag(cases)
  ) %>%
  filter(
    province_state %in% state.name
  ) -> jhu2


jhu2 %>%
  filter(
    province_state %in% c("Connecticut", "Maryland", "Massachusetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Vermont", "Virginia")
  ) %>%
  group_by(date) %>%
  summarise(
    new_cases = sum(new_cases, na.rm=T)
  ) -> east

jhu2 %>%
  filter(
    province_state %in% c("Illinois", "Indiana", "Iowa", "Michigan",  "Minnesota", "Ohio")
  ) %>%
  group_by(date) %>%
   summarise(
    new_cases = sum(new_cases)
  ) -> midwest
 


plotit <- function(df){
  p <- ggplot() + 
    geom_ma(data=df, aes(x=date, y=new_cases), n=7, linetype=1) +
    labs(
      x="",
      y = "Total Number of New Cases",
    ) +
    scale_y_continuous(label = comma) +
    theme(
      axis.text.x = element_text(angle=90),
      panel.background = element_blank(),
      axis.line = element_line("black", size=0.2),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y =element_blank(),
      panel.grid.major.x = element_line("black", 0.05) )
  return(p)
}


eastplot <- plotit(df = east)
save_plot("~/Desktop/east.png", eastplot, base_width=8, base_height=4)

midwestplot <- plotit(df = midwest)
save_plot("~/Desktop/midwest.png", midwestplot, base_width=8, base_height=4)


max(jhu2$date)

