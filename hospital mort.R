urlz <- "https://data.cdc.gov/resource/q3t8-zr7t.json"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'
library(tidyverse)
library(RSocrata)
read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble() -> df



df %>%
  filter(
    setting == "IP" & subgroup =="Total" & indicator == "Confirmed COVID-19" & measure == "Percent"
  ) %>%
  mutate(
    date = as.Date(end_time, format="%m/%d/%Y")
    )-> pcthosp

median(pcthosp$value, na.rm=T)

ggplot(data=pcthosp, aes(x=date, y = value)) +
  geom_line()



library(tidyverse)
df %>%
  filter(
    setting == "IP" & subgroup =="Total" & indicator == "Deaths of confirmed cases with intubation or ventilator use" & measure == "Percent"
  ) %>%
  mutate(
    date = as.Date(end_time, format="%m/%d/%Y")
  )-> mort.vent
mean(mort.vent$value, na.rm=T)


library(tidyverse)
df %>%
  filter(
    setting == "IP" & subgroup =="Total" & indicator == "Deaths of confirmed cases without intubation or ventilator use" & measure == "Percent"
  ) %>%
  mutate(
    date = as.Date(end_time, format="%m/%d/%Y")
  )-> mort.novent
mean(as.numeric(mort.novent$value), na.rm=T)
