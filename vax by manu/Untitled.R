library(tidyverse)
library(RSocrata)
library(janitor)

urlz <- "https://data.cdc.gov/resource/unsk-b7fc.json"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble() -> vaxmfr

lookup <- read_csv("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid_random/vax by manu/jurisdiction lookup.csv")

vaxmfr %>%
  left_join(lookup, by = "location") -> vax
vax <- vax[,c(ncol(vax), 1:ncol(vax)-1)]
vax$date <- as.Date(vax$date)

vax %>%
  group_by(date) %>%
  summarise(
    administered_pfizer = sum(as.numeric(administered_pfizer), na.rm=T),
    administered_moderna = sum(as.numeric(administered_moderna), na.rm=T)
  ) %>%
  ungroup() %>%
  mutate(
    administered_pfizer_new = administered_pfizer - lag(administered_pfizer),
    administered_moderna_new = administered_moderna - lag(administered_moderna)
    )-> out
  
ggplot(out) +
  tidyquant::geom_ma(aes(x = date, y = as.numeric(administered_pfizer_new)), n = 7, linetype = 1, color = "blue") +
  tidyquant::geom_ma(aes(x = date, y = as.numeric(administered_moderna_new)), n = 7, linetype = 1, color = "red")


class(vax$administered_pfizer)

