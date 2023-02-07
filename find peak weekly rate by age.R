### data from covid_prevention rate download 

case.df <- vroom::vroom("/Users/timothywiemken/Downloads/cases.csv")
library(tidyverse)
 case.df %>%
   mutate(
     week = as.Date(Week, format="%m/%d/%y")
   ) %>%
   group_by(week)%>%
   summarise(
     cases = sum(Corrected_Cases, na.rm=T)
   ) -> df

 df$pop <- 288189807 

 df$rate <- df$cases / df$pop *100000 

 library(plotly) 
p <- ggplot() +
  geom_line(data= df, aes(x=week, y=rate, group=1))

ggplotly(p)



case.df <- vroom::vroom("/Users/timothywiemken/Downloads/COVID-19 Cases and Hospitalizations Averted with Vaccine (2).csv")
library(tidyverse)
case.df %>%
  mutate(
    week = as.Date(Week, format="%m/%d/%y")
  ) %>%
  group_by(week)%>%
  summarise(
    cases = sum(Cases, na.rm=T)
  ) -> df

df$pop <- 288189807 

df$rate <- df$cases / df$pop *100000 

library(plotly) 
p_hosp <- ggplot() +
  geom_line(data= df, aes(x=week, y=rate, group=1))

ggplotly(p_hosp)
max(case.df$Week)
