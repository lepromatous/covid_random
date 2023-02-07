library(dplyr)
library(RSocrata)

urlz <- "https://data.cdc.gov/resource/8xkx-amqh.json"

tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble() -> df


df1 <- subset(df, df$fips == "01001")



df %>%
  mutate(
    date = as.Date(date),
    moyr = zoo::as.yearmon(date)
  ) %>%
  arrange(date) %>%
  group_by(fips, moyr) %>%
    filter(
      row_number() == n() 
    ) %>%
  dplyr::select(date, moyr, fips, booster_doses, series_complete_yes) %>%
  ungroup() %>%
  group_by(moyr) %>%
  summarise(
    series_complete_yes = sum(as.numeric(series_complete_yes), na.rm=T),
    booster_doses = sum(as.numeric(booster_doses), na.rm=T)
  ) %>%
  mutate(
    new_completions = series_complete_yes - lag(series_complete_yes),
    new_booster = booster_doses - lag(booster_doses)
  ) %>%
  ungroup() -> test
  



test %>%
  filter(
    moyr >= "Feb 2022"
  ) -> test2

library(ggplot2)

ggplot() +
  geom_line(data = test2, aes(x = moyr, y = new_completions)) +
  zoo::scale_x_yearmon(n=10) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4000000), n.breaks=10) +
  ylab("Newly Completed Primary Series \n") +
  theme(
    axis.text.x = element_text(angle=90),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x =  element_blank(),
    panel.grid.major.y = element_line("gray90", size =0.2),
    axis.line = element_line("black", size = 0.4)
  )

