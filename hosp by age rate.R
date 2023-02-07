library(vroom)
library(dplyr)
library(tidyr)
library(vroom)
library(qcensus)

df <- vroom::vroom("/Users/timothywiemken/Downloads/csv-2.csv")

df <- df[,-ncol(df)]

df %>%
  janitor::clean_names() -> df

df %>%
  pivot_wider(id_cols = "week", names_from = "age_group", values_from = "cases" ) -> df

df$hosp_50 <- rowSums(df[,c(2:5)])
df$hosp_65 <- rowSums(df[,c(3:5)])
df$pop_50 <- qcensus::qcensus(2022, c(50:100))
df$pop_65 <- qcensus::qcensus(2022, c(65:100))
df$rate50 <- df$hosp_50/df$pop_50 *100000
df$rate65 <- df$hosp_65/df$pop_65 *100000

mean(df$hosp_50[df$week>="2022-08-06"]) / qcensus::qcensus(2022, c(50:100)) *100000
mean(df$hosp_65[df$week>="2022-08-06"]) / qcensus::qcensus(2022, c(65:100)) *100000




