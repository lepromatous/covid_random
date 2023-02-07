library(tidyverse)
library(vroom)

covariants <- vroom::vroom("https://s3.amazonaws.com/quartzdata/datasets/covariants-country.csv")

covariants %>%
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(
    delta_all = sum(x21a_delta, x21i_delta, x21j_delta, na.rm = T),
    omicron_all = sum(x21k_omicron, x21l_omicron)
  ) %>%
  ungroup() %>%
  janitor::clean_names() %>%
  relocate(
    week, .before = x20h_beta_v2
  ) %>%
  relocate(
    country, .before = x20h_beta_v2
  ) %>%
  select(
    -c(x21a_delta, x21i_delta, x21j_delta, x21l_omicron, x21k_omicron)
  ) %>%
  mutate_all(
    ~ifelse(is.na(.), 0, .)
  ) %>%
  mutate(
    week = as.Date(covariants$week, origin = "1970-01-01"),
  ) %>%
  rowwise() %>%
  mutate(
    total = sum(c_across(where(is.numeric)), na.rm=T)
  ) %>%
  ungroup() %>%
 
  pivot_longer(cols = contains("_"), names_to = "variant", values_to = "vals") %>%
  mutate(
    vals= vals /total *100
  ) %>%
  pivot_wider(
    names_from = "variant", values_from = "vals"
  ) %>%
  select(
    -total
  ) %>%
  mutate_all(
    ~ifelse(is.nan(.), 0, .)
  ) -> covariants

covariants$week <- as.Date(covariants$week, origin="1970-01-01")

write.csv(covariants, "~/Desktop/variants.csv", row.names=F, na="")


covariants %>%
pivot_longer(cols = contains("_"), names_to = "variant", values_to = "vals") -> test

