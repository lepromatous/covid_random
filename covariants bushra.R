library(tidyverse)
library(vroom)

covariants <- vroom::vroom("https://s3.amazonaws.com/quartzdata/datasets/covariants-country.csv")
lookup <- vroom::vroom("/Users/timothywiemken/Library/CloudStorage/OneDrive-Pfizer/Documents/Research/github/covid_random/pango_lookup_covariants.csv")


yo <- function(countriez){
covariants %>%
  filter(
      country %in% countriez
    ) %>%
  janitor::clean_names() %>%
  relocate(
    week, .before = 1
  ) %>%
  relocate(
    country, .before = 2
  ) %>%
  mutate_all(
    ~ifelse(is.na(.), 0, .)
  ) %>%
  mutate(
    week = as.Date(week, origin = "1970-01-01"),
  ) %>%
  group_by(week) %>%
  mutate(
    total = sum(c_across(where(is.numeric)), na.rm=T)
  )  %>%
  ungroup() %>%
  pivot_longer(cols = contains("_"), names_to = "variant", values_to = "vals") %>%
  left_join(
    ., lookup, by=c("variant" = "clade")
  )  %>%
  select(c(1,2,3,5,8)) %>%
  ungroup() %>%
  group_by(week, plot_lab) %>%
  summarise(
    total = sum(total),
    vals = sum(vals)
  ) %>%
  ungroup() %>%
ungroup() %>%
 mutate(
    vals= vals /total *100
  ) %>%
  pivot_wider(
    names_from = "plot_lab", values_from = "vals"
  ) %>%
  arrange(
    week
  ) %>%
  select(
    -total
  ) %>%
  mutate_all(
    ~ifelse(is.nan(.), 0, .),
    ~ifelse(is.na(.), 0, .)
  ) %>%
  group_by(week) %>%
  fill(everything(), .direction = "downup") %>%
  slice(1) -> covariants

covariants$week <- as.Date(covariants$week, origin="1970-01-01")

covariants %>%
  pivot_longer(cols = -1, names_to = "variant", values_to = "vals") -> test


#write.csv(covariants, "~/Desktop/variants.csv", row.names=F, na="")

return(test)
}


test <- yo("USA")



ggplot2::ggplot(data=test, aes(x=week, y = as.numeric(vals), colour = factor(variant))) + 
  geom_line() +
  scale_y_continuous(limits = c(0,1.01), labels = scales::percent) +
  scale_fill_manual(values = c("#b2df8a", "#1f78b4", "#33a02c", "#a6cee3", "#beaed4")) + 
  labs(
    y = "Percent of Total Variants Identified \n",
    x = "\n Week Ending",
    fill = "Pango Lineage"
  ) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%B %d %Y", expand = c(0.02, 0.02)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line("black", size = 0.3),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )




