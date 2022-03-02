library(tidyverse)
library(vroom)


df <- vroom("/Users/timothywiemken/Downloads/test-burden-4.csv")

df %>%
  janitor::clean_names() %>%
  mutate(
    fips = stringr::str_pad(fips_code, side="left", width=5, pad="0")
     ) %>%
  filter(
    state_name=="California"
  ) %>%
  group_by(state_name, testing_end_date) %>%
  summarise(
    total.test.state = sum(total_test_results_reported_7_day_count_change),
    state.pop = state_population
    ) %>%
  distinct(testing_end_date, .keep_all = T) %>%
  ungroup()  %>%
  group_by(testing_end_date) %>%
  mutate(
    total.tests  = sum(total.test.state, na.rm=T),
    total.pop = sum(state.pop)
    )  %>%
  ungroup() %>%
  distinct(testing_end_date, .keep_all = T) %>%
  mutate (
   rate = total.tests / total.pop *100000
 ) -> out


ggplot() +
  geom_line(data=out, aes(x=testing_end_date, y=rate)) +
  scale_y_continuous(limits=c(0,10000)) +
  scale_x_date(breaks = out$testing_end_date) +
  labs(
    y= "Rate Per 100,000 Population\n",
    x="\n Day Ending"
  ) + 
 theme(
   panel.background = element_blank(),
   axis.text.x = element_text(angle=90),
   panel.grid = element_line(color="gray80", size=0.2),
   axis.line = element_line(color="gray30", size=0.8)
 )
  

