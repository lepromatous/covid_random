library(tidyverse)
library(vroom)
library(janitor)
library(tidyquant)
library(plotly)
library(cowplot)

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


plotit <- function(state = "Alabama"){
  p <- ggplot() + 
    geom_ma(data=jhu2[jhu2$province_state == state,], aes(x=date, y=new_cases), n=7, linetype=1) +
    labs(
      x="",
      y = "New Cases",
    ) +
    ggtitle(state)  +
    theme(
      axis.text.x = element_text(angle=90),
      panel.background = element_blank(),
      axis.line = element_line("black", size=0.2),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y =element_blank(),
      panel.grid.major.x = element_line("black", 0.05) )
 return(p)
}



x <- lapply(state.name, function(x) plotit(x))

x[7]


# library(cowplot)
# plot_grid(
#   x,
#   labels = "", ncol = 6
# ) -> p2

plot_grid(plotlist = x[1:12], labels=state.name[1:12], label_size = 8, hjust=-2)
plot_grid(plotlist = x[13:24], labels=state.name[13:24], label_size = 8, hjust=-2)
plot_grid(plotlist = x[25:36], labels=state.name[25:36], label_size = 8, hjust=-2)
plot_grid(plotlist = x[37:50], labels=state.name[38:50], label_size = 8, hjust=-2)

east <- plot_grid(plotlist = x[c(7, 20, 21, 29, 30, 32, 38, 45, 46)])
save_plot("~/Desktop/east.png", east, base_width=8, base_height=8)

midwest <- plot_grid(plotlist = x[c(13, 14, 15,  21, 23, 35)])
save_plot("~/Desktop/midwest.png", midwest, base_width=8, base_height=8)

plot_grid(plotlist = x[c(7,13, 21,30, 32, 45)], labels="", label_size = 8, hjust=-2)


