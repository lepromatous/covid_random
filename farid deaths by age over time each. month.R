library(pacman)

pacman::p_load(tidyverse, vroom, MMWRweek)


df <- vroom::vroom("/Users/timothywiemken/Downloads/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")

df$dead <- ifelse(df$death_yn %in% c("Missing", "Unknown"), NA, 
                    ifelse(df$death_yn == "Yes", 1, 0))

df <- subset(df, !is.na(df$age_group) & df$age_group != "Missing")

unique(df$age_group)
df$age_group_fac <- factor(df$age_group, levels = c("0 - 17 years", "18 to 49 years", "50 to 64 years", "65+ years"), labels = c("0-17 years", "18-49 years", "50-64 years", "65+ years"))
table(df$age_group_fac)
 
df %>%
    filter(!is.na(dead)) %>%
    group_by(case_month, age_group) %>%
    summarise(
        deaths = sum(dead, na.rm=T),
    ) -> out

out %>%
    group_by(case_month) %>%
    mutate(
        deaths_prop = deaths / sum(deaths, na.rm=T) *100
    ) -> out


out$year <- as.numeric(substr(out$case_month, start = 1, stop = 4))
out$month <- substr(out$case_month, start = 6, stop = 7)


out$date <- as.Date(paste0(out$year, "-", out$month, "-01"))

ggplot() +
    geom_line(data = out, aes(x = date, y = deaths_prop, group = age_group, color = age_group))+
    scale_color_brewer(palette = "Set1") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(limits = c(0, 100), n.breaks=10) +
    labs(x = "", y = "Percent Deaths \n", color = "Age Group",
        title = "COVID-19 Deaths Within Age Groups by Month", 
        subtitle = "United States, 2020 - 2022") +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold", angle = 90),
        axis.text.y = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10, face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white"),
        plot.margin = unit(c(1, 0.5, 3, 0.5), "cm")
    ) 



out %>% group_by(age_group, case_month) %>% summarise(deaths = sum(deaths, na.rm=T)) -> out2
View(out2)

