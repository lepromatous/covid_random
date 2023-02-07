library(RSocrata)

## "https://data.cdc.gov/Public-Health-Surveillance/Rates-of-COVID-19-Cases-or-Deaths-by-Age-Group-and/3rge-nu2a/data
urlz <- "https://data.cdc.gov/resource/3rge-nu2a.json"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

df <- read.socrata(
  urlz,
  app_token = tokenz,
  #####
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!" 
)


df %>%
  janitor::clean_names() %>%
  filter(
    !is.na(age_adj_vax_ir),
    outcome == "case",
    vaccine_product != "all_types"
  ) -> df

library(MMWRweek)
df$week <- MMWRweek2Date(MMWRyear = as.numeric(substr(df$mmwr_week, start=1, stop=4)), 
              MMWRweek= as.numeric(substr(df$mmwr_week, start=5, stop=6)), 
              MMWRday = NULL)

df$age_adj_vax_ir<-as.numeric(as.character(df$age_adj_vax_ir))

set.seed(1234)
ggplot() + 
  geom_smooth(data=df, 
              aes(x=week, y=age_adj_vax_ir, 
                  colour = factor(vaccine_product), 
                  group=vaccine_product),
              fullrange=F,
              se=T,
              method = "gam", formula = y ~ s(x, bs = "cs")) + 
  #scale_y_continuous(limits = c(-1000, 1200)) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(n=3, "Set1")) +
  labs(
    x = "\nDate",
    y = "Age Adjusted COVID-19 Death Rate Per 100,000 \nby Vaccine Manufacturer \n",
    colour = "Vaccine Manufacturer"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line("lightgray", 0.2),
    panel.grid.minor.y = element_line("lightgray", 0.2),
    axis.line = element_line("black", 0.4)
    
    
  )




#remotes::install_github("quartzsoftwarellc/shiny.quartz")

