library(tidyverse)
library(RSocrata)

remotes::install_github("quartzsoftwarellc/tidyiddr")
data <- tidyiddr::nervss()
data$date <- as.Date(data$rep_week_date, format("%m/%d/%y"))

# centered 3 week moving average
corona <- subset(data, data$virus =="Coronavirus")
corona %>%
  dplyr::select(date, subtype, value) %>%
  tidyr::pivot_wider(names_from = "subtype", values_from = "value") -> corona
  


urlz <- "https://healthdata.gov/resource/j8mb-icvb.json"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

RSocrata::read.socrata(
    urlz,
    app_token = tokenz,
    #####
    email     = "tim.wiemken@gmail.com",
    password  =  "ThisIsNotAGoodP@ssw0rd!!!" 
  ) -> covid

covid %>%
  mutate(
    date = as.Date(date)
  ) %>%
  group_by(date, overall_outcome) %>%
  summarise(
    tests = sum(as.numeric(new_results_reported), na.rm=T),
  ) %>%
  filter(
    overall_outcome != "Inconclusive"
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = "overall_outcome", values_from = "tests") %>%
  janitor::clean_names() %>%
  arrange(date) %>%
  mutate(
    neg.sum = cumsum(negative),
    pos.sum = cumsum(positive),
    weekday = lubridate::wday(date, label=T)
  ) %>%
  filter(
    weekday == "Sat"
  ) %>%
  mutate(
    sarscov2 = pos.sum / (neg.sum + pos.sum) *100,
    sarscov2 = zoo::rollmean(sarscov2, k=3, align = "center", na.pad = T)
  ) -> covid
  


df <- merge(corona, covid, by = "date", all.x=T)
df <- subset(df, !is.na(df$sarscov2))

df %>%
  dplyr::select(date, co_vhku1, co_vnl63, co_voc43, co_v229e, sarscov2) %>%
  tidyr::pivot_longer(cols = c(2:6), names_to = "virus", values_to = "pct.pos") -> df


idx <- seq(1,(nrow(df)/length(unique(df$virus))), by = 3)
labz.x <- seq.Date(min(df$date), max(df$date), by = "3 weeks")

par(mar=c(6,4.5,1,4.5))
plot(
  df$pct.pos[df$virus==unique(df$virus)[1]], 
  type = "l", 
  col = RColorBrewer::brewer.pal(n=5, "Set1")[1],
  xaxt="n",
  yaxt="n",
  xlab = "",
  ylab = "",
  ylim = c(0,5)
)
lines(
  df$pct.pos[df$virus==unique(df$virus)[2]], 
  type = "l", 
  col = RColorBrewer::brewer.pal(n=5, "Set1")[2],
  xaxt="n",
  yaxt="n",
  xlab = "",
  ylab = "",
  ylim = c(0,5)
)
lines(
  df$pct.pos[df$virus==unique(df$virus)[3]], 
  type = "l", 
  col = RColorBrewer::brewer.pal(n=5, "Set1")[3],
  xaxt="n",
  yaxt="n",
  xlab = "",
  ylab = "",
  ylim = c(0,5)
)
lines(
  df$pct.pos[df$virus==unique(df$virus)[4]], 
  type = "l", 
  col = RColorBrewer::brewer.pal(n=5, "Set1")[4],
  xaxt="n",
  yaxt="n",
  xlab = "",
  ylab = "",
  ylim = c(0,5)
)
axis(side = 1, 
     at = idx, 
     labels = labz.x,
     las = 2,
     cex.axis = 0.9)
axis(side = 2, 
     at = seq(0,5, by = 0.5), 
     labels = sprintf(seq(0,5, by = 0.5), fmt = '%#.1f'),
     las = 2,
     cex.axis = 0.9)
mtext(side = 2, line = 3, text = "Percent Positivity, Seasonal Coronaviruses")
legend(
  1,3, 
  legend = c("HKU-1", "NL-63", "OC-43", "229-E", "SARS-CoV-2"),
  col = RColorBrewer::brewer.pal(n=5, "Set1"),
  lty = rep(1, times=5),
  cex=0.9
)
par(new=T)
plot(
  df$pct.pos[df$virus==unique(df$virus)[5]], 
  type = "l", 
  col = RColorBrewer::brewer.pal(n=5, "Set1")[5],
  xaxt="n",
  yaxt="n",
  xlab = "",
  ylab = "",
  ylim = c(0,10)
)
axis(side = 4, 
     at = seq(0,10, by = 1), 
     labels = sprintf(seq(0,10, by = 1), fmt = '%#.1f'),
     las = 2,
     cex.axis = 0.9)
mtext(side = 4, line = 3, text = "Percent Positivity, SARS-CoV-2")





  

