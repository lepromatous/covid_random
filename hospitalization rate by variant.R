


# number hosp: https://ourworldindata.org/grapher/weekly-hospital-admissions-covid
hosp <- vroom::vroom("/Users/timothywiemken/Downloads/weekly-hospital-admissions-covid.csv")
hosp <- subset(hosp, hosp$Entity == "United States")
#number cases: https://ourworldindata.org/grapher/weekly-covid-cases
case <- vroom::vroom("/Users/timothywiemken/Downloads/weekly-covid-cases.csv")
case <- subset(case, case$Entity == "United States")

# merge and compute rate
df <- merge(hosp, case, by = "Day", all.x=T)
df$hosp.rate <- df[,4] / df[,7] *100

# variant data
# get URL and use token to scrape - select USA region only and weighted ----
urlz <- "https://data.cdc.gov/resource/jr58-6ysp.json?usa_or_hhsregion=USA&time_interval=weekly&modeltype=smoothed"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!"
) %>%
  tibble() -> variants

lookup <- vroom::vroom("~/Desktop/lookup.csv")
variants <- merge(variants, lookup, by = "variant", all.x=T)
variants$share <- as.numeric(variants$share)
variants$share <- variants$share*100
variants$share <- round(variants$share,2)

variants %>%
  group_by(week_ending, variant) %>%
  arrange(creation_date) %>%
  slice(n()) -> variants


variants %>%
  group_by(who_label, week_ending) %>%
  summarise(
    share = sum(share, na.rm=T)
  ) -> variants

### still DQ issues.  seems like they didnt update some variants after others were updated
variants$share[variants$share>100] <- 100

#timelines from variants - selected manually where >80%
delta <- c("2021-07-31", "2021-12-11")
BA1 <- c("2022-01-01", "2022-03-05")
BA2 <- c("2022-04-09", "2022-06-04")
BA45 <- c("2022-07-09", "2022-09-03")


df$variant <- ifelse(
  df$Day >= "2021-07-31" & df$Day <= "2021-12-11", "delta",
    ifelse(df$Day >= "2022-01-01" & df$Day <= "2022-03-05", "BA1",
      ifelse(df$Day >= "2022-04-09" & df$Day <= "2022-06-04", "BA2",
        ifelse(df$Day >= "2022-07-09" & df$Day <= "2022-09-03", "BA45", NA))))


df %>%
  group_by(variant) %>%
  summarise(
    mean = mean(hosp.rate)
  )
