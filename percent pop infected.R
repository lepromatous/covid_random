library(tidyverse)
library(vroom)
library(janitor)

census <- vroom("https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d1_mid.csv")
census %>%
  janitor::clean_names() %>%
  filter(sex == 0, origin==0, race==0, year %in%c(2022)) %>%
  rowwise() %>%
  mutate(
    pop0_4 = sum(pop_0, pop_1, pop_2, pop_3, pop_4),
    pop5_11 = sum(pop_5, pop_6, pop_7, pop_8, pop_9, pop_10, pop_11),
    pop12_15 = sum(pop_12, pop_13, pop_14, pop_15),
    pop16_17 = sum(pop_16, pop_17),
    pop18_29 = sum(pop_18, pop_19, pop_20, pop_21, pop_22, pop_23, pop_24, pop_25, pop_26, pop_27, pop_28, pop_29),
    pop30_39 = sum(pop_30, pop_31, pop_32, pop_33, pop_34, pop_35, pop_36, pop_37, pop_38, pop_39),
    pop40_49 = sum(pop_40, pop_41, pop_42, pop_43, pop_44, pop_45, pop_46, pop_47, pop_48, pop_49),
    pop50_64 = sum(pop_50, pop_51, pop_52, pop_53, pop_54, pop_55, pop_56, pop_57, pop_58, pop_59, pop_60, pop_61, pop_62, pop_63, pop_64),
    pop65_74 = sum(pop_65, pop_66, pop_67, pop_68, pop_69, pop_70, pop_71, pop_72, pop_73, pop_74),
    pop75 = sum(pop_75, pop_76, pop_77, pop_78, pop_79, pop_80, pop_81, pop_82, pop_83, pop_84, pop_85, pop_86, pop_87, pop_88, pop_89, pop_90, pop_91, pop_92, pop_93, pop_94, pop_95, pop_96, pop_97, pop_98, pop_99, pop_100)
  ) %>%
  select(107:116) -> census

census2 <- data.frame(t(census))




df <- vroom::vroom("/Users/timothywiemken/Downloads/COVID-19 Cases and Hospitalizations Averted with Vaccine-18.csv", col_select = c(1:3))
census2$age_group <- unique(df$Age_Group)



df %>%
  janitor::clean_names() %>%
  group_by(age_group) %>%
  summarise(
    total.cases = sum(corrected_cases)
  ) %>%
    arrange(total.cases) -> tot

out <- merge(tot, census2, by="age_group")
out$pct <- out$total.cases/out$t.census.


out$age_group<- factor(out$age_group, levels=c(unique(df$Age_Group)))



ggplot() + 
  geom_col(data=out, aes(x=age_group, y=pct), fill = "#0093d0", color="black") +
  scale_y_continuous(limits=c(0,.30), labels = scales::percent, expand = c(0.01,0)) + 
  geom_text(data=out, aes(x=age_group, y= pct, label=paste0(round(pct*100,1), "%")), vjust=1.6, color="white", size=3.5)+  
  scale_x_discrete(expand=c(0.05,0.05)) + 
  labs(
    y="Percent of Population Infected\n",
    x="\nAge Group"
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line("gray30", 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line("gray80", 0.2),
    panel.grid.minor.y = element_line("gray80", 0.2),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1)
  )




