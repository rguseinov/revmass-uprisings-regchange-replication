# FROM DICTATORSHIP TO DEMOCRACY? THE INFLUENCE OF REVOLUTIONARY MASS UPRISINGS ON REGIME CHANGE: A CROSS-COUNTRY PERSPECTIVE
# HSE UNIVERSITY. FACULTY OF SOCIAL SCIENCES. BACHELOR THESIS
# 
# Author:
# Ruslan Guseinov, riguseynov@edu.hse.ru
# 
# Visualizations script
#
# This analysis is performed using R version 4.5.0

#### Part 0. Load packages and data ####
library(devtools)
library(wpp2024)
library(countrycode)
library(vdemdata)
library(democracyData)
library(dplyr)
library(tidyr)
library(wbstats)
library(logistf)
library(brglm2)
library(mice)
library(naniar)
library(stargazer)
library(sjPlot)
library(pglm)
library(PanelMatch)
library(Amelia)
library(vcd)
library(peacesciencer)
library(states)
library(lmtest)
library(sandwich)
library(panelView)
library(fect)
library(ggplot2)
library(AER)
library(dagitty)
library(ggdag)
library(tibble)
library(stringr)

#### Part 1. Visualization 1 ####
#All campaigns from NAVCO 2.1
df_nvc2.1 <- read.csv('Desktop/Работа и учеба/Диплом/NAVCO2.1_vis.csv') %>% 
  filter(loc_cow == targ_cow) %>% 
  rename(cow = loc_cow) %>% 
  mutate(region = countrycode(cow, 'cown', 'region')) %>% 
  select(cow, year, region, NVC2.1_campaign)
df_beis <- read.csv('Desktop/Работа и учеба/Диплом/df_beis.csv') %>% 
  filter(beis_campaign == 1) %>% 
  select(cow, year, region, beis_campaign)

dfpivot = df_nvc2.1 %>% 
  group_by(region) %>% 
  count(NVC2.1_campaign) %>% 
  filter(NVC2.1_campaign == 1) %>%
  ungroup() %>% 
  mutate(NVC2.1 = n / sum(n) * 100) %>% 
  select(-n, NVC2.1_campaign)

dfpivot2 = df_beis %>% 
  group_by(region) %>% 
  count(beis_campaign) %>% 
  filter(beis_campaign == 1) %>%
  ungroup() %>% 
  mutate(beis = n / sum(n) * 100) %>% 
  select(-n, beis_campaign)
dfpivot = merge(dfpivot, dfpivot2, by.x = 'region', by.y = 'region', all = TRUE)
dfpivot = dfpivot %>% 
  select(-c(NVC2.1_campaign, beis_campaign))
#then create a pivot_longer table, where the columns become the rows
long_df = dfpivot %>% 
  group_by(region) %>% 
  pivot_longer(cols = c("NVC2.1", 'beis'), names_to = 'campaigns', values_to = 'value')
#save pivot table
ggplot(long_df, aes(x = region, y = value, fill = campaigns)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = paste(as.character(round(value, 1)),'%', sep=""), y = value + 0.05),
            position = position_dodge(0.9),
            vjust = 0)+
  theme_bw()+
  labs(x = 'Region', y = 'Share of events', fill = 'Datasets')+
  scale_fill_discrete(labels = c("Revolutionary Episodes\nDataset", 'NAVCO 2.1'))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'df_coverage.jpeg')  

#### Part 2. Visualization 2 ####
#Only regime change campaign from NAVCO 2.1
df_nvc2.1 <- read.csv('Desktop/Работа и учеба/Диплом/NAVCO2.1_vis.csv') %>% 
  filter(loc_cow == targ_cow,
         camp_goals == 0) %>% 
  rename(cow = loc_cow) %>% 
  mutate(region = countrycode(cow, 'cown', 'region')) %>% 
  select(cow, year, region, NVC2.1_campaign)
df_beis <- read.csv('Desktop/Работа и учеба/Диплом/df_beis.csv') %>% 
  filter(beis_campaign == 1) %>% 
  select(cow, year, region, beis_campaign)

dfpivot = df_nvc2.1 %>% 
  group_by(region) %>% 
  count(NVC2.1_campaign) %>% 
  filter(NVC2.1_campaign == 1) %>%
  ungroup() %>% 
  mutate(NVC2.1 = n / sum(n) * 100) %>% 
  select(-n, NVC2.1_campaign)

dfpivot2 = df_beis %>% 
  group_by(region) %>% 
  count(beis_campaign) %>% 
  filter(beis_campaign == 1) %>%
  ungroup() %>% 
  mutate(beis = n / sum(n) * 100) %>% 
  select(-n, beis_campaign)
dfpivot = merge(dfpivot, dfpivot2, by.x = 'region', by.y = 'region', all = TRUE)
dfpivot = dfpivot %>% 
  select(-c(NVC2.1_campaign, beis_campaign))
#then create a pivot_longer table, where the columns become the rows
long_df = dfpivot %>% 
  group_by(region) %>% 
  pivot_longer(cols = c("NVC2.1", 'beis'), names_to = 'campaigns', values_to = 'value')
#save pivot table
ggplot(long_df, aes(x = region, y = value, fill = campaigns)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = paste(as.character(round(value, 1)),'%', sep=""), y = value + 0.05),
            position = position_dodge(0.9),
            vjust = 0)+
  theme_bw()+
  labs(x = 'Region', y = 'Share of events', fill = 'Datasets')+
  scale_fill_discrete(labels = c("Revolutionary Episodes\nDataset", 'NAVCO 2.1'))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'df_coverage_regchange.jpeg')  
