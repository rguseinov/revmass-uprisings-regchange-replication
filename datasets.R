# FROM DICTATORSHIP TO DEMOCRACY? THE INFLUENCE OF REVOLUTIONARY MASS UPRISINGS ON REGIME CHANGE: A CROSS-COUNTRY PERSPECTIVE
# HSE UNIVERSITY. FACULTY OF SOCIAL SCIENCES. BACHELOR THESIS
# 
# Author:
# Ruslan Guseinov, riguseynov@edu.hse.ru
# 
# Datasets script
#
# This analysis is performed using R version 4.5.0

#### Part 0. Load packages and data ####
library(dplyr)
library(countrycode)
library(Amelia)
library(tidyr)
library(reshape2)
library(wpp2024)
library(vdemdata)
library(democracyData)
library(peacesciencer)
library(states)
library(wbstats)
set.seed(666)

#### Part 1. Revolutionary datasets ####
NAVCO1.3_default <- read.csv("~/Desktop/Работа и учеба/Диплом/NAVCO1.3_default .csv") #NAVCO 1.3 default structure
NAVCO1.3_panel <- NAVCO1.3_default %>%
  rowwise() %>%
  mutate(year = list(seq(BYEAR, EYEAR, by = 1))) %>%
  unnest(year) %>%
  mutate(cow = countrycode(LOCATION, 'country.name', 'cown')) %>% 
  rename(NVC1.3_campaignname = CAMPAIGNNAME) %>%
  select(-c(BYEAR, EYEAR, LOCATION, NVC1.3_campaignname))

NAVCO1.3_default <- NAVCO1.3_default %>% 
  mutate(cow = countrycode(LOCATION, 'country.name', 'cown')) %>% 
  rename(year = BYEAR,
         NVC1.3_EYEAR = EYEAR,
         NVC1.3_campaignname = CAMPAIGNNAME) %>% 
  select(!c(LOCATION, NVC1.3_campaignname))

NAVCO2.1_default <- read.csv("~/Desktop/Работа и учеба/Диплом/NAVCO2.1_default.csv") #navco 2.1 panel structure
View(NAVCO2.1_default %>% mutate(country = countrycode(loc_cow, 'cown', 'country.name'),
                                 region = countrycode(loc_cow, 'cown', 'region')) %>% 
       filter(NVC2.1_progress == 5 & NVC2.1_prim_meth == 0) %>% 
       select(NVC2.1_camp_name, loc_cow, country, region, year, NVC2.1_total_part, NVC2.1_camp_duration, NVC2.1_camp_goals))
NAVCO2.1_panel <- NAVCO2.1_default %>% 
  mutate(NVC2.1_campaign = 1,
         NVC2.1_total_part = ifelse(NVC2.1_total_part == -99, 0, NVC2.1_total_part),
         NVC2.1_VIOL= ifelse(NVC2.1_prim_meth == 0,1,0),
         NVC2.1_NONVIOL= ifelse(NVC2.1_prim_meth == 1,1,0),
         NVC2.1_failure = ifelse(NVC2.1_progress == 5,1,0),
         NVC2.1_camp_goals = NVC2.1_camp_goals + 1) %>% 
  filter(loc_cow == targ_cow,
         (NVC2.1_camp_goals >= 1 & NVC2.1_camp_goals <= 3)) %>% #filter out territorial campaigns 
  rename(cow = loc_cow) %>% 
  select(!c(targ_cow, NVC2.1_prim_meth, NVC2.1_camp_name, NVC2.1_progress))

beis_regr<- read.csv("Desktop/Работа и учеба/Диплом/beis_regr.csv", sep=",")
beis_regr <- beis_regr %>% 
  rowwise() %>% 
  mutate(beis_endyear = replace_na(beis_endyear, 2014),
         year = list(seq(beis_startyear, beis_endyear, by = 1))) %>%
  unnest(year) %>%
  rename(cow = beis_cowcode)  %>% 
  select(!c(beis_isoabb, beis_location, beis_startyear))

colpus_regr <- read.csv('Desktop/Работа и учеба/Сравнение баз данных большая статья/Базы данных (R)/colpus.csv') #coup data
colpus_regr = colpus_regr %>% 
  rename(cow = ccode,
         coup_nomil = nonmilcoup) %>% 
  mutate(coup_success = ifelse(success == 1, 1, 0),
         coup_failure = ifelse(success == 2, 1, 0),
         coup_regimechange = ifelse(type == 'CH', 1, 0),
         coup_antidemocratic = ifelse(type == 'AD', 1, 0),
         coup_attempt = 1) %>% 
  select(c(cow, year, starts_with('coup_')))

#### Part 2. Independent variables ####
data(pop1dt)
population <- pop1dt %>% #1949-2014
  mutate(cow = countrycode(name, 'country.name', 'cown'),
         iso3c = countrycode(name, 'country.name', 'iso3c'),
         pop = round(pop)) %>% 
  drop_na(cow) %>% 
  filter(!country_code %in% c(948, 2093),
         year %in% c(1945:2014)) %>% 
  select(cow, year, pop)

gdp_gapminder <- read.csv("Desktop/Работа и учеба/Berlin Wall статья/gdp_gapminder.csv", sep=";") #1800-2014
gdp_gapminder <- melt(setDT(gdp_gapminder), measure = patterns('^X'),
                      variable.name = 'year', value.name = c('gdp_pcap'))
gdp_gapminder$year = gsub("X", "", as.factor(gdp_gapminder$year))
gdp_gapminder$ISO3C <- toupper(gdp_gapminder$geo)
gdp_gapminder <- gdp_gapminder %>% 
  mutate(ISO3C = countrycode(ISO3C, "iso3c", "iso3c"),
         cow = countrycode(ISO3C, 'iso3c', 'cown')) %>% 
  rename(iso3c = ISO3C) %>%
  drop_na(cow) %>%
  filter(year %in% c(1945:2014)) %>% 
  select(cow, year, gdp_pcap)
gdp_gapminder <- gdp_gapminder %>%
  mutate(gdp_pcap = as.numeric(gdp_pcap)) %>% 
  arrange(cow, year) %>%
  group_by(cow) %>% 
  mutate(gdp_growth = ((gdp_pcap - dplyr::lag(gdp_pcap)) / dplyr::lag(gdp_pcap) * 100)) %>%  # Compute growth rate
  ungroup()

vdemdata::vdem -> vdem
vdem_vars <- vdem %>% #1789-2014
  mutate(iso3c = countrycode(country_name, 'country.name', 'iso3c')) %>%
  rename(cow = COWcode) %>%
  drop_na(cow) %>% 
  filter(year %in% c(1945:2014)) %>% 
  select(cow, year, v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_execorr, e_miinflat)
polityIV_vars <- polityIV %>% 
  mutate(cow = countrycode(polityIV_ccode, 'cown', 'cown')) %>% 
  drop_na(cow) %>% 
  filter(year %in% c(1945:2014)) %>% 
  select(cow, year, polity2, durable)

REIGN <- read.csv("Desktop/Работа и учеба/Berlin Wall статья/REIGN.csv") #1921-2014
reign <- REIGN %>% 
  mutate(iso3c = countrycode(cow, 'cown', 'iso3c')) %>%
  drop_na(cow) %>%
  filter(year %in% c(1945:2014)) %>% 
  select(cow, year, tenure)

urbanization <- wb_data('SP.URB.TOTL.IN.ZS', start_date = 1900, end_date = 2014) #1960-2014
urbanization <- urbanization %>% 
  mutate(cow = countrycode(iso3c, 'iso3c', 'cown')) %>%
  drop_na(cow) %>%
  rename(year = date) %>% 
  filter(year %in% c(1945:2014)) %>%
  select(cow, year, SP.URB.TOTL.IN.ZS)

oilrent <- wb_data('NY.GDP.PETR.RT.ZS', start_date = 1900, end_date = 2014) #1960-2014
oilrent <- oilrent %>% 
  mutate(cow = countrycode(iso3c, 'iso3c', 'cown')) %>%
  drop_na(cow) %>%
  rename(year = date) %>% 
  filter(year %in% c(1945:2014)) %>%
  select(cow, year, NY.GDP.PETR.RT.ZS)

gwf <- gwf_all_extended #1742-2010
gwf <- gwf %>% 
  rename(cow = gwf_cowcode) %>% 
  mutate(gwf_regimetype = replace_na(gwf_regimetype, 'democracy')) %>% 
  filter(year %in% c(1945:2014)) %>%
  select(cow, year, gwf_duration, gwf_regimetype, gwf_next)

cnts_2024_4_20 <- read.csv("Desktop/Работа и учеба/Berlin Wall статья/cnts_2024_4_20.csv", sep=";") #1946-2014
cnts = cnts_2024_4_20 %>%
  mutate(cow = countrycode(country, 'country.name', 'cown'),
         iso3c = countrycode(cow, 'cown', 'iso3c')) %>% 
  filter(year %in% c(1945:2014)) %>%
  drop_na(cow) %>%
  select(cow, year, area1, pop2, Mean_year_schooling_interpolated)
cnts <- cnts[!duplicated(cnts[c('cow', 'year')]), ]

data(popAge1dt)
youth <- popAge1dt %>% #1949-2014
  filter(!country_code %in% c(948, 2093)) %>% 
  filter(age >= 15) %>% 
  group_by(country_code, year) %>%
  summarise(median_age = {
    cumulative_pop <- cumsum(pop) / sum(pop)
    ages <- age
    ages[min(which(cumulative_pop >= 0.5))]
  },
  total_15_24 = sum(pop[age >= 15 & age <= 24]),
  total_15_plus = sum(pop),
  youthbulge = total_15_24 / total_15_plus * 100
  ) %>%
  rename(cow = country_code) %>% 
  #  mutate(cow = countrycode(name, 'country.name', 'cown')) %>% 
  drop_na(cow) %>%
  filter(year %in% c(1945:2014)) %>% 
  select(cow, year, median_age, youthbulge)

conscription <- read.csv("Desktop/Работа и учеба/Диплом/toronto_clean.csv") #1945-2013
conscription <- conscription %>% 
  mutate(conscript = ifelse(recruit == 0, 1, 0),
         conscript = replace_na(conscript, 0)) %>% 
  rename(cow = ccode) %>% 
  select(cow, year, conscript)
conscription <- conscription[!duplicated(conscription[c('cow', 'year', 'conscript')]), ] #delete a duplicate - Venezuela 2009 

#### Part 3. Merging ####
df0 = state_panel(start = 1945, end = 2014, by = 'year', useGW = FALSE) %>%  rename(cow = cowcode) %>% mutate(country = countrycode(cow, 'cown', 'country.name')) %>% 
  filter(!cow %in% c(260, 331, 347, 713)) #exclude non-UN members
  # filter(!cow >= 935) %>% filter(!cow %in% c(56, 57, 58, 60)) #filter islands
df1 = merge(df0, gdp_gapminder, by = c('cow', 'year'), all.x = TRUE)
df2 = merge(df1, population, by = c('cow', 'year'), all.x = TRUE)
df3 = merge(df2, vdem_vars, by = c('cow', 'year'), all.x = TRUE) 
df4 = merge(df3, cnts, by = c('cow', 'year'), all.x = TRUE)
df5 = merge(df4, reign, by = c('cow', 'year'), all.x = TRUE)
df6 = merge(df5, youth, by = c('cow', 'year'), all.x = TRUE)
df7 = merge(df6, urbanization, by = c('cow', 'year'), all.x = TRUE) 
df8 = merge(df7, oilrent, , by = c('cow', 'year'), all.x = TRUE)
df9 = merge(df8, gwf, by = c('cow', 'year'), all.x = TRUE)
df10 = merge(df9, conscription, by = c('cow', 'year'), all.x = TRUE)
df11 = merge(df10, polityIV_vars, by = c('cow', 'year'), all.x = TRUE) %>% filter(!is.na(v2x_polyarchy)) #filter out countries without democracy data. Generally - islands and Belize
df11_nvc1.3_def <- merge(df11, NAVCO1.3_default, by = c("cow", 'year'), all.x = TRUE)
df11_nvc1.3_pan <- merge(df11, NAVCO1.3_panel, by = c('cow', 'year'), all.x = TRUE)
df11_nvc2.1 <- merge(df11, NAVCO2.1_panel, by = c('cow', 'year'), all.x = TRUE)
df11_beis <- merge(df11, beis_regr, by = c('cow', 'year'), all.x = TRUE)
df11_colpus <- merge(df11, colpus_regr, by = c('cow', 'year'), all.x = TRUE)

#### Part 4. Editing datasets ####
df_nvc1.3_def = df11_nvc1.3_def %>% 
  group_by(cow) %>% 
  mutate(gdp_pcap = log(gdp_pcap+0.01),
         gdp_pcap_l = dplyr::lag(gdp_pcap, n = 1),
         gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
         pop = log(pop+0.01),
         pop_l = dplyr::lag(pop, n = 1),
         SP.URB.TOTL.IN.ZS = log(SP.URB.TOTL.IN.ZS+0.01),
         SP.URB.TOTL.IN.ZS_l = dplyr::lag(SP.URB.TOTL.IN.ZS, n = 1),
         NY.GDP.PETR.RT.ZS = log(NY.GDP.PETR.RT.ZS+0.01),
         NY.GDP.PETR.RT.ZS_l = dplyr::lag(NY.GDP.PETR.RT.ZS, n = 1),
         year = as.numeric(year),
         v2x_polyarchy = v2x_polyarchy + 0.01,
         v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
         v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
         youthbulge = log(youthbulge+1),
         youthbulge_l = dplyr::lag(youthbulge, n = 1),
         vdem_growth = ((v2x_polyarchy - dplyr::lag(v2x_polyarchy)) / dplyr::lag(v2x_polyarchy) * 100),
         vdem_growth_l = dplyr::lag(vdem_growth, n = 1),
         tenure_l = dplyr::lag(tenure, n = 1),
         polity2_growth = (polity2 - dplyr::lag(polity2)),
         NVC1.3_CAMPAIGN_l = dplyr::lag(NVC1.3_CAMPAIGN, n = 1),
         NVC1.3_NONVIOL_l = dplyr::lag(NVC1.3_NONVIOL, n = 1),
#         NVC1.3_VIOL_l = dplyr::lag(NVC1.3_VIOL, n = 1),
         NVC1.3_SUCCESS_l = dplyr::lag(NVC1.3_SUCCESS, n = 1),
#         NVC1.3_FAILURE_l = dplyr::lag(NVC1.3_FAILURE, n = 1),
         period = rep(1:12, length.out = n_distinct(year %/% 10))[match(year %/% 10, unique(year %/% 10))],
         region = countrycode(cow, 'cown', 'region')) %>%
  mutate_at(vars(starts_with('NVC1.3_'), v2x_polyarchy, vdem_growth, gdp_growth, youthbulge, youthbulge_l, gdp_pcap, pop, v2x_execorr_l, pop_l,
                 gdp_pcap_l, SP.URB.TOTL.IN.ZS_l, NY.GDP.PETR.RT.ZS_l, conscript), ~replace_na(., 0))

df_nvc1.3_pan = df11_nvc1.3_pan %>% 
  group_by(cow) %>% 
  mutate(gdp_pcap = log(gdp_pcap+0.01),
         gdp_pcap_l = dplyr::lag(gdp_pcap, n = 1),
         gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
         pop = log(pop+0.01),
         pop_l = dplyr::lag(pop, n = 1),
         SP.URB.TOTL.IN.ZS = log(SP.URB.TOTL.IN.ZS+0.01),
         SP.URB.TOTL.IN.ZS_l = dplyr::lag(SP.URB.TOTL.IN.ZS, n = 1),
         NY.GDP.PETR.RT.ZS = log(NY.GDP.PETR.RT.ZS+0.01),
         NY.GDP.PETR.RT.ZS_l = dplyr::lag(NY.GDP.PETR.RT.ZS, n = 1),
         year = as.numeric(year),
         v2x_polyarchy = v2x_polyarchy + 0.01,
         v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
         v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
         youthbulge = log(youthbulge+1),
         youthbulge_l = dplyr::lag(youthbulge, n = 1),
         vdem_growth = ((v2x_polyarchy - dplyr::lag(v2x_polyarchy)) / dplyr::lag(v2x_polyarchy) * 100),
         vdem_growth_l = dplyr::lag(vdem_growth, n = 1),
         tenure_l = dplyr::lag(tenure, n = 1),
         polity2_growth = (polity2 - dplyr::lag(polity2)),
         NVC1.3_CAMPAIGN_l = dplyr::lag(NVC1.3_CAMPAIGN, n = 1),
         NVC1.3_NONVIOL_l = dplyr::lag(NVC1.3_NONVIOL, n = 1),
#         NVC1.3_VIOL_l = dplyr::lag(NVC1.3_VIOL, n = 1),
         NVC1.3_SUCCESS_l = dplyr::lag(NVC1.3_SUCCESS, n = 1),
#         NVC1.3_FAILURE_l = dplyr::lag(NVC1.3_FAILURE, n = 1),
         period = rep(1:12, length.out = n_distinct(year %/% 10))[match(year %/% 10, unique(year %/% 10))],
         region = countrycode(cow, 'cown', 'region')) %>%
  mutate_at(vars(starts_with('NVC1.3_'),
                 v2x_polyarchy, vdem_growth, gdp_growth, youthbulge, youthbulge_l, gdp_pcap, pop, v2x_execorr_l, pop_l,
                 gdp_pcap_l, SP.URB.TOTL.IN.ZS_l, NY.GDP.PETR.RT.ZS_l, conscript), ~replace_na(., 0))


df_nvc2.1 = df11_nvc2.1 %>% 
  group_by(cow) %>% 
  mutate(total_part = ifelse(NVC2.1_total_part == -99, 0, NVC2.1_total_part),
    total_part = log(NVC2.1_total_part+0.01),
    total_part_l = dplyr::lag(total_part, n = 1),
    gdp_pcap = log(gdp_pcap+0.01),
    gdp_pcap_l = dplyr::lag(gdp_pcap, n = 1),
    gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
    pop = log(pop+0.01),
    pop_l = dplyr::lag(pop, n = 1),
    SP.URB.TOTL.IN.ZS = log(SP.URB.TOTL.IN.ZS+0.01),
    SP.URB.TOTL.IN.ZS_l = dplyr::lag(SP.URB.TOTL.IN.ZS, n = 1),
    NY.GDP.PETR.RT.ZS = log(NY.GDP.PETR.RT.ZS+0.01),
    NY.GDP.PETR.RT.ZS_l = dplyr::lag(NY.GDP.PETR.RT.ZS, n = 1),
    year = as.numeric(year),
    v2x_polyarchy = v2x_polyarchy + 0.01,
    v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
    v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
    youthbulge = log(youthbulge+1),
    youthbulge_l = dplyr::lag(youthbulge, n = 1),
    vdem_growth = ((v2x_polyarchy - dplyr::lag(v2x_polyarchy)) / dplyr::lag(v2x_polyarchy) * 100),
    vdem_growth_l = dplyr::lag(vdem_growth, n = 1),
    tenure_l = dplyr::lag(tenure, n = 1),
    polity2_l = dplyr::lag(polity2, n = 1),
    #         polity_growth = ((polity2 - dplyr::lag(polity2)) / dplyr::lag(polity2) * 100),
    polity2_growth = (polity2 - dplyr::lag(polity2)),
    NVC2.1_campaign_l = dplyr::lag(NVC2.1_campaign, n = 1),
#    NVC2.1_VIOL_l = dplyr::lag(NVC2.1_VIOL, n = 1),
    NVC2.1_NONVIOL_l = dplyr::lag(NVC2.1_NONVIOL, n = 1),
#    NVC2.1_failure = ifelse(NVC2.1_success == 0, 1, 0),
    NVC2.1_nonviolsucc = ifelse(NVC2.1_NONVIOL == 1 & NVC2.1_success == 1, 1, 0),
#    NVC2.1_violsucc = ifelse(NVC2.1_VIOL == 1 & NVC2.1_success == 1, 1, 0),
    v2x_polyarchy_l1 = dplyr::lead(v2x_polyarchy, n = 1),
    v2x_polyarchy_l2 = dplyr::lead(v2x_polyarchy, n = 2),
    v2x_polyarchy_l5 = dplyr::lead(v2x_polyarchy, n = 5),
    v2x_polyarchy_l10 = dplyr::lead(v2x_polyarchy, n = 10),
    vdem_growth_l1 = dplyr::lead(vdem_growth, n = 1),
    vdem_growth_l2 = dplyr::lead(vdem_growth, n = 2),
    vdem_growth_l5 = dplyr::lead(vdem_growth, n = 5),
    period = rep(1:12, length.out = n_distinct(year %/% 10))[match(year %/% 10, unique(year %/% 10))],
    region = as.factor(countrycode(cow, 'cown', 'region')),
    county = as.factor(country)) %>%
  mutate_at(vars(starts_with("NVC2.1_"),
                 v2x_polyarchy, vdem_growth, gdp_growth, youthbulge, youthbulge_l, gdp_pcap, pop, v2x_execorr_l, pop_l,
                 gdp_pcap_l, SP.URB.TOTL.IN.ZS_l, NY.GDP.PETR.RT.ZS_l, conscript), ~replace_na(., 0)) %>% 
  filter(year >= 1945)

df_beis = df11_beis %>% 
  group_by(cow) %>% 
  mutate(total_part = ifelse(beis_particnum == -99, 0, beis_particnum),
    total_part = log(beis_particnum+0.01),
    total_part_l = dplyr::lag(total_part, n = 1),
    gdp_pcap = log(gdp_pcap+0.01),
    gdp_pcap_l = dplyr::lag(gdp_pcap, n = 1),
    gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
    pop = log(pop+0.01),
    pop_l = dplyr::lag(pop, n = 1),
    SP.URB.TOTL.IN.ZS = log(SP.URB.TOTL.IN.ZS+0.01),
    SP.URB.TOTL.IN.ZS_l = dplyr::lag(SP.URB.TOTL.IN.ZS, n = 1),
    NY.GDP.PETR.RT.ZS = log(NY.GDP.PETR.RT.ZS+0.01),
    NY.GDP.PETR.RT.ZS_l = dplyr::lag(NY.GDP.PETR.RT.ZS, n = 1),
    year = as.numeric(year),
    v2x_polyarchy = v2x_polyarchy + 0.01,
    v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
    v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
    youthbulge = log(youthbulge+1),
    youthbulge_l = dplyr::lag(youthbulge, n = 1),
    vdem_growth = ((v2x_polyarchy - dplyr::lag(v2x_polyarchy)) / dplyr::lag(v2x_polyarchy) * 100),
    vdem_growth_l = dplyr::lag(vdem_growth, n = 1),
    tenure_l = dplyr::lag(tenure, n = 1),
    polity2_l = dplyr::lag(polity2, n = 1),
    #         polity_growth = ((polity2 - dplyr::lag(polity2)) / dplyr::lag(polity2) * 100),
    polity2_growth = (polity2 - dplyr::lag(polity2)),
    beis_campaign_l = dplyr::lag(beis_campaign, n = 1),
#    beis_armed_l = dplyr::lag(beis_armed, n = 1),
    beis_armednocivwar_l = dplyr::lag(beis_armednocivwar, n = 1),
    beis_unarmed = ifelse(beis_armed == 0, 1, 0),
    beis_unarmed_l = dplyr::lag(beis_unarmed, n = 1),
    beis_unarmednocivwar = ifelse(beis_armednocivwar == 0, 1, 0),
    beis_unarmednocivwar_l = dplyr::lag(beis_unarmednocivwar, n = 1),
    beis_urbancivic_l = dplyr::lag(beis_urbancivic, n = 1),
    beis_failure = ifelse(beis_success == 0, 1, 0),
#    beis_failure_l = dplyr::lag(beis_failure, n = 1),
    beis_democrat_l = dplyr::lag(beis_democrat, n = 1),
    beis_antimonarch = dplyr::lag(beis_antimonarch, n = 1),
    v2x_polyarchy_l1 = dplyr::lead(v2x_polyarchy, n = 1),
    v2x_polyarchy_l2 = dplyr::lead(v2x_polyarchy, n = 2),
    v2x_polyarchy_l5 = dplyr::lead(v2x_polyarchy, n = 5),
    v2x_polyarchy_l10 = dplyr::lead(v2x_polyarchy, n = 10),
    vdem_growth_l1 = dplyr::lead(vdem_growth, n = 1),
    vdem_growth_l2 = dplyr::lead(vdem_growth, n = 2),
    vdem_growth_l5 = dplyr::lead(vdem_growth, n = 5),
    period = rep(1:12, length.out = n_distinct(year %/% 10))[match(year %/% 10, unique(year %/% 10))],
    region = as.factor(countrycode(cow, 'cown', 'region')),
    county = as.factor(country)) %>%
  mutate_at(vars(starts_with("beis_"),
                 v2x_polyarchy, vdem_growth, gdp_growth, youthbulge, youthbulge_l, gdp_pcap, pop, v2x_execorr_l, pop_l,
                 gdp_pcap_l, SP.URB.TOTL.IN.ZS_l, NY.GDP.PETR.RT.ZS_l, conscript), ~replace_na(., 0)) %>% 
  filter(year >= 1945)

df_colpus = df11_colpus %>% 
  group_by(cow) %>% 
  mutate(gdp_pcap = log(gdp_pcap+0.01),
         gdp_pcap_l = dplyr::lag(gdp_pcap, n = 1),
         gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
         pop = log(pop+0.01),
         pop_l = dplyr::lag(pop, n = 1),
         SP.URB.TOTL.IN.ZS = log(SP.URB.TOTL.IN.ZS+0.01),
         SP.URB.TOTL.IN.ZS_l = dplyr::lag(SP.URB.TOTL.IN.ZS, n = 1),
         NY.GDP.PETR.RT.ZS = log(NY.GDP.PETR.RT.ZS+0.01),
         NY.GDP.PETR.RT.ZS_l = dplyr::lag(NY.GDP.PETR.RT.ZS, n = 1),
         year = as.numeric(year),
         v2x_polyarchy = v2x_polyarchy + 0.01,
         v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
         v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
         youthbulge = log(youthbulge+1),
         youthbulge_l = dplyr::lag(youthbulge, n = 1),
         vdem_growth = ((v2x_polyarchy - dplyr::lag(v2x_polyarchy)) / dplyr::lag(v2x_polyarchy) * 100),
         vdem_growth_l = dplyr::lag(vdem_growth, n = 1),
         tenure_l = dplyr::lag(tenure, n = 1),
         polity2_l = dplyr::lag(polity2, n = 1),
         #         polity_growth = ((polity2 - dplyr::lag(polity2)) / dplyr::lag(polity2) * 100),
         polity2_growth = (polity2 - dplyr::lag(polity2)),
         coup_nomil = dplyr::lag(coup_nomil, n = 1),
         coup_success = dplyr::lag(coup_success, n = 1),
         coup_failure = dplyr::lag(coup_failure, n = 1),
         coup_regimechange = dplyr::lag(coup_regimechange, n = 1),
         coup_antidemocratic = dplyr::lag(coup_antidemocratic, n = 1),
         coup_attempt = dplyr::lag(coup_attempt, n = 1),
         v2x_polyarchy_l2 = dplyr::lead(v2x_polyarchy, n = 2),
         v2x_polyarchy_l5 = dplyr::lead(v2x_polyarchy, n = 5),
         v2x_polyarchy_l10 = dplyr::lead(v2x_polyarchy, n = 10),
         vdem_growth_l1 = dplyr::lead(vdem_growth, n = 1),
         vdem_growth_l2 = dplyr::lead(vdem_growth, n = 2),
         vdem_growth_l5 = dplyr::lead(vdem_growth, n = 5),
         period = rep(1:12, length.out = n_distinct(year %/% 10))[match(year %/% 10, unique(year %/% 10))],
         region = as.factor(countrycode(cow, 'cown', 'region')),
         county = as.factor(country)) %>%
  mutate_at(vars(starts_with("coup_"),
                 v2x_polyarchy, vdem_growth, gdp_growth, youthbulge, youthbulge_l, gdp_pcap, pop, v2x_execorr_l, pop_l,
                 gdp_pcap_l, SP.URB.TOTL.IN.ZS_l, NY.GDP.PETR.RT.ZS_l, conscript), ~replace_na(., 0)) %>% 
  filter(year >= 1945)

#Transsforming an
df_nvc1.3_def <- df_nvc1.3_def %>% 
  filter(!cow %in% c(935, 940, 946, 947, 950, 955, 970, 983, 986, 987, 990))
df_nvc1.3_def <- transform(df_nvc1.3_def,region_int=as.numeric(factor(region)))

df_nvc1.3_pan <- df_nvc1.3_pan %>% 
  filter(!cow %in% c(935, 940, 946, 947, 950, 955, 970, 983, 986, 987, 990))
df_nvc1.3_pan <- transform(df_nvc1.3_pan,region_int=as.numeric(factor(region)))

df_nvc2.1 <- df_nvc2.1 %>% 
  filter(!cow %in% c(935, 940, 946, 947, 950, 955, 970, 983, 986, 987, 990))
df_nvc2.1 <- transform(df_nvc2.1,region_int=as.numeric(factor(region)))

df_beis <- df_beis %>% 
  filter(!cow %in% c(935, 940, 946, 947, 950, 955, 970, 983, 986, 987, 990))
df_beis <- transform(df_beis,region_int=as.numeric(factor(region)))

df_colpus <- df_colpus %>% 
  filter(!cow %in% c(935, 940, 946, 947, 950, 955, 970, 983, 986, 987, 990))
df_colpus <- transform(df_colpus,region_int=as.numeric(factor(region)))

write.csv(df_nvc1.3_def, 'Desktop/Работа и учеба/Диплом/df_nvc1.3_def.csv')
write.csv(df_nvc1.3_pan, 'Desktop/Работа и учеба/Диплом/df_nvc1.3_pan.csv')
write.csv(df_nvc2.1, 'Desktop/Работа и учеба/Диплом/df_nvc2.1.csv')
write.csv(df_beis, 'Desktop/Работа и учеба/Диплом/df_beis.csv')
write.csv(df_colpus, 'Desktop/Работа и учеба/Диплом/df_colpus.csv')

#### Part 5. Multiple imputation ####
df_nvc1.3_def_imp = df_nvc1.3_def %>% select(!c(country, gwf_regimetype)) %>% 
  select(cow, year, pop_l, v2x_polyarchy_l, v2x_execorr_l, youthbulge_l,tenure_l,gdp_pcap_l,gdp_growth_l,SP.URB.TOTL.IN.ZS_l,NY.GDP.PETR.RT.ZS_l,vdem_growth_l, period, region, starts_with('NVC1.3_') & ends_with('_l'),
         NVC1.3_REGCHANGE)
df_nvc1.3_pan_imp = df_nvc1.3_pan %>% select(!c(country, gwf_regimetype)) %>% 
  select(cow, year, pop_l, v2x_polyarchy_l, v2x_execorr_l, youthbulge_l,tenure_l,gdp_pcap_l,gdp_growth_l,SP.URB.TOTL.IN.ZS_l,NY.GDP.PETR.RT.ZS_l,vdem_growth_l, period, region, starts_with('NVC1.3_') & ends_with('_l'),
         NVC1.3_REGCHANGE)
df_nvc2.1_imp = df_nvc2.1 %>% select(!c(country, gwf_regimetype)) %>% 
  select(cow, year, pop_l, v2x_polyarchy_l, v2x_execorr_l, youthbulge_l,tenure_l,gdp_pcap_l,gdp_growth_l,total_part_l,SP.URB.TOTL.IN.ZS_l,NY.GDP.PETR.RT.ZS_l,vdem_growth_l, period, region, starts_with('NVC2.1_')) %>% 
  select(!c(total_part_l, NVC2.1_VIOL, NVC2.1_failure))
df_beis_imp = df_beis %>% select(!c(country, gwf_regimetype)) %>% 
  select(cow, year, pop_l, v2x_polyarchy_l, v2x_execorr_l, youthbulge_l,tenure_l,gdp_pcap_l,gdp_growth_l,total_part_l,SP.URB.TOTL.IN.ZS_l,NY.GDP.PETR.RT.ZS_l,vdem_growth_l, period, region, starts_with('beis_')) %>% 
  select(!c(total_part_l, beis_armed, beis_armednocivwar, beis_failure, beis_unarmednocivwar, beis_armednocivwar_l))
df_nvc1.3_def_imp <- as.data.frame(df_nvc1.3_def_imp)
df_nvc1.3_pan_imp <- as.data.frame(df_nvc1.3_pan_imp)
df_nvc2.1_imp <- as.data.frame(df_nvc2.1_imp)
df_beis_imp <- as.data.frame(df_beis_imp)

bds_df_nvc1.3_def_imp <- as.matrix(
  rbind(
    c(which(colnames(df_nvc1.3_def_imp)=="pop_l"),0,14.2),
    c(which(colnames(df_nvc1.3_def_imp)=="v2x_polyarchy_l"),0,1),
    c(which(colnames(df_nvc1.3_def_imp)=="v2x_execorr_l"),0,1),
    c(which(colnames(df_nvc1.3_def_imp)=="youthbulge_l"),0,100),
    c(which(colnames(df_nvc1.3_def_imp)=="tenure_l"),0,70),
    c(which(colnames(df_nvc1.3_def_imp)=="gdp_pcap_l"),0,12),
    c(which(colnames(df_nvc1.3_def_imp)=="gdp_growth_l"),-62,174),   
    c(which(colnames(df_nvc1.3_def_imp)=="SP.URB.TOTL.IN.ZS_l"),8,21),
    c(which(colnames(df_nvc1.3_def_imp)=="NY.GDP.PETR.RT.ZS_l"),0,100),
    c(which(colnames(df_nvc1.3_def_imp)=="vdem_growth_l"),-75,419)
  )
)
bds_df_nvc1.3_pan_imp <- as.matrix(
  rbind(
    c(which(colnames(df_nvc1.3_pan_imp)=="pop_l"),0,14.2),
    c(which(colnames(df_nvc1.3_pan_imp)=="v2x_polyarchy_l"),0,1),
    c(which(colnames(df_nvc1.3_pan_imp)=="v2x_execorr_l"),0,1),
    c(which(colnames(df_nvc1.3_pan_imp)=="youthbulge_l"),0,100),
    c(which(colnames(df_nvc1.3_pan_imp)=="tenure_l"),0,70),
    c(which(colnames(df_nvc1.3_pan_imp)=="gdp_pcap_l"),0,12),
    c(which(colnames(df_nvc1.3_pan_imp)=="gdp_growth_l"),-62,174),   
    c(which(colnames(df_nvc1.3_pan_imp)=="SP.URB.TOTL.IN.ZS_l"),8,21),
    c(which(colnames(df_nvc1.3_pan_imp)=="NY.GDP.PETR.RT.ZS_l"),0,100),
    c(which(colnames(df_nvc1.3_pan_imp)=="vdem_growth_l"),-75,419)
  )
)
bds_df_nvc2.1_imp <- as.matrix(
  rbind(
    c(which(colnames(df_nvc2.1_imp)=="pop_l"),0,14.2),
    c(which(colnames(df_nvc2.1_imp)=="v2x_polyarchy_l"),0,1),
    c(which(colnames(df_nvc2.1_imp)=="v2x_execorr_l"),0,1),
    c(which(colnames(df_nvc2.1_imp)=="youthbulge_l"),0,100),
    c(which(colnames(df_nvc2.1_imp)=="tenure_l"),0,70),
    c(which(colnames(df_nvc2.1_imp)=="gdp_pcap_l"),0,12),
    c(which(colnames(df_nvc2.1_imp)=="gdp_growth_l"),-62,174),   
#    c(which(colnames(df_nvc2.1_imp)=="total_part_l"),-4.6,17),
    c(which(colnames(df_nvc2.1_imp)=="SP.URB.TOTL.IN.ZS_l"),8,21),
    c(which(colnames(df_nvc2.1_imp)=="NY.GDP.PETR.RT.ZS_l"),0,100),
    c(which(colnames(df_nvc2.1_imp)=="vdem_growth_l"),-75,419)
  )
)
bds_df_beis_imp <- as.matrix(
  rbind(
    c(which(colnames(df_beis_imp)=="pop_l"),0,14.2),
    c(which(colnames(df_beis_imp)=="v2x_polyarchy_l"),0,1),
    c(which(colnames(df_beis_imp)=="v2x_execorr_l"),0,1),
    c(which(colnames(df_beis_imp)=="youthbulge_l"),0,100),
    c(which(colnames(df_beis_imp)=="tenure_l"),0,70),
    c(which(colnames(df_beis_imp)=="gdp_pcap_l"),0,12),
    c(which(colnames(df_beis_imp)=="gdp_growth_l"),-62,174),   
#    c(which(colnames(df_beis_imp)=="total_part_l"),-4.6,16.1),
    c(which(colnames(df_beis_imp)=="SP.URB.TOTL.IN.ZS_l"),8,21),
    c(which(colnames(df_beis_imp)=="NY.GDP.PETR.RT.ZS_l"),0,100),
    c(which(colnames(df_beis_imp)=="vdem_growth_l"),-75,419)
  )
)


df_nvc1.3_def_imput <- amelia(df_nvc1.3_def_imp, m = 5,
#                     idvars = "id",
                     ts = "year", 
                     cs = "cow",
                     polytime = 3,
#                     lags = ,
                     ords = c('period'),
                     noms = c('region'),
                     #                     idvars = c('year', 'cow'),
                     #                     startvals = 1,
                     incheck = T,
                     bounds = bds_df_nvc1.3_def_imp,
                     empri = 0.005*nrow(bds_df_nvc1.3_def_imp),
                     autopri = 0.05,
                     parallel = "multicore",
                     ncpus = 6)

df_nvc1.3_pan_imput <- amelia(df_nvc1.3_pan_imp, m = 5,
#                            idvars = "id",
                            ts = "year", 
                            cs = "cow",
                            polytime = 3,
                            #                     lags = vars,
                            ords = c('period'),
                            noms = c('region'),
                            #                     idvars = c('year', 'cow'),
                            #                     startvals = 1,
                            incheck = T,
                            bounds = bds_df_nvc1.3_pan_imp,
                            empri = 0.005*nrow(bds_df_nvc1.3_pan_imp),
                            autopri = 0.05,
                            parallel = "multicore",
                            ncpus = 6)

df_nvc2.1_imput <- amelia(df_nvc2.1_imp, m = 5,
#                            idvars = "id",
                            ts = "year", 
                            cs = "cow",
                            polytime = 3,
                            # lags = ,
                            ords = c('period'),
                            noms = c('region'),
                            #                     idvars = c('year', 'cow'),
                            #                     startvals = 1,
                            incheck = T,
                            bounds = bds_df_nvc2.1_imp,
                            empri = 0.005*nrow(bds_df_nvc2.1_imp),
                            autopri = 0.05,
                            parallel = "multicore",
                            ncpus = 6)

df_beis_imput <- amelia(df_beis_imp, m = 5,
#                           idvars = "id",
                            ts = "year", 
                            cs = "cow",
                            polytime = 3,
                            #                     lags = vars,
                            ords = c('period'),
                            noms = c('region'),
                            #                     idvars = c('year', 'cow'),
                            #                     startvals = 1,
                            incheck = T,
                            bounds = bds_df_beis_imp,
                            empri = 0.005*nrow(bds_df_beis_imp),
                            autopri = 0.05,
                            parallel = "multicore",
                            ncpus = 6)

#df_imputed <- transform(df_imputed, log_oilgasrents_l = log(NY.GDP.PETR.RT.ZS_l + NY.GDP.NGAS.RT.ZS_l))
save(df_nvc1.3_def_imput, file = "Desktop/Работа и учеба/Диплом/df_nvc1.3_def_imput.RData")
save(df_nvc1.3_pan_imput, file = "Desktop/Работа и учеба/Диплом/df_nvc1.3_pan_imput.RData")
save(df_nvc2.1_imput, file = "Desktop/Работа и учеба/Диплом/df_nvc2.1_imput.RData")
save(df_beis_imput, file = "Desktop/Работа и учеба/Диплом/df_beis_imput.RData")
View(df_beis_imput$imputations[[1]])
