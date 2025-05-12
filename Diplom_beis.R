# FROM DICTATORSHIP TO DEMOCRACY? THE INFLUENCE OF REVOLUTIONARY MASS UPRISINGS ON REGIME CHANGE: A CROSS-COUNTRY PERSPECTIVE
# HSE UNIVERSITY. FACULTY OF SOCIAL SCIENCES. BACHELOR THESIS
# 
# Author:
# Ruslan Guseinov, riguseynov@edu.hse.ru
# 
# Beissinger episodes script
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
library(patchwork)
library(Synth)
library(SCtools)
library(skimr)
set.seed(666)

#load datasets
df_beis <- read.csv('Desktop/Работа и учеба/Диплом/df_beis.csv') #Revolutionary Episodes Dataset. Event data

#prepare datasets for the matching procedure
df_beis <- df_beis[!duplicated(df_beis[c('cow', 'year')]), ]

#### Hypothesis 1. ####
#First step: estimate nonviolent campaigns effect
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_unarmed", "v2x_polyarchy")
PM.results_beisnonviol <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(gdp_growth_l, 1:3)) + I(lag(NY.GDP.PETR.RT.ZS_l, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)

#Check covariate balance - it is OK
print(get_covariate_balance(PM.results_beisnonviol, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'gdp_growth_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))

#Lets look at one matched set. Regions not matched, but similar socio-economic characteristics.
DisplayTreatment(panel.data = df_PM_beis, xlab = 'year',
                 ylab = 'cow', matched.set = PM.results_beisnonviol$att[1],
                 show.set.only = TRUE)
#PM.results_nvc2.1nonviol$att[1] #info about the first set
#as.data.frame(df_PM_nvc2.1) %>% 
#  filter(year %in% c(1984, 1985) & cow %in% c(41,432,490,110)) %>% 
#  select(cow, year, country, v2x_polyarchy_l, gdp_pcap_l, gdp_growth_l, v2x_execorr_l, tenure_l, region_int) #gather data and get the table about the matched set

#Estimate the effect
PE.results_beisnonviol <- PanelEstimate(
  sets = PM.results_beisnonviol, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_nonviol <- PE.results_beisnonviol$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beisnonviol$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beisnonviol$estimate %>% as.data.frame(),
              PE.results_beisnonviol$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  # Добавляем строку
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Second step: estimate violent campaigns effect
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_armed", "v2x_polyarchy")
PM.results_beisviol <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(gdp_growth_l, 1:3)) + I(lag(NY.GDP.PETR.RT.ZS_l, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is more or less OK
print(get_covariate_balance(PM.results_beisviol, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'gdp_growth_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Lets look at one matched set. Regions matched, pretty similar socio-economic characteristics.
#DisplayTreatment(panel.data = df_PM_beis, xlab = 'year',
#                 ylab = 'cow', matched.set = PM.results_nvc2.1viol$att[1],
#                 show.set.only = TRUE)
#PM.results_nvc2.1viol$att[1] #info about the first set
#df_PM_nvc2.1 %>% 
#  filter(year %in% c(1955, 1956) & cow %in% c(92, 41, 40, 140)) %>% 
#  select(cow, year, country, v2x_polyarchy_l, gdp_pcap_l, gdp_growth_l, v2x_execorr_l, tenure_l, region_int) 
#Estimate the effect
PE.results_beisviol <- PanelEstimate(
  sets = PM.results_beisviol, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_viol <- PE.results_beisviol$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beisviol$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beisviol$estimate %>% as.data.frame(),
              PE.results_beisviol$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_beis_nonviol <- panmatch_beis_nonviol %>% mutate(Dataset = "Nonviolent")
panmatch_beis_viol <- panmatch_beis_viol %>% mutate(Dataset = "Violent")
panmatch_comb <- bind_rows(panmatch_beis_nonviol, panmatch_beis_viol)

ggplot(panmatch_comb, aes(x = t, y = E, color = Dataset)) +
  geom_pointrange(aes(ymin = E - 1.96 * SE, ymax = E + 1.96 * SE), 
                  position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "t",
    y = "ATT",
    color = "Revolutionary tactic" 
  ) +
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"  
  ) +
  scale_x_continuous(breaks = seq(-4,10,1))
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp1_beis.jpeg') 

#### Hypothesis 2 ####
#Successful
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_success", "v2x_polyarchy")
PM.results_beissuccess <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(NY.GDP.PETR.RT.ZS_l, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is more or less OK
print(get_covariate_balance(PM.results_beissuccess, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Lets look at one matched set. Regions matched, pretty similar socio-economic characteristics.
#DisplayTreatment(panel.data = df_PM_nvc2.1, xlab = 'year',
#                 ylab = 'cow', matched.set = PM.results_nvc2.1success$att[2],
#                 show.set.only = TRUE)
#PM.results_nvc2.1success$att[2] #info about the first set
#df_PM_nvc2.1 %>% 
#  filter(year %in% c(1964, 1965) & cow %in% c(145,91,42,130)) %>% 
#  select(cow, year, country, v2x_polyarchy_l, gdp_pcap_l, v2x_execorr_l, tenure_l, region_int)

#Estimate the effect
PE.results_beissuccess <- PanelEstimate(
  sets = PM.results_beissuccess, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_success <- PE.results_beissuccess$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beissuccess$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beissuccess$estimate %>% as.data.frame(),
              PE.results_beissuccess$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>% 
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)
#Unsuccessful
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_failure", "v2x_polyarchy")
PM.results_beis_fail <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(NY.GDP.PETR.RT.ZS_l, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is more or less OK
print(get_covariate_balance(PM.results_beis_fail, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Estimate the effect
PE.results_beis_fail <- PanelEstimate(
  sets = PM.results_beis_fail, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_fail <- PE.results_beis_fail$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beis_fail$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beis_fail$estimate %>% as.data.frame(),
              PE.results_beis_fail$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>% 
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_beis_success <- panmatch_beis_success %>% mutate(Dataset = "Success")
panmatch_beis_fail <- panmatch_beis_fail %>% mutate(Dataset = "Failure")
panmatch_comb <- bind_rows(panmatch_beis_success, panmatch_beis_fail)

ggplot(panmatch_comb, aes(x = t, y = E, color = Dataset)) +
  geom_pointrange(aes(ymin = E - 1.96 * SE, ymax = E + 1.96 * SE), 
                  position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "t",
    y = "ATT",
    color = "Revolutionary tactic"  
  ) +
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom" 
  ) +
  scale_x_continuous(breaks = seq(-4,10,1))
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp2_beis.jpeg') 

#### Hypothesis 3 ####
#First step: nonviolent successful
df_beis <- df_beis %>% mutate(beis_nonviolsucc = ifelse(beis_unarmed == 1 & beis_success == 1, 1, 0),
                              beis_violsucc = ifelse(beis_armed == 1 & beis_success == 1, 1, 0))
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_nonviolsucc", "v2x_polyarchy")
PM.results_beisnonviolsucc <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(gdp_growth_l, 1:3)) + I(lag(NY.GDP.PETR.RT.ZS_l, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is OK
print(get_covariate_balance(PM.results_beisnonviolsucc, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'gdp_growth_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_beisnonviolsucc <- PanelEstimate(
  sets = PM.results_beisnonviolsucc, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_nonviolsucc <- PE.results_beisnonviolsucc$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beisnonviolsucc$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beisnonviolsucc$estimate %>% as.data.frame(),
              PE.results_beisnonviolsucc$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Second step: violent successful
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_violsucc", "v2x_polyarchy")
PM.results_beisviolsucc <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is more or less OK< fewer covariates
print(get_covariate_balance(PM.results_beisviolsucc, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_beisviolsucc <- PanelEstimate(
  sets = PM.results_beisviolsucc, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_violsucc <- PE.results_beisviolsucc$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beisviolsucc$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beisviolsucc$estimate %>% as.data.frame(),
              PE.results_beisviolsucc$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_beis_nonviolsucc <- panmatch_beis_nonviolsucc %>% mutate(Dataset = "Nonviolent successful")
panmatch_beis_violsucc <- panmatch_beis_violsucc %>% mutate(Dataset = "Violent successful")
panmatch_comb <- bind_rows(panmatch_beis_nonviolsucc, panmatch_beis_violsucc)

ggplot(panmatch_comb, aes(x = t, y = E, color = Dataset)) +
  geom_pointrange(aes(ymin = E - 1.96 * SE, ymax = E + 1.96 * SE), 
                  position = position_dodge(width = 0.5)) +  
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "t",
    y = "ATT",
    color = "Revolutionary tactic and success"  
  ) +
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"  
  ) +
  scale_x_continuous(breaks = seq(-4,10,1))
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp3_beis.jpeg') 

#### Hypothesis 5 ####
#Unsuccessful nonviolent
df_beis = df_beis %>% mutate(beis_nonviolfail = ifelse(beis_unarmed == 1 & beis_failure == 1, 1, 0),
                             beis_violfail = ifelse(beis_armed == 1 & beis_failure == 1, 1, 0))
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_nonviolfail", "v2x_polyarchy")

PM.results_beisnonviolfail <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(youthbulge_l, 1:3)) + I(lag(gdp_growth_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 5, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)

print(get_covariate_balance(PM.results_beisnonviolfail, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l', 'gdp_growth_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_beisnonviolfail <- PanelEstimate(
  sets = PM.results_beisnonviolfail, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_nonviolfail <- PE.results_beisnonviolfail$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beisnonviolfail$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beisnonviolfail$estimate %>% as.data.frame(),
              PE.results_beisnonviolfail$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Unsuccessful violent
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_violfail", "v2x_polyarchy")

PM.results_beisviolfail <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(youthbulge_l, 1:3)) + 
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 5, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)

print(get_covariate_balance(PM.results_beisviolfail, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_beisviolfail <- PanelEstimate(
  sets = PM.results_beisviolfail, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_violfail <- PE.results_beisviolfail$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beisviolfail$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beisviolfail$estimate %>% as.data.frame(),
              PE.results_beisviolfail$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_beis_nonviolfail <- panmatch_beis_nonviolfail %>% mutate(Dataset = "Nonviolent unuccessful")
panmatch_beis_violfail <- panmatch_beis_violfail %>% mutate(Dataset = "Violent unsuccessful")
panmatch_comb <- bind_rows(panmatch_beis_nonviolfail, panmatch_beis_violfail)

ggplot(panmatch_comb, aes(x = t, y = E, color = Dataset)) +
  geom_pointrange(aes(ymin = E - 1.96 * SE, ymax = E + 1.96 * SE), 
                  position = position_dodge(width = 0.5)) +  
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "t",
    y = "ATT",
    color = "Revolutionary tactic and success"  
  ) +
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom" 
  ) +
  scale_x_continuous(breaks = seq(-4,10,1))
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp4_beis.jpeg') 

