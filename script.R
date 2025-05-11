# FROM DICTATORSHIP TO DEMOCRACY? THE INFLUENCE OF REVOLUTIONARY MASS UPRISINGS ON REGIME CHANGE: A CROSS-COUNTRY PERSPECTIVE
# HSE UNIVERSITY. FACULTY OF SOCIAL SCIENCES. BACHELOR THESIS
# 
# Author:
# Ruslan Guseinov, riguseynov@edu.hse.ru
# 
# Main script
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
df_nvc2.1 <- read.csv('Desktop/Работа и учеба/Диплом/df_nvc2.1.csv') #NAVCO 2.1 Panel data
df_beis <- read.csv('Desktop/Работа и учеба/Диплом/df_beis.csv') #Revolutionary Episodes Dataset. Event data
df_colpus <- read.csv('Desktop/Работа и учеба/Диплом/df_colpus.csv') #Colpus coups data. Event data

#prepare datasets for the matching procedure
df_nvc2.1 <- df_nvc2.1[!duplicated(df_nvc2.1[c('cow', 'year')]), ]
df_beis <- df_beis[!duplicated(df_beis[c('cow', 'year')]), ]
df_colpus <- df_colpus[!duplicated(df_colpus[c('cow', 'year')]), ]

#DAG under strict exogeneity assumption
dag_strexo <- dagitty('dag {
bb="-4.18,-4.313,4.225,4.685"
"Covariates (t)" [pos="0.053,3.150"]
"Covariates (t+1)" [pos="2.043,3.150"]
"Covariates (t-1)" [pos="-2.077,3.150"]
"Regime change (t)" [outcome,pos="0.870,1.400"]
"Regime change (t+1)" [outcome,pos="2.600,1.400"]
"Regime change (t-1)" [outcome,pos="-0.912,1.400"]
"Revolutionary mass uprising (t)" [exposure,pos="0.009,-0.614"]
"Revolutionary mass uprising (t+1)" [exposure,pos="2.078,-0.620"]
"Revolutionary mass uprising (t-1)" [exposure,pos="-2.077,-0.620"]
"Covariates (t)" -> "Covariates (t+1)"
"Covariates (t)" -> "Regime change (t)"
"Covariates (t)" -> "Revolutionary mass uprising (t)"
"Covariates (t+1)" -> "Regime change (t+1)"
"Covariates (t+1)" -> "Revolutionary mass uprising (t+1)"
"Covariates (t-1)" -> "Covariates (t)"
"Covariates (t-1)" -> "Regime change (t-1)"
"Covariates (t-1)" -> "Revolutionary mass uprising (t-1)"
"Revolutionary mass uprising (t)" -> "Regime change (t)"
"Revolutionary mass uprising (t)" -> "Revolutionary mass uprising (t+1)"
"Revolutionary mass uprising (t+1)" -> "Regime change (t+1)"
"Revolutionary mass uprising (t-1)" -> "Regime change (t-1)"
"Revolutionary mass uprising (t-1)" -> "Revolutionary mass uprising (t)"
}')
plot(dag_strexo)

#DAG under sequential ignorability assumption
dag_seqignor <-  dagitty('dag {
  bb="-4.18,-4.313,4.225,4.685"
  "Covariates (t)" [pos="0.053,3.150"]
  "Covariates (t+1)" [pos="2.043,3.150"]
  "Covariates (t-1)" [pos="-2.077,3.150"]
  "Regime change (t+1)" [outcome,pos="2.600,1.400"]
  "Regime change (t-1)" [outcome,pos="-0.912,1.400"]
  "Regime change" [outcome,pos="0.870,1.400"]
  "Revolutionary mass uprising (t+1)" [exposure,pos="2.078,-0.620"]
  "Revolutionary mass uprising (t-1)" [exposure,pos="-2.077,-0.620"]
  "Revolutionary mass uprising" [exposure,pos="0.070,-0.620"]
  "Covariates (t)" -> "Covariates (t+1)"
  "Covariates (t)" -> "Regime change"
  "Covariates (t)" -> "Revolutionary mass uprising"
  "Covariates (t+1)" -> "Regime change (t+1)"
  "Covariates (t+1)" -> "Revolutionary mass uprising (t+1)"
  "Covariates (t-1)" -> "Covariates (t)"
  "Covariates (t-1)" -> "Regime change (t-1)"
  "Covariates (t-1)" -> "Revolutionary mass uprising (t-1)"
  "Regime change (t-1)" -> "Covariates (t)"
  "Regime change (t-1)" -> "Regime change"
  "Regime change (t-1)" -> "Revolutionary mass uprising"
  "Regime change" -> "Covariates (t+1)"
  "Regime change" -> "Regime change (t+1)"
  "Regime change" -> "Revolutionary mass uprising (t+1)"
  "Revolutionary mass uprising (t+1)" -> "Regime change (t+1)"
  "Revolutionary mass uprising (t-1)" -> "Regime change (t-1)"
  "Revolutionary mass uprising (t-1)" -> "Regime change"
  "Revolutionary mass uprising (t-1)" -> "Revolutionary mass uprising"
  "Revolutionary mass uprising" -> "Regime change (t+1)"
  "Revolutionary mass uprising" -> "Regime change"
  "Revolutionary mass uprising" -> "Revolutionary mass uprising (t+1)"
}')
plot(dag_seqignor)

#### Part 1. Visualise treatment regional distribution ####
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

#### Part 2. Visual diagnostics of the treatment ####
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_campaign", "v2x_polyarchy")
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_campaign", "v2x_polyarchy")
df_PM_nvc2.1 = df_PM_nvc2.1 %>% filter(NVC2.1_camp_goals == 0)
DisplayTreatment(panel.data = df_PM_nvc1.3_pan, legend.position = "none",
                 xlab = "year", ylab = "Country Code")
p1 <- DisplayTreatment(panel.data = df_PM_nvc2.1, legend.position = "none",
                 xlab = "year", ylab = "Country Code")
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) + labs(title = 'NAVCO 2.1')
p2 <- DisplayTreatment(panel.data = df_PM_beis, legend.position = "none",
                 xlab = "year", ylab = "Country Code")
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) + labs(title = 'Revolutionary Episodes Dataset') #treatment distributions are alike, suggesting that maximalist campaigns and revolutionary episodes are alike phenomena
combined_plot = p1 + p2 + 
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = 'bottom')
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'treatment_nvc2.1beis.jpeg') 

#Track dynamic treatment  effects. Effect of nonviolent campaigns
panelview(v2x_polyarchy ~ NVC2.1_NONVIOL_l + gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l +NY.GDP.PETR.RT.ZS_l + v2x_execorr_l + tenure_l + youthbulge_l, data = df_PM_nvc2.1, index = c("cow","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          theme.bw = TRUE, type = "outcome", main = "")
panelview(v2x_polyarchy ~ beis_unarmed_l + gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS_l + v2x_execorr_l + tenure_l + youthbulge_l, data = df_PM_beis, index = c("cow","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          theme.bw = TRUE, type = "outcome", main = "")

#### Part 3. Descriptive statistics of the covariates ####
stargazer(df_nvc2.1)

#### Part 4. Histograms for all the variables ####
p1=ggplot(df_nvc2.1)+geom_histogram(aes(x=gdp_pcap))+theme_minimal()
p2=ggplot(df_nvc2.1)+geom_histogram(aes(x=v2x_polyarchy))+theme_minimal()
p3=ggplot(df_nvc2.1)+geom_histogram(aes(x=v2x_execorr))+theme_minimal()
p4=ggplot(df_nvc2.1)+geom_histogram(aes(x=tenure))+theme_minimal()
p5=ggplot(df_nvc2.1)+geom_histogram(aes(x=youthbulge))+theme_minimal()
p6=ggplot(df_nvc2.1)+geom_histogram(aes(x=SP.URB.TOTL.IN.ZS))+theme_minimal()
p7=ggplot(df_nvc2.1)+geom_histogram(aes(x=NY.GDP.PETR.RT.ZS))+theme_minimal()
combined_plot = p1 + p2 + p3 + p4 + p5 + p6 + p7 +
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = 'bottom')
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'covariate_distribtuion.jpeg') 

#### Part 5. PanelMatch ####
#### Hypothesis 1. ####
#First step: estimate nonviolent campaigns effect
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_NONVIOL", "v2x_polyarchy")
PM.results_nvc2.1nonviol <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_nvc2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1nonviol, panel.data=df_PM_nvc2.1, 
                            covariates = c('gdp_pcap_l', 'gdp_growth_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))

#Lets look at one matched set. Regions not matched, but similar socio-economic characteristics.
DisplayTreatment(panel.data = df_PM_nvc2.1, xlab = 'year',
                 ylab = 'cow', matched.set = PM.results_nvc2.1nonviol$att[1],
                 show.set.only = TRUE)
PM.results_nvc2.1nonviol$att[1] #info about the first set
as.data.frame(df_PM_nvc2.1) %>% 
  filter(year %in% c(1984, 1985) & cow %in% c(41,432,490,110)) %>% 
  select(cow, year, country, v2x_polyarchy_l, gdp_pcap_l, gdp_growth_l, v2x_execorr_l, tenure_l, region_int) #gather data and get the table about the matched set

#Estimate the effect
PE.results_nvc2.1nonviol <- PanelEstimate(
  sets = PM.results_nvc2.1nonviol, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_nonviol <- PE.results_nvc2.1nonviol$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1nonviol$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1nonviol$estimate %>% as.data.frame(),
              PE.results_nvc2.1nonviol$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  # Добавляем строку
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Second step: estimate violent campaigns effect
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_VIOL", "v2x_polyarchy")
PM.results_nvc2.1viol <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_nvc2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1viol, panel.data=df_PM_nvc2.1, 
                            covariates = c('gdp_pcap_l', 'gdp_growth_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Lets look at one matched set. Regions matched, pretty similar socio-economic characteristics.
DisplayTreatment(panel.data = df_PM_nvc2.1, xlab = 'year',
                 ylab = 'cow', matched.set = PM.results_nvc2.1viol$att[1],
                 show.set.only = TRUE)
PM.results_nvc2.1viol$att[1] #info about the first set
df_PM_nvc2.1 %>% 
  filter(year %in% c(1955, 1956) & cow %in% c(92, 41, 40, 140)) %>% 
  select(cow, year, country, v2x_polyarchy_l, gdp_pcap_l, gdp_growth_l, v2x_execorr_l, tenure_l, region_int) 
#Estimate the effect
PE.results_nvc2.1viol <- PanelEstimate(
  sets = PM.results_nvc2.1viol, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_viol <- PE.results_nvc2.1viol$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1viol$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1viol$estimate %>% as.data.frame(),
              PE.results_nvc2.1viol$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_nvc2.1_nonviol <- panmatch_nvc2.1_nonviol %>% mutate(Dataset = "Nonviolent")
panmatch_nvc2.1_viol <- panmatch_nvc2.1_viol %>% mutate(Dataset = "Violent")
panmatch_comb <- bind_rows(panmatch_nvc2.1_nonviol, panmatch_nvc2.1_viol)

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
       filename = 'Hyp1_nvc2.1.jpeg') 

#### Hypothesis 2 ####
#Successful
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_success", "v2x_polyarchy")
PM.results_nvc2.1success <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_nvc2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1success, panel.data=df_PM_nvc2.1, 
                            covariates = c('gdp_pcap_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Lets look at one matched set. Regions matched, pretty similar socio-economic characteristics.
DisplayTreatment(panel.data = df_PM_nvc2.1, xlab = 'year',
                 ylab = 'cow', matched.set = PM.results_nvc2.1success$att[2],
                 show.set.only = TRUE)
PM.results_nvc2.1success$att[2] #info about the first set
df_PM_nvc2.1 %>% 
  filter(year %in% c(1964, 1965) & cow %in% c(145,91,42,130)) %>% 
  select(cow, year, country, v2x_polyarchy_l, gdp_pcap_l, v2x_execorr_l, tenure_l, region_int)

#Estimate the effect
PE.results_nvc2.1success <- PanelEstimate(
  sets = PM.results_nvc2.1success, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_success <- PE.results_nvc2.1success$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1success$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1success$estimate %>% as.data.frame(),
              PE.results_nvc2.1success$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>% 
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)
#Unsuccessful
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_failure", "v2x_polyarchy")
PM.results_nvc2.1_fail <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_nvc2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1_fail, panel.data=df_PM_nvc2.1, 
                            covariates = c('gdp_pcap_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Estimate the effect
PE.results_nvc2.1_fail <- PanelEstimate(
  sets = PM.results_nvc2.1_fail, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_fail <- PE.results_nvc2.1_fail$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1_fail$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1_fail$estimate %>% as.data.frame(),
              PE.results_nvc2.1_fail$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>% 
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_nvc2.1_success <- panmatch_nvc2.1_success %>% mutate(Dataset = "Success")
panmatch_nvc2.1_fail <- panmatch_nvc2.1_fail %>% mutate(Dataset = "Failure")
panmatch_comb <- bind_rows(panmatch_nvc2.1_success, panmatch_nvc2.1_fail)

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
       filename = 'Hyp2_nvc2.1.jpeg') 

#### Hypothesis 3 ####
#First step: nonviolent successful
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_nonviolsucc", "v2x_polyarchy")
PM.results_nvc2.1nonviolsucc <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_nvc2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1nonviolsucc, panel.data=df_PM_nvc2.1, 
                            covariates = c('gdp_pcap_l', 'gdp_growth_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_nvc2.1nonviolsucc <- PanelEstimate(
  sets = PM.results_nvc2.1nonviolsucc, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_nonviolsucc <- PE.results_nvc2.1nonviolsucc$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1nonviolsucc$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1nonviolsucc$estimate %>% as.data.frame(),
              PE.results_nvc2.1nonviolsucc$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Second step: violent successful
df_PM_2.1 = df_nvc2.1 %>% mutate(NVC2.1_violsucc = ifelse(NVC2.1_VIOL == 1 & NVC2.1_success == 1, 1, 0))
df_PM_2.1 <- PanelData(df_PM_2.1, "cow", "year", "NVC2.1_violsucc", "v2x_polyarchy")
PM.results_nvc2.1violsucc <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1violsucc, panel.data=df_PM_2.1, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_nvc2.1violsucc <- PanelEstimate(
  sets = PM.results_nvc2.1violsucc, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_violsucc <- PE.results_nvc2.1violsucc$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1violsucc$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1violsucc$estimate %>% as.data.frame(),
              PE.results_nvc2.1violsucc$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_nvc2.1_nonviolsucc <- panmatch_nvc2.1_nonviolsucc %>% mutate(Dataset = "Nonviolent successful")
panmatch_nvc2.1_violsucc <- panmatch_nvc2.1_violsucc %>% mutate(Dataset = "Violent successful")
panmatch_comb <- bind_rows(panmatch_nvc2.1_nonviolsucc, panmatch_nvc2.1_violsucc)

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
       filename = 'Hyp3_nvc2.1.jpeg') 

#### Hypothesis 4 ####
#Unsuccessful nonviolent
df_nvc2.1_prep = df_nvc2.1 %>% mutate(NVC2.1_nonviolfail = ifelse(NVC2.1_NONVIOL == 1 & NVC2.1_failure == 1, 1, 0),
                                 NVC2.1_violfail = ifelse(NVC2.1_VIOL == 1 & NVC2.1_failure == 1, 1, 0))
df_PM_2.1 <- PanelData(df_nvc2.1_prep, "cow", "year", "NVC2.1_nonviolfail", "v2x_polyarchy")

PM.results_nvc2.1nonviolfail <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_2.1, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(youthbulge_l, 1:3)) + I(lag(gdp_growth_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 5, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)

print(get_covariate_balance(PM.results_nvc2.1nonviolfail, panel.data=df_PM_2.1, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l', 'gdp_growth_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_nvc2.1nonviolfail <- PanelEstimate(
  sets = PM.results_nvc2.1nonviolfail, panel.data = df_PM_2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_nonviolfail <- PE.results_nvc2.1nonviolfail$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1nonviolfail$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1nonviolfail$estimate %>% as.data.frame(),
              PE.results_nvc2.1nonviolfail$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Unsuccessful violent
df_PM_2.1 <- PanelData(df_nvc2.1_prep, "cow", "year", "NVC2.1_violfail", "v2x_polyarchy")

PM.results_nvc2.1violfail <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_2.1, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(youthbulge_l, 1:3)) + 
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 5, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)

print(get_covariate_balance(PM.results_nvc2.1violfail, panel.data=df_PM_2.1, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
PE.results_nvc2.1violfail <- PanelEstimate(
  sets = PM.results_nvc2.1violfail, panel.data = df_PM_2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1_violfail <- PE.results_nvc2.1violfail$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1violfail$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1violfail$estimate %>% as.data.frame(),
              PE.results_nvc2.1violfail$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_nvc2.1_nonviolfail <- panmatch_nvc2.1_nonviolfail %>% mutate(Dataset = "Nonviolent unuccessful")
panmatch_nvc2.1_violfail <- panmatch_nvc2.1_violfail %>% mutate(Dataset = "Violent unsuccessful")
panmatch_comb <- bind_rows(panmatch_nvc2.1_nonviolfail, panmatch_nvc2.1_violfail)

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
       filename = 'Hyp4_nvc2.1.jpeg') 

#### Hypothesis 5 ####
#Revolutionary mass uprisings
df_PM_nvc2.1 <- PanelData(df_nvc2.1, "cow", "year", "NVC2.1_success", "v2x_polyarchy")
PM.results_nvc2.1success <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_nvc2.1, 
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
print(get_covariate_balance(PM.results_nvc2.1success, panel.data=df_PM_nvc2.1, 
                            covariates = c('gdp_pcap_l', 'NY.GDP.PETR.RT.ZS_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Estimate the effect
PE.results_nvc2.1 <- PanelEstimate(
  sets = PM.results_nvc2.1, panel.data = df_PM_nvc2.1,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_nvc2.1 <- PE.results_nvc2.1$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_nvc2.1$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_nvc2.1$estimate %>% as.data.frame(),
              PE.results_nvc2.1$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Coups
df_colpus2 = df_colpus %>% mutate(coup_regchangesucess = ifelse(coup_regimechange == 1 & coup_success == 1, 1, 0))
df_PM_colpus <- PanelData(df_colpus2, "cow", "year", "coup_regchangesucess", "v2x_polyarchy")
PM.results_colpus <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_colpus, 
  refinement.method = "mahalanobis", match.missing = FALSE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
#  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is OK
print(get_covariate_balance(PM.results_colpus, panel.data=df_PM_colpus, 
                            covariates = c('gdp_pcap_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Estimate the effect
PE.results_colpus <- PanelEstimate(
  sets = PM.results_colpus, panel.data = df_PM_colpus,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_colpus <- PE.results_colpus$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_colpus$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_colpus$estimate %>% as.data.frame(),
              PE.results_colpus$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  # Добавляем строку
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_nvc2.1 <- panmatch_nvc2.1 %>% mutate(Dataset = "Revolutionary mass uprisings")
panmatch_colpus <- panmatch_colpus %>% mutate(Dataset = "Coups")
panmatch_comb <- bind_rows(panmatch_nvc2.1, panmatch_colpus)

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
       filename = 'Hyp5_nvc2.1_colpus.jpeg') 

#### Hypothesis 6 ####
#Liberal revolutions
df_PM_beis<- PanelData(df_beis, "cow", "year", "beis_democrat", "v2x_polyarchy")
PM.results_beis_lib <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_polyarchy_l^3, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is more or less OK
print(get_covariate_balance(PM.results_beis_lib, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Estimate the effect
PE.results_beis_lib <- PanelEstimate(
  sets = PM.results_beis_lib, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_lib <- PE.results_beis_lib$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beis_lib$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beis_lib$estimate %>% as.data.frame(),
              PE.results_beis_lib$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%  
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

#Liberal successful revolutions
df_beis <- df_beis %>% mutate(beis_democrat_success = ifelse(beis_democrat == 1 & beis_success == 1, 1, 0))
df_PM_beis <- PanelData(df_beis, "cow", "year", "beis_democrat_success", "v2x_polyarchy")
PM.results_beis_lib_suc <- PanelMatch(
  lag = 4, 
  panel.data=df_PM_beis, 
  refinement.method = "mahalanobis", match.missing = TRUE, use.diagonal.variance.matrix = T,
  covs.formula = ~ I(lag(gdp_pcap_l, 1:3)) + 
    I(lag(gdp_pcap_l^2, 1:3)) + I(lag(tenure_l, 1:3)) + I(lag(youthbulge_l, 1:3)) +
    I(lag(v2x_polyarchy_l, 1:3)) + I(lag(v2x_polyarchy_l^2, 1:3)) + I(lag(v2x_polyarchy_l^3, 1:3)) + I(lag(v2x_execorr_l, 1:3)) + I(lag(SP.URB.TOTL.IN.ZS_l, 1:3)) + I(lag(region_int, 1:3)),
  #  exact.match.variables = 'region_int',
  size.match = 3, qoi = "att",
  lead = 0:10, forbid.treatment.reversal = FALSE, 
  placebo.test = T
)
#Check covariate balance - it is more or less OK
print(get_covariate_balance(PM.results_beis_lib_suc, panel.data=df_PM_beis, 
                            covariates = c('gdp_pcap_l', 'tenure_l', 'youthbulge_l',
                                           'v2x_polyarchy_l', 'v2x_execorr_l', 'SP.URB.TOTL.IN.ZS_l', 'region_int')))
#Estimate the effect
PE.results_beis_lib_suc <- PanelEstimate(
  sets = PM.results_beis_lib_suc, panel.data = df_PM_beis,
  se.method = "bootstrap", number.iterations = 1000, confidence.level = .95, parallel = T, num.cores = 8,
  include.placebo.test = T, #moderator = 'region',
) 
panmatch_beis_lib_suc <- PE.results_beis_lib_suc$placebo.test$estimates %>% 
  as.data.frame() %>% 
  cbind(., PE.results_beis_lib_suc$placebo.test$standard.errors %>% as.data.frame()) %>% 
  rename(E = 1, SE = 2) %>% 
  rbind(cbind(PE.results_beis_lib_suc$estimate %>% as.data.frame(),
              PE.results_beis_lib_suc$standard.error %>% as.data.frame()
  ) %>% rename(E = 1, SE = 2)) %>% 
  rownames_to_column(var = "time") %>% 
  add_row(E = 0, SE = 0, time = 't-1') %>%
  arrange(as.numeric(gsub("t[+]", "", gsub("t-", "-", time)))) %>% 
  mutate(t = 1:n() - 5)

panmatch_beis_lib <- panmatch_beis_lib %>% mutate(Dataset = "Liberal")
panmatch_beis_lib_suc <- panmatch_beis_lib_suc %>% mutate(Dataset = "Liberal successful")
panmatch_comb <- bind_rows(panmatch_beis_lib, panmatch_beis_lib_suc)

ggplot(panmatch_comb, aes(x = t, y = E, color = Dataset)) +
  geom_pointrange(aes(ymin = E - 1.96 * SE, ymax = E + 1.96 * SE), 
                  position = position_dodge(width = 0.5)) +  
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "t",
    y = "ATT",
    color = "Type"  
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
       filename = 'Hyp6_beis.jpeg') 

#### Quantitative case studies ####
#Case 1: Poland democratization. Successful nonviolent revolution
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1970:2000,
         region == "Europe & Central Asia") %>%
  mutate(dem_success = ifelse(NVC2.1_success == 1, 1, 0),
         dem_success = replace_na(dem_success, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_success_year = ifelse(any(dem_success == 1), min(year[dem_success == 1]), NA)
  ) %>%
  mutate(
    dem_success = ifelse(!is.na(first_dem_success_year) & year >= first_dem_success_year, 1, dem_success)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_success", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_success'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1983:1988,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1981:1988, "mean"),
    list("SP.URB.TOTL.IN.ZS", seq(1980,1988,2), "mean"),
    list("youthbulge", seq(1980,1988,2), "mean"),
    list("v2x_execorr", seq(1980,1988,2), "mean"),
    list("v2x_polyarchy", seq(1980,1988,2), "mean"),
    list("gdp_growth", seq(1980,1988,2), "mean"),
    list("NY.GDP.PETR.RT.ZS", seq(1980,1988,2), "mean"),
    list("tenure", seq(1980,1988,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 290,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-10],
    #unique(df_nvc2.1_synth$cow)[-10],  # the identifiers for the control groups
  
  time.optimize.ssr = 1983:1988,  # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1983:2000  # the time period for the plot
)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)

par(mfrow = c(1,2))
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1989,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,1),
          Legend = c("Poland","synthetic Poland"),
          Legend.position = "bottomright",
)

#Now estimate an in-space placebo. Turkey.
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1970:2000,
         region == "Europe & Central Asia") %>%
  mutate(dem_success = ifelse(NVC2.1_success == 1, 1, 0),
         dem_success = replace_na(dem_success, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_success_year = ifelse(any(dem_success == 1), min(year[dem_success == 1]), NA)
  ) %>%
  mutate(
    dem_success = ifelse(!is.na(first_dem_success_year) & year >= first_dem_success_year, 1, dem_success)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_success", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out.pl.space <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_success'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1984:1988,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1980:1988, "mean"),
    list("SP.URB.TOTL.IN.ZS", seq(1980,1988,1), "mean"),
    list("youthbulge", seq(1980,1988,1), "mean"),
    list("v2x_execorr", seq(1980,1988,1), "mean"),
    list("v2x_polyarchy", seq(1980,1988,1), "mean"),
    list("gdp_growth", seq(1980,1988,1), "mean"),
    list("NY.GDP.PETR.RT.ZS", seq(1980,1988,1), "mean"),
    list("tenure", seq(1980,1988,1), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 640,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-c(10, 26)],
  #unique(df_nvc2.1_synth$cow)[-10],  # the identifiers for the control groups
  
  time.optimize.ssr = 1984:1988,  # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1984:2000  # the time period for the plot
)

synth.out.pl.space <- synth(data.prep.obj = dataprep.out.pl.space,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
path.plot(synth.res = synth.out.pl.space,
          dataprep.res = dataprep.out.pl.space,
          tr.intake = 1989,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,1),
          Legend = c("Turkey","placebo Turkey"),
          Legend.position = "bottomright"
)
dev.off()

#Case 2: Tienanmen protests. Unsuccessful nonviolent protests
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1970:2000,
         region %in% c("South Asia", "East Asia & Pacific")) %>%
  mutate(dem_fail = ifelse(NVC2.1_failure == 1 & is.na(gwf_next), 1, 0),
         dem_fail = replace_na(dem_fail, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_fail_year = ifelse(any(dem_fail == 1), min(year[dem_fail == 1]), NA)
  ) %>%
  mutate(
    dem_fail = ifelse(!is.na(first_dem_fail_year) & year >= first_dem_fail_year, 1, dem_fail)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_fail", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_fail'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1983:1988,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1981:1988, "mean"),
    list("SP.URB.TOTL.IN.ZS", seq(1980,1988,2), "mean"),
    list("youthbulge", seq(1980,1988,2), "mean"),
    list("v2x_execorr", seq(1980,1988,2), "mean"),
    list("v2x_polyarchy", seq(1980,1988,2), "mean"),
    list("gdp_growth", seq(1980,1988,2), "mean"),
    list("NY.GDP.PETR.RT.ZS", seq(1980,1988,2), "mean"),
    list("tenure", seq(1980,1988,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 710,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-2],  # the identifiers for the control groups
  
  time.optimize.ssr = 1980:1988, # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1980:1996  # the time period for the plot
)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
par(mfrow=c(1,2))
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1989,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,0.5),
          Legend = c("China","synthetic China"),
          Legend.position = "topright"
)

#Now estimate an in-place placebo. Pakistan
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1970:2000,
         region %in% c("South Asia", "East Asia & Pacific")) %>%
  mutate(dem_fail = ifelse(NVC2.1_failure == 1 & is.na(gwf_next), 1, 0),
         dem_fail = replace_na(dem_fail, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_fail_year = ifelse(any(dem_fail == 1), min(year[dem_fail == 1]), NA)
  ) %>%
  mutate(
    dem_fail = ifelse(!is.na(first_dem_fail_year) & year >= first_dem_fail_year, 1, dem_fail)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_fail", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out.pl.space <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_fail'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1982:1988,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1981:1988, "mean"),
    list("SP.URB.TOTL.IN.ZS", seq(1980,1988,2), "mean"),
    list("youthbulge", seq(1980,1988,2), "mean"),
    list("v2x_execorr", seq(1980,1988,2), "mean"),
    list("v2x_polyarchy", seq(1980,1988,2), "mean"),
    list("gdp_growth", seq(1980,1988,2), "mean"),
    list("NY.GDP.PETR.RT.ZS", seq(1980,1988,2), "mean"),
    list("tenure", seq(1980,1988,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 770,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-c(2, 8)],  # the identifiers for the control groups
  
  time.optimize.ssr = 1980:1988, # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1980:1996  # the time period for the plot
)

synth.out.pl.space <- synth(data.prep.obj = dataprep.out.pl.space,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
path.plot(synth.res = synth.out.pl.space,
          dataprep.res = dataprep.out.pl.space,
          tr.intake = 1989,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,0.5),
          Legend = c("Pakistan","placebo Pakistan"),
          Legend.position = "topright"
)

#Case 3: successful violent revolution. Cuban revolution
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1945:1975,
         region %in% c("Latin America & Caribbean")) %>%
  mutate(dem_success = ifelse(NVC2.1_success == 1, 1, 0),
         dem_success = replace_na(dem_success, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_success_year = ifelse(any(dem_success == 1), min(year[dem_success == 1]), NA)
  ) %>%
  mutate(
    dem_success = ifelse(!is.na(first_dem_success_year) & year >= first_dem_success_year, 1, dem_success)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_success", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_success'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1955:1958,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1955:1958, "mean"),
#    list("SP.URB.TOTL.IN.ZS", seq(1952,1958,2), "mean"),
    list("youthbulge", seq(1955,1958,2), "mean"),
    list("v2x_execorr", seq(1955,1958,2), "mean"),
    list("v2x_polyarchy", seq(1955,1958,2), "mean"),
    list("gdp_growth", seq(1955,1958,2), "mean"),
#    list("NY.GDP.PETR.RT.ZS", seq(1952,1958,2), "mean"),
    list("tenure", seq(1955,1958,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 40,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-1],  # the identifiers for the control groups
  
  time.optimize.ssr = 1955:1958,  # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1955:1975  # the time period for the plot
)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
par(mfrow=c(1,2))
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1959,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,0.6),
          Legend = c("Cuba","synthetic Cuba"),
          Legend.position = "topright"
)

#An in-place placebo scenario. Costa Rica 
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1945:1975,
         region %in% c("Latin America & Caribbean")) %>%
  mutate(dem_success = ifelse(NVC2.1_success == 1, 1, 0),
         dem_success = replace_na(dem_success, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_success_year = ifelse(any(dem_success == 1), min(year[dem_success == 1]), NA)
  ) %>%
  mutate(
    dem_success = ifelse(!is.na(first_dem_success_year) & year >= first_dem_success_year, 1, dem_success)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_success", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out.pl.space <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_success'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1955:1958,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1955:1958, "mean"),
    #    list("SP.URB.TOTL.IN.ZS", seq(1952,1958,2), "mean"),
    list("youthbulge", seq(1955,1958,2), "mean"),
    list("v2x_execorr", seq(1955,1958,2), "mean"),
    list("v2x_polyarchy", seq(1955,1958,2), "mean"),
    list("gdp_growth", seq(1955,1958,2), "mean"),
    #    list("NY.GDP.PETR.RT.ZS", seq(1952,1958,2), "mean"),
    list("tenure", seq(1955,1958,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 94,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-c(1,9)],  # the identifiers for the control groups
  
  time.optimize.ssr = 1955:1958,  # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1955:1975  # the time period for the plot
)

synth.out.pl.space <- synth(data.prep.obj = dataprep.out.pl.space,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
path.plot(synth.res = synth.out.pl.space,
          dataprep.res = dataprep.out.pl.space,
          tr.intake = 1959,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,1),
          Legend = c("Costa Rica","placebo Costa Rica"),
          Legend.position = "topright"
)

#Case 4: unsuccessful violent revolution. Burundi ethnic violence 1988
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1970:2000,
         region %in% c("Sub-Saharan Africa")) %>%
  mutate(dem_fail = ifelse(NVC2.1_failure == 1, 1, 0),
         dem_fail = replace_na(dem_fail, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 31) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_fail_year = ifelse(any(dem_fail == 1), min(year[dem_fail == 1]), NA)
  ) %>%
  mutate(
    dem_fail = ifelse(!is.na(first_dem_fail_year) & year >= first_dem_fail_year, 1, dem_fail)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_fail", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_fail'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1980:1987,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1980:1987, "mean"),
    list("SP.URB.TOTL.IN.ZS", seq(1980,1987,2), "mean"),
    list("youthbulge", seq(1980,1987,2), "mean"),
    list("v2x_execorr", seq(1980,1987,2), "mean"),
    list("v2x_polyarchy", seq(1980,1987,2), "mean"),
    list("gdp_growth", seq(1980,1987,2), "mean"),
    list("NY.GDP.PETR.RT.ZS", seq(1980,1987,2), "mean"),
    list("tenure", seq(1980,1987,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 516,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-c(25, 11, 14, 15, 18, 26, 27)],
    #unique(df_nvc2.1_synth$cow)[-c(11, 14, 15, 18, 22, 25, 26, 27, 30)],
    #the identifiers for the control groups
  
  time.optimize.ssr = 1980:1987,  # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1980:1997  # the time period for the plot
)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
par(mfrow=c(1,2))
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1988,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,0.8),
          Legend = c("Burundi","synthetic Burundi"),
          Legend.position = "topright"
)

#An in-place placebo scenario. Comoros
df_nvc2.1_synth <- df_nvc2.1 %>%
  filter(year %in% 1977:2000,
         region %in% c("Sub-Saharan Africa")) %>%
  mutate(dem_fail = ifelse(NVC2.1_failure == 1, 1, 0),
         dem_fail = replace_na(dem_fail, 0)) %>%
  group_by(cow) %>%
  filter(n_distinct(year) == 24) %>%
  ungroup() %>%
  group_by(cow) %>%
  mutate(
    first_dem_fail_year = ifelse(any(dem_fail == 1), min(year[dem_fail == 1]), NA)
  ) %>%
  mutate(
    dem_fail = ifelse(!is.na(first_dem_fail_year) & year >= first_dem_fail_year, 1, dem_fail)
  ) %>%
  ungroup() %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  as.data.frame()

#Look at the treatment distribution
df_nvc2.1_synth_PM <- PanelData(df_nvc2.1_synth, "cow", "year", "dem_fail", "v2x_polyarchy")
DisplayTreatment(panel.data = df_nvc2.1_synth_PM, legend.position = "none",
                 xlab = "year", ylab = "Country Code")

#Data preparation
dataprep.out.pl.space <- dataprep(
  foo = as.data.frame(df_nvc2.1_synth),  # the dataset to be prepared
  
  # the predictor variables to be used in the model
  predictors = c('dem_fail'),
  
  predictors.op = "mean",  # operation to be applied on the predictors == classic version
  
  time.predictors.prior = 1982:1987,  # time period for the predictors
  
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdp_pcap", 1980:1987, "mean"),
    list("SP.URB.TOTL.IN.ZS", seq(1980,1987,2), "mean"),
    list("youthbulge", seq(1980,1987,2), "mean"),
    list("v2x_execorr", seq(1980,1987,2), "mean"),
    list("v2x_polyarchy", seq(1980,1987,2), "mean"),
    list("gdp_growth", seq(1980,1987,2), "mean"),
    list("NY.GDP.PETR.RT.ZS", seq(1980,1987,2), "mean"),
    list("tenure", seq(1980,1987,2), "mean")
  ),
  
  dependent = "v2x_polyarchy",  # the dependent variable
  
  unit.variable = "cow",  # the variable representing the unit of observation == i
  
  unit.names.variable = "country",  # the variable representing the names of the units
  
  time.variable = "year",  # the variable representing the time period == t
  
  treatment.identifier = 581,  # the identifier for the treatment group
  
  controls.identifier = unique(df_nvc2.1_synth$cow)[-c(42, 25)],
  #unique(df_nvc2.1_synth$cow)[-c(11, 14, 15, 18, 22, 25, 26, 27, 30)],
  #the identifiers for the control groups
  
  time.optimize.ssr = 1982:1987,  # the time period over which the sum of squared residuals (SSR) is minimized to estimate the weights
  
  time.plot = 1982:1997  # the time period for the plot
)

synth.out.pl.space <- synth(data.prep.obj = dataprep.out.pl.space,
                   method = "BFGS" #   set optim() to use the BFGS quasi-Newton algorithm. 
)
path.plot(synth.res = synth.out.pl.space,
          dataprep.res = dataprep.out.pl.space,
          tr.intake = 1988,
          Ylab = "Polyarchy",
          Xlab = "year",
          Ylim = c(0,0.8),
          Legend = c("Comoros","placebo Comoros"),
          Legend.position = "topright"
)
