
df_nvc2.1 <- read.csv('Desktop/Работа и учеба/Диплом/df_nvc2.1.csv')
df_nvc2.1 <- df_nvc2.1[!duplicated(df_nvc2.1[c('cow', 'year')]), ]

#Hypothesis 1
nonviol_h1 <- fect(v2x_polyarchy ~ NVC2.1_NONVIOL_l +  gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                   data = df_nvc2.1, index = c("cow","year"),
                   method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                   parallel = TRUE, nboots = 500, 
                   vartype = "bootstrap",
                   loo = F, 
                   carryoverTest = T, carryover.period = c(1, 4))
plot(nonviol_h1, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp1_nonviol_fect.jpeg') 

df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_VIOL_l = dplyr::lag(NVC2.1_VIOL, n = 1),
                                 NVC2.1_VIOL_l = replace_na(NVC2.1_VIOL_l, 0))
viol_h1 <- fect(v2x_polyarchy ~ NVC2.1_VIOL_l + gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                   data = df_nvc2.1, index = c("cow","year"),
                   method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                   parallel = TRUE, nboots = 500, 
                   vartype = "bootstrap",
                   loo = F, 
                   carryoverTest = T, carryover.period = c(1, 4))
plot(viol_h1, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp1_viol_fect.jpeg') 

#Hypothesis 2
df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_success_l = dplyr::lag(NVC2.1_success, n = 1),
                                 NVC2.1_success_l = replace_na(NVC2.1_success_l, 0))
success_h2 <- fect(v2x_polyarchy ~ NVC2.1_success_l +  gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                   data = df_nvc2.1, index = c("cow","year"),
                   method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                   parallel = TRUE, nboots = 500, 
                   vartype = "bootstrap",
                   loo = F, 
                   carryoverTest = T, carryover.period = c(1, 4))
plot(success_h2, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp2_success_fect.jpeg') 

df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_failure_l = dplyr::lag(NVC2.1_failure, n = 1),
                                 NVC2.1_failure_l = replace_na(NVC2.1_failure_l, 0))
fail_h2 <- fect(v2x_polyarchy ~ NVC2.1_failure_l + gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                data = df_nvc2.1, index = c("cow","year"),
                method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                parallel = TRUE, nboots = 500, 
                vartype = "bootstrap",
                loo = F, 
                carryoverTest = T, carryover.period = c(1, 4))
plot(fail_h2, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp2_fail_fect.jpeg') 

#Hypothesis 3
df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_nonviolsucc_l = dplyr::lag(NVC2.1_nonviolsucc, n = 1),
                                 NNVC2.1_nonviolsucc_l = replace_na(NVC2.1_nonviolsucc_l, 0))
nonviolsucc_h3 <- fect(v2x_polyarchy ~ NVC2.1_success_l +  gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                   data = df_nvc2.1, index = c("cow","year"),
                   method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                   parallel = TRUE, nboots = 500, 
                   vartype = "bootstrap",
                   loo = F, 
                   carryoverTest = T, carryover.period = c(1, 4))
plot(nonviolsucc_h3, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp3_nonviolsuccess_fect.jpeg') 

df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_violsucc = ifelse(NVC2.1_VIOL == 1 & NVC2.1_success == 1, 1, 0),
                                 NVC2.1_violsucc_l = dplyr::lag(NVC2.1_violsucc, n = 1),
                                 NVC2.1_violsucc_l = replace_na(NVC2.1_violsucc_l, 0))
violsucc_h3 <- fect(v2x_polyarchy ~ NVC2.1_violsucc_l + gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                data = df_nvc2.1, index = c("cow","year"),
                method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                parallel = TRUE, nboots = 500, 
                vartype = "bootstrap",
                loo = F, 
                carryoverTest = T, carryover.period = c(1, 4))
plot(violsucc_h3, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp3_violsuccess_fect.jpeg') 

#Hypothesis 4
df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_nonviolfail = ifelse(NVC2.1_NONVIOL == 1 & NVC2.1_failure == 1, 1, 0),
                                 NVC2.1_nonviolfail_l = dplyr::lag(NVC2.1_nonviolfail, n = 1),
                                 NVC2.1_nonviolfail_l = replace_na(NVC2.1_nonviolfail_l, 0))
nonviolfail_h4 <- fect(v2x_polyarchy ~ NVC2.1_nonviolfail_l +  gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                       data = df_nvc2.1, index = c("cow","year"),
                       method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                       parallel = TRUE, nboots = 500, 
                       vartype = "bootstrap",
                       loo = F, 
                       carryoverTest = T, carryover.period = c(1, 4))
plot(nonviolfail_h4, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp4_nonviolfail_fect.jpeg') 

df_nvc2.1 = df_nvc2.1 %>% mutate(NVC2.1_violfail = ifelse(NVC2.1_VIOL == 1 & NVC2.1_failure == 1, 1, 0),
                                 NVC2.1_violfail_l = dplyr::lag(NVC2.1_violfail, n = 1),
                                 NVC2.1_violfail_l = replace_na(NVC2.1_violfail_l, 0))
violfail_h4 <- fect(v2x_polyarchy ~ NVC2.1_violfail_l + gdp_pcap_l + gdp_growth_l + SP.URB.TOTL.IN.ZS_l + NY.GDP.PETR.RT.ZS + youthbulge_l + v2x_execorr_l + v2x_polyarchy_l, 
                    data = df_nvc2.1, index = c("cow","year"),
                    method = "fe", force = "two-way", na.rm = F, se = TRUE, 
                    parallel = TRUE, nboots = 500, 
                    vartype = "bootstrap",
                    loo = F, 
                    carryoverTest = T, carryover.period = c(1, 4))
plot(violfail_h4, main = "Estimated DATT (FEct)", ylab = "Effect of D on v2x_polyarchy", type = "gap", show.points = FALSE)
ggsave(path = 'Desktop/Работа и учеба/Диплом/Visualizations', width = 9, height = 5, device='jpeg', dpi=300,
       filename = 'Hyp4_violfail_fect.jpeg') 
