## This script plots cumulative respiration by stream order
##
## Peter Regier (with code from Francisco)
## Contact: peter.regier@pnnl.gov
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")


# 2. make plot (currently Figure S6) -------------------------------------------

p1 <- scaling_analysis_dat %>% 
 ggplot(aes(as.factor(stream_order), totco2_o2g_day, fill = basin)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  labs(x = "Stream Order", fill = "Basin") + 
  ylab(expression(paste("Reach-scale", " ", respiration[Hyp], "(", gCO[2] * d^-1, ")")))

p2 <- scaling_analysis_dat %>% 
  ggplot(aes(as.factor(stream_order), accm_totco2_o2g_day, fill = basin)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  labs(x = "Stream Order", fill = "Basin") + 
  ylab(expression(paste("Cumulative Aerobic", " ", respiration[Hyp], "(", gCO[2] * d^-1, ")")))

plot_grid(p1, p2, ncol = 1)
ggsave("figures/s6_resp_v_stream_order.png", width = 6, height = 8)
ggsave("figures/s6_resp_v_stream_order.pdf", width = 6, height = 8)

## Assign scaling categories based on simple rules
regression_estimates <- regression_estimates_raw %>% 
  clean_names() %>% 
  mutate(quantile = fct_relevel(quantile, "Q100", after = Inf)) %>% 
  mutate(r_squared = round(r_squared, 2)) %>% 
  mutate(scaling = case_when(slope_ci_2_5 <= 1 & slope_ci_97_5 >= 1 & r_squared > 0.8 ~ "Linear", 
                             slope_ci_2_5 < 1 & slope_ci_97_5 < 1 & r_squared > 0.8 ~ "Sublinear", 
                             slope_ci_2_5 > 1 & slope_ci_97_5 > 1 & r_squared > 0.8 ~ "Super-linear", 
                             TRUE ~ "Uncertain")) %>% 
  dplyr::select(basin, quantile, r_squared, slope, contains("slope_ci"), scaling) %>% 
  mutate(scaling = fct_relevel(scaling, c("Uncertain", "Sublinear")))

scaling_data_raw <- scaling_analysis_dat %>% 
  rename("quantile" = accm_hzt_cat) %>% 
  mutate(quantile_n = as.numeric(str_sub(quantile, 2, n())))

scaling_data_combined <- inner_join(scaling_data_raw, 
                                    regression_estimates, 
                                    by = c("basin", "quantile"))

scaling_data_long <- scaling_data_combined %>% 
  dplyr::select(basin, scaling, contains("_3scp")) %>% 
  pivot_longer(cols = contains("_3scp"))

scaling_data_combined %>% 
  ggplot(aes(as.factor(stream_order), fill = scaling)) + 
  geom_bar() + 
  facet_wrap(~basin)  +
  labs(x = "Stream Order", y = "# of reaches", fill = "Scaling")


scaling_data_combined %>% 
  filter(human_3scp > 50) %>% 
  ggplot(aes(as.factor(scaling), human_3scp)) + 
  geom_boxplot() + 
  facet_wrap(~basin) +
  labs(x = "Stream Order", y = "% urban land-cover", fill = "Scaling")
ggsave("figures/s7_urban_v_scaling.png", width = 6, height = 4)
ggsave("figures/s7_urban_v_scaling.pdf", width = 6, height = 4)


scaling_data_combined %>% 
  mutate(hef_cat = case_when(quantile %in% c("Q10", "Q20", "Q30") ~ "1_low", 
                             quantile %in% c("Q40", "Q50", "Q60", "Q70") ~ "2_mod",
                             quantile %in% c("Q80", "Q90", "Q100") ~ "3_high")) %>% 
  ggplot(aes(hef_cat, totco2_o2g_m2_day, fill = basin)) + 
  geom_boxplot() + 
  scale_y_log10()

x <- scaling_data_combined %>% 
  mutate(hef_cat = case_when(quantile %in% c("Q10", "Q20", "Q30") ~ "Q10-30", 
                             quantile %in% c("Q40", "Q50", "Q60", "Q70") ~ "Q40-70",
                             quantile %in% c("Q80", "Q90", "Q100") ~ "Q80-100"))

  
plot_grid(ggplot(x, aes(hef_cat, totco2_o2g_m2_day, fill = basin)) + 
            geom_boxplot() + 
            scale_y_log10(), 
          ggplot(x, aes(hef_cat, water_exchng_kg_d, fill = basin)) + 
  geom_boxplot() + 
  scale_y_log10(), 
  ncol = 1)

ggplot(x, aes(accm_water_exchng_kg_d, accm_totco2_o2g_day, color = basin)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~hef_cat, scales = "free")

ggplot(x, aes(wshd_max_elevation_m, color = hef_cat, fill = hef_cat)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~basin)


ggplot(x, aes(totco2_o2g_day, color = hef_cat, fill = hef_cat)) + 
  geom_density(alpha = 0.2) + 
  scale_x_log10() +
  facet_wrap(~basin, scales = "free_x") + 
  labs(x = "HZ reach-scale aerobic respiration (gCO2d-1)", 
       y = "Density", 
       color = "HEF quantile", 
       fill = "HEF quantile")
ggsave("figures/s8_urban_v_scaling.png", width = 7, height = 4)
ggsave("figures/s8_urban_v_scaling.pdf", width = 7, height = 4)


###########

scaling_analysis_dat %>% 
  mutate(accm_hzt_cat = fct_relevel(accm_hzt_cat, "Q100", after = Inf)) %>% 
  ggplot(aes(accm_hzt_cat, wshd_max_elevation_m, color = basin)) + 
         geom_boxplot() %>% 
  

###########

x <- scaling_data_combined %>% 
  mutate(quantile = fct_relevel(as.factor(quantile), "Q100", after = Inf))

plot_grid(ggplot(x, aes(as.factor(quantile), 
                                           doc_load_kg_d, 
                                            color = basin)) + 
            geom_boxplot(show.legend = F) + 
            scale_y_log10(), 
          ggplot(x, aes(as.factor(quantile),
                                           water_exchng_kg_d, 
                                            color = basin)) + 
            geom_boxplot(show.legend = F) + 
            scale_y_log10(), 
          ggplot(x, aes(as.factor(quantile), 
                                           tot_rt_hz_s, 
                                            color = basin)) + 
            geom_boxplot(show.legend = F) + 
            scale_y_log10(), 
          nrow = 1)
ggsave("figures/sa_urban_v_scaling.png", width = 8, height = 4)
ggsave("figures/sa_urban_v_scaling.pdf", width = 8, height = 4)






