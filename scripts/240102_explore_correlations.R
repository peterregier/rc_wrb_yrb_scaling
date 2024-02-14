## Explore potential correlations
##
## Peter Regier
## 2023-12-30
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")


# 2. Read in scaling dataset ---------------------------------------------------

# Loading regression estimates dataset
regression_estimates_raw <-  read_csv("data/guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv")

## Assign scaling categories based on simple rules
regression_estimates <- regression_estimates_raw %>% 
  clean_names() %>% 
  mutate(quantile = fct_relevel(quantile, "Q100", after = Inf)) %>% 
  mutate(r_squared = round(r_squared, 2)) %>% 
  mutate(scaling = case_when(slope_ci_2_5 <= 1 & slope_ci_97_5 >= 1 & r_squared > 0.8 ~ "Linear", 
                             slope_ci_2_5 < 1 & slope_ci_97_5 < 1 & r_squared > 0.8 ~ "Sublinear", 
                             slope_ci_2_5 > 1 & slope_ci_97_5 > 1 & r_squared > 0.8 ~ "Super-linear", 
                             TRUE ~ "Uncertain")) %>% 
  select(basin, quantile, r_squared, slope, contains("slope_ci"), scaling) %>% 
  mutate(scaling = fct_relevel(scaling, c("Uncertain", "Sublinear")))

scaling_data_raw <- scaling_analysis_dat %>% 
  rename("quantile" = accm_hzt_cat)

scaling_data_combined <- inner_join(scaling_data_raw, 
                                    regression_estimates, 
                                    by = c("basin", "quantile"))

scaling_sf <- st_as_sf(scaling_data_combined, 
                       coords = c("longitude", "latitude"), 
                       crs = common_crs)


# 3. Start experimenting with plots --------------------------------------------

ggplot(scaling_data_combined, aes(scaling, wshd_max_elevation_m, color = basin)) + 
  geom_boxplot() + 
 # scale_y_log10() + 
  geom_smooth() 

## lm models
summary(lm(log10(accm_totco2_o2g_day)~log10(wshd_max_elevation_m), 
           data = scaling_data_combined %>% filter(basin == "willamette")))

p_load(ggConvexHull)
ggplot(scaling_data_combined, aes(wshd_max_elevation_m, accm_totco2_o2g_day)) + 
  #ggplot(scaling_data_combined, aes(wshd_max_elevation_m, accm_totco2_o2g_day / wshd_area_km2)) + 
  geom_point(aes(color = scaling)) + 
  geom_convexhull(aes(group = scaling, color = scaling, 
                      fill = scaling), alpha = 0.2) +
  #scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method = "lm", color = "black")  + 
  facet_wrap(~basin, nrow = 1) + 
  scale_color_viridis_d() +
  ggpubr::stat_cor(aes(label = after_stat(rr.label)), geom = "label") + 
  labs(x = "Maximum watershed elevation (m)", 
       y = "Cumulative respiration (gCO2/day/m2)", 
       color = "Scaling", 
       fill = "Scaling")
ggsave("figures/231230_resp_v_elevation_by_scaling.png", width = 8, height = 4)

## See if we can get an R2 for a non-linear fit
## https://www.r-bloggers.com/2016/02/first-steps-with-non-linear-regression-in-r/

# a_start = 1
# b_start = 1
# 
# m <- nls(accm_totco2_o2g_day~a*wshd_max_elevation_m/(b+wshd_max_elevation_m), 
#          start=list(a=a_start,b=b_start), 
#          data = scaling_data_combined)
# 
# cor(accm_totco2_o2g_day,predict(m))

ggplot(scaling_data_combined, 
       aes(scaling, accm_totco2_o2g_day, color = basin)) + 
  geom_boxplot() + 
  scale_y_log10()
ggsave("figures/240102_resp_by_scaling_boxplots.png", width = 6, height = 3.5)

scaling_data_combined %>% 
  filter(scaling != "Uncertain") %>% 
ggplot(aes(wshd_max_elevation_m, accm_totco2_o2g_day)) + 
  geom_point(aes(color = scaling)) + 
  geom_convexhull(aes(group = scaling, color = scaling, 
                      fill = scaling), alpha = 0.2) +
  scale_y_log10() + 
  geom_smooth(method = "lm", color = "black")  + 
  facet_wrap(~basin, nrow = 1) + 
  scale_color_viridis_d() + 
  ggpubr::stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")






