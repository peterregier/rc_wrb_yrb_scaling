## This script creates Figure 5 and Figure S5
##
## Code from Francisco J. Guerrero and Peter Regier
## Contact: peter.regier@pnnl.gov
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
  dplyr::select(basin, quantile, r_squared, slope, contains("slope_ci"), scaling) %>% 
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

## lm models - doing this way so R2 values are calculated consistently across 
## analyses
calc_r2 <- function(which_basin){
  r2 = summary(lm(log10(accm_totco2_o2g_day)~log10(wshd_max_elevation_m), 
                  data = scaling_data_combined %>% filter(basin == which_basin)))[[9]]
  
  paste0("R2: ", round(r2, 2))
}

calc_r2("willamette")
calc_r2("yakima")


p_load(ggConvexHull)
scaling_data_combined %>% 
  mutate(basin = case_when(basin == "yakima" ~ "Yakima (YRB)", 
                           basin == "willamette" ~ "Willamette (WRB)",)) %>% 
  ggplot(aes(wshd_max_elevation_m, accm_totco2_o2g_day)) + 
  #ggplot(scaling_data_combined, aes(wshd_max_elevation_m, accm_totco2_o2g_day / wshd_area_km2)) + 
  #geom_point(aes(color = scaling, size = mean_ann_pcpt_mm), alpha = 0.5) + 
  geom_point(aes(color = scaling), alpha = 0.5) + 
  geom_convexhull(aes(group = scaling, color = scaling, 
                      fill = scaling), alpha = 0.2) +
  #scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method = "lm", color = "black")  + 
  facet_wrap(~basin, nrow = 1) + 
  scale_size_continuous(range = c(0.1, 3)) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  #ggpubr::stat_cor(aes(label = after_stat(rr.label)), geom = "label") + 
  labs(x = "Maximum watershed elevation (m)", 
       y = "Cumulative respiration (gCO2/day)",
       #size = "Mean precip (mm/yr)",
       color = "Scaling", 
       fill = "Scaling")
ggsave("figures/5_Figure5.png", width = 10, height = 5)
ggsave("figures/5_Figure5.pdf", width = 10, height = 5)


scaling_data_combined %>% 
  filter(scaling == "Uncertain") %>% 
  mutate(basin = case_when(basin == "yakima" ~ "Yakima (YRB)", 
                           basin == "willamette" ~ "Willamette (WRB)",)) %>% 
  ggplot(aes(wshd_max_elevation_m, mean_ann_pcpt_mm)) + 
  geom_point(aes(color = scaling)) + 
  facet_wrap(~basin)

# Make supplemental figure -----------------------------------------------------

calc_lm <- function(selected_basin){
  x <- scaling_data_raw %>% 
    filter(basin == selected_basin)
  
  model <- summary(lm(log10(wshd_max_elevation_m)~log10(wshd_area_km2), 
               data = x))
  
  model[[9]]
}

calc_lm("willamette")
calc_lm("yakima")

ggplot(scaling_data_raw, aes(wshd_area_km2, wshd_max_elevation_m, color = basin)) + 
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", aes(group = basin), color = "black") + 
  scale_x_log10() + 
  scale_y_log10()
ggsave("figures/s5_elevation_v_area.png", width = 5, height = 4)
ggsave("figures/s5_elevation_v_area.pdf", width = 5, height = 4)
