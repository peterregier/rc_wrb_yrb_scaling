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

ggplot(scaling_data_combined, aes(scaling, wshd_max_elevation_m, color = basin)) + 
  geom_boxplot() + 
 # scale_y_log10() + 
  geom_smooth() 

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
ggplot(scaling_data_combined, aes(wshd_max_elevation_m, accm_totco2_o2g_day)) + 
  #ggplot(scaling_data_combined, aes(wshd_max_elevation_m, accm_totco2_o2g_day / wshd_area_km2)) + 
  geom_point(aes(color = scaling, size = mean_ann_pcpt_mm), alpha = 0.5) + 
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
       size = "Mean precip (mm/yr)",
       color = "Scaling", 
       fill = "Scaling")
ggsave("figures/5_Figure5.png", width = 10, height = 5)
ggsave("figures/5_Figure5.pdf", width = 10, height = 5)

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
ggsave("figures/s_elevation_v_area.png", width = 5, height = 4)
ggsave("figures/s_elevation_v_area.pdf", width = 5, height = 4)

## What about velocity?
ggplot(scaling_analysis_dat, aes(stream_order, mean_ann_vel_ms, color = basin)) + 
  geom_point(alpha = 0.4) + 
  facet_wrap(~basin, ncol = 1)

## How do watershed area and HEF correlate? 
scaling_analysis_dat %>% 
  mutate(accm_hzt_cat = fct_relevel(accm_hzt_cat, "Q100", after = Inf)) %>% 
  ggplot(aes(accm_hzt_cat, wshd_area_km2)) + 
  geom_boxplot() + 
  facet_wrap(~basin, ncol = 1) + 
  scale_y_log10()

## What about precip and cumulative respiration? 
ggplot(scaling_analysis_dat, aes(mean_ann_pcpt_mm, accm_totco2_o2g_day, color = basin)) + 
  geom_point(alpha = 0.4) + 
  ggpubr::stat_cor(aes(label = after_stat(rr.label)), geom = "label") 

ggplot(scaling_analysis_dat, aes(mean_ann_pcpt_m3, accm_totco2_o2g_day, color = basin)) + 
  geom_point(alpha = 0.4) + 
  ggpubr::stat_cor(aes(label = after_stat(rr.label)), geom = "label") 

ggplot(scaling_analysis_dat, aes(mean_ann_pcpt_m3, mean_ann_pcpt_mm, color = basin)) + 
  geom_point(alpha = 0.4) + 
  ggpubr::stat_cor(aes(label = after_stat(rr.label)), geom = "label") 


## WW point is good: HEF, stream order and max elevation are likely all similar...
## Let's examine here:

p_order_hef <- scaling_analysis_dat %>% 
  mutate(accm_hzt_cat = fct_relevel(accm_hzt_cat, "Q100", after = Inf)) %>% 
  ggplot(aes(accm_hzt_cat, stream_order, color = basin)) + 
  geom_boxplot(alpha = 0.4) 

p_hef_elev <- scaling_analysis_dat %>% 
  mutate(accm_hzt_cat = fct_relevel(accm_hzt_cat, "Q100", after = Inf)) %>% 
  ggplot(aes(accm_hzt_cat, wshd_max_elevation_m, color = basin)) + 
  geom_boxplot(alpha = 0.4) 

p_order_elev <- scaling_analysis_dat %>% 
  ggplot(aes(as.factor(stream_order), wshd_max_elevation_m, color = basin)) + 
  geom_boxplot(alpha = 0.4) 

plot_grid(p_order_hef, p_hef_elev, p_order_elev, 
          ncol = 1)
ggsave("figures/240224_comparison_hef_stream_order_elevation.png", 
       width = 7, height = 11)

make_boxplot <- function(var){
  ggplot(scaling_data_combined, aes(basin, {{var}}, fill = scaling)) + 
    geom_boxplot()
}

make_boxplot(wshd_max_elevation_m)


p_load(infotheo)

scaling_data_mi <- scaling_data_combined %>% 
  select(basin, scaling, d50_cat, rnf_cat, mean_ann_pcpt_mm, wshd_max_elevation_m) %>% 
  mutate()


## Try out geographically weighted random forest
p_load(SpatialML)

rf_data <- scaling_data_combined %>% 
  select(stream_order, doc_stream_mg_l, no3_stream_mg_l, 
         do_stream_mg_l, stream_area_m2, stream_width_m, reach_slope, d50_m) 

coordinates <- scaling_data_combined %>% 
  select(latitude, longitude) 

grf_model <- grf(scaling ~ basin + doc_stream_mg_l + no3_stream_mg_l + do_stream_mg_l, 
                 dframe = rf_data, 
                 bw = 60, 
                 kernel = "adaptive",
                 coords = coordinates)

data(Income)
Coords<-Income[ ,1:2]
grf <- grf(Income01 ~ UnemrT01 + PrSect01, dframe=Income, bw=60,
           kernel="adaptive", coords=Coords)













