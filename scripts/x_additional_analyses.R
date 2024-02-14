## Explore median values for scaling based on HEF bins per WW comment

source("scripts/0_setup.R")

scaling_analysis_dat %>% 
  mutate(accm_hzt_cat = factor(accm_hzt_cat,
                               levels = c("Q10","Q20","Q30","Q40","Q50",
                                          "Q60","Q70","Q80","Q90","Q100"))) %>% 
  ungroup() %>% 
  group_by(basin_cat, accm_hzt_cat) %>% 
  summarize(median_wshd_area_km2 = median(wshd_area_km2), 
            median_accm_totco2_o2g_day = median(accm_totco2_o2g_day)) %>% 
  ggplot(aes(x = median_wshd_area_km2, 
             y = median_accm_totco2_o2g_day, 
             color = basin_cat)) + 
  geom_point(size = 3, alpha = 0.8) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_abline(intercept = 0, slope = 1)
ggsave("figures/x_fig2_by_medians.png", width = 6, height = 4)

p_load(ggpmisc, #stat_poly_eq
       car) #vif

ggplot(scaling_analysis_dat, aes(wshd_area_km2, wshd_max_elevation_m, color = basin_cat)) + 
  geom_point() + 
  scale_x_log10() + 
  stat_poly_eq()


wrb <- scaling_analysis_dat %>% 
  filter(basin == "willamette") %>% 
  drop_na()

yrb <- scaling_analysis_dat %>% 
  filter(basin == "yakima") %>% 
  drop_na()

wrb_model = lm(wshd_max_elevation_m~wshd_area_km2, data = wrb)
yrb_model = lm(wshd_max_elevation_m~wshd_area_km2, data = yrb)

vif(wrb_model)
vif(yrb_model)



