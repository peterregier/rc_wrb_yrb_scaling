## Explore median values for scaling based on HEF bins per WW comment

source("scripts/0_setup.R")

p_load(segmented, 
       ggpubr)

medians <- scaling_analysis_dat %>% 
  mutate(accm_hzt_cat = factor(accm_hzt_cat,
                               levels = c("Q10","Q20","Q30","Q40","Q50",
                                          "Q60","Q70","Q80","Q90","Q100"))) %>% 
  ungroup() %>% 
  group_by(basin_cat, accm_hzt_cat) %>% 
  summarize(median_wshd_area_km2 = median(wshd_area_km2), 
            median_accm_totco2_o2g_day = median(accm_totco2_o2g_day))

lm_model <- lm(median_accm_totco2_o2g_day~median_wshd_area_km2, data = medians)
seg_model <- segmented(lm_model, seg.Z = ~median_wshd_area_km2, psi = c(20))
breakpoint <- summary(seg_model)[[12]][2]

lm_small <- summary(lm(log10(median_accm_totco2_o2g_day)~log10(median_wshd_area_km2), 
                       data = medians %>% filter(median_wshd_area_km2 < 10)))

lm_large <- summary(lm(log10(median_accm_totco2_o2g_day)~log10(median_wshd_area_km2), 
                       data = medians %>% filter(median_wshd_area_km2 > 10)))

## Helper function to plot useful stats
lm_stats <- function(model){
  m = model[[4]][2,1]
  b = model[[4]][1,1]
  r2 = model[[9]]
  
  paste0("R2: ", round(r2, 2), 
         " , slope = ", round(m, 1))
}

#p_load(ggpmisc)

small_watershed_color = "blue"
big_watershed_color = "red"

ggplot(medians, aes(x = median_wshd_area_km2, 
           y = median_accm_totco2_o2g_day)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") + 
  geom_smooth(data = medians %>% filter(median_wshd_area_km2 < 10), 
              method = "lm", se = T, color = small_watershed_color) +
  geom_smooth(data = medians %>% filter(median_wshd_area_km2 > 10), 
              method = "lm", se = T, , color = big_watershed_color) +
  geom_point(aes(shape = basin_cat), size = 3, alpha = 0.7) + 
  annotate(geom = "text", x = 10, y = 4e8, label = lm_stats(lm_small), color = small_watershed_color) + 
  annotate(geom = "text", x = 10, y = 1e8, label = lm_stats(lm_large), color = big_watershed_color) + 
  #ggpubr::stat_cor(aes(label = after_stat(rr.label)), geom = "label") + 
  #stat_poly_eq(data = medians %>% filter(median_wshd_area_km2 < 10), 
  #             color = small_watershed_color) + 
  #stat_poly_eq(data = medians %>% filter(median_wshd_area_km2 > 10), 
  #             label.y = 0.9, color = big_watershed_color) + 
  scale_shape(labels = c("WRB", "YRB")) + 
  labs(x = "Median Watershed area (km2)", 
         y = "Median Cumulative aerobic respiration (g/day)", 
         shape = "Basin") +
  scale_x_log10() +  
  scale_y_log10()
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



