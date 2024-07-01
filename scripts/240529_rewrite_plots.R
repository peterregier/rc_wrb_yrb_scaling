

# ######### #
# ######### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
p_load(ggpmisc)


## How does HEF relate to reach-scale respiration
ggplot(scaling_analysis_dat, 
       aes(tot_q_hz_ms, totco2_o2g_m2_day, color = basin)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq()
  
## How does hyporheic area scale with watershed area? --------------------------

ggplot(scaling_analysis_dat, aes(wshd_area_km2, accm_stream_area_m2, color = basin)) + 
  geom_point() + 
  geom_smooth()

