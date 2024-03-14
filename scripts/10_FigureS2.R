## This script tries to replicate parts of the conceptual figure in Wil's paper
## to illustrate claculating cumualtive metrics for allometric scaling
##

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
p_load(tmaptools)

# 2. Prep datasets -------------------------------------------------------------

yrb <- read_sf("data/nsi_network_ywrb/nsi_network_ywrb.shp") %>% 
  st_transform(crs = common_crs) %>% 
  st_crop(nsi, xmin = -122, xmax = -119, ymin = 45.9, ymax = 48)

yakima_boundary <- get_huc(AOI = st_union(yrb), type = "huc04") %>% 
  filter(huc4 == "1703")

scaling_map_dat <- scaling_analysis_dat %>% 
  mutate(log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm)) %>% 
  mutate(dominant_lc = colnames(select(., contains("_3scp")))[max.col(select(., contains("_3scp")), "first")])

scaling_dat_trimmed <- scaling_map_dat %>% 
  select(comid, 
         basin_cat,
         basin,
         wshd_area_km2, 
         accm_hzt_cat,
         totco2_o2g_day,
         accm_totco2_o2g_day, 
         longitude,
         latitude,
         log_mean_ann_pcpt_mm, 
         wshd_avg_elevation_m, 
         dominant_lc)

scaling_sf <- inner_join(yrb %>% clean_names(), 
                             scaling_dat_trimmed, by = "comid")


## 50 is pretty good
## 60 is ok
## 100 is skinny but interesting
## 130 is excellent but a weird shape
## 150 is excellent, and a good shape, but not headwaters
## 240 is almost perfect except for one weird spot
## 320 is excellent
## 360 is perfect.
huc12 <- get_huc(AOI = scaling_sf %>% slice(360), type = "huc12") 

sub_flow <- scaling_sf %>% 
  crop_shape(huc12, polygon = T) %>% 
  mutate(log_area = log10(wshd_area_km2), 
         log_resp = log10(accm_totco2_o2g_day))

ggplot() + 
  geom_sf(data = scaling_sf) + 
  geom_sf(data = huc12, fill = "forestgreen")

plot_resp <- ggplot() + 
  geom_sf(data = huc12, fill = "forestgreen", alpha = 0.2) + 
  geom_sf(data = sub_flow, color = "gray50", lwd = 2, alpha = 0.5) + 
  geom_sf(data = sub_flow, aes(color = totco2_o2g_day), lwd = 1) + 
  geom_sf_text(data = sub_flow, aes(label = round(totco2_o2g_day, 0)), 
               size = 2.5, fontface = "bold") + 
  scale_color_viridis_c(trans = "log10", option = "D") + 
  theme_map() + 
  labs(title = "Reach-scale respiration", color = "g CO2/d") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5))

plot_accm_resp <- ggplot() + 
  geom_sf(data = huc12, fill = "forestgreen", alpha = 0.2) + 
  geom_sf(data = sub_flow, color = "gray50", lwd = 2, alpha = 0.5) + 
  geom_sf(data = sub_flow, aes(color = accm_totco2_o2g_day), lwd = 1) + 
  geom_sf_text(data = sub_flow, aes(label = round(accm_totco2_o2g_day, 0)), 
               size = 2.5, fontface = "bold") + 
  scale_color_viridis_c(trans = "log10", option = "D") + 
  theme_map() + 
  labs(title = "Cumulative respiration", color = "g CO2/d") + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5))

plot_grid(plot_resp, plot_accm_resp, 
          nrow = 1)
ggsave("figures/s2_cumulative_concept_map.png", width = 8, height = 4)
ggsave("figures/s2_cumulative_concept_map.pdf", width = 8, height = 4)
