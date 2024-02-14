## This script tries to replicate parts of the conceptual figure in Wil's paper
## to illustrate claculating cumualtive metrics for allometric scaling
##

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")


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
ggsave("figures/240202_cumulative_concept_map.png",
       width = 8, height = 4)
# ggsave("figures/proposal/240108_proposal_maps.pdf", 
#        width = 8, height = 4)

b = summary(lm(log_resp ~ log_area, data = sub_flow))[[4]][1,1]
line_width = 1.2

ggplot(sub_flow, aes(wshd_area_km2, accm_totco2_o2g_day))  + 
  geom_point(alpha = 0.5, size = 2) + 
  #geom_smooth(method = "lm", se = F) + 
  geom_abline(intercept = b, slope = 1, color = "forestgreen", lwd = line_width) + 
  geom_abline(intercept = b, slope = 0.5, color = "red", alpha = 0.8, lwd = line_width) + 
  geom_abline(intercept = b, slope = 2, color = "blue", alpha = 0.8, lwd = line_width) + 
  #scale_x_log10(limits = c(1.4, 1100), ) + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Cumulative watershed area (km)", 
       y = "Cumulative respiration (g CO2/d", 
       title = "Allometric scaling") + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figures/proposal/240108_scaling_conceptual_figure.pdf", 
       width = 4, height = 3.5)




min_resp = min(scaling_sf$accm_totco2_o2g_day)
max_resp = max(scaling_sf$accm_totco2_o2g_day)

## make scaling depiction
ggplot() + 
  geom_sf(data = huc12, fill = "forestgreen", alpha = 0.2, color = "black", lwd = 0.5) + 
  geom_sf(data = sub_flow, color = "gray50", lwd = 2, alpha = 0.5) + 
  geom_sf(data = sub_flow, aes(color = accm_totco2_o2g_day), lwd = 1) + 
  scale_color_viridis_c(trans = "log10", option = "D", 
                        limits = c(min_resp, max_resp)) + 
  theme_map() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
ggsave("figures/240109_subbasin_inset_rescaled.png", 
       width = 4, height = 3)


conus <- ne_states(country = "united states of america", 
                    returnclass = "sf") %>% 
  filter(gn_name != "Alaska" & gn_name != "Hawaii")

ggplot() + 
  geom_sf(data = conus, fill = "gray95", color = "gray80") + 
  geom_sf(data = yakima_boundary, fill = "forestgreen") + 
  theme_map() + 
  coord_sf(crs = coord_sf_crs)
ggsave("figures/240109_conus_map.png", 
       width = 5, height = 4)

## Time to pull some random watersheds to show ... diversity? Mostly to fill space

plot_subbasin12 <- function(row){
  huc12 <- get_huc(AOI = scaling_sf %>% slice(row), type = "huc12") 
  
  sub_flow <- scaling_sf %>% 
    crop_shape(huc12, polygon = T) 
  
  p1 <- ggplot() + 
    geom_sf(data = huc12, fill = "forestgreen", alpha = 0.2, color = "black", lwd = 0.5) + 
    geom_sf(data = sub_flow, color = "gray50", lwd = 2, alpha = 0.5) + 
    geom_sf(data = sub_flow, aes(color = accm_totco2_o2g_day), lwd = 1) + 
    scale_color_viridis_c(trans = "log10", option = "D", 
                          limits = c(min_resp, max_resp)) + 
    theme_map() + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
  
 # p2 <- ggplot() + 
 #   geom_sf(data = yakima_boundary, color = "black", fill = "white") + 
 #   geom_sf(data = scaling_sf, color = "gray90", show.legend = F) + 
 #   geom_sf(data = huc12, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) + 
 #   scale_color_viridis_c()
 #   theme_map() 
 # 
 # plot_grid(p1, p2)
  
  p1
}

plot_subbasin10 <- function(row){
  huc10 <- get_huc(AOI = scaling_sf %>% slice(row), type = "huc10")
  
  #huc10 <- get_huc(AOI = scaling_sf %>% filter(gnis_id == row), type = "huc10")
  
  sub_flow <- scaling_sf %>% 
    crop_shape(huc10, polygon = T) 
  
  p1 <- ggplot() + 
    geom_sf(data = huc10, fill = "forestgreen", alpha = 0.2, color = "black", lwd = 0.5) + 
    geom_sf(data = sub_flow, color = "gray50", lwd = 2, alpha = 0.5) + 
    geom_sf(data = sub_flow, aes(color = accm_totco2_o2g_day), lwd = 1) + 
    scale_color_viridis_c(trans = "log10", option = "D", 
                          limits = c(min_resp, max_resp)) + 
    theme_map() + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
  
  # p2 <- ggplot() +
  #   geom_sf(data = yakima_boundary, color = "black", fill = "white") +
  #   geom_sf(data = scaling_sf, color = "gray90", show.legend = F) +
  #   geom_sf(data = huc10, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) +
  #   scale_color_viridis_c()
  #   theme_map()
  # 
  # plot_grid(p1, p2)
  
  p1
}

plot_subbasin8 <- function(row){
  huc8 <- get_huc(AOI = scaling_sf %>% slice(row), type = "huc08") 
  
  sub_flow <- scaling_sf %>% 
    crop_shape(huc8, polygon = T) 
  
  p1 <- ggplot() + 
    geom_sf(data = huc8, fill = "forestgreen", alpha = 0.2, color = "black", lwd = 0.5) + 
    geom_sf(data = sub_flow, color = "gray50", lwd = 2, alpha = 0.5) + 
    geom_sf(data = sub_flow, aes(color = accm_totco2_o2g_day), lwd = 1) + 
    scale_color_viridis_c(trans = "log10", option = "D", 
                          limits = c(min_resp, max_resp)) + 
    theme_map() + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
  
  p2 <- ggplot() +
    geom_sf(data = yakima_boundary, color = "black", fill = "white") +
    geom_sf(data = scaling_sf, color = "gray90", show.legend = F) +
    geom_sf(data = huc8, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) +
    scale_color_viridis_c()
  theme_map()
  
  plot_grid(p1, p2)
  
  #p1
}

plot_subbasin10(6550)

sb_width = 4
sb_height = 3

plot_subbasin12(360)
ggsave("figures/proposal/230110_sb1.pdf", width = sb_width, height = sb_height)

plot_subbasin10(1000)
ggsave("figures/proposal/230110_sb2.pdf", width = sb_width, height = sb_height)

plot_subbasin10(2000)
ggsave("figures/proposal/230110_sb3.pdf", width = sb_width, height = sb_height)

plot_subbasin10(3928)
ggsave("figures/proposal/230110_sb4.pdf", width = sb_width, height = sb_height)



## Set up the 4 sub-basins
huc_1 <- get_huc(AOI = scaling_sf %>% slice(360), type = "huc08") 
huc_2 <- get_huc(AOI = scaling_sf %>% slice(1000), type = "huc10") 
huc_3 <- get_huc(AOI = scaling_sf %>% slice(2000), type = "huc10") 
huc_4 <- get_huc(AOI = scaling_sf %>% slice(3928), type = "huc10")



ggplot() + 
  geom_sf(data = yakima_boundary, color = "black", fill = "white") + 
  geom_sf(data = huc_1)

satus <- scaling_sf %>% 
  filter(grepl("Satus", gnis_name))

naches <- scaling_sf %>% 
  filter(grepl("Naches", gnis_name))

tieton <- scaling_sf %>% 
  filter(grepl("Tieton", gnis_name))

teanaway <- scaling_sf %>% 
  filter(grepl("Teanaway", gnis_name))

yakima <- scaling_sf %>% 
  filter(grepl("Yakima", gnis_name))

ggplot() + 
  geom_sf(data = yakima_boundary, color = "black", fill = "white") + 
  geom_sf(data = satus, color = "blue") + 
  geom_sf(data = naches, color = "blue") + 
  geom_sf(data = teanaway, color = "blue") + 
  geom_sf(data = tieton, color = "lightblue") + 
  geom_sf(data = yakima, color = "red") + 
  ggtitle("Satus (S) Naches (NW) and Teanaway (N)")


## Now for something completely different....
custom_yrb_boundaries <- read_sf("/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/RC/rcsfa-ST2-2-streampulse/data/Yakama_Basin_Custom_Boundaries/Yakama_Basin_Custom_Boundaries.shp")

ggplot() + 
  geom_sf(data = yakima_boundary, color = "black", fill = "white") + 
  geom_sf(data = scaling_sf, 
          aes(color = accm_totco2_o2g_day), show.legend = F) + 
  geom_sf(data = custom_yrb_boundaries, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) + 
  #geom_sf(data = huc_1, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) + 
  #geom_sf(data = huc_2, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) + 
  #geom_sf(data = huc_3, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) + 
  #geom_sf(data = huc_4, fill = "forestgreen", color = "black", lwd = 0.5, alpha = 0.5) + 
  theme_map() + 
  scale_color_viridis_c(trans = "log10", option = "D", 
                        limits = c(min_resp, max_resp))
ggsave("figures/proposal/240108_yrb_basin_map.pdf", 
       width = 4, height = 5)



plot_watershed <- function(i){
  
  watershed <- custom_yrb_boundaries %>% slice(i)
  
  min_resp = min(scaling_sf$accm_totco2_o2g_day)
  max_resp = max(scaling_sf$accm_totco2_o2g_day)
  
  x <- scaling_sf %>% 
    crop_shape(watershed, polygon = T)
  
  ggplot() + 
    geom_sf(data = watershed, fill = "forestgreen", alpha = 0.2, color = "black", lwd = 0.5) + 
    geom_sf(data = x, color = "gray50", lwd = 1.5, alpha = 0.5) + 
    geom_sf(data = x, aes(color = accm_totco2_o2g_day), lwd = 0.5) + 
    scale_color_viridis_c(trans = "log10", option = "D", 
                          limits = c(min_resp, max_resp)) + 
    theme_map() + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))
  ggsave(paste0("figures/proposal/subbasin", i, ".pdf"), 
         width = 5, height = 5)
}

c(1:3) %>% 
  map(plot_watershed)




