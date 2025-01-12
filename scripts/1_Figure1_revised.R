## This script makes the map elements used in Figure 1 as well as supplemental
## maps of 
## 
## Code from Francisco Guerrero and Peter Regier
## Contact: peter.regier@pnnl.gov
##
# ######### #
# ######### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
p_load(viridis)


# 2. Prep watershed boundaries and flowlines -----------------------------------

## Read in shapefiles with flowlines for the two study basins
nsi <- read_sf("data/nsi_network_ywrb/nsi_network_ywrb.shp") %>% 
  st_transform(crs = common_crs)

## There isn't a convenient way to break these apart, so I'll use st_crop
yakima_flowlines <- st_crop(nsi, xmin = -122, xmax = -119, ymin = 45.9, ymax = 48)
willamette_flowlines <- st_crop(nsi, xmin = -124, xmax = -121, ymin = 43, ymax = 46)

## YRB
yakima_boundary <- get_huc(AOI = st_union(yakima_flowlines), type = "huc04") %>% 
  filter(huc4 == "1703")

## WRB
willamette_boundary <- read_sf("data/basin_boundaries/Willamette_Custom_Watershed_Boundary_noCorner/Willamette_Custom_Watershed_Boundary_noCorner.shp") %>% 
  st_transform(crs = common_crs)


## 3. Make WA/OR with watersheds map -------------------------------------------

## US map
states <- ne_states(country = "united states of america", 
                    returnclass = "sf") %>% 
  filter(gn_name == "Oregon" | gn_name == "Washington")

## Create map layer with the two states containing study basins
wa_or_plot <- ggplot() + 
  geom_sf(data = states, fill = "gray95") + 
  geom_sf(data = yakima_boundary, fill = "gray50", alpha = 0.2) + 
  geom_sf(data = willamette_boundary, fill = "gray50", alpha = 0.2) + 
  #geom_sf(data = nsi, color = "blue", lwd = 0.02, alpha = 0.8) + 
  # geom_sf(data = nsi %>% filter(grepl("Yakima|^Willamette River", GNIS_NAME)), color = "blue", lwd = 0.3, alpha = 1) + 
  #geom_sf(data = nsi %>% filter(grepl("Yakima River", GNIS_NAME)), color = "blue", lwd = 0.2, alpha = 1) + 
  geom_sf(data = yakima_boundary, fill = NA, color = "black", lwd = 0.3) + 
  geom_sf(data = willamette_boundary, fill = NA, color = "black", lwd = 0.3) + 
  coord_sf(crs = coord_sf_crs) + 
  theme_map()


# 4. Prep dataset --------------------------------------------------------------

## Prep scaling_analysis_dat
scaling_map_dat <- scaling_analysis_dat %>% 
  mutate(log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm)) %>% 
  mutate(dominant_lc = colnames(dplyr::select(., contains("_3scp")))[max.col(dplyr::select(., contains("_3scp")), "first")])

## Prep scaling_analysis_dat
scaling_dat_trimmed <- scaling_map_dat %>% 
  dplyr::select(comid, 
         basin_cat,
         basin,
         wshd_area_km2, 
         tot_q_hz_ms,
         accm_hzt_cat,
         accm_water_exchng_kg_d,
         accm_totco2_o2g_day, 
         longitude,
         latitude,
         log_mean_ann_pcpt_mm, 
         wshd_avg_elevation_m, 
         dominant_lc)

## Convert prepped dataset to an sf object for plotting
scaling_map_sf <- inner_join(nsi %>% clean_names(), 
                             scaling_dat_trimmed, by = "comid") %>% 
  mutate(dominant_lc = case_when(dominant_lc == "shrub_3scp" ~ "Shrub", 
                                 dominant_lc == "forest_3scp" ~ "Forest", 
                                 dominant_lc == "human_3scp" ~ "Human"))


# 5. Make respiration maps -----------------------------------------------------

wrb_rel_height = 0.6

make_map <- function(var, color_scheme, color_direction, y_lab){
  
  min_color = min(scaling_map_sf %>% pull({{var}}), na.rm = T)
  max_color = max(scaling_map_sf %>% pull({{var}}), na.rm = T)
  
  yrb <- ggplot() +
    geom_sf(data = scaling_map_sf %>% filter(basin == "yakima"), 
            aes(color = {{var}}), show.legend = T) + 
    geom_sf(data = yakima_boundary, fill = NA, color = "black", lwd = 0.8) + 
    scale_color_viridis_c(option = color_scheme, 
                          direction = color_direction,
                          trans = "log10", 
                          limits = c(min_color, max_color)) + 
    theme_map() + 
    labs(color = y_lab) + 
    theme(legend.background = element_blank()) + 
    theme(legend.position = "bottom", 
          legend.justification = "center") + 
    guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) 
  
  wrb <- ggplot() +
    geom_sf(data = scaling_map_sf %>% filter(basin == "willamette"), 
            aes(color = {{var}}), show.legend = F) + 
    geom_sf(data = willamette_boundary, fill = NA, color = "black", lwd = 0.8) + 
    scale_color_viridis_c(option = color_scheme,
                          direction = color_direction,
                          trans = "log10", 
                          limits = c(min_color, max_color)) + 
    theme_map() 
  
  plot_grid(wrb, yrb, 
            ncol = 1, 
            rel_heights = c(wrb_rel_height, 1))
}

## Because landscape is categorical, we can't use that function and have to manually modify

yrb_landscape <- ggplot() +
  geom_sf(data = scaling_map_sf %>% filter(basin == "yakima"), 
          aes(color = dominant_lc), show.legend = T) + 
  geom_sf(data = yakima_boundary, fill = NA, color = "black", lwd = 0.8) + 
  scale_color_viridis_d() + 
  theme_map() + 
  labs(color = "Dominant landscape") + 
  theme(legend.background = element_blank()) + 
  theme(legend.position = "bottom", 
        legend.justification = "center") + 
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) 

wrb_landscape <- ggplot() +
  geom_sf(data = scaling_map_sf %>% filter(basin == "willamette"), 
          aes(color = dominant_lc), show.legend = F) + 
  geom_sf(data = willamette_boundary, fill = NA, color = "black", lwd = 0.8) + 
  scale_color_viridis_d() + 
  theme_map() 

landscape_plot <- plot_grid(wrb_landscape, yrb_landscape, 
          ncol = 1, 
          rel_heights = c(wrb_rel_height, 1))

plot_grid(make_map(log_mean_ann_pcpt_mm, "turbo", -1, "Precipitation (log-mm)"),
          make_map(wshd_avg_elevation_m, "inferno", 1, "Elevation (m)"),
          landscape_plot, 
          #make_map(tot_q_hz_ms, "mako", -1, "HEF (m/s"),
          make_map(accm_water_exchng_kg_d, "mako", -1, "HEF (m/s)"),
          make_map(accm_totco2_o2g_day, "viridis", -1, "Cumulative Respiration (gCO2/d)"),
          nrow = 1)
ggsave("figures/1_Figure1_unformatted.png", width = 15, height = 10)
ggsave("figures/1_Figure1_unformatted.pdf", width = 15, height = 10)



scaling_no_sf <- scaling_map_sf %>% 
  st_drop_geometry() 

p1 <- scaling_no_sf %>% 
  ggplot(aes(wshd_avg_elevation_m, accm_water_exchng_kg_d)) + 
  geom_point(alpha = 0.05) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~basin, scale = "free") + 
  labs(x = "Elevation (m)", y = "Cumulative HEF (kg/d)")

p2 <- scaling_no_sf %>% 
  ggplot(aes(wshd_area_km2, accm_water_exchng_kg_d)) + 
  geom_point(alpha = 0.05) + 
  scale_x_log10() + 
  scale_y_log10() + 
  facet_wrap(~basin, scale = "free") + 
  labs(x = "Watershed area (km2)", y = "Cumulative HEF (kg/d)")

plot_grid(p1, p2, ncol = 1)
ggsave("figures/sd_elevation_v_HEF.png", width = 7, height = 7)

