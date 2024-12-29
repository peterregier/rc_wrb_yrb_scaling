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
  mutate(dominant_lc = colnames(select(., contains("_3scp")))[max.col(select(., contains("_3scp")), "first")])

## Prep scaling_analysis_dat
scaling_dat_trimmed <- scaling_map_dat %>% 
  select(comid, 
         basin_cat,
         basin,
         wshd_area_km2, 
         accm_hzt_cat,
         accm_totco2_o2g_day, 
         longitude,
         latitude,
         log_mean_ann_pcpt_mm, 
         wshd_avg_elevation_m, 
         dominant_lc)

## Convert prepped dataset to an sf object for plotting
scaling_map_sf <- inner_join(nsi %>% clean_names(), 
                             scaling_dat_trimmed, by = "comid")


# 5. Make respiration maps -----------------------------------------------------

## Calculate stats to match colors between maps (facet_wrap(..., scales = F) fails w coord_sf)
min_resp = min(scaling_dat_trimmed$accm_totco2_o2g_day)
max_resp = max(scaling_dat_trimmed$accm_totco2_o2g_day)

## Make a respiration map for the YRB
yrb_resp <- ggplot() +
  geom_sf(data = scaling_map_sf %>% filter(basin == "yakima"), 
          aes(color = accm_totco2_o2g_day), show.legend = F) + 
  geom_sf(data = yakima_boundary, fill = NA, color = "black") + 
  scale_color_viridis_c(trans = "log10", limits = c(min_resp, max_resp)) + 
  theme_map() + 
  labs(color = "Cumulative \n Respiration \n (gCO2/d)") + 
  theme(legend.position = c(0.8, 0.7), 
        legend.background = element_blank())

## Make a respiration map for the WRB
wrb_resp <- ggplot() +
  geom_sf(data = scaling_map_sf %>% filter(basin == "willamette"), aes(color = accm_totco2_o2g_day)) + 
  geom_sf(data = willamette_boundary, fill = NA, color = "black") + 
  scale_color_viridis_c(trans = "log10", limits = c(min_resp, max_resp)) + 
  theme_map() + 
  labs(color = "Cumulative \n Respiration \n (gCO2/d)")


# 6. Assemble plots and export -------------------------------------------------

## Pull the legend as a separate object
resp_legend = get_legend(wrb_resp + 
                           theme(legend.position = c(0.5, 0.3)))

## Create the final plot by combining plot objects
plot_grid(wa_or_plot, 
          yrb_resp, 
          wrb_resp + theme(legend.position = "none"),
          resp_legend, 
          nrow = 1, 
          rel_widths = c(1, 1, 0.9, 0.4), 
          labels = c("A", "B", "C"))
          
## Save layers as raw figure, which is then cleaned up in Affinity Designer
ggsave("figures/1_raw_Figure1_maps.pdf", width = 12, height = 4)

