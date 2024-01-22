## This script is to make maps for AGU Scaling Poster

require(pacman)
p_load(tidyverse, 
       janitor,
       rnaturalearth,
       nhdplusTools,
       ggthemes,
       ggallin,
       sf)

source("scripts/0_setup.R")

common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"


# 1. Read in watershed shapefiles ----------------------------------------------

nsi <- read_sf("data/nsi_network_ywrb/nsi_network_ywrb.shp") %>% 
  st_transform(crs = common_crs)

ggplot() + 
  geom_sf(data = nsi)

## There isn't a convenient way to break these apart, so I'll use st_crop
yakima_flowlines <- st_crop(nsi, xmin = -122, xmax = -119, ymin = 45.9, ymax = 48)
willamette_flowlines <- st_crop(nsi, xmin = -124, xmax = -121, ymin = 43, ymax = 46)

## Now squish em back together with watershed labeled
flowlines <- bind_rows(yakima_flowlines %>% mutate(watershed = "Yakima"), 
                       willamette_flowlines %>% mutate(watershed = "Willamette"))

## YRB
yakima_boundary <- get_huc(AOI = st_union(yakima_flowlines), type = "huc04") %>% 
  filter(huc4 == "1703")

# WRB - same as before, this doesn't cut it. Commenting out but keeping for now
# willamette_boundary <- get_huc(AOI = st_union(willamette_flowlines), type = "huc04") %>%
#   filter(huc4 == "1709")
willamette_boundary <- read_sf("data/basin_boundaries/Willamette_Custom_Watershed_Boundary/Willamette_Custom_Watershed_Boundary.shp") %>% 
  st_transform(crs = common_crs)


## 2. Make WA/OR with watersheds map -------------------------------------------

## US map
states <- ne_states(country = "united states of america", 
                returnclass = "sf") %>% 
  filter(gn_name == "Oregon" | gn_name == "Washington")

ggplot() + 
  geom_sf(data = states, fill = "gray95") + 
  geom_sf(data = yakima_boundary, fill = "forestgreen", alpha = 0.2) + 
  geom_sf(data = willamette_boundary, fill = "forestgreen", alpha = 0.2) + 
  geom_sf(data = nsi, color = "blue", lwd = 0.02, alpha = 0.8) + 
 # geom_sf(data = nsi %>% filter(grepl("Yakima|^Willamette River", GNIS_NAME)), color = "blue", lwd = 0.3, alpha = 1) + 
  #geom_sf(data = nsi %>% filter(grepl("Yakima River", GNIS_NAME)), color = "blue", lwd = 0.2, alpha = 1) + 
  geom_sf(data = yakima_boundary, fill = NA, color = "black", lwd = 0.3) + 
  geom_sf(data = willamette_boundary, fill = NA, color = "black", lwd = 0.3) + 
  coord_sf(crs = coord_sf_crs) + 
  theme_map()
ggsave("figures/agu_poster/1a_wa_or_map.pdf", width = 4, height = 6)


## 2. Make dataset -------------------------------------------------------------

scaling_map_dat <- scaling_analysis_dat %>% 
  mutate(log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm)) %>% 
  mutate(dominant_lc = colnames(select(., contains("_3scp")))[max.col(select(., contains("_3scp")), "first")])

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

scaling_map_sf <- inner_join(nsi %>% clean_names(), 
                             scaling_dat_trimmed, by = "comid")


## 3. Make plot function -------------------------------------------------------
generate_faceted_map <- function(data, var, color_label, trans_function){
  
  color_alpha = 1
  min_color <- min(data %>% dplyr::pull({{var}}), na.rm = T)
  max_color <- max(data %>% dplyr::pull({{var}}), na.rm = T)
  
  wrb <- ggplot() + 
    geom_sf(data = data %>% filter(basin == "willamette"), 
            aes(color = {{var}}), alpha = color_alpha) + 
    scale_color_viridis_c(limits = c(min_color, max_color), trans = trans_function) + 
    theme_map()
  
  yrb <- ggplot() + 
    geom_sf(data = data %>% filter(basin == "yakima"), 
            aes(color = {{var}}), alpha = color_alpha) + 
    scale_color_viridis_c(limits = c(min_color, max_color), trans = trans_function) + 
    theme_map() + 
    labs(color = color_label)
  
  legend = get_legend(yrb)
  
  plots <- plot_grid(yrb + theme(legend.position = "none"), 
            wrb + theme(legend.position = "none"), 
            ncol = 1, 
            rel_heights = c(0.8, 1))
  
  plot_grid(plots, legend, nrow = 1, rel_widths = c(1, 0.5))
    
}

respiration_plot <- generate_faceted_map(scaling_map_sf, accm_totco2_o2g_day, "C. Resp.", "log10") 
hex_plot <- generate_faceted_map(scaling_map_sf, accm_hzt_cat, "C. HEX", 1) # not working because need to define default transform





##### GRAVEYARD ######


generate_plot <- function(data, aes_color_var, scale_color_func, color_name, title, ncols, add_y_axis_label = FALSE, ...) {
  plot <- ggplot() + 
    geom_sf(data = data, aes(color = .data[[aes_color_var]])) + 
    scale_color_func(name = color_name, ...) +
    #coord_sf(coord_sf_crs) + 
    facet_wrap(~basin_cat, ncol = ncols, scales = "free") +
    labs(title = title) #+
    # theme(
    #   legend.position = c(0.8, 0.35),
    #   legend.background = element_blank(), 
    #   legend.box.background = element_blank(),
    #   legend.title = element_text(size = 12, face = "bold"),
    #   legend.text = element_text(size = 10),
    #   axis.title = element_blank(),
    #   axis.text = element_text(size = 14),
    #   plot.title = element_text(size = 20, face = "bold"),
    #   strip.background = element_blank(),
    #   strip.text = element_blank()
    # )
  
  return(plot)
}


generate_plot(scaling_map_sf, "accm_totco2_o2g_day", scale_color_viridis_c, "Cumulative \nRespiration (g/d)", "Respiration", 1)


## 2. Make precip maps
precip_plot <- generate_plot(scaling_map_sf, "log_mean_ann_pcpt_mm", scale_color_viridis_c, "Precipitation \nlog[mm]", "Precipitation", 1)

## 3. Make elevation maps
elevation_plot <- generate_plot(scaling_map_dat, "wshd_avg_elevation_m", scale_color_gradientn, "Elevation (m)", "Elevation", 1, colours = terrain.colors(10))

## 4. Make land cover maps
lc_plot <- generate_plot(scaling_map_dat, "dominant_lc", scale_color_viridis_d, "Dominant \n land cover", "Land cover", 1)


plot_grid(precip_plot, elevation_plot, lc_plot, 
          nrow = 1)
ggsave("figures/agu_poster/231203_watershed_characteristics_map.pdf", 
       width = 14, height = 8)


