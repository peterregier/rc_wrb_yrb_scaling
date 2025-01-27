## This script creates Figure 4 (spatial distributions of scaling by reach) and
## Figure S3
##
## Code from Francisco J. Guerrero and Peter Regier
## Contact: peter.regier@pnnl.gov
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
p_load(plotly, tmaptools)


# 2. Read in scaling dataset ---------------------------------------------------

# Loading regression estimates dataset
regression_estimates_raw <-  read_csv("data/guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv")

## Prep data
reg_estimates_long <- regression_estimates_raw %>% 
  select(basin,
         quantile,
         Slope,
         RSquared,
         SlopeCI_2.5,
         SlopeCI_97.5,
         RSquaredCI_2.5,
         RSquaredCI_97.5) %>% 
  gather(.,
         key = "metric",
         value = "value",
         factor_key = TRUE,
         c(3:4))

## Assign scaling categories based on simple rules
regression_estimates <- regression_estimates_raw %>% 
  clean_names() %>% 
  mutate(quantile = fct_relevel(quantile, "Q100", after = Inf)) %>% 
  mutate(r_squared = round(r_squared, 2)) %>% 
  mutate(scaling = case_when(slope_ci_2_5 <= 1 & slope_ci_97_5 >= 1 & r_squared > 0.8 ~ "Linear", 
                             slope_ci_2_5 < 1 & slope_ci_97_5 < 1 & r_squared > 0.8 ~ "Sublinear", 
                             slope_ci_2_5 > 1 & slope_ci_97_5 > 1 & r_squared > 0.8 ~ "Super-linear", 
                             TRUE ~ "Uncertain")) %>% 
  select(basin, quantile, r_squared, contains("r_squared_ci"), slope, contains("slope_ci"), intercept, contains("intercept_ci"), scaling) %>% 
  mutate(scaling = fct_relevel(scaling, c("Uncertain", "Sublinear")))


scaling_data_raw <- scaling_analysis_dat %>% 
  dplyr:: select(basin_cat,
                 basin,
                 stream_order,
                 comid, 
                 longitude,
                 latitude,
                 mean_ann_pcpt_mm,
                 wshd_avg_elevation_m,
                 hrel_3,
                 accm_hzt_cat) %>% 
  rename("quantile" = accm_hzt_cat)

scaling_data_combined <- inner_join(scaling_data_raw, 
                           regression_estimates, 
                           by = c("basin", "quantile"))

## thx chat
# last_row <- tail(scaling_data_combined, 1)
# columns_to_keep <- c('basin_cat', 'basin', 'longitude', 'latitude', 'scaling')
# last_row[, !(names(last_row) %in% columns_to_keep)] <- NA

## Add a single row to the end of WRB so sub-linear will have a facet
#scaling_data <- bind_rows(scaling_data_raw, last_row)
  
scaling_sf <- st_as_sf(scaling_data_combined, 
                       coords = c("longitude", "latitude"), 
                       crs = common_crs)

make_scaling_plot <- function(selected_basin, title){
  
  plot_title = selected_basin
  
  data <- scaling_sf %>% 
    filter(basin_cat == selected_basin)
  
  ggplot() + 
    geom_sf(data = data, 
            aes(color = scaling), alpha = 0.6) + 
    scale_color_viridis_d() + 
    ggtitle(title) + 
    #theme_minimal() + 
    theme_map() + 
    theme(plot.title = element_text(hjust = 0.5, size=14,face="bold")) + 
    #theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"), 
    #       axis.text=element_text(size=12),
    #      axis.title=element_text(size=12), 
    #      axis.text.y = element_text(angle = 30, vjust = 0.5, hjust=1)) + 
    labs(color = "") #+ 
    #coord_sf(crs = coord_sf_crs)
}

scaling_plot <- plot_grid(make_scaling_plot("Yakima River (Dry)", "Yakima River (YRB)") + 
                       theme(legend.position = "none"),
                     make_scaling_plot("Willamette River (Wet)", "Willamette River (WRB)") + 
                       theme(legend.position = "none"), 
                     nrow = 1)
                     #ncol = 1, rel_heights = c(1, 0.1, 1))

scaling_legend <- get_legend(make_scaling_plot("Yakima River (Dry)", "Yakima River"))

# figure3 <- plot_grid(make_scaling_plot("Yakima River (Dry)", "Yakima River (YRB)") + 
#                        theme(legend.position = "none"),
#                      make_scaling_plot("Willamette River (Wet)", "Willamette River (WRB)") + 
#                        theme(legend.position = "none"), 
#                      scaling_legend, 
#                      nrow = 1, 
#                      rel_widths = c(1, 1, 0.3))
# ggsave("figures/231219_Figure3.png", width = 10, height = 5)


# 1. Read in watershed shapefiles ----------------------------------------------

nsi <- read_sf("data/nsi_network_ywrb/nsi_network_ywrb.shp") %>% 
  st_transform(crs = common_crs)

## There isn't a convenient way to break these apart, so I'll use st_crop
yakima_flowlines <- st_crop(nsi, xmin = -122, xmax = -119, ymin = 45.9, ymax = 48)
willamette_flowlines <- st_crop(nsi, xmin = -124, xmax = -121, ymin = 43, ymax = 46)

## YRB
yakima_boundary <- get_huc(AOI = st_union(yakima_flowlines), type = "huc04") %>% 
  filter(huc4 == "1703")

# WRB - same as before, this doesn't cut it. Commenting out but keeping for now
# willamette_boundary <- get_huc(AOI = st_union(willamette_flowlines), type = "huc04") %>%
#   filter(huc4 == "1709")
willamette_boundary <- read_sf("data/basin_boundaries/Willamette_Custom_Watershed_Boundary_noCorner/Willamette_Custom_Watershed_Boundary_noCorner.shp") %>% 
  st_transform(crs = common_crs)


# 2.5. Make a faceted scaling plot ---------------------------------------------

## Since we have no sublinear in WRB, need to manually match colors to levels

color_mapping <- c("Uncertain" = "#3E1152", 
                   "Sublinear" = "#40688C", 
                   "Linear" = "#5FB57F", 
                   "Super-linear" = "#F8E755")

## To label outlets, we need to figure out where they are: 

## Columbia: 24520498
ggplot() + 
  geom_sf(data = nsi %>% filter(grepl("^Columbia River", GNIS_NAME))) +
  geom_sf_text(data = nsi %>% filter(grepl("^Columbia River", GNIS_NAME)), aes(label = COMID))

wrb_outlet <- nsi %>% 
  filter(COMID == 24520498) %>% 
  slice(1) %>% 
  st_point_on_surface()

## Yakima: 23099408
ggplotly(ggplot() + 
  geom_sf(data = nsi %>% filter(grepl("Yakima River", GNIS_NAME))) +
  geom_sf_text(data = nsi %>% filter(grepl("Yakima River", GNIS_NAME)), aes(label = COMID)))

yrb_outlet <- nsi %>% 
  filter(COMID == 23099408) %>% 
  slice(1) %>% 
  st_point_on_surface()


yrb_faceted_scaling <- make_scaling_plot("Yakima River (Dry)", "Yakima River (YRB)") + 
  geom_sf(data = yakima_boundary, fill = NA, color = "black") + 
  geom_sf(data = nsi %>% filter(grepl("Yakima River", GNIS_NAME)), color = "blue", lwd = 0.2, alpha = 1) + 
  geom_sf(data = yrb_outlet, color = "black", size = 2) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = color_mapping) + 
  facet_wrap(~scaling, nrow = 1)

wrb_faceted_scaling <- make_scaling_plot("Willamette River (Wet)", "Willamette River (WRB)") + 
  geom_sf(data = willamette_boundary, fill = NA, color = "black") + 
  geom_sf(data = nsi %>% filter(grepl("^Willamette River", GNIS_NAME)), color = "blue", lwd = 0.2, alpha = 1) +
  #geom_sf(data = nsi %>% filter(grepl("^Columbia River", GNIS_NAME)), color = "blue", lwd = 0.4, alpha = 1) +
  geom_sf(data = wrb_outlet, color = "black", size = 2) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = color_mapping) + 
  facet_wrap(~scaling, nrow = 1, drop = F)

## plot_grid is distorting the aspect ratio of YRB
# plot_grid(yrb_faceted_scaling, 
#           wrb_faceted_scaling, 
#           ncol = 1, 
#           rel_heights = c(0.6, 1), 
#           align = "h")

## Chat suggests patchwork instead
p_load(patchwork)

combined_plot <- yrb_faceted_scaling / wrb_faceted_scaling + 
  plot_layout(ncol = 1, heights = c(0.595, 1))

combined_plot
ggsave("figures/4_Figure4.png", width = 11, height = 7)
ggsave("figures/4_Figure4.pdf", width = 11, height = 7)


## Make example plot of what binning by HUC would look like --------------------

scaling_yrb <- scaling_sf %>% 
  filter(basin == "yakima")

yrb_huc8s <- get_huc(AOI = yakima_boundary, type = "huc08") %>% 
  crop_shape(., yakima_boundary, polygon = T)

yrb_huc10s_raw <- get_huc(AOI = yakima_boundary, type = "huc10") %>% 
  crop_shape(., yakima_boundary, polygon = T) %>% 
  mutate(geometry_type = st_geometry_type(geometry)) 

## There are some weird multistrings in here that don't belong. Filter em out
yrb_huc10s <- bind_rows(yrb_huc10s_raw %>% filter(geometry_type == "POLYGON"),
                        yrb_huc10s_raw %>% filter(geometry_type == "GEOMETRYCOLLECTION") %>% slice(1:2))

ggplot() + 
  geom_sf(data = yakima_boundary, fill = NA, color = "black") + 
  geom_sf(data = yrb_huc10s, aes(fill = huc10), 
          alpha = 0.5, 
          show.legend = F) + 
  scale_fill_viridis_d(option = "turbo")

i = 1

pull_max_scaling <- function(i){
  
  x <- yrb_huc10s %>% slice(i) %>% st_make_valid()
  huc10 = x$huc10
  
  message(huc)
  
  crop_shape(scaling_yrb, x, polygon = T) %>% 
    st_drop_geometry() %>% 
    group_by(scaling) %>%
    count() %>% 
    ungroup() %>% 
    slice_max(n, with_ties = FALSE) %>% 
    mutate(huc10 = huc10)
}

max_scaling <- 1:nrow(yrb_huc10s) %>% 
  map(pull_max_scaling) %>% 
  bind_rows()

scaling_by_huc10 <- inner_join(yrb_huc10s, max_scaling, by = "huc10")

ggplot() + 
  geom_sf(data = yakima_boundary) + 
  geom_sf(data = scaling_by_huc10, aes(fill = scaling), alpha = 0.5) + 
  scale_fill_manual(values = color_mapping) 



