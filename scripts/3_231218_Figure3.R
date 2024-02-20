## This script is for a redesign of Figure 4, which was originally just scaling
## maps, but is going to be scaling maps smooshed together with mutual information
##
## Peter Regier
## 2023-12-18
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
p_load(plotly)

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
  select(basin, quantile, r_squared, contains("r_squared_ci"), slope, contains("slope_ci"), scaling) %>% 
  mutate(scaling = fct_relevel(scaling, c("Uncertain", "Sublinear")))


p_r2 <-  ggplot(regression_estimates, aes(quantile, r_squared)) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_hline(yintercept = 0.8, linetype = "dashed") + 
  geom_errorbar(aes(ymin = r_squared_ci_2_5, ymax = r_squared_ci_97_5), color = "gray", width = 0) + 
  geom_point(size = 4) + 
  labs(y = expression(R^2)) +
  facet_wrap(~basin, ncol = 1) + 
  scale_color_viridis_d()
  
p_slope <- ggplot(regression_estimates, aes(quantile, slope)) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_errorbar(aes(ymin = slope_ci_2_5, ymax = slope_ci_97_5), color = "gray", width = 0) + 
  geom_point(size = 4, aes( color = scaling)) + 
  facet_wrap(~basin, ncol = 1) + 
  scale_color_viridis_d()

plot_grid(p_r2, p_slope, 
          labels = c("A", "B"), 
          rel_widths = c(1, 1.5),
          nrow = 1)
ggsave("figures/231221_scaling_by_quantile.png", width = 10, height = 5)

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
    labs(color = "") + 
    coord_sf(crs = coord_sf_crs)
}

scaling_plot <- plot_grid(make_scaling_plot("Yakima River (Dry)", "Yakima River (YRB)") + 
                       theme(legend.position = "none"),
                     make_scaling_plot("Willamette River (Wet)", "Willamette River (WRB)") + 
                       theme(legend.position = "none"), 
                     nrow = 1)
                     #ncol = 1, rel_heights = c(1, 0.1, 1))

scaling_legend <- get_legend(make_scaling_plot("Yakima River (Dry)", "Yakima River"))

figure3 <- plot_grid(make_scaling_plot("Yakima River (Dry)", "Yakima River (YRB)") + 
                       theme(legend.position = "none"),
                     make_scaling_plot("Willamette River (Wet)", "Willamette River (WRB)") + 
                       theme(legend.position = "none"), 
                     scaling_legend, 
                     nrow = 1, 
                     rel_widths = c(1, 1, 0.3))
ggsave("figures/231219_Figure3.png", width = 10, height = 5)


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

plot_grid(yrb_faceted_scaling, 
          wrb_faceted_scaling, 
          ncol = 1, 
          rel_heights = c(1, 1), 
          align = "hv")
ggsave("figures/3_figure3_faceted.png", width = 12, height = 8)
ggsave("figures/3_figure3_faceted.pdf", width = 12, height = 8)

# 3. Read in MI dataset --------------------------------------------------------

mi <- read_csv("data/231130_mutual_info.csv")

mi_n <- mi %>% 
  select(basin, variable, accm_totco2_o2g_day) %>% 
  group_by(basin) %>% 
  mutate(accm_totco2_o2g_day_n = accm_totco2_o2g_day / max(accm_totco2_o2g_day)) %>% 
  filter(variable != "accm_totco2_o2g_day") %>% 
  mutate(variable = case_when(variable == "mean_ann_pcpt_mm" ~ "Mean precip (mm/yr)", 
                              variable == "wshd_max_elevation_m" ~ "Max elevation (m)",
                              variable == "wshd_min_elevation_m" ~ "Min elevation (m)",
                              variable == "forest_3scp" ~ "Forest (%)",
                              variable == "shrub_3scp" ~ "Shrubland (%)",
                              variable == "human_3scp" ~ "Human-modified (%)",
                              variable == "hrel_3" ~ "Landscape entropy",
                              variable == "simpson_d3" ~ "Simpson's diversity index"))

## set up a color scheme for water quality
mi_vars <- unique(mi_n$variable)
mi_colors <- PNWColors::pnw_palette("Bay", n = length(mi_vars))

mi_colors = tibble(var = mi_vars, 
                   colors = mi_colors)


make_mi_plot <- function(data, title){
  ggplot(data, 
         aes(reorder(variable, accm_totco2_o2g_day_n), fill = variable)) + 
    geom_col(aes(y = accm_totco2_o2g_day_n), alpha = 0.8) + 
    labs(x = "", y = "Normalized \n mutual information", title = title) + 
    coord_flip() + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size=14,face="bold"), 
          axis.text=element_text(size=12),
          axis.title=element_text(size=12), 
          axis.text.y = element_text(angle = 30, vjust = 0.5, hjust=1)) + 
    scale_fill_manual(values = mi_colors$colors) + 
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank(), #remove x axis ticks
          axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank()  #remove y axis ticks
    )
}       

mi_plot <- plot_grid(NULL, 
                     make_mi_plot(mi_n %>% filter(basin == "YRB"), "Yakima (YRB)") + 
                       theme(legend.position = "none"), 
                     NULL,
                     make_mi_plot(mi_n %>% filter(basin == "WRB"), "Willamette (WRB)") + 
                       theme(legend.position = "none"),
                     NULL, 
                     nrow = 1, rel_widths = c(0.2, 1, 0.1, 1, 0.2))
                    # ncol = 1, rel_heights = c(1, 0.1, 1))

mi_legend <- get_legend(make_mi_plot(mi_n %>% filter(basin == "YRB"), "Yakima (YRB)") + 
                        labs(fill = ""))

figure4 <- plot_grid(mi_plot, mi_legend, 
                     nrow = 1, 
                     rel_widths = c(1, 0.3))
ggsave("figures/231219_Figure4.png", width = 10, height = 3)

# 4. Assemble plots ------------------------------------------------------------

legends <- plot_grid(NULL, scaling_legend, mi_legend, NULL,
                     ncol = 1, 
                     rel_heights = c(1, 0.8, 1, 1))

plots <- plot_grid(scaling_plot, 
                   mi_plot, 
                   ncol = 1, 
                   rel_heights = c(1, 0.8))

plot_grid(plots,
          legends, 
          nrow = 1, 
          rel_widths = c(1, 0.2))
ggsave("figures/231219_Figure3_combined.png", 
       width = 10, 
       height = 10)




