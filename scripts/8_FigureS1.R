## This script takes Fco's code from 
## and creates Figure S1
##
## Peter Regier (with code from Francisco)
## See: https://github.com/Scaling-Watershed-Function/2-swf-analytical.engine
## Contact: peter.regier@pnnl.gov
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")

scaling_map_dat <- scaling_analysis_dat %>% 
  dplyr:: select(basin_cat,
                 basin,
                 longitude,
                 latitude,
                 mean_ann_pcpt_mm,
                 wshd_avg_elevation_m,
                 hrel_3,
                 accm_hzt_cat) %>% 
  mutate(
    quantile_num = as.numeric(sub("[^0-9]+", "", accm_hzt_cat)),
    scaling_cat = 
      case_when(basin == "willamette" & quantile_num >= 60 ~ "Superlinear",
                basin == "willamette" & quantile_num >= 40 & quantile_num < 60 ~ "Linear",
                basin == "willamette" & quantile_num < 40 ~ "Uncertain",
                basin == "yakima" & quantile_num >= 80 ~ "Superlinear",
                basin == "yakima" & quantile_num == 70 ~ "Linear",
                basin == "yakima" & quantile_num >= 40 & quantile_num < 70 ~ "Sub-linear",
                basin == "yakima" & quantile_num < 40 ~"Uncertain"),
    scaling_cat = factor(scaling_cat, levels = c("Uncertain",
                                                 "Sub-linear",
                                                 "Linear",
                                                 "Superlinear")),
    log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm),
    scaling_cat_num = 
      case_when(scaling_cat == "Uncertain"~1,
                scaling_cat == "Sub-linear"~2,
                scaling_cat == "Linear"~3,
                scaling_cat == "Superlinear"~4)
  )


###############################################################################

generate_plot <- function(data, aes_color_var, scale_color_func, color_name, title, ncols, add_y_axis_label = FALSE, ...) {
  plot <- ggplot(data, aes(x = longitude, y = latitude)) +
    geom_point(aes(color = .data[[aes_color_var]])) +
    scale_color_func(name = color_name, ...) +
    facet_wrap(~fct_relevel(basin_cat, "Yakima River (Dry)"), ncol = ncols, scales = "free") +
    labs(title = title) +
    theme(
      legend.position = c(0.8, 0.85),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      axis.title = element_blank(),
      axis.text = element_text(size = 18),
      plot.title = element_text(size = 24, face = "bold"),
      strip.background = element_blank(),
      strip.text = element_blank()
    )
  
  if (add_y_axis_label) {
    plot <- plot + labs(y = "Latitude") + 
      theme(axis.title.y = element_text(size = 24, face = "bold"))
  }
  
  return(plot)
}

# Generate each plot using the function
scaling_plot <- generate_plot(scaling_map_dat, "scaling_cat", scale_color_viridis_d, "Scaling type", "Allometric Scaling", 2, TRUE)
precipt_plot <- generate_plot(scaling_map_dat, "log_mean_ann_pcpt_mm", scale_color_viridis_c, "Precipitation \nlog[mm]", "Precipitation", 1)
elevat_plot <- generate_plot(scaling_map_dat, "wshd_avg_elevation_m", scale_color_gradientn, "Elevation (m)", "Elevation", 1, colours = terrain.colors(10))
heterog_plot <- generate_plot(scaling_map_dat, "hrel_3", scale_color_viridis_c, "Entropy \n(relative)", "Landscape entropy", 1, option = "mako")

## Write out figure
plot_grid(precipt_plot, elevat_plot, heterog_plot, nrow = 1)
ggsave("figures/s1_watershed_characteristics.png", width = 18, height = 9)
ggsave("figures/s1_watershed_characteristics.pdf", width = 18, height = 9)




