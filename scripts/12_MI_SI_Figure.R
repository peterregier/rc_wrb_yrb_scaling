## What about mutual information?
##

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")

p_load(RandomForestsGLS, #not using (doesn't easily give VI)
       ggpmisc,
       SpatialML,
       PNWColors,
       infotheo,
       tidymodels,
       tictoc)


# 2. Read in scaling dataset ---------------------------------------------------

# Loading regression estimates dataset
regression_estimates_raw <-  read_csv("data/guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv")

## Assign scaling categories based on simple rules
regression_estimates <- regression_estimates_raw %>% 
  clean_names() %>% 
  mutate(quantile = fct_relevel(quantile, "Q100", after = Inf)) %>% 
  mutate(r_squared = round(r_squared, 2)) %>% 
  mutate(scaling = case_when(slope_ci_2_5 <= 1 & slope_ci_97_5 >= 1 & r_squared > 0.8 ~ "Linear", 
                             slope_ci_2_5 < 1 & slope_ci_97_5 < 1 & r_squared > 0.8 ~ "Sublinear", 
                             slope_ci_2_5 > 1 & slope_ci_97_5 > 1 & r_squared > 0.8 ~ "Super-linear", 
                             TRUE ~ "Uncertain")) %>% 
  dplyr::select(basin, quantile, r_squared, slope, contains("slope_ci"), scaling) %>% 
  mutate(scaling = fct_relevel(scaling, c("Uncertain", "Sublinear")))

scaling_data_raw <- scaling_analysis_dat %>% 
  rename("quantile" = accm_hzt_cat) %>% 
  mutate(quantile_n = as.numeric(str_sub(quantile, 2, n())))

scaling_data_combined <- inner_join(scaling_data_raw, 
                                    regression_estimates, 
                                    by = c("basin", "quantile"))


# 3. Prep dataset --------------------------------------------------------------

predictors_new <- c("forest_3scp",
                    "shrub_3scp",
                    "human_3scp",
                    #"stream_order",
                    #"accm_water_exchng_kg_d",
                    "wshd_max_elevation_m",
                    #"simpson_d3",
                    "mean_ann_pcpt_mm")

# predictors_new <- c("accm_doc_load_kg_d",
#                     "accm_no3_load_kg_d",
#                     "accm_water_exchng_kg_d",
#                     "accm_wshd_stream_dens",
#                     "accm_reach_length_km",
#                     #"wshd_max_elevation_m", 
#                     "accm_mean_ann_pcpt_mm")

df <- scaling_data_combined %>% 
  #filter(quantile_n < 40 | quantile_n > 70) %>% 
  mutate(quantile_cat = case_when(quantile_n < 40 ~ "Q10-30",
                                  quantile_n > 70 ~ "Q80-100",
                                  TRUE ~ NA)) %>%
  dplyr::select(basin, quantile_cat, all_of(predictors_new), accm_totco2_o2g_day) %>% 
  drop_na() %>% 
  ungroup() %>% 
  dplyr::select(-c(quantile_cat))

mi_to_run <- tibble(expand_grid(selected_basin = unique(df$basin), 
                                selected_quantile = unique(df$quantile_cat)))

calculate_mi <- function(selected_basin, selected_quantile){
  
  message("calculating MI for ", selected_basin, ", ", selected_quantile)
  
  x <- infotheo::discretize(df %>% 
                              filter(basin == selected_basin) %>% 
                              filter(quantile_cat == selected_quantile) %>% 
                              #ungroup() %>% 
                              dplyr::select(-c(basin, quantile_cat)))
                              #dplyr::select(all_of(predictors_new), accm_totco2_o2g_day))
  
  vars <- colnames(x)[1:ncol(x) - 1]

  mi_values <- numeric()
  for (i in 1:(ncol(x) - 1)) {
    y <- x[, i]
    mi <- mutinformation(y, x[, ncol(x)])
    mi_values <- c(mi_values, mi)
  }

  output <- tibble(basin = selected_basin,
                   quantile_cat = selected_quantile,
                   nrow = nrow(x),
                   vars = vars,
                   mi = mi_values)
  
  return(output)
}

calculate_mi_basin <- function(selected_basin){
  
  message("calculating MI for ", selected_basin)
  
  x <- infotheo::discretize(df %>% 
                              filter(basin == selected_basin) %>% 
                              dplyr::select(-c(basin)))
  
  vars <- colnames(x)[1:ncol(x) - 1]
  
  mi_values <- numeric()
  for (i in 1:(ncol(x) - 1)) {
    y <- x[, i]
    mi <- mutinformation(y, x[, ncol(x)])
    mi_values <- c(mi_values, mi)
  }
  
  output <- tibble(basin = selected_basin,
                   nrow = nrow(x),
                   vars = vars,
                   mi = mi_values)
  
  return(output)
}

mi_basin_outputs <- unique(df$basin) %>% 
  map(calculate_mi_basin) %>% 
  bind_rows()

write_csv(mi_basin_outputs, "data/250107_MI_by_basin.csv")

# mi_outputs <- mi_to_run %>% 
#   pmap(calculate_mi_basin) %>% 
#   bind_rows() %>% 
#   mutate(vars = case_when(vars == "accm_doc_load_kg_d" ~ "C. DOC load", 
#                           vars == "accm_no3_load_kg_d" ~ "C. NO3 load", 
#                           vars == "accm_water_exchng_kg_d" ~ "C. water exchange", 
#                           vars == "accm_wshd_stream_dens" ~ "C. stream density", 
#                           vars == "accm_reach_length_km" ~ "C. reach length", 
#                           vars == "accm_mean_ann_pcpt_mm" ~ "C. annual precip.", 
#                           TRUE ~ vars))

mi_vars <- unique(mi_basin_outputs$vars)
mi_palette <- PNWColors::pnw_palette("Bay", n = length(mi_vars))

mi_colors = tibble(var = mi_vars, 
                   colors = mi_palette)

mi_basin_outputs %>% 
  rename("variable" = vars) %>% 
  mutate(variable = case_when(variable == "mean_ann_pcpt_mm" ~ "Mean precip (mm/yr)", 
                              variable == "wshd_max_elevation_m" ~ "Max elevation (m)",
                              variable == "wshd_min_elevation_m" ~ "Min elevation (m)",
                              variable == "forest_3scp" ~ "Forest (%)",
                              variable == "shrub_3scp" ~ "Shrubland (%)",
                              variable == "human_3scp" ~ "Human-modified (%)",
                              variable == "hrel_3" ~ "Landscape entropy",
                              variable == "simpson_d3" ~ "Simpson's diversity index")) %>% 
  group_by(basin) %>% 
  mutate(mi_n = mi / max(mi)) %>% 
  ggplot(aes(mi_n, reorder(variable, mi_n), color = variable, fill = variable)) + 
  geom_col(alpha = 0.7, width = 0.7, show.legend = F) + 
  facet_wrap(~basin) + 
  labs(x = "Normalized Mutual information (0-1)", 
       y = "Cumulative watershed variable") + 
  scale_color_manual(values = mi_colors$colors) + 
  scale_fill_manual(values = mi_colors$colors)
ggsave("figures/se_mutual_info_by_basin.png", width = 6, height = 3)



