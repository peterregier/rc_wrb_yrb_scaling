## This script creates Figure 4, which presents mutual information for two groups
## of quantiles (Q10-30 and Q80-100) for each basin
##
## Peter Regier
## Contact: peter.regier@pnnl.gov
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load setup information
source("scripts/0_setup.R")

## Load additional packages
p_load(ggpmisc,
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
  ungroup()

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


mi_outputs <- mi_to_run %>% 
  pmap(calculate_mi) %>% 
  bind_rows() %>% 
  mutate(vars = case_when(vars == "forest_3scp" ~ "% Forest", 
                          vars == "shrub_3scp" ~ "% Schrubland", 
                          vars == "human_3scp" ~ "% Human-influenced", 
                          vars == "wshd_max_elevation_m" ~ "Max. elevation", 
                          vars == "mean_ann_pcpt_mm" ~ "Mean annual precip", 
                          TRUE ~ vars)) %>% 
  mutate(basin = case_when(basin == "yakima" ~ "Yakima", 
                           basin == "willamette" ~ "Willamette")) %>% 
  mutate(quantile_cat = case_when(quantile_cat == "Q10-30" ~ "Q10-30 (Uncertain)",
                                  quantile_cat == "Q80-100" ~ "Q80-100 (Uncertain)"))

mi_vars <- unique(mi_outputs$vars)
mi_palette <- PNWColors::pnw_palette("Bay", n = length(mi_vars))

mi_colors = tibble(var = mi_vars, 
                   colors = mi_palette)

mi_outputs %>% 
  group_by(basin, quantile_cat) %>% 
  mutate(mi_n = mi / max(mi)) %>% 
  ggplot(aes(mi_n, reorder(vars, mi_n), color = vars, fill = vars)) + 
  geom_col(alpha = 0.7, width = 0.7, show.legend = F) + 
  facet_wrap(basin~quantile_cat) + 
  labs(x = "Normalized Mutual information", 
       y = "Cumulative watershed variable") + 
  scale_color_manual(values = mi_colors$colors) + 
  scale_fill_manual(values = mi_colors$colors)
ggsave("figures/4_Figure4.png", width = 8, height = 6)
ggsave("figures/4_Figure4.pdf", width = 8, height = 6)
