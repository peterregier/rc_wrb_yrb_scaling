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

color_mapping <- c("Uncertain" = "#3E1152", 
                   "Sublinear" = "#40688C", 
                   "Linear" = "#5FB57F", 
                   "Super-linear" = "#F8E755")

scaling_data_combined %>% 
  select(basin_cat, stream_order, scaling) %>% 
  group_by(stream_order, basin_cat, scaling) %>% 
  mutate(count = n()) %>% 
  #summarize(n = sum(count)) %>% 
  mutate(percent = count / sum(count)) %>% 
  ggplot(aes(stream_order, count, 
             fill = scaling, 
             group = scaling)) + 
  geom_area(alpha = 0.7) + 
  facet_wrap(~basin_cat, ncol = 1) + 
  scale_color_manual(values = color_mapping) + 
  scale_fill_manual(values = color_mapping) + 
  labs(color = "Scaling", 
       fill = "Scaling", 
       x = "Strahler Stream Order", 
       y = "Number of Reaches")
ggsave("figures/s_figure_scaling_by_stream_order.png", width = 7, height = 6)
