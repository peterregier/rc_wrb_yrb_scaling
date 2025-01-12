## This script creates a figure with the three allometric scaling metrics of 
## interest: slope, intercept, and R2. Code is repurposed from other scripts that
## are no longer using it
##
## Code from Francisco J. Guerrero and Peter Regier
## Contact: peter.regier@pnnl.gov
##
# ######### #
# ######### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")


# 4. Load regression data ------------------------------------------------------

# Load regression estimates dataset
regression_estimates_raw <- read_csv("data/guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv")

## Assign scaling categories based on simple rules
regression_estimates <- regression_estimates_raw %>% 
  clean_names() %>% 
  mutate(quantile = fct_relevel(quantile, "Q100", after = Inf)) %>% 
  mutate(basin = case_when(basin == "willamette"~ "Willamette (WRB)", 
                           basin == "yakima"~ "Yakima (YRB)")) %>% 
  mutate(r_squared = round(r_squared, 2)) %>% 
  mutate(scaling = case_when(slope_ci_2_5 <= 1 & slope_ci_97_5 >= 1 & r_squared > 0.8 ~ "Linear", 
                             slope_ci_2_5 < 1 & slope_ci_97_5 < 1 & r_squared > 0.8 ~ "Sublinear", 
                             slope_ci_2_5 > 1 & slope_ci_97_5 > 1 & r_squared > 0.8 ~ "Super-linear", 
                             TRUE ~ "Uncertain")) %>% 
  #select(basin, quantile, r_squared, contains("r_squared_ci"), slope, contains("slope_ci"), intercept, contains("intercept_ci"), scaling) %>% 
  mutate(scaling = fct_relevel(scaling, c("Uncertain", "Sublinear")))


p_r2 <-  ggplot(regression_estimates, aes(quantile, r_squared)) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_hline(yintercept = 0.8, linetype = "dashed") + 
  geom_errorbar(aes(ymin = r_squared_ci_2_5, ymax = r_squared_ci_97_5), color = "gray", width = 0) + 
  geom_point(size = 4, aes( color = scaling), 
             alpha = 0.8, show.legend = F) + 
  labs(y = expression(R^2), x = "HEF Quantile") +
  facet_wrap(~basin, ncol = 1) + 
  scale_color_viridis_d() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p_int <- ggplot(regression_estimates, aes(quantile, intercept)) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_errorbar(aes(ymin = intercept_ci_2_5, ymax = intercept_ci_97_5), color = "gray", width = 0) + 
  geom_point(size = 4, aes( color = scaling), 
             alpha = 0.8, show.legend = F) + 
  labs(x = "HEF Quantile", y = "Y-intercept") + 
  facet_wrap(~basin, ncol = 1) + 
  scale_color_viridis_d() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p_slope <- ggplot(regression_estimates, aes(quantile, slope)) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_errorbar(aes(ymin = slope_ci_2_5, ymax = slope_ci_97_5), color = "gray", width = 0) + 
  geom_point(size = 4, aes( color = scaling), 
             alpha = 0.8, show.legend = F) + 
  facet_wrap(~basin, ncol = 1) + 
  labs(x = "HEF Quantile", y = "Slope") + 
  scale_color_viridis_d() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

legend <- get_legend( ggplot(regression_estimates, aes(quantile, slope)) + 
                        geom_hline(yintercept = 1, linetype = "dashed") + 
                        geom_errorbar(aes(ymin = slope_ci_2_5, ymax = slope_ci_97_5), color = "gray", width = 0) + 
                        geom_point(size = 4, aes( color = scaling), 
                                   alpha = 0.8, show.legend = T) + 
                        facet_wrap(~basin, ncol = 1) + 
                        labs(color = "Scaling \n category") + 
                        scale_color_viridis_d())

plot_grid(p_slope, p_r2, p_int, legend,  
          labels = c("A", "B", "C", ""), 
          rel_widths = c(1, 1, 1, 0.5),
          nrow = 1)
ggsave("figures/3_scaling_by_quantile.png", width = 10, height = 4)
ggsave("figures/3_scaling_by_quantile.pdf", width = 10, height = 4)


## Potential supplemental figure
make_regression_plot <- function(variable){
  
  df <- regression_estimates %>% 
    dplyr::select("basin", "quantile", variable) %>% 
    pivot_wider(names_from = "basin", values_from = variable) %>% 
    clean_names() 
  
  fit = summary(lm(yakima_yrb~willamette_wrb, data = df))
  r2 = round(fit[[9]], 2)
  
  r2

  df %>%
    ggplot(aes(willamette_wrb, yakima_yrb)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    labs(x = "WRB", y = "YRB", 
         title = paste0(variable, ", r2 = ", r2))

}

make_regression_plot("slope")
make_regression_plot("r_squared")
make_regression_plot("intercept")

