## This script takes Fco's code from https://github.com/Scaling-Watershed-Function/2-swf-analytical.engine
## as of 11/30/23 and recreates Figure 2 elements so I can edit for AGU poster
## 
## INPUTS: 
### scaling_analysis_dat (via 0_setup.R)
### guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv
##
## Peter Regier (some code from Francisco)
## 2023-11-30
##
# ######### #
# ######### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
p_load(segmented)

# ## Alterative time-line: what's the scaling feature as described in Wollheim et al. 2022?
# scaling_means <- scaling_analysis_dat %>% 
#   filter(wshd_area_km2 > 0.9 & 
#            wshd_area_km2 < 1.1) %>% 
#   group_by(basin) %>% 
#   summarize(mean_accm_totco2_o2g_day = mean(accm_totco2_o2g_day))
# 
# scaling_analysis_dat2 <- scaling_analysis_dat %>% 
#   mutate(n_accm_totco2_o2g_day = case_when(basin == "willamette" ~ accm_totco2_o2g_day / 8441,  # see scaling_means
#                                            basin == "yakima" ~ accm_totco2_o2g_day / 3804))

# 2. Plotting function from Fco ------------------------------------------------

create_faceted_plots <- function(data, selected_basin) {
  
  # Filter data for the specified basin
  filtered_data <- data %>% filter(basin == selected_basin)
  
  # Create the plot
  plot <- ggplot(
    filtered_data %>% 
      mutate(
        accm_hzt_cat = factor(
          accm_hzt_cat,
          levels = c("Q10","Q20","Q30","Q40","Q50",
                     "Q60","Q70","Q80","Q90","Q100")
        )
      ),
    aes(
      x = wshd_area_km2, 
      y = accm_totco2_o2g_day, 
      color = accm_hzt_cat
    )
  ) +
    facet_wrap(~basin_cat, ncol = 2) +
    geom_smooth(method = "lm", linewidth = 3.0, fullrange = TRUE) +
    geom_point(alpha = 0.75, size = 3.5) +
    scale_x_log10(
      breaks = breaks, 
      labels = trans_format("log10", 
                            math_format(10^.x)),
      limits = c(0.001,32000)
    ) +
    scale_y_log10(
      breaks = breaks_c, 
      labels = trans_format("log10", 
                            math_format(10^.x)),
      limits = c(0.0001, 15000000000)
    ) +
    scale_color_viridis_d(name = expression(bold(paste("Cumulative \nhyporheic \nexchange \n(quantiles)")))) +
    annotation_logticks(size = 0.75, sides = "tblr") +
    geom_abline(linewidth = 1.0, linetype = "dashed") +
    xlab(expression(paste("Watershed area (", km^2, ")"))) + 
    theme_httn +
    theme(legend.position = if(selected_basin == "yakima") c(.9,.2) else "none",
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 30),
          strip.text = element_text(size = 24, face = "bold", hjust = 0),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title.position = "plot") 
  
  # Conditional formatting for yakima basin
  if (selected_basin == "yakima") {
    plot <- plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  } else {
    plot <- plot + ylab(expression(bold(paste("Cumulative aerobic", " ", respiration[Hyp], "(", gCO[2] * d^-1, ")"))))
  }
  
  # No x-axis title for any plots
  #plot <- plot + theme(axis.title.x = element_blank())
  
  return(plot)
}


# 3. Make main plots -----------------------------------------------------------

#Willamette
#plot_willamette <- create_faceted_plots(scaling_analysis_dat, "willamette")
plot_willamette <- create_faceted_plots(scaling_analysis_dat, "willamette")
plot_willamette


# Yakima
#plot_yakima <- create_faceted_plots(scaling_analysis_dat, "yakima")
plot_yakima <- create_faceted_plots(scaling_analysis_dat, "yakima")
plot_yakima

plot_grid(plot_willamette, plot_yakima, nrow = 1)

#ggsave("figures/240220_normalized_respiration.png", width = 24, height = 10)

# 3.5. Make full watershed plots -----------------------------------------------

create_full_plots <- function(data, selected_basin) {
  
  # Filter data for the specified basin
  filtered_data <- data %>% filter(basin == selected_basin)
  
  lm_model = summary(lm(log10(accm_totco2_o2g_day)~log10(wshd_area_km2), filtered_data))
  r2 = round(lm_model[[9]], 2)
  m = round(lm_model[[4]][1,1], 1)
  
  # Create the plot
  plot <- ggplot(
    filtered_data,
    aes(x = wshd_area_km2, 
        y = accm_totco2_o2g_day)) +
    facet_wrap(~basin_cat, ncol = 2) +
    geom_smooth(method = "lm", linewidth = 3.0, fullrange = TRUE) +
    geom_point(alpha = 0.5, size = 3.5) +
    scale_x_log10(
      breaks = breaks, 
      labels = trans_format("log10", 
                            math_format(10^.x)),
      limits = c(0.001,32000)) +
    scale_y_log10(
      breaks = breaks_c, 
      labels = trans_format("log10", 
                            math_format(10^.x)),
      limits = c(0.0001, 15000000000)) +
    xlab(expression(paste("Watershed area (", km^2, ")"))) + 
    annotation_logticks(size = 0.75, sides = "tblr") +
    annotate(geom = "text", x = 10e-2, y = 10e8, label = paste("R2 =", r2), size = 10) + 
    annotate(geom = "text", x = 10e-2, y = 10e7, label = paste("Slope =", m), size = 10) + 
    geom_abline(linewidth = 1.0, linetype = "dashed") +
    theme_httn +
    theme(legend.position = if(selected_basin == "yakima") c(.9,.2) else "none",
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 32),
          strip.text = element_text(size = 24, face = "bold", hjust = 0),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title.position = "plot") 
  
  # Conditional formatting for yakima basin
  if (selected_basin == "yakima") {
    plot <- plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  } else {
    plot <- plot + ylab(expression(bold(paste("Cumulative Aerobic", " ", respiration[Hyp], "(", gCO[2] * d^-1 * m^-2, ")"))))
  }
  
  # No x-axis title for any plots
  #plot <- plot + theme(axis.title.x = element_blank())
  
  return(plot)
}

plot_grid(create_full_plots(scaling_analysis_dat, "willamette"), 
          create_full_plots(scaling_analysis_dat, "yakima"), 
          nrow = 1)
ggsave("figures/s_figure2_no_hef.png", width = 24, height = 12)
ggsave("figures/s_figure2_no_hef.pdf", width = 24, height = 12)


# 4. Load regression data ------------------------------------------------------

# Load regression estimates dataset
regression_estimates <- read_csv("data/guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv")

## Prep regression data
reg_estimates_long <- regression_estimates %>% 
  dplyr::select(basin,
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


# 5. Make inset plots ----------------------------------------------------------

## Function for regression plot insets
reg_inset_plot <- function(selected_basin) {
  plot <- ggplot(
    data = reg_estimates_long %>% 
      filter(basin == selected_basin) %>%
      mutate(
        quantile = factor(quantile,
                          levels = c("Q10", "Q20", "Q30", "Q40", "Q50", 
                                     "Q60", "Q70", "Q80", "Q90", "Q100"))
      ),
    aes(
      x = as.factor(quantile),
      y = value,
      color = metric,
      group = metric
    )
  ) +
    geom_line(linetype = "dashed") +
    geom_errorbar(
      aes(
        ymin = ifelse(metric == "Slope", SlopeCI_2.5, RSquaredCI_2.5),
        ymax = ifelse(metric == "Slope", SlopeCI_97.5, RSquaredCI_97.5)
      ),
      width = 0.25,
      position = position_dodge(width = 0.15)
    ) +
    geom_point(size = 4.5) +
    geom_hline(yintercept = c(1.0, 1.2, 0.8), color = 'black', linewidth = c(1.0, 0.5, 0.5), linetype = c("solid", "dashed", "dashed")) +
    scale_y_continuous(limits = c(0.35, 1.7), breaks = c(0.5, 0.75, 1.0, 1.25, 1.50)) +
    scale_x_discrete(labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")) +
    scale_color_manual(name = "Metric", values = c("#3F2145", "#996300")) +
    labs(x = "Cumulative Hyp. Exchange \n(quantiles)", y = "Values") +
    ggtitle("Regression slopes and r-squared") +
    theme_httn +
    theme(legend.position = if(selected_basin == "willamette") c(.8,.2) else "none",
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title = element_text(size = 16, face ="bold"),
          plot.background = element_blank())
  
  return(plot)
}

# WRB inset plot
willamette_inset_plot <- reg_inset_plot(
  selected_basin = "willamette"
)
willamette_inset_plot

w_inset <- ggplotGrob(willamette_inset_plot)

w_scaling_grob <- plot_willamette +
  annotation_custom(
    w_inset,
    ymin = 5.0,
    ymax = 10.35,
    xmin = -3.25,
    xmax = 0.6
  )
w_scaling_grob

# Yakima inset plot
yakima_inset_plot <- reg_inset_plot(
  selected_basin = "yakima"
)
yakima_inset_plot

y_inset <- ggplotGrob(yakima_inset_plot)

y_scaling_grob <- plot_yakima +
  annotation_custom(
    y_inset,
    ymin = 5.0,
    ymax = 10.35,
    xmin = -3.25,
    xmax = 0.6
  )
y_scaling_grob


# 6. Make final plots and export -----------------------------------------------

# p_load(grid)
# x.grob <- textGrob(x.grob <- paste0(expression("Watershed area (", "km^2", ")")), 
#                    gp=gpar(fontface="bold", fontsize=15))
# 
# grid.arrange(w_scaling_grob, y_scaling_grob, 
#              bottom = x.grob, 
#              nrow = 1)


plot_grid(w_scaling_grob, y_scaling_grob,
          nrow = 1)
ggsave("figures/2_figure2.png", width = 24, height = 12)
ggsave("figures/2_figure2.pdf", width = 24, height = 12)

# plot_grid(willamette_inset_plot, NULL, yakima_inset_plot, 
#           nrow = 1, rel_widths = c(1, 0.1, 1))
# ggsave("figures/agu_poster/fig2_insets.pdf", 
#        width = 12, height = 4)


# 7. Alternative of inset plots that are colored by type -----------------------
regression_estimates %>% 
  clean_names() %>% 
  mutate(quantile = fct_relevel(quantile, "Q100", after = Inf)) %>% 
  mutate(r_squared = round(r_squared, 2)) %>% 
  mutate(scaling = case_when(slope_ci_2_5 <= 1 & slope_ci_97_5 >= 1 & r_squared > 0.8 ~ "Linear", 
                             slope_ci_2_5 < 1 & slope_ci_97_5 < 1 & r_squared > 0.8 ~ "Sublinear", 
                             slope_ci_2_5 > 1 & slope_ci_97_5 > 1 & r_squared > 0.8 ~ "Super-linear", 
                             TRUE ~ "Uncertain")) %>% 
  dplyr::select(basin, quantile, r_squared, slope, contains("slope_ci"), scaling) %>% 
  ggplot(aes(x = quantile)) + 
  geom_point(aes(y = slope, color = scaling), size = 4.5) +
  geom_errorbar(aes(ymin = slope_ci_2_5, ymax = slope_ci_97_5), width = 0.2) + 
  geom_hline(yintercept = 1) + 
  facet_wrap(~basin) + 
  scale_y_continuous(limits = c(0.35, 1.7), breaks = c(0.5, 0.75, 1.0, 1.25, 1.50)) +
  scale_x_discrete(labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")) + 
  scale_color_manual(values = c("gray", "blue", "forestgreen", "red"))
ggsave("figures/s_scaling_by_hef_quantiles.png", width = 8, height = 3)

