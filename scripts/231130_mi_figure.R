## Make a mutual information figure a la LOM paper for RC Scaling AGU poster

source("scripts/0_setup.R")

#path <- "/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/presentations/2023-12 AGU RC2 scaling poster/"
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
                              variable == "hrel_3" ~ "hrel_3",
                              variable == "simpson_d3" ~ "Simpson's diversity index"))
 
## set up a color scheme for water quality
mi_vars <- unique(mi_n$variable)
mi_colors <- PNWColors::pnw_palette("Bay", n = length(mi_vars))

mi_colors = tibble(var = mi_vars, 
                   colors = mi_colors)


ggplot(mi_n %>% filter(basin == "WRB"), 
       aes(reorder(variable, accm_totco2_o2g_day_n), fill = variable)) + 
  geom_col(aes(y = accm_totco2_o2g_day_n), show.legend = F) + 
  #geom_errorbar(aes(ymin = min_fi, ymax = max_fi), width = 0.2) + 
  labs(x = "Predictor", y = "Predictor Importance") + 
  #scale_x_discrete(limits=rev) + 
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = mi_colors$colors)

make_mi_plot <- function(data, title){
  ggplot(data, 
         aes(reorder(variable, accm_totco2_o2g_day_n), fill = variable)) + 
    geom_col(aes(y = accm_totco2_o2g_day_n), show.legend = F) + 
    labs(x = "Predictor", y = "Normalized \n mutual information", title = title) + 
    coord_flip() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual(values = mi_colors$colors)
}                            
          
plot_grid(make_mi_plot(mi_n %>% filter(basin == "WRB"), "Mutual information - WRB"), 
          NULL,
          make_mi_plot(mi_n %>% filter(basin == "YRB"), "Mutual information - YRB"), 
          nrow = 1, rel_widths = c(1, 0.1, 1))
ggsave("figures/agu_poster/231130_mutal_information.png", 
       width = 11, height = 5)