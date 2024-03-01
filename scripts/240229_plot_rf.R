
source("scripts/0_setup.R")

p_load(RandomForestsGLS, #not using (doesn't easily give VI)
       ggpmisc,
       SpatialML,
       PNWColors,
       tidymodels,
       tictoc)

vi <- read_csv("data/240228_vi_by_basin_and_scaling_category.csv") %>% 
  mutate(predictor = case_when(predictor == "doc_stream_mg_l" ~ "DOC (mg/L)", 
                               predictor == "no3_stream_mg_l" ~ "NO3 (mg/L)", 
                               predictor == "do_stream_mg_l" ~ "DO (mg/L)", 
                               predictor == "forest_3scp" ~ "LC: Forest", 
                               predictor == "shrub_3scp" ~ "LC: Shrub", 
                               predictor == "human_3scp" ~ "LC: Human", 
                               predictor == "d50_m" ~ "d50 (m)")) %>% 
  mutate(basin = case_when(basin == "yakima" ~ "Yakima (YRB)", 
                           basin == "willamette" ~ "Willamette (WRB)"), 
         cat = case_when(cat == "sub_or_linear" ~ "Sublinear/Linear", 
                         cat == "superlinear" ~ "Super-linear", 
                         cat == "uncertain" ~ "Uncertain"))

vi_vars <- unique(vi$predictor)
vi_colors <- PNWColors::pnw_palette("Bay", n = length(vi_vars))

fi_colors = tibble(var = vi_vars, 
                   colors = vi_colors)

vi %>% 
  ggplot(aes(fi_n, reorder(predictor, fi_n), color = predictor, fill = predictor)) + 
  geom_vline(xintercept = 15, color = "gray", linetype = "dashed") + 
  geom_col(width = 0.7, alpha = 0.7, show.legend = F) + 
  labs(x = "Feature importance (%)", y = "Predictor", color = "", fill = "") +
  facet_wrap(basin~cat, nrow = 2)  + 
  scale_color_manual(values = fi_colors$colors) + 
  scale_fill_manual(values = fi_colors$colors)
ggsave("figures/x_grf_vi.png", width = 10, height = 7)
