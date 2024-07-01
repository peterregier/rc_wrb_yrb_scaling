## Does stream area scale super-linearly with watershed area?

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")

colnames(scaling_analysis_dat)

df <- scaling_analysis_dat %>% 
  mutate(accm_stream_area_km2 = accm_stream_area_m2 / 1e6)

scale_var <- function(var) {
  
  ratios <- df %>% 
    filter(stream_order == 1) %>% 
    group_by(basin) %>% 
    summarize(mean_wshd_area_km2 = mean(wshd_area_km2, na.rm = T), 
              mean_var = mean(!!sym(var), na.rm = T)) %>% 
    mutate(ratio = mean_var/mean_wshd_area_km2)
  
  x <- df %>% 
    mutate(ratio = case_when(basin == "willamette" ~ ratios %>% filter(basin == "willamette") %>% pull(ratio), 
                             basin == "yakima" ~ ratios %>% filter(basin == "yakima") %>% pull(ratio)))
  return(x)
}

# Use the function
x <- scale_var("accm_stream_area_km2")
# x <- scale_var("accm_totco2_o2g_day")

ggplot(x, 
       aes(wshd_area_km2, accm_stream_area_km2 / ratio, color = basin)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

