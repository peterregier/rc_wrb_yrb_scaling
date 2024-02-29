## Explore geographically weighted random forests to understand relationships 
## between scaling (or scaling properties) and catchment characteristics
##
## Peter Regier
## 2023-12-30
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")

p_load(RandomForestsGLS, 
       ggpmisc,
       SpatialML,
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
  rename("quantile" = accm_hzt_cat)

scaling_data_combined <- inner_join(scaling_data_raw, 
                                    regression_estimates, 
                                    by = c("basin", "quantile"))


rf_data <- scaling_data_combined %>% 
  select(stream_order, doc_stream_mg_l, no3_stream_mg_l, 
         do_stream_mg_l, stream_area_m2, stream_width_m, reach_slope, d50_m) 

coordinates <- scaling_data_combined %>% 
  select(latitude, longitude) 


#https://github.com/ArkajyotiSaha/RandomForestsGLS?tab=readme-ov-file
set.seed(42)

n = 200

rf_trim <- rf_data %>% 
  slice(1:n)

coords_trim <- coordinates %>% 
  slice(1:n)

tic("run model")
## 200 (h=1) = 40s...
## 200 (h=2) = 26s!
## 200 (h=8) = 9s!!
## 300 (h=8) = 38s
## 400 (h=8) = 113
## 500 (h=8) = 206s
## 600 (h=8) = 273
rf_sp <- RFGLS_estimate_spatial(coords = cbind(coords_trim$latitude, 
                                               coords_trim$longitude), 
                                y = rf_trim$stream_order, 
                                X = rf_trim %>% select(-stream_order), 
                                verbose = TRUE,
                                h = 8) #h = # of cores
toc()


tibble(n = c(300, 400, 500, 600), 
       time = c(38, 113, 206, 273)) %>% 
  ggplot(aes(time, n)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  stat_poly_eq(use_label(c("eq", "R2")))

# m = 1.25
# b = 253
# slice_n = 16000
# ((m * slice_n) + b)/3600 # ~5 hours to run the full model if it all

rf_pred <- RFGLS_predict_spatial(RFGLS_out = rf_sp, 
                      coords = cbind(coords_trim$latitude, 
                                     coords_trim$longitude), 
                      verbose = TRUE,
                      h = 8)
## I don't see how to get feature importance out which is obviously clutch, so
## let's try a different package


## SpatialML try ---------------------------------------------------------------


sm_n = 500

rf_data2 <- scaling_data_combined %>% 
  select(slope, basin, stream_order, doc_stream_mg_l, no3_stream_mg_l, 
         do_stream_mg_l, stream_area_m2, stream_width_m, reach_slope, d50_m) %>% 
  slice(1:sm_n)

coords2 <- scaling_data_combined %>% 
  select(latitude, longitude) %>% 
  slice(1:sm_n)

tic("run model")
grf_model <- grf(slope ~ basin + 
                   doc_stream_mg_l + 
                   no3_stream_mg_l + 
                   do_stream_mg_l + 
                   #reach_slope + 
                   d50_m,
                 dframe = rf_data2, 
                 importance="impurity",
                 bw = 60, 
                 kernel = "adaptive",
                 coords = coords2)
toc()

grf_model$LocalModelSummary$l.VariableImportance



run_grf_model_by_basin <- function(selected_basin){
  
  #sm_n = 3000
  
  df <- scaling_data_combined %>% 
    filter(scaling != "Uncertain") %>% 
    filter(basin == selected_basin) %>% 
    select(slope, stream_order, doc_stream_mg_l, no3_stream_mg_l, 
           do_stream_mg_l, stream_area_m2, stream_width_m, reach_slope, d50_m) #%>% 
    #slice(1:sm_n)
  
  coordinates <- scaling_data_combined %>% 
    filter(scaling != "Uncertain") %>% 
    filter(basin == selected_basin) %>% 
    select(latitude, longitude) #%>% 
    #slice(1:sm_n)
  
  tic("run model")
  grf_model <- grf(slope ~ stream_order + 
                     doc_stream_mg_l + 
                     no3_stream_mg_l + 
                     do_stream_mg_l + 
                     reach_slope + 
                     d50_m,
                   dframe = df, 
                   nthreads = 8, 
                   geo.weighted = TRUE,
                   importance="impurity",
                   bw = 60, 
                   kernel = "adaptive",
                   coords = coordinates)
  toc()
  
  grf_model
}

wrb_model <- run_grf_model_by_basin("willamette") #185
yrb_model <- run_grf_model_by_basin("yakima") #86s


yrb_model$LocalModelSummary$l.VariableImportance





