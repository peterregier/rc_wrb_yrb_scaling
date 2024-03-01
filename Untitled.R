## Yet another iteration...
## Let's create 4 models to test is it basin or scaling that results in more
## different fits/feature importance. We'll use a subset of data that includes
## Q10-30 and Q80-100 for both watersheds (the areas that behave similarly)
## 1. YRB (all quantiles)
## 2. WRB (all quantiles)
## 3. Q10-30 (both basins)
## 4. Q80-100 (both basins)
## Assess 1) GOF (NSE), which models performed the best? and 2) how similar
## was the order of importance of variables?

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")

p_load(RandomForestsGLS, #not using (doesn't easily give VI)
       ggpmisc,
       SpatialML,
       PNWColors,
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
                                    by = c("basin", "quantile")) %>% 
  filter(quantile_n < 40 | quantile_n > 70) %>% 
  mutate(quantile_cat = case_when(quantile_n < 40 ~ "Q10-30", 
                                  quantile_n > 70 ~ "Q80-100", 
                                  TRUE ~ NA))

predictors_new <- c(#"forest_3scp", "shrub_3scp", "human_3scp", 
                    "accm_doc_load_kg_d", 
                    "accm_no3_load_kg_d", 
                    "accm_water_exchng_kg_d", 
                    "accm_wshd_stream_dens",
                    "accm_reach_length_km", 
                    "accm_mean_ann_pcpt_mm") 

df <- scaling_data_combined %>% 
  filter(!is.na(quantile_cat))  %>% 
  dplyr::select(basin, quantile_cat, # Categories
                accm_totco2_o2g_day, # Dependent (allometric scaling slope)
                all_of(predictors_new), # Predictors
                latitude, longitude) #%>% # Coordinates
  #group_by(basin, quantile_cat) %>% 
  #slice(1:500) %>% 
  #ungroup()


models_to_run <- tibble(expand_grid(selected_basin = unique(df$basin), 
                                    cat = unique(df$quantile_cat)))

prep_grf_model <- function(selected_basin = selected_basin, 
                           cat = cat){
  
  message(paste("Running", selected_basin, "for", cat))
  
  ## Set seed and other stuff
  set.seed(42)
  proportion = 3/4
  coord_cols = c("longitude", "latitude")
  
  x <- df %>% 
    filter(quantile_cat == cat) %>% 
    filter(basin == selected_basin)
  
  model_data_raw <- x %>%
    dplyr::select(accm_totco2_o2g_day, all_of(predictors_new))
  
  model_coordinates <- x %>%
    dplyr::select(latitude, longitude)
  
  model_recipe <- model_data_raw %>%
    recipe(accm_totco2_o2g_day ~ .) %>%
    #step_corr(all_predictors()) %>%
    step_normalize(all_predictors(), -all_outcomes()) %>%
    recipes::prep()
  
  model_data <- model_recipe %>%
    bake(model_data_raw)
  
  return(list(model_data = model_data, 
              model_coordinates = model_coordinates, 
              model_recipe = model_recipe))
  
}

data_for_modeling <- models_to_run %>% 
  pmap(prep_grf_model) 


run_rf_model <- function(x2){

  predictors_used <- colnames(x2 %>% select(-accm_totco2_o2g_day))

  ## Set formula
  f <- as.formula(paste("accm_totco2_o2g_day ~", paste(predictors_used, collapse = " + ")))

  tic("run model")
  rf_model <- ranger(formula = f,
                              data = x2,
                              importance="impurity")

 # return(rf_model)
  
  vi <- as.data.frame(rf_model$variable.importance) %>%
    rownames_to_column() %>%
    clean_names() %>%
    rename("predictor" = 1,
           "fi_raw" = 2) %>%
    as_tibble() %>%
    mutate(fi_n = fi_raw / sum(fi_raw) * 100)
  toc()

  return(vi)
}

outputs <- list()
for(i in 1:nrow(models_to_run)){
  outputs[[i]] <- run_rf_model(data_for_modeling[[i]]$model_data) %>% 
    mutate(basin = models_to_run$selected_basin[i], 
           cat = models_to_run$cat[i])
}

bind_rows(outputs) %>% 
  ggplot(aes(fi_n, predictor, fill = predictor)) + 
  geom_col() + 
  facet_wrap(basin ~ cat)


scaling_data_combined %>% 
  dplyr::select(accm_totco2_o2g_day, all_of(predictors_new)) %>% 
  cor()

## This is a stupid way to do this, but I don't know how else to get grf() to loop. 
## It absolutely refuses to play nicely with pmap() for some reason...
vi_list <- list()

tic("run for-loop")
for(i in 3:3){
  
  ## Pull predictors to include
  predictors_used <- colnames(data_for_modeling[[i]]$model_data %>% select(-accm_totco2_o2g_day))
  
  ## Set up formula
  f <- as.formula(paste("accm_totco2_o2g_day ~", paste(predictors_used, collapse = " + ")))
  
  grf_model <- SpatialML::grf(formula = f,
                              dframe = data_for_modeling[[i]]$model_data,
                              importance="impurity",
                              bw = 20,
                              kernel = "adaptive",
                              nthreads = 8,
                              coords = data_for_modeling[[i]]$model_coordinates)
  
  vi_list[[i]] <- as.data.frame(grf_model$Global.Model$variable.importance) %>%
    rownames_to_column() %>%
    clean_names() %>%
    rename("predictor" = 1,
           "fi_raw" = 2) %>%
    as_tibble() %>%
    mutate(fi_n = fi_raw / sum(fi_raw) * 100, 
           basin = models_to_run$selected_basin[[i]], 
           cat = models_to_run$cat[[i]])
  
  rm(grf_model)
}

toc()


bind_rows(vi_list) %>% 
  ggplot(aes(fi_n, predictor, fill = predictor)) + 
  geom_col(width = 0.7, alpha = 0.7) + 
  facet_wrap(basin~cat, nrow = 2)
  



