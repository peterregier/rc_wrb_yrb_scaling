## Random Forest v3
## Based on Wil's feedback, let's try and model cumulative respiration, but
## divided by 1) scaling x basin, and 2) median HEF x basin

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
                                    by = c("basin", "quantile"))


# 3. Prep dataset --------------------------------------------------------------

## Predictor columns to keep
predictors = c("doc_stream_mg_l", "no3_stream_mg_l", "do_stream_mg_l", 
               "forest_3scp",  # removed: correlation filter for YRB
               "shrub_3scp", "human_3scp", #swapping out for something easier to explain
               #"wshd_max_elevation_m", 
               #"simpson_d", 
               #"mean_ann_pcpt_mm", 
               "d50_m")

## Dataset for feeding to the model
df_rf_all <- scaling_data_combined %>% 
  mutate(hef_cat = ifelse(quantile_n > 50, "high_hef", "low_hef")) %>%
  mutate(scaling_cat = case_when(scaling == "Super-linear" ~ "superlinear", 
                                 scaling == "Uncertain" ~ "uncertain", 
                                 TRUE ~ "sub_or_linear")) %>% 
  dplyr::select(basin, scaling, # Metadata
                hef_cat, scaling_cat, # Categories
                accm_totco2_o2g_day, # Dependent (allometric scaling slope)
                all_of(predictors), # Predictors
                latitude, longitude) # Coordinates


# 4. Make a flexible RF function -----------------------------------------------

## Two inputs: basin and category - that can be set up via a dataframe then
## fed to the function

models_to_run <- tibble(expand_grid(selected_basin = unique(df_rf_all$basin), 
                                    cat = unique(df_rf_all$scaling_cat)))


prep_grf_model <- function(selected_basin = selected_basin, 
                          cat = cat){
  
  message(paste("Running", selected_basin, "for", cat))
  
  ## Set seed and other stuff
  set.seed(42)
  proportion = 3/4
  coord_cols = c("longitude", "latitude")
  
  x <- df_rf_all %>% 
    filter(scaling_cat == cat) %>% 
    filter(basin == selected_basin)
  
  model_data_raw <- x %>%
    dplyr::select(accm_totco2_o2g_day, all_of(predictors))

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
             model_coordinates = model_coordinates))
 
 # tic("run model")
 # grf_model <- SpatialML::grf(formula = f,
 #                             dframe = model_data,
 #                             importance="impurity",
 #                             bw = 60,
 #                             kernel = "adaptive",
 #                             #nthreads = 4,
 #                             coords = model_coordinates)
 # 
 # vi <- as.data.frame(grf_model$Global.Model$variable.importance) %>%
 #   rownames_to_column() %>%
 #   clean_names() %>%
 #   rename("predictor" = 1,
 #          "fi_raw" = 2) %>%
 #   as_tibble() %>%
 #   mutate(fi_n = fi_raw / sum(fi_raw) * 100,
 #          basin = basin,
 #          scaling = cat)
 # toc()
}

data_for_modeling <- models_to_run %>% 
  pmap(prep_grf_model) 

run_grf_model <- function(x2, coordinates){
  
  predictors_used <- colnames(x2 %>% select(-accm_totco2_o2g_day))
  
  ## Set formula
  f <- as.formula(paste("accm_totco2_o2g_day ~", paste(predictors_used, collapse = " + ")))
  
  tic("run model")
  grf_model <- SpatialML::grf(formula = f,
                              dframe = x2,
                              importance="impurity",
                              bw = 60,
                              kernel = "adaptive",
                              #nthreads = 4,
                              coords = model_coordinates)

  # vi <- as.data.frame(grf_model$Global.Model$variable.importance) %>%
  #   rownames_to_column() %>%
  #   clean_names() %>%
  #   rename("predictor" = 1,
  #          "fi_raw" = 2) %>%
  #   as_tibble() %>%
  #   mutate(fi_n = fi_raw / sum(fi_raw) * 100,
  #          basin = basin,
  #          scaling = cat)
  # toc()
  # 
  return(vi)
}

## This is a stupid way to do this, but I don't know how else to get grf() to loop. 
## It absolutely refuses to play nicely with pmap() for some reason...
vi_list <- list()
for(i in 1:6){
  
  ## Pull predictors to include
  predictors_used <- colnames(data_for_modeling[[i]]$model_data %>% select(-accm_totco2_o2g_day))
  
  ## Set up formula
  f <- as.formula(paste("accm_totco2_o2g_day ~", paste(predictors_used, collapse = " + ")))
  
  grf_model <- SpatialML::grf(formula = f,
                              dframe = data_for_modeling[[i]]$model_data,
                              importance="impurity",
                              bw = 60,
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
}

vi <- bind_rows(vi_list) 
write_csv(vi, "data/240228_vi_by_basin_and_scaling_category.csv")

vi %>% 
  ggplot(aes(fi_n, predictor, fill = predictor)) + 
  geom_col(width = 0.7, alpha = 0.7) + 
  facet_wrap(basin~cat, nrow = 2)
  



## First, we need to prep data (normalize, examine for correlations)
wrb_low <- df_rf_all %>% 
  filter(basin == "willamette") %>% 
  filter(hef_cat == "low_hef")

wrb_high <- df_rf_all %>% 
  filter(basin == "willamette") %>% 
  filter(hef_cat == "high_hef")

yrb_low <- df_rf_all %>% 
  filter(basin == "yakima") %>% 
  filter(hef_cat == "low_hef")

yrb_high <- df_rf_all %>% 
  filter(basin == "yakima") %>% 
  filter(hef_cat == "high_hef")

## Prep model data
model_prep <- function(data){
  
  model_data <- data %>% 
    dplyr::select(accm_totco2_o2g_day, all_of(predictors))
  
  coordinates <- data %>% 
    dplyr::select(latitude, longitude)
  
  model_recipe <- model_data %>% 
    recipe(accm_totco2_o2g_day ~ .) %>% 
    #step_corr(all_predictors()) %>% 
    step_normalize(all_predictors(), -all_outcomes()) %>% 
    recipes::prep()
  
  data <- model_recipe %>% 
    bake(model_data)
  
  list(data = bind_cols(data, coordinates), 
       model_recipe = model_recipe)
}

wrb_low_data <- model_prep(wrb_low)
wrb_high_data <- model_prep(wrb_high)
yrb_low_data <- model_prep(yrb_low)
yrb_high_data <- model_prep(yrb_high)

## Set seed and other stuff
set.seed(42)
proportion = 3/4
coord_cols = c("longitude", "latitude")

## Set formula
f <- as.formula(paste("accm_totco2_o2g_day ~", paste(predictors, collapse = " + ")))

tic("run wrb low model")
wrb_low_grf_model <- SpatialML::grf(formula = f,
                                dframe = wrb_low_data$data %>% select(-c("latitude", "longitude")),
                                importance="impurity",
                                bw = 60,
                                kernel = "adaptive",
                                #nthreads = 8,
                                coords = wrb_low_data$data %>% select(c("latitude", "longitude")))

wrb_low_vi <- as.data.frame(wrb_low_grf_model$Global.Model$variable.importance) %>%
  rownames_to_column() %>%
  clean_names() %>%
  rename("predictor" = 1,
         "fi_raw" = 2) %>%
  as_tibble() %>%
  mutate(fi_n = fi_raw / sum(fi_raw) * 100)
toc()

tic("run wrb high model")
wrb_high_grf_model <- SpatialML::grf(formula = f,
                                dframe = wrb_high_data$data %>% select(-c("latitude", "longitude")),
                                importance="impurity",
                                bw = 60,
                                kernel = "adaptive",
                                #nthreads = 8,
                                coords = wrb_high_data$data %>% select(c("latitude", "longitude")))

wrb_high_vi <- as.data.frame(wrb_high_grf_model$Global.Model$variable.importance) %>%
  rownames_to_column() %>%
  clean_names() %>%
  rename("predictor" = 1,
         "fi_raw" = 2) %>%
  as_tibble() %>%
  mutate(fi_n = fi_raw / sum(fi_raw) * 100)
toc()

tic("run yrb low model")
yrb_low_grf_model <- SpatialML::grf(formula = f,
                                    dframe = yrb_low_data$data %>% select(-c("latitude", "longitude")),
                                    importance="impurity",
                                    bw = 60,
                                    kernel = "adaptive",
                                    #nthreads = 8,
                                    coords = yrb_low_data$data %>% select(c("latitude", "longitude")))

yrb_low_vi <- as.data.frame(yrb_low_grf_model$Global.Model$variable.importance) %>%
  rownames_to_column() %>%
  clean_names() %>%
  rename("predictor" = 1,
         "fi_raw" = 2) %>%
  as_tibble() %>%
  mutate(fi_n = fi_raw / sum(fi_raw) * 100)
toc()

tic("run yrb high model")
yrb_high_grf_model <- SpatialML::grf(formula = f,
                                     dframe = yrb_high_data$data %>% select(-c("latitude", "longitude")),
                                     importance="impurity",
                                     bw = 60,
                                     kernel = "adaptive",
                                     #nthreads = 8,
                                     coords = yrb_high_data$data %>% select(c("latitude", "longitude")))

yrb_high_vi <- as.data.frame(yrb_high_grf_model$Global.Model$variable.importance) %>%
  rownames_to_column() %>%
  clean_names() %>%
  rename("predictor" = 1,
         "fi_raw" = 2) %>%
  as_tibble() %>%
  mutate(fi_n = fi_raw / sum(fi_raw) * 100)
toc()

bind_rows(wrb_low_vi %>% mutate(hef = "low", basin = "Willamette"), 
          wrb_high_vi %>% mutate(hef = "high", basin = "Willamette"),
          yrb_low_vi %>% mutate(hef = "low", basin = "Yakima"), 
          yrb_high_vi %>% mutate(hef = "high", basin = "Yakima")) %>% 
  ggplot(aes(fi_n, predictor)) + 
  geom_col() + 
  facet_wrap(basin~hef, nrow = 2)




