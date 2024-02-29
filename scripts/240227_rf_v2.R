## Random Forest v2
## The purpose of this is 1) to replace the dubious and boring MI with something
## that is easier to interpret and doesn't suffer the same multicollinearity issues, 
## and 2) get more quantitative vibes (feature importance and PDPs) to compare
## how similar the basins behave
## Using the SpatialML::grf() function, but will try and set up a tidymodels 
## workflow to feed things in at least (normalize, train/test split, and check for
## correlations).
##
## 2024-02-27
## Peter Regier
##
# ########## #
# ########## #

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

no_uncertainty <- scaling_data_combined %>% 
  filter(scaling != "Uncertain") ## We are pulling the Uncertain data out early

## Predictor columns to keep
predictors = c("doc_stream_mg_l", "no3_stream_mg_l", "do_stream_mg_l", 
               #"forest_3scp",  # removed: correlation filter for YRB
               "shrub_3scp", "human_3scp",
               "mean_ann_pcpt_mm", "d50_m")

## Dataset for feeding to the model
df_rf_all <- no_uncertainty %>% 
  dplyr::select(basin, scaling, quantile_n, # Metadata
                slope, # Dependent (allometric scaling slope)
                all_of(predictors), # Predictors
                latitude, longitude) # Coordinates

# model_data <- df_rf_all %>% 
#   filter(basin == selected_basin) %>% 
#   dplyr::select(slope, all_of(predictors))
# 
# model_recipe <- model_data %>% 
#   recipe(slope ~ .) %>% 
#   step_corr(all_predictors()) %>% 
#   step_normalize(all_predictors(), -all_outcomes()) %>% 
#   recipes::prep()

## Prep model data
model_prep <- function(selected_basin){
  
  ## Subset to basin
  df_rf_basin <- df_rf_all %>% 
    filter(basin == selected_basin)
  
  model_data <- df_rf_basin %>% 
    dplyr::select(slope, all_of(predictors))
  
  coordinates <- df_rf_basin %>% 
    dplyr::select(latitude, longitude)
  
  model_recipe <- model_data %>% 
    recipe(slope ~ .) %>% 
    step_corr(all_predictors()) %>% 
    step_normalize(all_predictors(), -all_outcomes()) %>% 
    recipes::prep()
  
  data <- model_recipe %>% 
    bake(model_data)
  
  list(data = bind_cols(data, coordinates), 
       model_recipe = model_recipe)
}

## Prep datasets by basin
wrb_data <- model_prep("willamette")  
yrb_data <- model_prep("yakima")


# I have no idea why, but the function is messing things up. So. We'll just do it
# the stupid way. I'm not frustrated, you're frustrated.

## Set seed and other stuff
set.seed(42)
proportion = 3/4
coord_cols = c("longitude", "latitude")

## Set formula
f <- as.formula(paste("slope ~", paste(predictors, collapse = " + ")))

tic("run wrb model")
wrb_grf_model <- SpatialML::grf(formula = f,
                            dframe = wrb_data$data %>% select(-c("latitude", "longitude")),
                            importance="impurity",
                            bw = 60,
                            kernel = "adaptive",
                            #nthreads = 8,
                            coords = wrb_data$data %>% select(c("latitude", "longitude")))

wrb_vi <- as.data.frame(wrb_grf_model$Global.Model$variable.importance) %>%
  rownames_to_column() %>%
  clean_names() %>%
  rename("predictor" = 1,
         "fi_raw" = 2) %>%
  as_tibble() %>%
  mutate(fi_n = fi_raw / sum(fi_raw) * 100)
toc()

tic("run yrb model")
yrb_grf_model <- SpatialML::grf(formula = f,
                                dframe = yrb_data$data %>% select(-c("latitude", "longitude")),
                                importance="impurity",
                                bw = 60,
                                kernel = "adaptive",
                                #nthreads = 8,
                                coords = yrb_data$data %>% select(c("latitude", "longitude")))

yrb_vi <- as.data.frame(yrb_grf_model$Global.Model$variable.importance) %>%
  rownames_to_column() %>%
  clean_names() %>%
  rename("predictor" = 1,
         "fi_raw" = 2) %>%
  as_tibble() %>%
  mutate(fi_n = fi_raw / sum(fi_raw) * 100)
toc()


## set up a color scheme for water quality
vi_vars <- unique(wrb_vi$predictor)
vi_colors <- PNWColors::pnw_palette("Bay", n = length(vi_vars))

fi_colors = tibble(var = vi_vars, 
                   colors = vi_colors)

bind_rows(wrb_vi %>% mutate(basin = "Willamette"), 
          yrb_vi %>% mutate(basin = "Yakima")) %>% 
  ggplot(aes(basin, fi_n, fill = predictor)) + 
  geom_col(width = 0.7, alpha = 0.7)


fi_plot <- function(data){
  
  x <- data %>% 
    mutate(predictor = case_when(predictor == "doc_stream_mg_l" ~ "DOC (mg/L)", 
                                 predictor == "no3_stream_mg_l" ~ "NO3 (mg/L)", 
                                 predictor == "do_stream_mg_l" ~ "DO (mg/L)", 
                                 predictor == "shrub_3scp" ~ "Shrubland", 
                                 predictor == "human_3scp" ~ "Humanscapes", 
                                 predictor == "mean_ann_pcpt_mm" ~ "Precip (mm/yr)", 
                                 predictor == "d50_m" ~ "d50 (m)"))
  
  ggplot(x, aes(reorder(predictor, fi_n), fi_n, fill = predictor)) + 
    geom_col(alpha = 0.7, width = 0.7) + 
    labs(x = "Feature importance (%)", y = "Predictor") +
    coord_flip() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_manual(values = fi_colors$colors) + 
    facet_wrap(~basin, nrow = 1)
}

plot_grid(fi_plot(wrb_vi %>% mutate(basin = "Willamette")) + theme(legend.position = "none"), 
          fi_plot(yrb_vi %>% mutate(basin = "Yakima"))  + theme(legend.position = "none"), 
          nrow = 1)
ggsave("figures/x_grf_feature_importance.png", width = 7, height = 3.5)
  

p_load(pdp)




make_pdp <- function(model, basin, var){
  stats <- df_rf_all %>% 
    filter(basin == basin) %>% 
    summarize(mean = mean({{var}}), 
              sd = sd({{var}}))
  
  pdp::partial(model$Global.Model, 
               pred.var = c("do_stream_mg_l")) %>% 
    as_tibble() %>% 
    rename("scaling_slope" = `yhat`) %>% 
    mutate(x_var = ({{var}} * stats$sd) + stats$mean, 
           basin = basin)
}

bind_rows(make_pdp(wrb_grf_model, "willamette", do_stream_mg_l), 
          make_pdp(yrb_grf_model, "yakima", do_stream_mg_l)) %>% 
  ggplot(aes(x_var, scaling_slope, color = basin)) + 
  geom_line()

df_rf_all %>% 
  filter(basin == "yakima") %>% 
  ggplot(aes(do_stream_mg_l)) + 
  geom_density()


wrb_d50_pdp <- pdp::partial(yrb_grf_model$Global.Model, 
                           pred.var = c("do_stream_mg_l")) %>% 
  mutate(do_mgl = (do_stream_mg_l))

autoplot(wrb_d50_pdp)









p_load(party, rpart, rpart.plot)

rpart_data <- scaling_data_combined %>% 
  mutate(scaling_bool = case_when(scaling == "Uncertain" ~ FALSE, 
                                  TRUE ~ TRUE)) %>% 
  dplyr::select(basin, # Metadata
                scaling_bool, # Dependent (allometric scaling slope)
                all_of(predictors)) # Predictors

wrb_rpart <- rpart_data %>% 
  filter(basin == "willamette") %>% 
  dplyr::select(-basin)

yrb_rpart <- rpart_data %>% 
  filter(basin == "yakima") %>% 
  dplyr::select(-basin)

tree_w <- rpart(scaling_bool ~ ., data = wrb_rpart)
tree_y <- rpart(scaling_bool ~ ., data = yrb_rpart)

rpart.plot(tree_w)






















run_grf_model <- function(input_data, coord_data){
  

  
  ## Split data and coordinates
  #model_df <- x %>% dplyr::select(-coord_cols)
  #model_coords <- x %>% dplyr::select(dplyr::all_of(coord_cols))
  
  ## Set formula
  f <- as.formula(paste("slope ~", paste(predictors, collapse = " + ")))
  
  ## Make model
  tic("run model")
  grf_model <- SpatialML::grf(formula = f,
                   dframe = input_data, #train_data,
                   importance="impurity",
                   bw = 60,
                   kernel = "adaptive",
                   #nthreads = 8,
                   coords = coord_data)
  toc()
  # 
  vi <- as.data.frame(grf_model$Global.Model$variable.importance) %>%
    rownames_to_column() %>%
    clean_names() %>%
    rename("predictor" = 1,
           "fi_raw" = 2) %>%
    as_tibble() %>%
    mutate(fi_n = fi_raw / sum(fi_raw) * 100)

  list(grf_model = grf_model,
       vi = vi)
}
# 
# run_grf_model(yrb_data$data)
# 
# 
# 

set.seed(42)
proportion = 3/4
coord_cols = c("longitude", "latitude")

## Split data and coordinates
#model_df <- x %>% dplyr::select(-coord_cols)
#model_coords <- x %>% dplyr::select(dplyr::all_of(coord_cols))

## Set formula
f <- as.formula(paste("slope ~", paste(predictors, collapse = " + ")))

## Make model
tic("run model")
grf_model <- SpatialML::grf(formula = f,
                            dframe = wrb_data$data %>% select(-c("latitude", "longitude")),
                            importance="impurity",
                            bw = 60,
                            kernel = "adaptive",
                            #nthreads = 8,
                            coords = wrb_data$data %>% select(c("latitude", "longitude")))
toc()
# 

# 
# 
# 

wrb_model <- run_grf_model(wrb_data$data %>% select(-c("latitude", "longitude")), 
                           wrb_data$data %>% select(c("latitude", "longitude")))

yrb_model <- run_grf_model(yrb_data$data)

bind_rows(wrb_model$vi %>% mutate(basin = "Willamette"), 
          yrb_model$vi %>% mutate(basin = "Yakima")) %>% 
  ggplot(aes(basin, fi_n, fill = predictor)) + 
  geom_col()
  



## Set seed and other stuff
# set.seed(42)
# proportion = 3/4
# coord_cols = c("longitude", "latitude")

## Split data
#split <- initial_split(x$data, prop = proportion)

## Set up training datasets (separately because two inputs are needed)
#train_data <- training(split) %>% dplyr::select(-all_of(coord_cols))
#train_coordinates <- training(split) %>% dplyr::select(all_of(coord_cols))

## Set up testing datasets (separately because two inputs are needed)
#test_data <- testing(split) #%>% dplyr::select(-all_of(coord_cols))

## Split data and coordinates
# model_data <- x$data %>% dplyr::select(-all_of(coord_cols))
# model_coords <- x$data %>% dplyr::select(all_of(coord_cols))
# 
# ## Set formula
# f <- as.formula(paste("slope ~", paste(predictors, collapse = " + ")))
# 
# ## Make model
# tic("run model")
# grf_model <- grf(f,
#                  dframe = model_data, #train_data, 
#                  importance="impurity",
#                  bw = 60, 
#                  kernel = "adaptive",
#                  coords = model_coords)
# toc()
# 
# vi <- as.data.frame(grf_model$Global.Model$variable.importance) %>% 
#   rownames_to_column() %>% 
#   clean_names() %>% 
#   rename("predictor" = 1, 
#          "fi_raw" = 2) %>% 
#   as_tibble() %>%
#   mutate(fi_n = fi_raw / sum(fi_raw) * 100)
# 
# list(grf_model = grf_model, 
#      vi = vi)
# 
# 
# tic("run predictions")
# predictions <- SpatialML::predict.grf(grf_model, 
#                                       new.data = test_data, 
#                                       x.var.name = "longitude", 
#                                       y.var.name = "latitude")
# toc()


