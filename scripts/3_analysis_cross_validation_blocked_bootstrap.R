################################################################################
# Blocked Bootstrap Regression for Scaling Exponent estimation in longitudinally
# dependent cumulative data: Cross-validation
################################################################################

## Code from Francisco J. Guerrero and Peter Regier
## Contact: peter.regier@pnnl.gov

# Algorithm design and code evaluation: Francisco J. Guerrero 
# Code writing: AI-driven coding and data analysis tool developed by OpenAI.


# 1. Setup ---------------------------------------------------------------------

#source("./source/function_blocked_bootstrap.R")
source("./scripts/0_setup.R")




# Load the data
data <- read_csv("data/231008_scaling_analysis_dat.csv",
                 show_col_types = FALSE)

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((predicted - actual) ^ 2))
}

calculate_ssr <- function(actual, predicted) {
  sum((actual - predicted) ^ 2)
}

# Assuming 'data' is your original dataset
results_per_group <- list()

for (basin in unique(data$basin)) {
  for (hzt_cat in unique(data$accm_hzt_cat)) {
    basin_data <- data %>% filter(basin == !!basin, accm_hzt_cat == !!hzt_cat)
    
    if (nrow(basin_data) < block_size) next
    
    test_size <- min(block_size, nrow(basin_data))
    fold_rmse <- numeric()
    fold_ssr <- numeric()
    
    for (i in seq_len(nrow(basin_data) / test_size)) {
      test_indices <- ((i - 1) * test_size + 1):(i * test_size)
      test_data <- basin_data[test_indices, ]
      train_data <- basin_data[-test_indices, ]
      
      model <- lm(log(accm_totco2_o2g_day) ~ log(wshd_area_km2), data = train_data)
      
      predictions <- predict(model, newdata = test_data)
      actual <- log(test_data$accm_totco2_o2g_day)
      
      fold_rmse[i] <- calculate_rmse(actual, predictions)
      fold_ssr[i] <- calculate_ssr(actual, predictions)
    }
    
    results_per_group[[paste(basin, hzt_cat, sep = "_")]] <- list(
      RMSE = mean(fold_rmse),
      SSR = sum(fold_ssr)
    )
  }
}

# Convert the list of results to a data frame
results_df <- do.call(rbind, lapply(names(results_per_group), function(name) {
  data.frame(Group = name, RMSE = results_per_group[[name]]$RMSE, SSR = results_per_group[[name]]$SSR)
}))

# View the results
print(results_df)

# Split the 'Group' column into 'basin' and 'quantile'
results_df <- results_df %>%
  separate(Group, into = c("basin", "quantile"), sep = "_") %>%
  mutate(quantile = gsub("Q", "Q", quantile))  # Adjust the quantile format if needed

# Merging with model results

model_results <- read_csv("data/guerrero_etal_23_results_block_bootstrap_scaling.csv", show_col_types = FALSE)


# Merge with the existing model results
combined_results <- left_join(model_results, results_df, by = c("basin", "quantile"))

# View the combined results
#print(combined_results)


# Comparing R-squared values to RMSE

# p <- ggplot(data = combined_results,
#             aes(x = RSquared,
#                 y = SSR,
#                 color = basin))+
#   geom_point()
# p

## This is commented out because the bootstrapping process will always slightly change the dataset used 
# write.csv(combined_results,
#           file = paste(local_data,"guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv",
#                        sep = '/'),
#           row.names = FALSE)

