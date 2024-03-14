################################################################################
# Blocked Bootstrap Regression for Scaling Exponent estimation in longitudinally
# dependent cumulative data: Regression Coefficients
################################################################################

# Algorithm design and code evaluation: Francisco J. Guerrero 
# Code writing: AI-driven coding and data analysis tool developed by OpenAI.


librarian::shelf(dplyr,
                 tidyr,
                 purrr,
                 broom,
                 ggplot2,
                 tidyverse)

local_data <- "./data"
#results <- "./results"
#results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"
#findings_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/findings"

#source("./source/function_blocked_bootstrap.R")
source("./scripts/0_setup.R")

# Load the data
data <- read_csv(paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
                 show_col_types = FALSE)



# We selected a block size of 50

# Run the bootstrap regression
n_bootstraps <- 1000
block_size <- 50
bootstrap_results <- bootstrap_regression(data, n_bootstraps, block_size)

# Convert the results to a data frame
results_df <- map_df(bootstrap_results, ~tibble(.x), .id = "Basin_HztCat")

# Print the results
print(results_df)

# Unlist the nested list structure and create a vector
unlisted_results <- unlist(results_df$.x)

# Determine the number of rows in the final dataframe
num_rows <- length(unlisted_results) / 9

# Create a new dataframe with each row containing a set of 8 values
reshaped_results <- data.frame(matrix(unlisted_results, nrow = num_rows, byrow = TRUE))

# Assign column names to the reshaped dataframe
colnames(reshaped_results) <- c("Slope", "Intercept", "SlopeCI_2.5", "SlopeCI_97.5", 
                                "InterceptCI_2.5", "InterceptCI_97.5", "RSquared", 
                                "RSquaredCI_2.5","RSquaredCI_97.5")

# Add the Basin_HztCat column
reshaped_results$Basin_HztCat <- unique(results_df$Basin_HztCat)

# View the reshaped dataframe
head(reshaped_results)

# Reordering data frame
reordered_results <- reshaped_results %>% 
mutate(basin = str_extract(Basin_HztCat, "^[^_]+",),
       quantile = str_extract(Basin_HztCat,"(?<=_).*$")) %>% 
  mutate(quantile_cat = factor(quantile, levels = paste0("Q", rev(seq(10, 100, by = 10))))) %>% 
  arrange(basin, desc(quantile_cat)) %>% 
  dplyr::select(basin,
         quantile,
         Slope,
         Intercept,
         RSquared,
         SlopeCI_2.5,
         SlopeCI_97.5,
         InterceptCI_2.5,
         InterceptCI_97.5,
         RSquaredCI_2.5,
         RSquaredCI_97.5)

# Plotting results:
# Convert 'quantile' to a factor with levels ordered as desired
reordered_results$quantile <- factor(reordered_results$quantile, 
                                     levels = c("Q10", "Q20", "Q30", "Q40", "Q50", "Q60", "Q70", "Q80", "Q90", "Q100"))

# Saving results table

## This is commented out because the bootstrapping process will always slightly change the dataset used 
# write.csv(reordered_results,
#           file = paste(local_data,"guerrero_etal_23_results_block_bootstrap_scaling.csv",
#                        sep = '/'),
#           row.names = FALSE)
