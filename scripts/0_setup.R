## Setup file for consistency across scripts
##
## Code from Francisco Guerrero and Peter Regier

## Load packages
require(pacman)
p_load(tidyverse,
       cowplot,
       janitor,
       sf, 
       nhdplusTools,
       rnaturalearth, 
       ggthemes, 
       ggallin,
       scales, #trans_format()
       PNWColors)

## Set ggplot theme default
theme_set(theme_bw())

## Core dataset that's used for most things, so load it
scaling_analysis_dat <- read_csv("data/231008_scaling_analysis_dat.csv")

## Plotting details baked into Fco functions

# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:20)
breaks_c <- 10^seq(-10,20,by=4)
minor_breaks <- rep(1:9, 31)*(10^rep(-10:20, each=9))

theme_httn<-  theme(axis.text=element_text(colour="black",size=22),
                    axis.title = element_text(size = 32, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", linewidth = 1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", linewidth = 1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())

## Set common CRS so everything is projected the same way
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"


### Originally from https://github.com/Scaling-Watershed-Function/2-swf-analytical.engine/blob/32dab31ca15c2c5f689ebd2de72d02ffef24e44f/scaling_analysis_willamette_yakima_rcm_23/source/function_blocked_bootstrap.R

################################################################################
# Blocked Bootstrap Regression for Scaling Exponent estimation in longitudinally
# dependent cumulative data
################################################################################

# Algorithm design and code evaluation: Francisco J. Guerrero 
# Code writing: AI-driven coding and data analysis tool developed by OpenAI.

# Define the bootstrap function
bootstrap_regression <- function(data, n_bootstraps, block_size) {
  results <- list()
  
  for (basin in unique(data$basin)) {
    for (hzt_cat in unique(data$accm_hzt_cat)) {
      basin_data <- data %>% filter(basin == !!basin, accm_hzt_cat == !!hzt_cat)
      
      if (nrow(basin_data) < block_size) next
      
      slopes <- vector("numeric", n_bootstraps)
      intercepts <- vector("numeric", n_bootstraps)
      r_squares <- vector("numeric", n_bootstraps)
      
      for (i in 1:n_bootstraps) {
        sample_indices <- sample(seq_len(nrow(basin_data)), size = block_size, replace = TRUE)
        sample_data <- basin_data[sample_indices, ]
        
        model <- lm(log(accm_totco2_o2g_day) ~ log(wshd_area_km2), data = sample_data)
        slopes[i] <- coef(model)["log(wshd_area_km2)"]
        intercepts[i] <- coef(model)["(Intercept)"]
        r_squares[i] <- summary(model)$r.squared
      }
      
      results[[paste(basin, hzt_cat, sep = "_")]] <- list(
        Slope = median(slopes),
        Intercept = median(intercepts),
        SlopeCI = quantile(slopes, probs = c(0.025, 0.975)),
        InterceptCI = quantile(intercepts, probs = c(0.025, 0.975)),
        RSquared = median(r_squares),
        RSquaredCI = quantile(r_squares, probs = c(0.025, 0.975))
      )
    }
  }
  
  return(results)
}


## From 


