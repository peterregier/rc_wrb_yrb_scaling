## Setup file for consistency across scripts

require(pacman)
p_load(tidyverse,
       cowplot,
       janitor,
       sf, 
       scales, #trans_format()
       PNWColors)

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

common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"
