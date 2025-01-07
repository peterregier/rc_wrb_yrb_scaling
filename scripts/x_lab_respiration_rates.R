##

source("scripts/0_setup.R")
p_load(tmaptools, ggallin)

data <- read_csv("data/from_ess_dive/CM_SSS_lat_longs_unformatted.csv", col_names = F)

parse_data <- function(data) {
  result <- list()
  i <- 1
  while (i <= nrow(data)) {
    if (data$X1[i] != "") {
      # Extract information from the first cell
      cell_info <- unlist(strsplit(data$X1[i], ",\\s*"))
      id <- cell_info[1]
      ignore <- cell_info[2]
      site <- cell_info[3]
      state <- cell_info[4]
      
      # Extract longitude and latitude
      long <- data$X1[i + 1]
      lat <- data$X1[i + 3]
      
      # Create a data frame row
      result[[length(result) + 1]] <- data.frame(sample_name = id, 
                                                 ignore = ignore, 
                                                 site = site, 
                                                 state = state, 
                                                 long = as.numeric(long), 
                                                 lat = as.numeric(lat), 
                                                 stringsAsFactors = FALSE)
      i <- i + 5
    } else {
      i <- i + 1
    }
  }
  
  # Combine all rows into a single data frame
  final_result <- bind_rows(result)
  
  return(final_result)
}

# Parse the example data
df <- st_as_sf(parse_data(data), coords = c("long", "lat"), crs = common_crs)

ggplot() + 
  geom_sf(data = df)


# 2. Prep watershed boundaries and flowlines -----------------------------------

## Read in shapefiles with flowlines for the two study basins
nsi <- read_sf("data/nsi_network_ywrb/nsi_network_ywrb.shp") %>% 
  st_transform(crs = common_crs)

## There isn't a convenient way to break these apart, so I'll use st_crop
yakima_flowlines <- st_crop(nsi, xmin = -122, xmax = -119, ymin = 45.9, ymax = 48)
willamette_flowlines <- st_crop(nsi, xmin = -124, xmax = -121, ymin = 43, ymax = 46)

## YRB
yakima_boundary <- get_huc(AOI = st_union(yakima_flowlines), type = "huc04") %>% 
  filter(huc4 == "1703")

## WRB
willamette_boundary <- read_sf("data/basin_boundaries/Willamette_Custom_Watershed_Boundary_noCorner/Willamette_Custom_Watershed_Boundary_noCorner.shp") %>% 
  st_transform(crs = common_crs)

## US map
states <- ne_states(country = "united states of america", 
                    returnclass = "sf") %>% 
  filter(gn_name == "Oregon" | gn_name == "Washington")


# 3. Sites within YRB and WRB --------------------------------------------------

wrb_sites <- crop_shape(df, willamette_boundary, polygon = T)
yrb_sites <- crop_shape(df, yakima_boundary, polygon = T)

ggplot() + 
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = yakima_boundary, fill = "forestgreen", alpha = 0.3) + 
  geom_sf(data = willamette_boundary, fill = "forestgreen", alpha = 0.3) + 
  geom_sf(data = yrb_sites) + 
  geom_sf(data = wrb_sites)

relevant_sites <- bind_rows(wrb_sites %>% mutate(basin = "WRB"), 
                            yrb_sites %>% mutate(basin = "YRB")) 


# 4. Pull in model estimates ---------------------------------------------------

## Prep scaling_analysis_dat
scaling_dat_trimmed <- scaling_map_dat %>% 
  dplyr::select(comid, 
         basin_cat,
         basin,
         wshd_area_km2, 
         stream_order,
         totco2_o2g_day,
         accm_totco2_o2g_day,
         doc_stream_mg_l,
         no3_stream_mg_l, 
         do_stream_mg_l,
         longitude,
         latitude, 
         wshd_avg_elevation_m)

## Convert prepped dataset to an sf object for plotting
scaling_map_sf <- inner_join(nsi %>% clean_names(), 
                             scaling_dat_trimmed, by = "comid")


# 5. Pull in flowlines ---------------------------------------------------------

## We have all the sites that are within the basins of interest, now we need to
## match them to the closest comid: 
## 1. find comids for each site
## 2. match comids between lab and model
## 3. see if there are correlations / relationships between them
wrb_flowlines <- get_nhdplus(AOI = willamette_boundary)
yrb_flowlines <- get_nhdplus(AOI = yakima_boundary)

wrb_sites_comids <- wrb_sites %>% 
  mutate(comid = wrb_flowlines %>% slice(st_nearest_feature(wrb_sites, wrb_flowlines)) %>% 
           pull(comid)) %>% 
  st_drop_geometry()

yrb_sites_comids <- yrb_sites %>% 
  mutate(comid = yrb_flowlines %>% slice(st_nearest_feature(yrb_sites, yrb_flowlines)) %>% 
           pull(comid)) %>% 
  st_drop_geometry()

sites_with_comids <- as_tibble(bind_rows(wrb_sites_comids %>% mutate(basin = "WRB"), 
                                         yrb_sites_comids %>% mutate(basin = "YRB")))


# 6. Finally, bind with both sources of respiration estimates ------------------
ess_path <- "data/from_ess_dive/"

## Function to read the stupid ess-dive formatting
read_custom_csv <- function(file_name){
  file_path = paste0(ess_path, file_name)
  headers <- read_csv(file_path, skip = 2, n_max = 1, col_names = FALSE)
  col_names <- as.character(headers[1, ])
  all_data <- read_csv(file_path, col_names = FALSE)
  start_row <- which(all_data$X1 == "Data_Status") + 1
  
  read_csv(file_path, skip = start_row, col_names = col_names) %>% 
    clean_names()
}

## Read in potentially useful info
ersed_norm <- read_custom_csv("v2_CM_SSS_Sediment_Normalized_Respiration_Rates.csv") %>% 
  mutate(sample_name = str_remove(sample_name, "_INC-\\d")) %>% 
  mutate(ersed_mg_l_hr = case_when(normalized_respiration_rate_mg_do_per_h_per_l_sediment == -9999 ~ NA, 
                                   TRUE ~ normalized_respiration_rate_mg_do_per_h_per_l_sediment), 
         ersed_mg_l_d = ersed_mg_l_hr * 24) %>% 
  group_by(sample_name) %>% 
  summarize(ersed_mg_l_d_mean = mean(ersed_mg_l_d, na.rm = T), 
            ersed_mg_l_d_sd = sd(ersed_mg_l_d, na.rm = T))

# ersed_inc <- read_custom_csv("v3_CM_SSS_Sediment_Incubations_Respiration_Rates.csv") %>% 
#   mutate(sample_name = str_remove(sample_name, "_INC-\\d")) %>% 
#   mutate(ersed_mg_l_hr = case_when(respiration_rate_mg_do_per_l_per_h == -9999 ~ NA, 
#                                    TRUE ~ respiration_rate_mg_do_per_l_per_h)) %>% 
#   group_by(sample_name) %>% 
#   summarize(ersed_mg_l_hr_mean = mean(ersed_mg_l_hr, na.rm = T), 
#             ersed_mg_l_hr_sd = sd(ersed_mg_l_hr, na.rm = T))

# ions <- read_custom_csv("CM_SSS_Water_Ions.csv")
# doc_tn <- read_custom_csv("v3_CM_SSS_Water_NPOC_TN.csv")

sites_with_comids_and_ersed <- left_join(sites_with_comids, ersed_norm, by = "sample_name")

rate_comparisons <- inner_join(sites_with_comids_and_ersed, 
           scaling_dat_trimmed %>% 
             dplyr::select(comid, stream_order, totco2_o2g_day), 
           by = "comid") %>% 
  mutate(ersed_flipped = ersed_mg_l_d_mean * -1)

calculate_max <- function(var){
  
  x <- rate_comparisons %>% pull({{var}})
  
  q3 <- summary(x)[5]
  max_value <- as.numeric(q3 + (IQR(x) * 1.5))
  
  max_value
}

rate_comparisons_labeled <- rate_comparisons %>% 
  mutate(lab_outlier = case_when(ersed_flipped > calculate_max(ersed_flipped) ~ "outlier", 
         TRUE ~ "nope")) %>% 
  mutate(model_outlier = case_when(totco2_o2g_day > calculate_max(totco2_o2g_day) ~ "outlier", 
         TRUE ~ "nope"))

rate_comparisons_trim <- rate_comparisons_labeled %>%
  #filter(lab_outlier != "outlier" & model_outlier != "outlier")
  filter(totco2_o2g_day < 8e6) %>%
  filter(ersed_mg_l_d_mean > -25000)



plot_grid(rate_comparisons %>% 
  ggplot(aes(ersed_mg_l_d_mean, totco2_o2g_day, pch = basin)) + 
  geom_point(color = "gray") + 
    geom_point(data = rate_comparisons_trim, color = "black") + 
    labs(x = "Sediment resp (mg DO/L/d)", 
         y = "HZ resp. (g O2/d)", 
         pch = "Basin") + 
    theme(legend.position = c(0.2, 0.7), 
          legend.background = element_blank()), 
  rate_comparisons_trim %>% 
    ggplot(aes(ersed_mg_l_d_mean, totco2_o2g_day)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    labs(x = "Sediment resp (mg DO/L/d)", 
         y = "HZ resp. (g O2/d)") + 
    annotate(geom = "text", x = -1500, y = 4e6, 
             label = "R2adj = 0.09 (p = 0.03)"), 
  nrow = 1)
ggsave("figures/SC_lab_model_resp_comparison.png", width = 8, height = 3.5)

## adjR2 = 0
summary(lm(totco2_o2g_day~ersed_mg_l_d_mean, data = rate_comparisons))

## adjR2 = 0.1
summary(lm(totco2_o2g_day~ersed_mg_l_d_mean, data = rate_comparisons_trim))








