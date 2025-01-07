## Count the number of sites used for each solute in each watershed to address
## reviewer comments

# 1. Setup ---------------------------------------------------------------------

source("scripts/0_setup.R")
require(dataRetrieval)

data_path <- "data/4Peter/usgs_solute_sites/"


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

basins <- bind_rows(yakima_boundary, willamette_boundary)

# 3. Read in data --------------------------------------------------------------

do_sites <- read_csv(paste0(data_path, "CR_annual_DO_sites.csv"))


pull_lat_longs <- function(site){
  
  message(site)
  
  whatNWISsites(sites = site) %>%
    as_tibble() %>%
    rename("lat" = dec_lat_va,
           "long" = dec_long_va) %>%
    dplyr::select(site_no, lat, long)
}

do_lat_longs <- do_sites$site_no %>% 
  map(pull_lat_longs) %>% 
  bind_rows()

do_sf <- st_as_sf(do_lat_longs, coords = c("long", "lat"), crs = common_crs)

ggplot() + 
  geom_sf(data = basins) + 
  geom_sf(data = do_sf)

## 13 sites in study basins - 11 in WRB, 2 in YRB
do_in_basins <- do_sf %>% 
  crop_shape(., basins, polygon = T)

ggplot() + 
  geom_sf(data = basins) + 
  geom_sf(data = do_in_basins)





