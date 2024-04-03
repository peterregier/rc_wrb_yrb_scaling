###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# CUMULATIVE CALCULATIONS ACROSS THE STREAM NETWORKS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"
## Contact: peter.regier@pnnl.gov

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis, GGally, nhdplusTools, gridExtra, ggExtra, data.table, viridis)

set.seed(2703)

# Data

# Local data saving
local_data <- "./data" 

#  Local figure export
results <- "./results" #For svg files that can be explored online

# GitHub import
wshd_ent_dat <-read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima_rcm_23/data/231008_landscape_heterogeneity_pnw.csv",
                                show_col_types = FALSE)

rcm_23_model_output_dat <- read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima_rcm_23/data/rcm_23_model_output_data.csv",
                                    show_col_types = FALSE)


# Merging datasets
rcm_23_model_dat <- rcm_23_model_output_dat %>% 
  merge(.,
        wshd_ent_dat %>% 
          dplyr::select(comid,
                 forest_scp,
                 shrub_scp,
                 human_scp,
                 grass_scp,
                 water_scp,
                 barren_scp,
                 ht,
                 hrel,
                 simpson_d),
        by = "comid",
        all.x = TRUE) 


# Adding quantiles for key variables

qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80","Q90","Q100")

scaling_analysis_dat <- rcm_23_model_dat %>% 
  group_by(basin) %>% 
  mutate(ent_cat = factor(Hmisc::cut2(hrel, g = 10),labels = qlabel),
         smp_cat = factor(Hmisc::cut2(simpson_d, g = 10),labels = qlabel),
         rst_cat = factor(Hmisc::cut2(tot_rt_hz_s, g = 10),labels = qlabel),
         hzt_cat = factor(Hmisc::cut2(tot_q_hz_ms, g = 10),labels = qlabel),
         pct_cat = factor(Hmisc::cut2(mean_ann_pcpt_mm, g = 10),labels = qlabel),
         rnf_cat = factor(Hmisc::cut2(mean_ann_runf_mm, g = 10),labels = qlabel),
         d50_cat = factor(Hmisc::cut2(d50_m, g = 10),labels = qlabel),
         are_cat = factor(Hmisc::cut2(log(wshd_area_km2), g = 10),labels = qlabel),
         sto_fct = as.factor(stream_order)) %>% 
  ungroup() %>% 
  mutate(basin_cat = as.factor(if_else(basin == "yakima",
                                       "Yakima River (Dry)",
                                       "Willamette River (Wet)")))

# Cumulative calculations
scaling_analysis_dat <- scaling_analysis_dat %>% 
  group_by(basin) %>% 
  mutate(water_exchng_kg_d = tot_q_hz_ms * stream_area_m2 * 997 * 86400,
         doc_load_kg_d = doc_stream_mg_l * stream_area_m2 * mean_ann_vel_ms * 86400,
         no3_load_kg_d = no3_stream_mg_l * stream_area_m2 * mean_ann_vel_ms * 86400,
         mean_ann_pcpt_m3 = (mean_ann_pcpt_mm/1000)*wshd_area_km2*1000000) %>% 
  mutate(across(c(wshd_stream_dens,
                  reach_length_km,
                  mean_ann_pcpt_mm,
                  mean_ann_pcpt_m3,
                  mean_ann_runf_mm,
                  stream_area_m2,
                  water_exchng_kg_d,
                  doc_load_kg_d,
                  no3_load_kg_d,
                  totco2g_day,
                  totco2_o2g_day,
                  totco2_ang_day), 
                ~ calculate_arbolate_sum(data.frame(ID = comid, toID = tocomid, length = .x)),
                .names = "accm_{.col}")) %>%
  ungroup()

## Commenting out because this dataset is not used
# write.csv(scaling_analysis_dat,paste(local_data,"scaling_analysis_dat_cumulative.csv", sep = "/"),
#           row.names = FALSE)


# Cumulative entropy

# The calculation of cumulative entropy differs from the calculations above. In this
# case entropy along a branched network accumulates following the compositional law
# (Jaynes, 1957). According to the compositional law, the entropy of a branched system
# is the sum of the uncertainties experienced by an observer at each branching point 
# (or node) weighted by the frequency at which such a branching node would be encountered. 

# In the case of a fluvial network, the cumulative uncertainties would be experienced in 
# proportion of the area draining to that point. If it is a pretty small area, that 
# uncertainty will be experienced by the observer only until they reach the headwaters. 
# Conversely, it is more likely for the observer to encounter uncertainties representing
# larger areas of a landscape. 

# In this case we need to make some intermediate calculations to calculate the accumulated
# uncertainty at each branching node. All entropies need to be weighted by the fraction of 
# land ocupied by the nested catchment with respect to the area closing at the most inmmediate
# downstream node. In that regard, we need to identify those nodes' (tocomid) area values, 
# and match them with their most inmmediate upstream node (comid).

# We also need to add all the entropies corresponding to headwater areas, which are experienced
# when the observer goes beyond the first junction in the network. 


downstream_area <- function(input_df) {
  result_df <- input_df %>%
    group_by(basin) %>%
    mutate(
      max_area = max(wshd_area_km2),
      down_wshd_area_km2 = case_when(
        tocomid == 0 ~ wshd_area_km2,
        wshd_area_km2 == max_area ~ wshd_area_km2,
        TRUE ~ lag(wshd_area_km2)
      )
    ) %>%
    select(comid, tocomid, basin, down_wshd_area_km2, -max_area) %>%
    ungroup()
  
  return(result_df)
}

# Usage
result <- downstream_area(scaling_analysis_dat) 


# Calculating cumulative entropy
scaling_analysis_accm_dat <- scaling_analysis_dat %>% 
  merge(.,
        result %>% 
          select(comid,
                 down_wshd_area_km2),
        by = "comid",
        all.x = TRUE) %>% 
  mutate(weight = wshd_area_km2/down_wshd_area_km2,
         h_hw = if_else(stream_order == 1, ht, 0),
         h_nw = weight*ht,
         h_tot = h_hw + h_nw)

# Calculating entropy and simpson's D with a 3 classes space
scaling_analysis_accm_dat <-  scaling_analysis_accm_dat %>% 
  rowwise() %>% 
  mutate(forest_3scp = forest_scp,
         shrub_3scp = shrub_scp + grass_scp +barren_scp + water_scp,
         human_3scp = human_scp,
         ht_3 = entropy(c(forest_3scp,
                        shrub_3scp,
                        human_3scp),
                        unit = "log2"),
         hmax_3 = log(3,2),
         hrel_3 = ht_3/hmax_3,
         simpson_d3 = 1 - ((forest_3scp/100)^2 + (shrub_3scp/100)^2 + (human_3scp/100)^2),
         forest_3_area_km2 = forest_3scp * wshd_area_km2,
         shrub_3_area_km2 = shrub_3scp * wshd_area_km2,
         human_3_area_km2 = human_3scp * wshd_area_km2 + runif(length(human_3scp), min = 0, max = 0.001)) %>%
  ungroup() %>% 
  mutate(accm_hzt_cat = factor(Hmisc::cut2(accm_water_exchng_kg_d/wshd_area_km2, g = 10),labels = qlabel),
         forest_area_cat = factor(Hmisc::cut2(forest_3_area_km2, g = 10),labels = qlabel),
         shrub_area_cat = factor(Hmisc::cut2(shrub_3_area_km2, g = 10),labels = qlabel),
         human_area_cat = factor(Hmisc::cut2(human_3_area_km2, g = 10),labels = qlabel),
         hr3_cat = factor(Hmisc::cut2(hrel_3, g = 10),labels = qlabel),
         smp3_cat = factor(Hmisc::cut2(simpson_d3, g = 10),labels = qlabel),
         frs3_cat = factor(Hmisc::cut2(forest_3scp, g = 10),labels = qlabel))

## This is commented out because the bootstrapping process will always slightly change the dataset used 
# write.csv(scaling_analysis_accm_dat,paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
#             row.names = FALSE)


 