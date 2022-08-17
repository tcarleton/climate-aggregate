## Tracey Mangin
## August 16, 2022
## Review NZ and ECU outputs (previous and current sp overlay)

library(tidyverse)
library(data.table)
library(sf)
library(cowplot)

## add paths
main_path <- '/Volumes/GoogleDrive/Shared Drives/emlab/projects/current-projects/climate-data-pipeline/'

save_path <- paste0(main_path, 'agg-outputs/diagnostic-figs/comparison/')

output_path <- paste0(main_path, 'agg-outputs/')

## files
weight_prev_file  <- 'weights/nzl_region_era5_area_crop_weights.csv'
weight_curr_file  <- 'weights/nzl_region_sp_check_era5_area_crop_weights.csv'

temp_prev_file <- 'output/nzl_region_era5_temp_average_2009_2020_polynomial_5_area_crop_weights.csv'
temp_curr_file <- 'output/nzl_region_sp_check_era5_temp_average_2009_2020_polynomial_5_area_crop_weights.csv'

prcp_prev_file <- 'output/nzl_region_era5_prcp_sum_2009_2020_polynomial_3_area_crop_weights.csv'
prcp_curr_file <- 'output/nzl_region_sp_check_era5_prcp_sum_2009_2020_polynomial_3_area_crop_weights.csv'

polygon_file <- 'shapefiles/NZL/gadm36_NZL_1.shp'


## Information for saving/filtering/plotting
## ----------------------------------------------

country_name <- "NZL"
low_temp_month <- 'month_01'
high_temp_month <- 'month_07'
low_prcp_m <- 'month_02'
high_prcp_m <- 'month_07'
plot_yrs <- seq(2009, 2020, by = 5)

## read in dfs
## -----------------------------------------------

weights_prev <- read_csv(paste0(output_path, weight_prev_file))
weights_curr <- read_csv(paste0(output_path, weight_curr_file))

temp_prev <- read_csv(paste0(output_path, temp_prev_file))
temp_curr <- read_csv(paste0(output_path, temp_curr_file))

prcp_prev <- read_csv(paste0(output_path, prcp_prev_file))
prcp_curr <- read_csv(paste0(output_path, prcp_curr_file))

nzl_poly <- read_sf(paste0(main_path, polygon_file)) %>%
  select(NAME_1, geometry) %>%
  rename(poly_id = NAME_1) %>%
  st_shift_longitude()

## polygon
## -------------------------------------------------

nzl_poly_fig <- nzl_poly %>%
  st_shift_longitude() %>%
  ggplot() + 
  geom_sf(aes(fill = poly_id), lwd = 0) +
  theme(legend.position = "none")
  

## weights
## -------------------------------------------------

check_id_counts_prev <- weights_prev %>% 
  select(poly_id) %>% 
  distinct() 

nrow(nzl_poly) == nrow(check_id_counts_prev)

check_id_counts_curr <- weights_curr %>% 
  select(poly_id) %>% 
  distinct() 

nrow(nzl_poly) == nrow(check_id_counts_curr)

## -------------------------------------------------

# Check sum weights
sum_area_weights_prev <- weights_prev %>% 
  group_by(poly_id) %>% 
  mutate(sum_w_area = sum(w_area)) %>% 
  ungroup() # All = 1

unique(sum_area_weights_prev$sum_w_area)

sum_area_weights_curr<- weights_curr %>% 
  group_by(poly_id) %>% 
  mutate(sum_w_area = sum(w_area)) %>% 
  ungroup() # All = 1

unique(sum_area_weights_curr$sum_w_area)

## maps

map_crop_weights_p <- 
  ggplot() + 
  geom_tile(data = weights_prev, aes(x=x, y=y, fill=weight)) +
  labs(fill = 'Area + Crop Weight',
       x = '',
       y = '') + 
  scale_fill_viridis_c() + 
  # scale_x_continuous(limits = c(165, 184)) +
  theme_bw() +
  theme(legend.position = "bottom")

map_crop_weights_c <- 
  ggplot() + 
  geom_tile(data = weights_curr, aes(x=x, y=y, fill=weight)) +
  labs(fill = 'Area + Crop Weight',
       x = '',
       y = '') + 
  scale_fill_viridis_c() + 
  # scale_x_continuous(limits = c(165, 184)) +
  theme_bw() +
  theme(legend.position = "bottom")

## merge the two dfs together
weights_comp <- weights_curr %>%
  rename(w_area_curr = w_area,
         weight_curr = weight) %>%
  left_join(weights_prev) %>%
  mutate(diff_weight = weight_curr - weight,
         diff_w_area = w_area_curr - w_area) %>%
  filter(diff_weight != 0 | diff_w_area != 0)

## same.. checks out

## temperature
## -------------------------------------------------

temp_curr_long <- temp_curr %>%
  pivot_longer(names_to = "transformation", values_to = "curr_value", order_1:order_5)
  
temp_comp <- temp_prev %>%
  pivot_longer(names_to = "transformation", values_to = "prev_value", order_1:order_5) %>%
  left_join(temp_curr_long) %>%
  mutate(diff = curr_value - prev_value) 
  
max(temp_comp$diff)
min(temp_comp$diff)
## very small, seems good

## prcp
## -------------------------------------------------

prcp_curr_long <- prcp_curr %>%
  pivot_longer(names_to = "transformation", values_to = "curr_value", order_1:order_3)

prcp_comp <- prcp_prev %>%
  pivot_longer(names_to = "transformation", values_to = "prev_value", order_1:order_3) %>%
  left_join(prcp_curr_long) %>%
  mutate(diff = curr_value - prev_value) 

max(prcp_comp$diff)
min(prcp_comp$diff)
## very small, seems good


## ---------------------------------------------------
## check ecuador
## ---------------------------------------------------

## files
ecu_weight_prev_file  <- 'weights/ecu_province_era5_area_crop_weights.csv'
ecu_weight_curr_file  <- 'weights/ecu_check_era5_area_crop_weights.csv'

ecu_temp_prev_file <- 'output/ecu_province_era5_temp_average_1990_2018_polynomial_5_area_crop_weights.csv'
ecu_temp_curr_file <- 'output/ecu_check_era5_temp_average_1990_2018_polynomial_5_area_crop_weights.csv'

ecu_prcp_prev_file <- 'output/ecu_province_era5_prcp_sum_1990_2018_polynomial_3_area_crop_weights.csv'
ecu_prcp_curr_file <- 'output/ecu_check_era5_prcp_sum_1990_2018_polynomial_3_area_crop_weights.csv'

ecu_polygon_file <- 'shapefiles/ECU/gadm36_ECU_1.shp'

## Information for saving/filtering/plotting
## ----------------------------------------------

ecu_country_name <- "ECU"
ecu_low_temp_month <- 'month_09'
ecu_high_temp_month <- 'month_06'
ecu_low_prcp_m <- 'month_07'
ecu_high_prcp_m <- 'month_04'
ecu_plot_yrs <- seq(1990, 2018, by = 5)

## read in dfs
## -----------------------------------------------

ecu_weights_prev <- read_csv(paste0(output_path, ecu_weight_prev_file))
ecu_weights_curr <- read_csv(paste0(output_path, ecu_weight_curr_file))

ecu_temp_prev <- read_csv(paste0(output_path, ecu_temp_prev_file))
ecu_temp_curr <- read_csv(paste0(output_path, ecu_temp_curr_file))

ecu_prcp_prev <- read_csv(paste0(output_path, ecu_prcp_prev_file))
ecu_prcp_curr <- read_csv(paste0(output_path, ecu_prcp_curr_file))

ecu_poly <- read_sf(paste0(main_path, ecu_polygon_file)) %>%
  select(NAME_1, geometry) %>%
  rename(poly_id = NAME_1) 


## polygon
## -------------------------------------------------

ecu_poly_fig <- ecu_poly %>%
  ggplot() + 
  geom_sf(aes(fill = poly_id), lwd = 0) +
  theme(legend.position = "bottom") 


## weights
## -------------------------------------------------

ecu_check_id_counts_prev <- ecu_weights_prev %>%
  select(poly_id) %>%
  distinct()

nrow(ecu_poly) == nrow(ecu_check_id_counts_prev)

ecu_check_id_counts_curr <- ecu_weights_curr %>%
  select(poly_id) %>%
  distinct()

nrow(ecu_poly) == nrow(ecu_check_id_counts_curr)

## -------------------------------------------------

# Check sum weights
ecu_sum_area_weights_prev <- ecu_weights_prev %>%
  group_by(poly_id) %>%
  mutate(sum_w_area = sum(w_area)) %>%
  ungroup() # All = 1

unique(ecu_sum_area_weights_prev$sum_w_area)

ecu_sum_area_weights_curr<- ecu_weights_curr %>%
  group_by(poly_id) %>%
  mutate(sum_w_area = sum(w_area)) %>%
  ungroup() # All = 1

unique(ecu_sum_area_weights_curr$sum_w_area)

## maps

ecu_map_crop_weights_p <-
  ggplot() +
  geom_tile(data = ecu_weights_prev, aes(x=x, y=y, fill=weight)) +
  labs(fill = 'Area + Crop Weight',
       x = '',
       y = '') +
  scale_fill_viridis_c() +
  # scale_x_continuous(limits = c(165, 184)) +
  theme_bw() +
  theme(legend.position = "bottom")

ecu_map_crop_weights_c <-
  ggplot() +
  geom_tile(data = ecu_weights_curr, aes(x=x, y=y, fill=weight)) +
  labs(fill = 'Area + Crop Weight',
       x = '',
       y = '') +
  scale_fill_viridis_c() +
  # scale_x_continuous(limits = c(165, 184)) +
  theme_bw() +
  theme(legend.position = "bottom")

## merge the two dfs together
ecu_weights_comp <- ecu_weights_curr %>%
  rename(w_area_curr = w_area,
         weight_curr = weight) %>%
  left_join(ecu_weights_prev) %>%
  mutate(diff_weight = weight_curr - weight,
         diff_w_area = w_area_curr - w_area) %>%
  filter(diff_weight != 0 | diff_w_area != 0)
## differences for diff weight... driven by duplicates, ok.

## temperature
## -------------------------------------------------

ecu_temp_curr_long <- ecu_temp_curr %>%
  pivot_longer(names_to = "transformation", values_to = "curr_value", order_1:order_5)

ecu_temp_comp <- ecu_temp_prev %>%
  pivot_longer(names_to = "transformation", values_to = "prev_value", order_1:order_5) %>%
  left_join(ecu_temp_curr_long) %>%
  mutate(diff = curr_value - prev_value)

max(ecu_temp_comp$diff)
min(ecu_temp_comp$diff)
## zero

## prcp
## -------------------------------------------------

ecu_prcp_curr_long <- ecu_prcp_curr %>%
  pivot_longer(names_to = "transformation", values_to = "curr_value", order_1:order_3)

ecu_prcp_comp <- ecu_prcp_prev %>%
  pivot_longer(names_to = "transformation", values_to = "prev_value", order_1:order_3) %>%
  left_join(ecu_prcp_curr_long) %>%
  mutate(diff = curr_value - prev_value)

max(prcp_comp$diff)
min(prcp_comp$diff)
## very small, seems good


weights_dt <- fread(paste0(main_path, 'demo-data/era5_cropland_2003_full.csv'))

dupl <- weights_dt %>%
  group_by(x, y) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(id = paste(x, y, sep = "_"))

dupl2 <-  duplicated(weights_dt)




