## Tracey Mangin
## August 24, 2022
## compare outputs

## libraries
library(tidyverse)
library(data.table)
library(rebus)

## paths
main_path <- '/Volumes/GoogleDrive/Shared Drives/emlab/projects/current-projects/climate-data-pipeline/agg-outputs/'

## stagg outputs
stagg_path <- paste0(main_path, 'stagg-out/')

## read in files
weights_df <- fread(paste0(main_path, 'weights/chl_area_era5_area_crop_weights.csv'))
weights_stagg_df <- fread(paste0(stagg_path, '/weights/chile-adm2_id-era5-area_crop.csv'))

ch_temp_df <- fread(paste0(main_path, 'output/chl_area_era5_temp_average_1997_2013_polynomial_5_area_crop_weights.csv'))
ch_prcp_df <- fread(paste0(main_path, 'output/chl_area_era5_prcp_sum_1997_2013_polynomial_3_area_crop_weights.csv'))

ch_temp_stagg_df <- fread(paste0(stagg_path, 'outputs/chile-adm2_id-era5-area_crop-1997-2013-temp.csv'))
ch_prcp_stagg_df <- fread(paste0(stagg_path, 'outputs/chile-adm2_id-era5-area_crop-1997-2013-prcp.csv'))

## NZL: read in files
nz_weights_df <- fread(paste0(main_path, 'weights/nzl_region_era5_area_crop_weights.csv'))
nz_weights_stagg_df <- fread(paste0(stagg_path, '/weights/new_zealand-NAME_1-era5-area_crop.csv'))

nz_temp_df <- fread(paste0(main_path, 'output/nzl_region_era5_temp_average_2009_2020_polynomial_5_area_crop_weights.csv'))
nz_prcp_df <- fread(paste0(main_path, 'output/nzl_region_era5_prcp_sum_2009_2020_polynomial_3_area_crop_weights.csv'))

nz_temp_stagg_df <- fread(paste0(stagg_path, 'outputs/new_zealand-NAME_1-era5-area_crop-2009-2020-temp.csv'))
nz_prcp_stagg_df <- fread(paste0(stagg_path, 'outputs/new_zealand-NAME_1-era5-area_crop-2009-2020-prcp.csv'))



## compare weights
## --------------------------------------------------

weights0 <- weights_df %>%
  pivot_longer(names_to = 'type', values_to = 'value0', w_area:weight)

weight_comp <- weights_stagg_df %>%
  pivot_longer(names_to = 'type', values_to = 'value_new', w_area:weight) %>%
  full_join(weights0) %>%
  mutate(diff = value_new - value0)
## diff is zero

## compare temp
## --------------------------------------------------
temp0 <- ch_temp_df %>%
  mutate(month = as.integer(str_remove(month, 'month_'))) %>%
  pivot_longer(names_to = 'order', values_to = 'value0', order_1:order_5)

temp_comp <- ch_temp_stagg_df %>%
  pivot_longer(names_to = 'order', values_to = 'value_new', order_1:order_5) %>%
  full_join(temp0) %>%
  mutate(diff = value_new - value0)
## conversion...

## compare prcp
## --------------------------------------------------
prcp0 <- ch_prcp_df %>%
  mutate(month = as.integer(str_remove(month, 'month_'))) %>%
  pivot_longer(names_to = 'order', values_to = 'value0', order_1:order_3)

prcp_comp <- ch_prcp_stagg_df %>%
  pivot_longer(names_to = 'order', values_to = 'value_new', order_1:order_3) %>%
  full_join(prcp0) %>%
  mutate(diff = value_new - value0)

## ----------------------------------------------------
## NZL
## ----------------------------------------------------

## compare weights
## --------------------------------------------------

nz_weights0 <- nz_weights_df %>%
  pivot_longer(names_to = 'type', values_to = 'value0', w_area:weight)

nz_weight_comp <- nz_weights_stagg_df %>%
  pivot_longer(names_to = 'type', values_to = 'value_new', w_area:weight) %>%
  full_join(nz_weights0) %>%
  mutate(diff = value_new - value0)
## diff is zero

## compare temp
## --------------------------------------------------
nz_temp0 <- nz_temp_df %>%
  mutate(month = as.integer(str_remove(month, 'month_'))) %>%
  pivot_longer(names_to = 'order', values_to = 'value0', order_1:order_5)

nz_temp_comp <- nz_temp_stagg_df %>%
  pivot_longer(names_to = 'order', values_to = 'value_new', order_1:order_5) %>%
  full_join(nz_temp0) %>%
  mutate(diff = value_new - value0)
## conversion...

## compare prcp
## --------------------------------------------------
nz_prcp0 <- nz_prcp_df %>%
  mutate(month = as.integer(str_remove(month, 'month_'))) %>%
  pivot_longer(names_to = 'order', values_to = 'value0', order_1:order_3)

nz_prcp_comp <- nz_prcp_stagg_df %>%
  pivot_longer(names_to = 'order', values_to = 'value_new', order_1:order_3) %>%
  full_join(nz_prcp0) %>%
  mutate(diff = value_new - value0)




