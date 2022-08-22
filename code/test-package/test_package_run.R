## Tracey Mangin
## August 22, 2022
## Test climate package

# ## install package
# devtools::install_github("tcarleton/stagg")

## attach libraries necessary for sourcing functions/setup
library(stagg)
library(tidyverse)
library(sf)
library(here)

## set paths
# root_dir <- file.path("/home/traceymangin/climate-aggregate")
input_dir <- file.path("/home/tcarleton/Climate")
save_dir <- file.path("/home/traceymangin")

## set inputs for run
## ----------------------------------------------------------------

## secondardy weights
sec_weight <- cropland_world_2003_era5 ## cropland_world_2003_era5 = crops, pop_world_2015_era5 = pop

## polygon
## cluster
# input_polygons <- read_sf(file.path(input_dir, "data", "shapefiles", "NZL", "gadm36_NZL_1.shp"))

## local
input_polygons <- read_sf('/Volumes/GoogleDrive/Shared Drives/emlab/projects/current-projects/climate-data-pipeline/shapefiles/NZL/gadm36_NZL_1.shp')

## polygon id
polygon_id <- 'NAME_1'

## Step 1: filter weights for polygon extent
## -----------------------------------------------------

polygon_extent <- extent(input_polygons)

sec_weight_filt <- dplyr::filter(sec_weight, 
                            x >= round(polygon_extent@xmin) - 1, 
                            x <= round(polygon_extent@xmax) + 1, 
                            y >= round(polygon_extent@ymin) - 1,
                            y <= round(polygon_extent@ymax) + 1)

## Step 1: Overlay administrative regions onto your data's grid
## ------------------------------------------------------------

polygon_weights <- overlay_weights(polygons = input_polygons,
                                   polygon_id_col = polygon_id,
                                   grid = era5_grid,
                                   secondary_weights = sec_weight_filt)

## check these outputs with those from non-package run
nz_weights <- fread('/Volumes/GoogleDrive/Shared Drives/emlab/projects/current-projects/climate-data-pipeline/agg-outputs/weights/nzl_region_era5_area_crop_weights.csv')

nz_weights <- nz_weights %>%
  pivot_longer(names_to = "weight", values_to = "orig_value", w_area:weight)

new_weights <- polygon_weights %>%
  pivot_longer(names_to = "weight", values_to = "new_value", w_area:weight) %>%
  left_join(nz_weights) %>%
  mutate(diff = new_value - orig_value)

