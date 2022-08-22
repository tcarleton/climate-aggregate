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

## input polygons
input_polygons <- read_sf(file.path(input_dir, "data", "shapefiles", "NZL", "gadm36_NZL_1.shp"))

## step 1: resample a secondary weight
polygon_extent <- extent(input_polygons)
polygon_extent_vec <- c(polygon_extent@xmin, polygon_extent@xmax, polygon_extent@ymin, polygon_extent@ymax)

cropland_weights <- secondary_weights(secondary_raster = cropland_world_2003_era5,
                                      grid = era5_grid,
                                      extent = polygon_extent_vec)

