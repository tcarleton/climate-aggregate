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

## years
years <- c(2009:2020)

## prcp files
prcp_vec <- vector()

for(i in 1:length(years)) {
  
  temp_name <- paste0('era5_prcp_', years[i], '.nc')
  
  prcp_vec[i] <- temp_name

}

## temp files
temp_vec <- vector()

for(i in 1:length(years)) {
  
  temp_name <- paste0('era5_temp_', years[i], '.nc')
  
  temp_vec[i] <- temp_name
  
}
  
  
## Step 1: filter weights for polygon extent
## -----------------------------------------------------

polygon_extent <- extent(input_polygons)

sec_weight_filt <- dplyr::filter(sec_weight, 
                            x >= round(polygon_extent@xmin) - 1, 
                            x <= round(polygon_extent@xmax) + 1, 
                            y >= round(polygon_extent@ymin) - 1,
                            y <= round(polygon_extent@ymax) + 1)

## Step 2: Overlay administrative regions onto your data's grid
## ------------------------------------------------------------

polygon_weights <- overlay_weights(polygons = input_polygons,
                                   polygon_id_col = polygon_id,
                                   grid = era5_grid,
                                   secondary_weights = sec_weight_filt)

## Step 3: Aggregation (polynomial)
## ------------------------------------------------------------

## for cropping .nc files
min_x <- min(polygon_weights$x) - 0.5
max_x <- max(polygon_weights$x) + 0.5
min_y <- min(polygon_weights$y) - 0.5
max_y <- max(polygon_weights$y) + 0.5

weights_ext <- raster::extent(min_x, max_x, min_y, max_y)



## temperature

temp_list <- list()

for(i in 1:length(years)) {
  
  # Climate data file paths
  ncpath  <- file.path(input_dir, 'data/raw/temp')
  nc_file <- paste0(ncpath, '/', temp_vec[i])
  
  # Immediately crop to weights extent 
  clim_raster_tmp <- raster::crop(raster::stack(nc_file), weights_ext)
  
  temp_out <- staggregate_polynomial(clim_raster_tmp,
                                     polygon_weights,
                                     daily_agg = 'average',
                                     time_agg = 'month',
                                     degree = 5)
  
  temp_list[i] <- temp_out
  
}



