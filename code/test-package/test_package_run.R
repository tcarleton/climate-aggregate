## Tracey Mangin
## August 22, 2022
## Test climate package

# ## install package
# devtools::install_github("tcarleton/stagg")

## attach libraries necessary for sourcing functions/setup
library(stagg)
library(tidyverse)
library(sf)
library(raster)
library(data.table)
library(rgdal)
library(parallel)

## set paths
input_dir <- file.path("/home/tcarleton/Climate")
save_dir <- file.path("/home/traceymangin")

## for saving outputs
## ---------------------------------------------------------------

## define paths and create folders for saving weights 
weights_save_path <- file.path(save_dir, 'climate-out', 'weights')

if(!dir.exists(weights_save_path)){
  
  # If no - create it
  message(crayon::yellow('Creating climate-out/weights/'))
  dir.create(file.path(save_dir, "climate-out", "weights"), recursive=T)
  
}

## define paths and create folders for saving outputs 
output_save_path <- file.path(save_dir, "climate-out", "output")  

# Check if there is already an output folder
if(!dir.exists(output_save_path)){
  
  # If no - create it
  message(crayon::yellow('Creating climate-out/output'))
  dir.create(file.path(save_dir, "climate-out", "output"), recursive=T)
  
}

## for naming files
data_source <- 'era5'
sec_weights <- TRUE
weight_type <- 'area_crop'
country_name <- 'chile'

## set inputs for run
## ----------------------------------------------------------------

## secondardy weights
sec_weight <- cropland_world_2003_era5 ## cropland_world_2003_era5 = crops, pop_world_2015_era5 = pop

## polygon
input_polygons <- read_sf(file.path(input_dir, "data", "shapefiles", "CHL", "cl_comunas_geo_mortality_adm2.shp"))

## polygon id
polygon_id <- 'adm2_id'

## years
years <- c(1997:2013)
  
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

## weights save name
weights_save_name <- paste0(paste(country_name, polygon_id, data_source, weight_type, sep="-"), ".csv")

## save message
message(crayon::yellow('Saving', weights_save_name, 'to', weights_save_path))

## save file
fwrite(polygon_weights, file = file.path(weights_save_path, weights_save_name))


## Step 3: Aggregation (polynomial)
## ------------------------------------------------------------

## for cropping .nc files
min_x <- min(polygon_weights$x) - 0.5
max_x <- max(polygon_weights$x) + 0.5
min_y <- min(polygon_weights$y) - 0.5
max_y <- max(polygon_weights$y) + 0.5

weights_ext <- raster::extent(min_x, max_x, min_y, max_y)

## run staggregate_polynomial function over multiple years in parallel
## --------------------------------------------------------------------------

## function for cropping raster and running staggregate

run_stagg_year_temp <- function(year) {  
    
  # Climate data file paths
  ncpath  <- file.path(input_dir, 'data/raw/temp')
  nc_file <- paste0(ncpath, '/', 'era5_temp_', year, '.nc')
  
  # Immediately crop to weights extent 
  clim_raster_tmp <- raster::crop(raster::stack(nc_file), weights_ext)
  
  ## run stagg for temp
  temp_out <- staggregate_polynomial(clim_raster_tmp,
                                     polygon_weights,
                                     daily_agg = 'average',
                                     time_agg = 'month',
                                     degree = 5)
  
}

run_stagg_year_prcp <- function(year) {  
  
  # Climate data file paths
  ncpath  <- file.path(input_dir, 'data/raw/prcp')
  nc_file <- paste0(ncpath, '/', 'era5_prcp_', year, '.nc')
  
  # Immediately crop to weights extent 
  clim_raster_tmp <- raster::crop(raster::stack(nc_file), weights_ext)
  
  ## run stagg for prcp  
  prcp_out <- staggregate_polynomial(clim_raster_tmp,
                                     polygon_weights,
                                     daily_agg = 'sum',
                                     time_agg = 'month',
                                     degree = 3)
  
}


## set up (cores, cluster)
no_cores <- parallel::detectCores() - 1 # Calculate the number of cores. Leave one in case something else needs to be done on the same computer at the same time. 
cl <- makeCluster(no_cores, type = "FORK") # Initiate cluster. "FORK" means bring everything in your current environment with you. 

## run 
stagg_multiyear_temp <- parLapply(cl, years, run_stagg_year_temp)
stagg_multiyear_prcp <- parLapply(cl, years, run_stagg_year_prcp)

## stop cluster
stopCluster(cl)

## rbind
stagg_multiyear_temp_all <- data.table::rbindlist(stagg_multiyear_temp)
stagg_multiyear_prcp_all <- data.table::rbindlist(stagg_multiyear_prcp)

## save outputs
save_name_temp <- paste0(paste(country_name, polygon_id, data_source, weight_type,
                              years[1], years[length(years)], 'temp', sep="-"), ".csv")

save_name_prcp <- paste0(paste(country_name, polygon_id, data_source, weight_type,
                                 years[1], years[length(years)], 'prcp', sep="-"), ".csv")
  
## save message
message(crayon::yellow('Saving', save_name_temp, 'to', output_save_path))

## save output
fwrite(stagg_multiyear_temp_all, file = file.path(output_save_path, save_name_temp))
  
## save message
message(crayon::yellow('Saving', save_name_prcp, 'to', output_save_path))

## save output
fwrite(stagg_multiyear_prcp_all, file = file.path(output_save_path, save_name_prcp))

## fin
message(crayon::green('fin'))










