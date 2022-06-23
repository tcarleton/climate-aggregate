## Anna Boser
## March 27, 2022
## Make a general function that resamples a raster of interest to the ERA grid
## Processing step for potapov crop weights to turn into a single global map by year

#' Function that resamples a raster of interest to the ERA grid
#' 
#' @param weights_raster a raster of a continuous variable, for example cropland coverage or population
#' @param data_source the source of climate data (default is era5)
#' @param extent an optional extent to crop the weights_raster to for faster processing
#' 
#' @return a data.table of geoweights (area weighted raster/polygon overlap)

# Package will not write to any data folder, and instead put objects into environment

calc_raster_weights <- function(weights_raster, data_source, extent = "full"){
  
  ## Setup 
  ## -----------------------------------------------
  # Removed load packages lines which will be installed by the imports section in description and called using the pkg::fun() syntax. See bottom of user_run_example for full list of packages to be included in imports. 
  
  ## If an extent was included, crop it to the extent to save ram
  ## -----------------------------------------------
  if (!is.character(extent)){
    weights_raster <- raster::crop(weights_raster, extent)
  }
  

  # Create ERA raster from input raster
  clim_raster <- raster::raster(data_source) # only reads the first band
  
  ## Raster alignment: make sure clim_raster is -180 to 180 longitude
  ## -----------------------------------------------
  
  message(crayon::yellow('Checking for raster alignment'))
  
  poly_xmin <- -180
  poly_xmax <- 180
  rast_xmin <- raster::extent(clim_raster)@xmin
  rast_xmax <- raster::extent(clim_raster)@xmax
  
  # Rotate raster if initial longitudes don't align 
  if(!dplyr::near(poly_xmax, rast_xmax, tol=1.01)) {
    
    message(crayon::yellow('Adjusting raster longitude from',
                           round(rast_xmin,0), '-', round(rast_xmax,0),
                           'to', round(poly_xmin,0), '-', round(poly_xmax,0)))
    
    clim_raster <- raster::rotate(clim_raster)
    
  }
  
  # Check longitude ranges match (with a tolerance of 1 in case lon +- 179 vs. +-180)
  poly_range <- c(-180, 180)
  rast_range <- c(raster::extent(clim_raster)@xmin, raster::extent(clim_raster)@xmax)
  
  if(dplyr::near(poly_range[1], rast_range[1], tol=1.01) & dplyr::near(poly_range[2], rast_range[2], tol=1.01)){
    
    message(crayon::green('Longitude ranges match'))
    
  } else {
    
    stop(crayon::red('Raster longitude must be -180 to 180'))
    
  }
  
  ## crop the ERA raster to the polygon or at least the raster extent
  ## -----------------------------------------------
  if (!is.character(extent)){
    clim_raster <- raster::crop(clim_raster, extent)
  } else {
    clim_raster <- raster::crop(clim_raster, raster::extent(weights_raster))
  }
  
  ## Match raster crs 
  ## -----------------------------------------------
  # weights_raster <- weights_raster %>% 
  #   st_transform(crs = st_crs(clim_raster))
  # this doesn't work and as far as I can tell this isn't super necessary
  
  ## Make the values of the clim_raster resampled weights
  ## -----------------------------------------------
  resampled_raster = raster::resample(weights_raster, clim_raster, method="bilinear")
  
  
  
  ## Make a data.table of the values of the resampled raster with lat/lon
  ## -----------------------------------------------
  weight_table <- raster::as.data.frame(resampled_raster, xy=TRUE)
  colnames(weight_table) <- c("x", "y", "weight")
  weight_table <- data.table::as.data.table(weight_table)
  
  
  ## Return the weights table
  ## -----------------------------------------------
  return(weight_table)
  
}

# Test function may be included in examples or not at all in package

## Test function 
## -----------------------------------------------

read_potapov <- function(quad, year){
  require(pacman)
  pacman::p_load(here, raster)
  # data_folder = here::here("data")
  # data_folder = '/home/tcarleton/Climate/data'
  crop_path <- file.path(data_folder, "raw", "weights", "cropland")
  return(raster(file.path(crop_path, paste0("cropland_", quad, "_", year, ".tif"))))
}

# baby trial
# p <- read_potapov(quad="NW", year=2011)
# e <- extent(c(-1, 0, 9, 10))
# calc_raster_weights(data_source = 'era5',  weights_raster = p, extent = e)

# bigger test
# us_counties <- tigris::counties() #Input polygons for testing
# us_extent <- extent(us_counties)
# calc_raster_weights(data_source = 'era5',  weights_raster = p, extent = us_extent)
# p <- read_potapov(quad="NW", year=2011)
# calc_raster_weights(data_source = 'era5',  weights_raster = p, extent = "full")


# full run
library(parallel)

years <- c(2007, 2015, 2011, 2003, 2019)
quads <- c("NW", "NE", "SW", "SE")

full_potapov <- function(year){
  library(data.table)
  
  rasters <- lapply(quads, read_potapov, year = year)
  
  no_cores <- detectCores() # Calculate the number of cores. Leave one in case something else needs to be done on the same computer at the same time. 
  paste("working on year", year, "with", no_cores, "cores")
  cl <- makeCluster(no_cores, type="FORK") # Initiate cluster. "FORK" means bring everything in your current environment with you. 
  
  tables <- parLapply(cl, rasters, calc_raster_weights, data_source = 'era5', extent = "full")
  
  stopCluster(cl)
  
  full_table <- rbindlist(tables)
  # save the whole world of potapov data
  path = file.path(data_folder, "int", "rasterweights")
  fwrite(full_table, file = file.path(path, paste0("era5_cropland_", year, "_full.csv")))
  paste("done with year", year)
}

for (year in years){
  full_potapov(year)
}
#lapply(years, full_potapov)


