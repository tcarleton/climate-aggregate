## Sara Orofino
## January 31, 2022
## Spatial Overlap: Pipeline Step 01


#' Function to find spatial overlap between a raster and a set of polygons
#' 
#' @param climate_param the climate parameter abbreviation 
#' @param year the year
#' @param input_polygons a simple features polygon or multipolygon object  
#' @param polygon_id the name of a column in the sf object representing a unique identifier for each polygon  
#' 
#' @return a data.table of geoweights (area weighted raster/polygon overlap)


calc_geoweights <- function(climate_param, year, input_polygons, polygon_id){
  
  ## Setup 
  ## -----------------------------------------------
  require(pacman)
  pacman::p_load(ncdf4, data.table, raster, exactextractr, tidyverse, sf, here, crayon)
  
  ## Pull climate data  
  ## -----------------------------------------------

  # Check with Anna on the file structure to call the raw data 
  # ncpath  <- file.path(here::here(), 'data/raw', climate_param)
  # ncname  <- paste('/era5', climate_param, year, sep="_")
  # nc_file <- paste0(ncpath, ncname, '.nc')
  
  # For now just use the example ncdf file from Anna saved to my desktop
  # Example ERA5 file
  ncpath <- "/Users/saraorofino/Desktop/"
  ncname <- "era5prcp2020_04_06"  
  nc_file <- paste0(ncpath, ncname, ".nc")
  
  
  clim_raster <- raster(nc_file)
  
  ## Raster/polygon alignment 
  ## -----------------------------------------------
  
  message(crayon::yellow('Checking for raster/polygon alignment'))
  
  poly_xmin <- extent(input_polygons)@xmin
  poly_xmax <- extent(input_polygons)@xmax
  rast_xmin <- extent(clim_raster)@xmin
  rast_xmax <- extent(clim_raster)@xmax
  
  # Rotate raster if initial longitudes don't align 
  if(!dplyr::near(poly_xmax, rast_xmax, tol=1.01)) {

    message(crayon::yellow('Adjusting raster longitude from',
                            round(rast_xmin,0), '-', round(rast_xmax,0),
                           'to', round(poly_xmin,0), '-', round(poly_xmax,0)))

    clim_raster <- raster::rotate(clim_raster)

  }
  
  # Check longitude ranges match (with a tolerance of 1 in case lon +- 179 vs. +-180)
  poly_range <- c(extent(input_polygons)@xmin, extent(input_polygons)@xmax)
  rast_range <- c(extent(clim_raster)@xmin, extent(clim_raster)@xmax)
  
  if(dplyr::near(poly_range[1], rast_range[1], tol=1.01) & dplyr::near(poly_range[2], rast_range[2], tol=1.01)){
    
    message(crayon::green('Longitude ranges match'))
    
  } else {
    
    stop(crayon::red('Raster and polygon longitude ranges do not match'))
    
  }
  
  # Match raster and polygon crs 
  crs_raster <- crs(clim_raster)
  polygons_reproj <- input_polygons %>% 
    st_transform(crs = crs_raster)
  
  ## Geoweights (using data.table) 
  ## -----------------------------------------------
  message(crayon::yellow('Extracting raster polygon overlap'))
    
  geoweights <- rbindlist(exactextractr::exact_extract(clim_raster, polygons_reproj, progress = T, include_cell = T), idcol = "poly_id")
  geoweights[, poly_id := polygons_reproj[[polygon_id]][poly_id]] # Add the unique id for each polygon based on the input col name
  geoweights <- geoweights[, .(gridNumber = cell, poly_id, w_geo = coverage_fraction, value = NULL)]
  geoweights[, w_geo := w_geo / sum(w_geo), by = poly_id] # Normalize by polygon

  
  message(crayon::yellow('Checking sum of weights within polygons'))
  check_weights <- geoweights[, w_sum := sum(w_geo), by=poly_id]
  
  # Check that each polygon sums to 1, if not error w/polygon id 
  for(i in nrow(check_weights)){
    
    if(check_weights$w_sum[i] != 1){
      
      stop(crayon::red('Area weights for polygon', check_weights$poly_id, 'do not sum to 1'))
      
    }
    
  }
  # If it doesn't error out then all weight sums = 1
  message(crayon::green('All weights sum to 1'))
  
  return(geoweights)
  
}

## Test function 
us_counties <- tigris::counties() #Input polygons for testing
test_weights <- calc_geoweights(climate_param = 'prcp', year=2020, input_polygons=us_counties, polygon_id='GEOID')
