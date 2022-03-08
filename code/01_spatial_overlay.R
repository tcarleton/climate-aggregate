## Sara Orofino
## January 31, 2022
## Spatial Overlap: Pipeline Step 01


#' Function to find spatial overlap between a raster and a set of polygons
#' 
#' @param data_source the source of climate data (default is era5)
#' @param input_polygons a simple features polygon or multipolygon object  
#' @param polygon_id the name of a column in the sf object representing a unique identifier for each polygon  
#' 
#' @return a data.table of geoweights (area weighted raster/polygon overlap)


calc_geoweights <- function(data_source = 'era5', input_polygons, polygon_id){
  
  ## Setup 
  ## -----------------------------------------------
  require(pacman)
  pacman::p_load(ncdf4, data.table, raster, exactextractr, tidyverse, sf, here, crayon)
  
  ## Pull a demo climate data raster based on the input data source
  ## -----------------------------------------------

  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # Error if the data source is one that is not currently supported 
  if(!data_source_norm %in% c('era5')){
    stop(crayon::red('Unsupported climate data source. Supported formats are: era5'))
  }
  

  # Call the demo data (small example raster)
  ncpath  <- file.path(here::here(), 'data', 'demo')
  ncname  <- paste(data_source_norm, 'demo', sep="_")
  nc_file <- paste0(ncpath, '/', ncname,'.nc')
  
  
  # Create raster
  clim_raster <- raster(nc_file)
  
  ## Raster cell area 
  ## -----------------------------------------------
  
  clim_area_raster <- area(clim_raster)
  
  ## Raster/polygon alignment 
  ## -----------------------------------------------
  
  message(crayon::yellow('Checking for raster/polygon alignment'))
  
  poly_xmin <- extent(input_polygons)@xmin
  poly_xmax <- extent(input_polygons)@xmax
  rast_xmin <- extent(clim_area_raster)@xmin
  rast_xmax <- extent(clim_area_raster)@xmax
  
  # Rotate raster if initial longitudes don't align 
  if(!dplyr::near(poly_xmax, rast_xmax, tol=1.01)) {

    message(crayon::yellow('Adjusting raster longitude from',
                            round(rast_xmin,0), '-', round(rast_xmax,0),
                           'to', round(poly_xmin,0), '-', round(poly_xmax,0)))

    clim_area_raster <- raster::rotate(clim_area_raster)

  }
  
  # Check longitude ranges match (with a tolerance of 1 in case lon +- 179 vs. +-180)
  poly_range <- c(extent(input_polygons)@xmin, extent(input_polygons)@xmax)
  rast_range <- c(extent(clim_area_raster)@xmin, extent(clim_area_raster)@xmax)
  
  if(dplyr::near(poly_range[1], rast_range[1], tol=1.01) & dplyr::near(poly_range[2], rast_range[2], tol=1.01)){
    
    message(crayon::green('Longitude ranges match'))
    
  } else {
    
    stop(crayon::red('Raster and polygon longitude ranges do not match'))
    
  }
  
  # Match raster and polygon crs 
  crs_raster <- crs(clim_area_raster)
  polygons_reproj <- input_polygons %>% 
    st_transform(crs = crs_raster)
  
  ## Geoweights (using data.table) 
  ## -----------------------------------------------
  message(crayon::yellow('Extracting raster polygon overlap'))
    
  geoweights <- rbindlist(exactextractr::exact_extract(clim_area_raster, polygons_reproj, progress = T, include_cell = T, include_xy = T), idcol = "poly_id")
  geoweights[, ':=' (poly_id = polygons_reproj[[polygon_id]][poly_id], cell_area_km2 = value)] # Add the unique id for each polygon based on the input col name
  geoweights <- geoweights[, .(gridNumber = cell, x, y, poly_id, w_geo = coverage_fraction * cell_area_km2)] # Geoweight = area km2 * coverage fraction 
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
  
  ## Save outputs 
  ## -----------------------------------------------
  
  # Check if there is already a general geoweights folder
  if(!dir.exists(here::here("data", "int", "geoweights"))){

    # If no - create it
    message(crayon::yellow('Creating data/int/geoweights/'))
    dir.create(here::here("data", "int", "geoweights"), recursive=T)

  }
  
  # File save name
  polygon_input_name <- deparse(substitute(input_polygons))
  save_name <- paste0(paste(polygon_input_name, data_source_norm, sep="_"), ".csv")
  save_path <- file.path(here::here(), "data", "int", "geoweights")
  
  # Save message
  message(crayon::yellow('Saving', save_name, 'to', save_path))
  
  # Save geoweights 
  fwrite(geoweights[, ':=' (gridNumber = NULL, w_sum = NULL)], file = file.path(save_path, save_name))


  ## Return weights 
  ## -----------------------------------------------
  return(geoweights)
  
}

## Test function 
us_counties <- tigris::counties() #Input polygons for testing
test_weights <- calc_geoweights(data_source = 'ERA5', input_polygons=us_counties, polygon_id='GEOID')

