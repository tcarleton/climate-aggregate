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

source(here::here('code', 'file_paths.R')) # define the root directory for where data is stored

calc_geoweights <- function(data_source = 'era5',  input_polygons, polygon_id, weights){
  
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
  # ncpath  <- file.path(file.path(input_dir, "data", "int", "weights"), 'data', 'demo')
  ncpath  <- file.path(input_dir, 'data', 'demo')
  ncname  <- paste(data_source_norm, 'demo', sep="_")
  nc_file <- paste0(ncpath, '/', ncname,'.nc')
  
  
  # Create raster
  clim_raster <- raster(nc_file) # only reads the first band
  
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
  
  # Shift polygons if initial longitudes don't align 
  if(!dplyr::near(poly_xmax, rast_xmax, tol=1.01)) {

    message(crayon::yellow('Adjusting raster longitude from',
                            round(rast_xmin,0), '-', round(rast_xmax,0),
                           'to -180 to 180'))

    clim_area_raster <- raster::rotate(clim_area_raster)

  }
  
  # Match raster and polygon crs 
  crs_raster <- crs(clim_area_raster)
  polygons_reproj <- input_polygons %>% 
    st_transform(crs = crs_raster)
  
  ## Raster / Polygon overlap (using data.table) 
  ## -----------------------------------------------
  message(crayon::yellow('Extracting raster polygon overlap'))
    
  overlap <- rbindlist(exactextractr::exact_extract(clim_area_raster, polygons_reproj, progress = T, include_xy = T), idcol = "poly_id")
  overlap[, ':=' (poly_id = polygons_reproj[[polygon_id]][poly_id], cell_area_km2 = value)] # Add the unique id for each polygon based on the input col name
  
  
  ## Calculate weights  
  ## -----------------------------------------------
  
  # Calculate area weight per grid cell 
  area_weight <- overlap[, .(x, y, poly_id, w_area = coverage_fraction * cell_area_km2)] # area weight = area km2 * coverage fraction 
  
  # IF weights = TRUE, merge secondary weights with area weights
  if(weights){
    
    # File name for secondary weights 
    weights_file <- paste0(weights_name, '.csv')
    
    # Data.table of secondary weights 
    weights_dt <- fread(file.path(input_dir, "data", "int", "rasterweights", weights_file))
    
    # Min/Max of secondary weights
    weights_xmin <- min(weights_dt$x)
    weights_xmax <- max(weights_dt$x)
    weights_ymin <- min(weights_dt$y)
    weights_ymax <- max(weights_dt$y)
    
    # Updated Min/Max of raster (post first shift)
    rast_xmin <- extent(clim_area_raster)@xmin
    rast_xmax <- extent(clim_area_raster)@xmax
    
    # If weights don't match raster convert them 
    if(!dplyr::near(weights_xmax, rast_xmax, tol=1.01)) {
      
      message(crayon::yellow('Adjusting secondary weights longitude from',
                             round(weights_xmin,0), '-', round(weights_xmax,0),
                             'to', round(rast_xmin,0), '-', round(rast_xmax,0)))
      
      weights_dt[, x := ifelse(x >= 180, x - 360, x)]
      
    } else {
      message(crayon::green('No need to adjust secondary weights'))}
    
    # Set key column in the merged dt table
    keycols = c("x", "y")
    setkeyv(area_weight, keycols)
    
    # Merge with secondary weights, NA for missing values
    w_merged <- merge(area_weight, weights_dt,
                      by = c('x', 'y'),
                      all.x = T)
    
    # Weight in pixel = w_area * weight
    w_merged[, weight := weight * w_area]
    
    # Create column that determines if entire polygon has a weight == 0
    zero_polys <- data.frame(w_merged) %>%
      group_by(poly_id) %>%
      summarise(sum_weight = sum(weight)) %>%
      ungroup() %>%
      filter(sum_weight == 0) %>%
      dplyr::select(poly_id) %>%
      distinct()
    
    if(nrow(zero_polys > 0)) {
      
      warning(crayon::red("Warning: weight = 0 for all pixels in some of your polygons; area-weights will be returned"))
      
    }
    
    
    # List any polygons with NA values in 1 or more grid cells
    na_polys <- data.frame(w_merged) %>% 
      dplyr::filter(is.na(weight)) %>% 
      dplyr::select(poly_id) %>% 
      distinct()
    
    # Warning if there are polygons with NA weight values
    if(nrow(na_polys > 0)) {
      
      warning(crayon::red("Warning: some of the secondary weights are NA, meaning weights cannot be calculated and area-weights will be returned"))
      
    }
    
    # Update the weight to equal w_area for all grid cells in na_polys 
    w_merged[, weight := data.table::fifelse(poly_id %in% c(na_polys$poly_id, zero_polys$poly_id), w_area, weight)]
    
    
    
  }

  # Normalize weights by polygon
  if(weights){
    
    w_norm <- w_merged[, ':=' (w_area = w_area / sum(w_area), weight = weight / sum(weight)), by = poly_id]

    
  } else {
    w_norm <- area_weight[, w_area := w_area / sum(w_area), by = poly_id]
  }

  
  message(crayon::yellow('Checking sum of weights within polygons'))
  if(weights){
    
    check_weights <- w_norm[, lapply(.SD, sum), by = poly_id,
                            .SDcols = c('w_area', 'weight')]

  } else{
    check_weights <- w_norm[, w_sum := sum(w_area), by=poly_id]
  }
  
  # Check that polygon weights sum to 1
  if (weights){
    for(i in nrow(check_weights)){
      
      if(!dplyr::near(check_weights$w_area[i], 1, tol=0.001) | !dplyr::near(check_weights$weight[i], 1, tol=0.001)){
        
        stop(crayon::red('Weights for polygon', check_weights$poly_id, 'do not sum to 1'))
        
      }
      
    }
  } else {
    
    for(i in nrow(check_weights)){
      
      if(!dplyr::near(check_weights$w_sum[i], 1, tol=0.001)){
        
        stop(crayon::red('Area weights for polygon', check_weights$poly_id, 'do not sum to 1'))
        
      }
      
    }
  }
  
  # If it doesn't error out then all weight sums = 1
  message(crayon::green('All weights sum to 1'))
  
  ## Convert back to 0-360 
  ## -----------------------------------------------
  
  w_norm[, x := ifelse(x < 0, x + 360, x)]
  
  ## Save outputs 
  ## -----------------------------------------------
  
  # Check if there is already a general weights folder
  if(!dir.exists(file.path(save_dir, "data", "int", "weights"))){

    # If no - create it
    message(crayon::yellow('Creating data/int/weights/'))
    dir.create(file.path(save_dir, "data", "int", "weights"), recursive=T)

  }
  
  # If secondary weights are used add to save name
  # Otherwise just include other inputs
  if(weights){
    save_name <- paste0(paste(input_polygons_name, data_source_norm, "area", weights_type, "weights", sep="_"), ".csv")
  } else{
    save_name <- paste0(paste(input_polygons_name, data_source_norm, "area", "weights", sep="_"), ".csv")
  }
  
  # File save path
  save_path <- file.path(save_dir, "data", "int", "weights")
  
  # Save message
  message(crayon::yellow('Saving', save_name, 'to', save_path))
  
  # Save weights 
  if(weights){
    fwrite(w_norm, file = file.path(save_path, save_name))
  } else {
    fwrite(w_norm, file = file.path(save_path, save_name))
  }

  ## Done
  ## -----------------------------------------------
  message(crayon::green('Done'))
  
}
