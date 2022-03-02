## Sara Orofino
## February 22, 2022
## Merge Climate Rasters: Pipeline Step 02

## Inputs so far: data_source, climate_var, year (one yr, multiple yrs?), input_polygons_name, nonlinearity transformation  

  ## Setup 
  ## -----------------------------------------------
  require(pacman)
  pacman::p_load(ncdf4, data.table, raster, lazyraster, tidyverse, here, crayon)

  
  ## Load saved geoweights 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # File name to call the weights
  polygon_name <- deparse(substitute(input_polygons_name))
  weights_file <- paste0(paste(polygon_name, data_source_norm, sep="_"), ".csv")
  
  # Data.table of geoweights 
  weights <- fread(file.path(here::here(), "data", "int", "geoweights", weights_file))
  
  # Extent of geoweights
  min_x <- min(weights$x)
  max_x <- max(weights$x)
  min_y <- min(weights$y)
  max_y <- max(weights$y)
  
  weights_ext <- raster::extent(min_x, max_x, min_y, max_y)
  
  ## Lazy load climate data 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # Check if the climate variable is one we have data for 
  if(!climate_var %in% c('prcp', 'temp', 'uv')){
    stop(crayon::red('No ERA5 data available. Supported variables are: prcp, temp, or uv'))
  }
  
  # Read in climate rasters based on weights extent
  ncpath  <- file.path(here::here(), 'data/raw', climate_var)
  ncname  <- paste(data_source_norm, climate_var, year, sep="_")
  nc_file <- paste0(ncpath, ncname,'.nc')
  
  clim_lazy <- lazyraster(nc_file)
  clim_raster <- raster::crop(clim_lazy, weights_ext)
  
  # Convert climate raster to climate data.table
  clim_dt <- data.table(gridNumber = 1:length(clim_raster), value = getValues(clim_raster))
  
  ### For a single year: 
  
  ## Nonlinearities (by grid cell and day)
  ## -----------------------------------------------
  
  
  

  
  
  
  ## Merge weights with climate raster 
  ## -----------------------------------------------
  
  # Set key column in the climate data table
  setkey(clim_dt, gridNumber)
  
  # Keyed merge on the gridNumber column 
  merged_dt <- clim_dt[weights, allow.cartesian = T] #4 cols: gridNumber, value, poly_id, and w_geo
  
 
  
  ## Multiply weights x climate value; aggregate by polygon  
  ## -----------------------------------------------
  
  merged_dt[, weighted_value := value * w_geo]
  sum_by_poly <- merged_dt[, sum(weighted_value), by = poly_id]
  
  ## Agg by date and polygons example
  # agg <- dt[, lapply(.SD, weighted.mean, w = w, na.rm = T), 
  #           by = .(fips3059, dateNum), 
  #           .SDcols = agg_vars]
  
  ## Save outputs
  ## -----------------------------------------------
  
  # Add a more informative column name for the climate parameter
  clim_var    <- names(clim_raster) %>% tolower(.)
  setnames(sum_by_poly, old = "V1", new = clim_var)
  

    
  }
