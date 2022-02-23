## Sara Orofino
## February 22, 2022
## Merge Climate Rasters: Pipeline Step 02

## Inputs so far: data_source, climate_var, year (one yr, multiple yrs?), something related to weights - weights_name or weights_polygons 

  ## Setup 
  ## -----------------------------------------------
  require(pacman)
  pacman::p_load(ncdf4, data.table, raster, tidyverse, here, crayon)

  ## Pull climate data 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # Check if the climate variable is one we have data for 
  if(!climate_var %in% c('prcp', 'temp', 'uv')){
    stop(crayon::red('No ERA5 data available. Supported variables are: prcp, temp, or uv'))
  }
  
  # Read in climate rasters
  ncpath  <- file.path(here::here(), 'data/raw', climate_var)
  ncname  <- paste(data_source_norm, climate_var, year, sep="_")
  nc_file <- paste0(ncpath, ncname,'.nc')
  
  clim_raster <- raster(nc_file)
  
  # Convert climate raster to climate data.table
  clim_dt <- data.table(gridNumber = 1:length(clim_raster), value = getValues(clim_raster))
  
  ## Pull saved geoweights 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # File name to call the weights
  weights_file <- paste0(paste(input_polygons, data_source_norm, sep="_"), ".csv")
  
  # Data.table of geoweights 
  weights <- fread(file.path(here::here(), "data", "int", "geoweights", weights_file), as.data.table = T)
  
  ## Merge weights with climate raster 
  ## -----------------------------------------------
  
  # Set key column in the climate data table
  setkey(clim_dt, gridNumber)
  
  # Keyed merge on the gridNumber column 
  merged_dt <- clim_dt[weights, allow.cartesian = T] #4 cols: gridNumber, value, poly_id, and w_geo
  
  
  ## Multiply weights x climate values; aggregate by polygon  
  ## -----------------------------------------------
  
  merged_dt[, weighted_value := value * w_geo]
  merged_dt[, sum_value := sum(weighted_value), by = poly_id]
  
  ## Save outputs
  ## -----------------------------------------------
  
  # Add a more informative column name for the climate parameter
  clim_var    <- names(clim_raster) %>% tolower(.)
  setnames(merged_dt, old = "sum_value", new = clim_var)
  
  # Check if there is already an int/? folder
  if(!dir.exists(here::here("data", "int"))){
    
    # If no - create it
    message(crayon::yellow('Creating data/int/'))
    dir.create(here::here("data", "int"))
    
  }
