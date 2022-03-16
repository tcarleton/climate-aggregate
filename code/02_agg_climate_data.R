## Sara Orofino
## February 22, 2022
## Aggregate Climate Data: Pipeline Step 02

## Inputs so far: data_source, climate_var, year (one yr, multiple yrs?), input_polygons_name, nonlinearity transformation  
agg_climate_data <- function(years, data_source, climate_var, input_polygons_name, nonlinear_transformation = 'daily_mean') {
  
  ## Setup 
  ## -----------------------------------------------
  require(pacman)
  pacman::p_load(ncdf4, data.table, rgdal, raster, lazyraster, tidyverse, here, crayon)

  # Function to convert raster to data.table from https://gist.github.com/etiennebr/9515738
  as.data.table.raster <- function(x, row.names = NULL, optional = FALSE, xy=FALSE, inmem = canProcessInMemory(x, 2), ...) {
    stopifnot(require("data.table"))
    if(inmem) {
      v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
    } else {
      tr <- blockSize(x, n=2)
      l <- lapply(1:tr$n, function(i) 
        as.data.table(as.data.frame(getValues(x, 
                                              row=tr$row[i], 
                                              nrows=tr$nrows[i]), 
                                    row.names=row.names, optional=optional, xy=xy, ...)))
      v <- rbindlist(l)
    }
    coln <- names(x)
    if(xy) coln <- c("x", "y", coln)
    setnames(v, coln)
    v
  }
  setMethod('as.data.table', signature(x='Raster'), as.data.table.raster)
  
  ## Load saved geoweights 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # File name to call the weights
  polygon_name <- deparse(substitute(input_polygons_name))
  weights_file <- paste0(paste(polygon_name, data_source_norm, sep="_"), ".csv")
  
  # Data.table of geoweights 
  weights <- fread(file.path(here::here(), "data", "int", "geoweights", weights_file))
  # Convert 0-360 to match climate raster
  weights[, x := ifelse(x < 0, x + 360, x)]
  
  # Extent of geoweights 
  min_x <- min(weights$x)
  max_x <- max(weights$x)
  min_y <- min(weights$y)
  max_y <- max(weights$y)
  
  weights_ext <- raster::extent(min_x, max_x, min_y, max_y)
  
  ### Will be the start of in parallel 
  
  ## Load climate data 
  ## -----------------------------------------------
  
  # Check if the climate variable is one we have data for 
  if(!climate_var %in% c('prcp', 'temp', 'uv')){
    stop(crayon::red('No ERA5 data available. Supported variables are: prcp, temp, or uv'))
  }
  
  # Climate data file paths
  ncpath  <- file.path(here::here(), 'data/raw', climate_var)
  ncname  <- paste(data_source_norm, climate_var, year, sep="_")
  nc_file <- paste0(ncpath, '/', ncname,'.nc')
  
  # Immediately crop to weights extent 
  clim_raster <- raster::crop(raster::stack(nc_file), weights_ext)
  
  # Get layer names (dates)
  all_layers <- names(clim_raster)
  layer_names <- all_layers[seq(1, length(all_layers), 24)] # Keep every 24th layer name (1 per day)
  layer_names <- paste0('month_', substring(layer_names, 7,8)) # Extract month for each layer (1 per day)
  
  ## Nonlinearities 
  ## -----------------------------------------------
  
  
  # Simplest version - daily mean per grid cell (default right now)
  if(nonlinear_transformation == 'daily_mean'){
  
    # Average over each set of 24 layers - assuming there are 24*365 layers  
    indices<-rep(1:(nlayers(clim_raster)/24),each=24)
    clim_nonlinear <- raster::stackApply(clim_raster, indices = indices, fun=mean) 
  
  }
  
  ## Later on: add more options based on other input values

  
  # Convert the aggregated climate raster to data.table
  # Should output raster cells x/y with 365 days as column names
  clim_dt <- as.data.table.raster(clim_nonlinear, xy = TRUE)
  
  # Set column names with dates
  new_names <- c('x', 'y', layer_names)
  setnames(clim_dt, new_names)
  
  # Change from wide to long format
  clim_dt = melt(clim_dt, id.vars = c("x", "y"))
  setnames(clim_dt, old='variable', new='month')
  
  ## Merge weights with climate raster 
  ## -----------------------------------------------
  
  # Set key column in the climate data table
  keycols = c("x", "y")
  setkeyv(clim_dt, keycols)

  # Keyed merge on the x/y column 
  merged_dt <- clim_dt[weights, allow.cartesian = T] #6 cols: x, y, month, value, poly_id, and w_geo
  
 
  ## Multiply weights x climate value; aggregate by month and polygon  
  ## -----------------------------------------------
  
  ## Note this is not the same as below - geoweighted value in each grid cell, then summed by month and polygon
  merged_dt[, weighted_value := value * w_geo]
  sum_by_poly <- merged_dt[, sum(weighted_value), by = .(poly_id, month)]
  
  ## This is weighted mean of climate value by month and polygon (NOT a sum of the weighted mean of each cell) 
  # test <- merged_dt[, lapply(.SD, weighted.mean, w = w_geo, na.rm = T),
  #           by = .(poly_id, month),
  #           .SDcols = 'value']
  
  ## Add year column 
  sum_by_poly[, year := year] # Make sure this matches with the in parallel (might need to be x or something else)
  
  ## Order columns 
  setcolorder(sum_by_poly, neworder = c('year', 'month', 'poly_id', 'V1'))
  
  ## More informative name for column with climate parameter 
  setnames(sum_by_poly, old = "V1", new = climate_var)
  
  
  ## Return data.table with: year, month, polygon id, climate value 
  ## -----------------------------------------------
  
  return(sum_by_poly)

    
  }
