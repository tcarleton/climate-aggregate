## Sara Orofino
## February 22, 2022
## Merge Climate Rasters: Pipeline Step 02

## Inputs so far: data_source, climate_var, year (one yr, multiple yrs?), input_polygons_name, nonlinearity transformation  

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
  
  ## Lazy load climate data 
  ## -----------------------------------------------
  
  # Check if the climate variable is one we have data for 
  if(!climate_var %in% c('prcp', 'temp', 'uv')){
    stop(crayon::red('No ERA5 data available. Supported variables are: prcp, temp, or uv'))
  }
  
  # Read in climate rasters based on weights extent
  ncpath  <- file.path(here::here(), 'data/raw', climate_var)
  ncname  <- paste(data_source_norm, climate_var, year, sep="_")
  nc_file <- paste0(ncpath, '/', ncname,'.nc')
  
  clim_lazy <- raster::crop(lazyraster(nc_file), weights_ext)  
  clim_raster <- lazyraster::as_raster(clim_lazy) # has no crs? 
  clim_stack  <- raster::stack(clim_lazy) # errors out - differing number of rows
  
  
  # Convert climate raster to climate data.table
  ## Problem: gridNumbers are just 1:length so they aren't going to align with the gridNumber in geoweights
  ## Found a function to covert rasters to data tables including xy so we can match on that? 
  ## Second problem: resolution is different if I lazy load it. Instead of 0.25x0.25 it's 0.11355 x 0.20633 so the xy doesn't align
  clim_dt <- as.data.table.raster(clim_raster, xy = TRUE)
  
  
  ## Nonlinearities 
  ## -----------------------------------------------
  
  # Simplest version - avg daily temperature in a grid cell 
  clim_dt[,  ]
  


  
  
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
