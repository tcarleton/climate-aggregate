## Sara Orofino
## February 22, 2022
## Aggregate Climate Data: Pipeline Step 02

source(here::here('code', 'file_paths.R')) # define the root directory for where data is stored

agg_climate_data <- function(year, data_source, climate_var, daily_agg, trans = 'polynomial', trans_specs, weights) {
  
  ## Setup 
  ## -----------------------------------------------
  require(pacman)
  pacman::p_load(ncdf4, data.table, rgdal, raster, tidyverse, here, crayon)

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
  
  ## Load saved weights 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # File name to call the weights
  if(weights){
    weights_file <- paste0(paste(input_polygons_name, data_source_norm, "area", weights_type, "weights", sep="_"), ".csv")
  } else{
    weights_file <- paste0(paste(input_polygons_name, data_source_norm, "area", "weights", sep="_"), ".csv")
  }
  
  # Data.table of weights 
  weights_dt <- fread(file.path(save_dir, "data", "int", "weights", weights_file))
  
  # Extent of area weights with slight buffer to make sure all cells are included 
  min_x <- min(weights_dt$x) - 0.5
  max_x <- max(weights_dt$x) + 0.5
  min_y <- min(weights_dt$y) - 0.5
  max_y <- max(weights_dt$y) + 0.5
  
  weights_ext <- raster::extent(min_x, max_x, min_y, max_y)
  
  ## Load climate data 
  ## -----------------------------------------------
  
  # Check if the climate variable is one we have data for 
  if(!climate_var %in% c('prcp', 'temp', 'uv')){
    stop(crayon::red('No ERA5 data available. Supported variables are: prcp, temp, or uv'))
  }
  
  # Climate data file paths
  ncpath  <- file.path(input_dir, 'data/raw', climate_var)
  ncname  <- paste(data_source_norm, climate_var, year, sep="_")
  nc_file <- paste0(ncpath, '/', ncname,'.nc')
  
  # Immediately crop to weights extent 
  clim_raster <- raster::crop(raster::stack(nc_file), weights_ext)
  
  # Get layer names (dates)
  all_layers <- names(clim_raster)
  layer_names <- all_layers[seq(1, length(all_layers), 24)] # Keep every 24th layer name (1 per day)
  layer_names <- paste0('month_', substring(layer_names, 7,8), '_day_', substring(layer_names, 10,11)) # Extract month/day for each layer 
  
  ## Aggregate to grid-day level  
  ## -----------------------------------------------
  
  if(!daily_agg %in% c('average', 'sum')){
    
    stop(crayon::red("Daily aggregation must be 'average' or 'sum'"))
  }
  
  ## Average 
  if(daily_agg == 'average'){
    
    # Check if there are 24*365 or 24*366 layers
    if(!raster::nlayers(clim_raster) %in% c(8760, 8784)){
      
      stop(crayon::red("Incomplete year of data; raster has", length(all_layers),
                       "layers, but a complete year should have 8760 layers or 8784 layers on a leap year"))
    }
    
    # Average over each set of 24 layers 
    indices<-rep(1:(nlayers(clim_raster)/24),each=24)
    clim_daily <- raster::stackApply(clim_raster, indices = indices, fun=mean) #Stack of 365/366 layers
    
    # For temperature convert values in Kelvin to Celsius C = K - 273.15
    if(climate_var == 'temp'){
      
      clim_daily <- clim_daily - 273.15
    }
    
  }

  ## Sum 
  if(daily_agg == 'sum'){
    
    # Check if there are 24*365 or 24*366 layers
    if(!raster::nlayers(clim_raster) %in% c(8760, 8784)){
      
      stop(crayon::red("Incomplete year of data; raster has", length(all_layers),
                       "layers, but a complete year should have 8760 layers or 8784 layers on a leap year"))
    }
    
    # Sum over each set of 24 layers 
    indices<-rep(1:(nlayers(clim_raster)/24),each=24)
    clim_daily <- raster::stackApply(clim_raster, indices = indices, fun=sum) #Stack of 365/366 layers
    
    # For temperature convert values in Kelvin to Celsius C = K - 273.15
    if(climate_var == 'temp'){
      
      clim_daily <- clim_daily - (273.15 * 24)
    }
    
  }
  
  ## convert prcp m to mm
  if(climate_var == 'prcp'){
    
    clim_daily <- clim_daily * 1000
  }
  
  ## Nonlinearities 
  ## -----------------------------------------------
  
  
  # Polynomial 
  if(trans == 'polynomial'){
    
    k <- trans_specs
    poly_orders <- seq(1:k) # Compute values from 1 to K 
    list_length <- length(poly_orders) # How many lists are in the final object 
    list_names <- sapply(1:list_length, FUN=function(x){paste("order", poly_orders[x], sep="_")})
    
    # For each daily layer, raise the value to k, k-1, k-2 etc. until 1
    r <- lapply(poly_orders, FUN=function(x){clim_daily ^ x})
    
    ## Function: Set names of data table by month, change from wide to long format, rename based on polynomial orders
    create_dt <- function(x){
      
      # Should output raster cells x/y with 365 days as column names
      dt <- as.data.table.raster(r[[x]], xy=TRUE)
      
      # Set column names with months
      new_names <- c('x', 'y', layer_names)
      setnames(dt, new_names)
      
      # Change from wide to long format
      dt = melt(dt, id.vars = c("x", "y"))
      
      # Update variable names 
      var_names <- c('date', list_names[x])
      setnames(dt, old=c('variable', 'value'), new=var_names)
    }
    
    # Make each raster layer a data.table 
    list_dt <- lapply(1:list_length, create_dt) 
    
    # Merge all data tables together
    clim_dt <- list_dt[[1]]
    for(i in 2:list_length){
      dt_m <- list_dt[[i]]
      clim_dt <- merge(clim_dt, dt_m, by=c('x', 'y', 'date'))
    }

  }
  
  ## Merge weights with climate raster 
  ## -----------------------------------------------
  
  # Set key column in the climate data table
  keycols = c("x", "y")
  setkeyv(clim_dt, keycols)

  # Keyed merge on the x/y column 
  merged_dt <- clim_dt[weights_dt, allow.cartesian = TRUE] #cols: x, y, date, value cols 1:k, poly_id, w_area, weight (if weights = T)
 
  ## Multiply weights x climate value (all 1:k values); aggregate by month and polygon  
  ## -----------------------------------------------

  # Multiply by secondary weights if weights = TRUE (already normalized by polygon area)
  # Otherwise multiply by just area weights 
  if(weights){
    merged_dt[, (list_names) := lapply(list_names, function(x) {get(x) * weight})]
  } else {
    merged_dt[, (list_names) := lapply(list_names, function(x) {get(x) * w_area})]
  }
  
  # Separate month and day columns 
  merged_dt[, ':=' (month = substring(date, first=1, last=8),
                    day = substring(date, first=10))]
  
  # Can customize this in the future to aggregate by day & month 
  # Right now just sum by month 
  sum_by_poly <- merged_dt[,  lapply(.SD, sum), by = .(poly_id, month),
                           .SDcols = list_names]
  
  ## Add year column 
  sum_by_poly[, year := year] 
  
  ## Order columns 
  setcolorder(sum_by_poly, neworder = c('year', 'month', 'poly_id', list_names))
  
  ## Return the sums by polygon 
  return(sum_by_poly)

    
  }

# function to call agg_climate_data over multiple years in parallel
agg_climate_data_multiyear <- function(years, data_source, climate_var, daily_agg, trans = 'polynomial', trans_specs, weights){
  
  library(parallel)
  library(data.table)
  
  no_cores <- parallel::detectCores() - 1 # Calculate the number of cores. Leave one in case something else needs to be done on the same computer at the same time. 
  cl <- makeCluster(no_cores, type="FORK") # Initiate cluster. "FORK" means bring everything in your current environment with you. 
  sum_by_poly_multiyear <- parLapply(cl, years, agg_climate_data, data_source, climate_var, daily_agg, trans, trans_specs, weights)
  stopCluster(cl)
  
  sum_by_poly_multiyear <- data.table::rbindlist(sum_by_poly_multiyear)
  
  # Check if there is already an output folder
  if(!dir.exists(file.path(save_dir, "data", "output"))){
    
    # If no - create it
    message(crayon::yellow('Creating data/output'))
    dir.create(file.path(save_dir, "data", "output"), recursive=T)
    
  }
  
  # File save name
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # If secondary weights are used add to save name
  # Otherwise just include other inputs
  if(weights){
    save_name <- paste0(paste(input_polygons_name, data_source_norm, climate_var, daily_agg,
                            years[1], years[length(years)], trans, trans_specs, "area", weights_type, 'weights', sep="_"), ".csv")
  } else{
    save_name <- paste0(paste(input_polygons_name, data_source_norm, climate_var, daily_agg,
                            years[1], years[length(years)], trans, trans_specs, "area", "weights", sep="_"), ".csv")
  }
  
  save_path <- file.path(save_dir, "data", "output")
  
  # Save message
  message(crayon::yellow('Saving', save_name, 'to', save_path))
  fwrite(sum_by_poly_multiyear, file = file.path(save_path, save_name))
  
  # Done
  message(crayon::green('Done'))
}