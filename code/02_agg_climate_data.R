## Sara Orofino
## February 22, 2022
## Aggregate Climate Data: Pipeline Step 02

agg_climate_data <- function(year, data_source, climate_var, trans = 'polynomial', trans_specs) {
  
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
  
  ## Load saved geoweights 
  ## -----------------------------------------------
  
  # Normalize the data source input - remove spaces & lower case
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  
  # File name to call the weights
  weights_file <- paste0(paste(input_polygons_name, data_source_norm, sep="_"), ".csv")
  
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
  layer_names <- paste0('month_', substring(layer_names, 7,8), '_day_', substring(layer_names, 10,11)) # Extract month/day for each layer 
  
  ## Aggregate to grid-day level  
  ## -----------------------------------------------
  
  # Average over each set of 24 layers - assuming there are 24*365 layers  
  indices<-rep(1:(nlayers(clim_raster)/24),each=24)
  clim_daily <- raster::stackApply(clim_raster, indices = indices, fun=mean) #Stack of 365 layers
  
  # For temperature convert values in Kelvin to Celsius C = K - 273.15
  # Check that it's okay to do this with the daily values or if I should do it before collapsing by day
  if(climate_var == 'temp'){
    
    clim_daily <- clim_daily - 273.15
  }
  
  ## Nonlinearities 
  ## -----------------------------------------------
  
  
  # Simplest version of polynomial - just square the values
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
    
    # Make each raster a data.table 
    list_dt <- lapply(1:list_length, create_dt) 
    
    # Merge all data tables together
    # Note: a non for loop way to do this? I tried rbindlist, bind_rows but those options don't join on x,y,month so I'm getting a table triple the size 
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
  merged_dt <- clim_dt[weights, allow.cartesian = T] #cols: x, y, date, value cols 1:k, poly_id, and w_geo
  
 
  ## Multiply weights x climate value (all 1:k values); aggregate by month and polygon  
  ## -----------------------------------------------

  merged_dt[, (list_names) := lapply(list_names, function(x) {get(x) * w_geo})]
  
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
agg_climate_data_multiyear <- function(years, data_source, climate_var, trans = 'polynomial', trans_specs){
  
  library(parallel)
  library(data.table)
  
  no_cores <- parallel::detectCores() - 1 # Calculate the number of cores. Leave one in case something else needs to be done on the same computer at the same time. 
  cl <- makeCluster(no_cores, type="FORK") # Initiate cluster. "FORK" means bring everything in your current environment with you. 
  sum_by_poly_multiyear <- parLapply(cl, years, agg_climate_data, data_source, climate_var, trans, trans_specs)
  stopCluster(cl)
  
  sum_by_poly_multiyear <- data.table::rbindlist(sum_by_poly_multiyear)
  
  # Check if there is already an output folder
  if(!dir.exists(here::here("data", "output"))){
    
    # If no - create it
    message(crayon::yellow('Creating data/output'))
    dir.create(here::here("data", "output"), recursive=T)
    
  }
  
  # File save name
  data_source_norm <- gsub(" ", "", data_source) %>% tolower(.)
  save_name <- paste0(paste(input_polygons_name, data_source_norm, climate_var,
                            years[1], years[length(years)], trans, trans_specs, sep="_"), ".csv")
  save_path <- file.path(here::here(), "data", "output")
  
  # Save message
  message(crayon::yellow('Saving', save_name, 'to', save_path))
  fwrite(sum_by_poly_multiyear, file = file.path(save_path, save_name))
  
  # Done
  message(crayon::green('Done'))
}