## Run Climate Functions

## Load packages necessary for sourcing functions/setup
library(tidyverse)
library(sf)
library(here)

## Load functions
source(here::here('code', '01_spatial_overlay.R'))
source(here::here('code', '02_agg_climate_data.R'))

## Load other required data (i.e. input polygons)
# Testing with CA counties
input_polygons <- read_sf(file.path(here::here(), "data", "shapefiles", "tl_2019_us_county", "tl_2019_us_county.shp")) %>% dplyr::filter(STATEFP == '06')

## Inputs - update as necessary 
# Defined - must match one of the options
climate_data <- 'era5' # era5 is only option for now
climate_variable <- 'temp' # temp, prcp, or uv
years <- 2005:2010 # any sequence of years 2002:2020
trans <- 'polynomial' # polynomial is only option for now
trans_specs <- 3 # Specs must match the trans type, numeric when trans=polynomial
id_var <- 'GEOID' # Col name in the input_polygons shp that uniquely identifies each polygon 
weights <- FALSE # True to define a second set of weights, FALSE to use only area weights

# Flexible - not limited to specific options
input_polygons_name <- 'ca_counties' # Name used in saving function outputs; should relate to the polygons used
weights_type <- 'crop' # Added to the name of the output file to indicate what secondary weights are used 
weights_name <- 'era5_cropland_2003_full' # The name of the secondary weights file, cannot be blank if weights = TRUE


## Steps to run 
# Specify if you need to run step 1 and step 2
# If TRUE runs both, if FALSE runs step 2 only 
both_steps <- FALSE

# both_steps = T run both, else run step 2 only 
if(both_steps){
  
  calc_geoweights(data_source = climate_data,
                  input_polygons = input_polygons,
                  polygon_id = id_var,
                  weights = weights)
  

} 
  
agg_climate_data_multiyear(years = years,
                           data_source = climate_data,
                           climate_var = climate_variable,
                           trans = trans,
                           trans_specs = trans_specs,
                           weights = weights)
