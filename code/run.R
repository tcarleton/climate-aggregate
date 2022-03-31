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
climate_data <- 'era5'
input_polygons_name <- 'ca_counties'
id_var <- 'GEOID' #col name that uniquely identifies each polygon 
years <- 2005:2010
climate_variable <- 'temp'
trans <- 'polynomial'
trans_specs <- 3

## Steps to run 
# Specify if you need to run step 1 and step 2
# If TRUE runs both, if FALSE runs step 2 only 
both_steps <- TRUE

# both_steps = T run both, else run step 2 only 
if(both_steps){
  
  calc_geoweights(data_source = climate_data,
                  input_polygons = input_polygons,
                  polygon_id = id_var)
  

} 
  
agg_climate_data_multiyear(years = years,
                           data_source = climate_data,
                           climate_var = climate_variable,
                           trans = trans,
                           trans_specs = trans_specs)
