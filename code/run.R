## Run Climate Functions

## Load functions
source(here::here('code', '01_spatial_overlay.R'))
source(here::here('code', '02_agg_climate_data.R'))

## Load other required data (i.e. input polygons)
input_polygons <- tigris::counties()

## Inputs - update as necessary 
# Note: input polygons and input_polygon_name vars have to be entered directly into the functions
climate_data <- 'era5'
input_polygons_name <- 'us_counties'
id_var <- 'GEOID' #col name that uniquely identifies each polygon 
years <- 2000:2020
climate_variable <- 'prcp'
trans <- 'polynomial'
trans_specs <- 3

## Steps to run 
# Specify if you need to run step 1 and step 2
# If TRUE runs both, if FALSE runs step 2 only 
both_steps <- TRUE

# both_steps = T run both, else run step 2 only 
if(both_steps){
  
  # Edit input polygons 
  calc_geoweights(data_source = climate_data,
                  input_polygons = input_polygons,
                  polygon_id = id_var)
  

} 
  
# Edit input_polygon_names
agg_climate_data_multiyear(years = years,
                           data_source = climate_data,
                           climate_var = climate_variable,
                           trans = trans,
                           trans_specs = trans_specs)
