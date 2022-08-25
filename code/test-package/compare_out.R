## Tracey Mangin
## August 24, 2022
## Compare outputs from stagg and previous

library(tidyverse)

## path



# Step 1 output
out_1 <- read_csv(file.path(here::here(), "data", "int", "weights", "ca_counties_era5_area_crop_weights.csv"))

# Step 2 output
out_2 <- read_csv(file.path(here::here(), "data", "output", "ca_counties_era5_temp_2005_2010_polynomial_3.csv"))

