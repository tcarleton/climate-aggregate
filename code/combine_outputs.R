## Tracey Mangin
## July 22, 2022
## Combine outputs for EU and USA

library(tidyverse)
library(data.table)

## add paths
main_path <- '/Volumes/GoogleDrive/Shared Drives/emlab/projects/current-projects/climate-data-pipeline/'

output_path <- paste0(main_path, 'agg-outputs/output/')

## usa prcp files
usa_prcp1 <- fread(paste0(output_path, 'usa_area_era5_prcp_sum_1968_1970_polynomial_3_area_crop_weights.csv'))
usa_prcp2 <- fread(paste0(output_path, 'usa_area_era5_prcp_sum_1971_1980_polynomial_3_area_crop_weights.csv'))
usa_prcp3 <- fread(paste0(output_path, 'usa_area_era5_prcp_sum_1981_1990_polynomial_3_area_crop_weights.csv'))
usa_prcp4 <- fread(paste0(output_path, 'usa_area_era5_prcp_sum_1991_2004_polynomial_3_area_crop_weights.csv'))

## usa temp files
usa_temp1 <- fread(paste0(output_path, 'usa_area_era5_temp_average_1968_1980_polynomial_5_area_crop_weights.csv'))
usa_temp2 <- fread(paste0(output_path, 'usa_area_era5_temp_average_1981_1990_polynomial_5_area_crop_weights.csv'))
usa_temp3 <- fread(paste0(output_path, 'usa_area_era5_temp_average_1991_2004_polynomial_5_area_crop_weights.csv'))


## bind
usa_prcp <- rbind(usa_prcp1, usa_prcp2, usa_prcp3, usa_prcp4)
usa_temp <- rbind(usa_temp1, usa_temp2, usa_temp3)

## save
fwrite(usa_prcp, file = paste0(output_path, 'usa_area_era5_prcp_sum_1968_2004_polynomial_3_area_crop_weights.csv'))
fwrite(usa_temp, file = paste0(output_path, 'usa_area_era5_temp_average_1968_2004_polynomial_5_area_crop_weights.csv'))





