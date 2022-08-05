## Tracey Mangin
## August 4, 2022
## spatial overlay for polygons that straddle 0

library(sf)
library(tidyverse)
library(tmap)

root_dir <- file.path("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/climate-data-pipeline")

input_polygons <- read_sf(file.path(root_dir, "shapefiles", "EU", "NUTS2_RG_60M_2013_mortality.shp"))

## convert sf to spatial data frame 
input_polygons_spd <- as_Spatial(input_polygons)

## extract the NUTS_ID column from spatial data frame
id_vals <- as.data.frame(input_polygons_spd) %>%
  select(NUTS_ID)

## When recentering a world map, say to change an "Atlantic" view 
## with longitude range -180 to 180, to a "Pacific" view, with 
## longitude range 0 to 360, polygons crossed by the new offset, 
## here 0/360, need to be clipped into left and right sub.polygons 
## to avoid horizontal scratches across the map. The nowrapSpatialPolygons 
## function performs this operation using polygon intersection, and nowrapRecenter 
## recenters the output SpatialPolygons object.

## recenter
input_polygons_spd <- nowrapRecenter(input_polygons_spd)

## convert back to sf, bind ids
input_polygons <- st_as_sf(input_polygons_spd) %>%
  cbind(id_vals)

## View map
map_fig <- ggplot(input_polygons) + 
  geom_sf(fill = "grey", lwd = 0) + 
  theme(legend.position = "none")

map_fig2 <- ggplot(input_polygons) + 
  geom_sf(aes(fill = NUTS_ID), lwd = 0) + 
  theme(legend.position = "none")

plotly::ggplotly(map_fig2)

## plot a polygon that intersects with 0
split_areas <- c("FR62")

split_fig <- ggplot(input_polygons %>% filter(NUTS_ID %in% split_areas)) + 
  geom_sf(aes(fill = NUTS_ID), lwd = 0) +  
  theme(legend.position = "none")

split_fig
plotly::ggplotly(split_fig)


