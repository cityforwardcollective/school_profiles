library(tidyverse)
library(sf)

leges_sf <- read_rds("data/electeds_with_sf_2024.rda")

st_write(leges_sf, driver = "GeoJSON", 
         dsn = "../000_data_temp/leges.geojson", 
         layer_options = "ID_GENERATE=YES")
