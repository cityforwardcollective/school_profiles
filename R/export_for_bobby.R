library(tidyverse)
library(sf)

d <- read_rds("data/electeds_with_sf_2024.rda")

dd <- d |> 
  filter(!house %in% c(
    "City of Milwaukee",
    "Milwaukee County"
  ))

saveRDS(dd, "data/wi_elected_with_sf.rda")

write_csv(dd, "data/wi_elected_with_sf.csv")

st_crs(dd)
