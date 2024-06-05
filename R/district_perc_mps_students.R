library(tidyverse)
library(wisconsink12)
library(sf)


leges_sf <- read_rds("data/electeds_with_sf_2024.rda")

leges_sf |> 
  as_tibble() |> 
  select(-geometry) |> 
  mutate(reviewed = "",
         needs_update = "",
         notes = "") |> 
  write_csv("data/electeds_for_audit.csv")

mps <- c("Traditional Public", "Instrumentality Charter")

mke_schools <- make_mke_schools() |> 
  filter(school_year == "2022-23") |> 
  left_join(enrollment |> 
              filter(group_by == "All Students")) |> 
  select(school_year,
         dpi_true_id,
         accurate_agency_type,
         student_count) |> 
  mutate(is_mps = ifelse(accurate_agency_type %in% mps, TRUE, FALSE)) |> 
  left_join(geocodes |> 
              select(-school_year)) |> 
  filter(!is.na(lat)) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = st_crs(leges_sf))

w_percs <- map_df(1:nrow(leges_sf), function(i) {
  this <- leges_sf[i,]
  
  j <- st_join(mke_schools, this, left = FALSE)
  
  if (nrow(j) > 0) {
    perc_mps <- j |> 
      as_tibble() |> 
      group_by(is_mps) |> 
      summarise(total = sum(student_count)) |> 
      mutate(perc = total / sum(total)) |> 
      filter(is_mps)
    
    if (nrow(perc_mps) > 0) {
      perc_mps <- perc_mps[[1, "perc"]]
    } else {
      perc_mps <- 0
    }
  } else {
    perc_mps <- NA
  }
  
  this |> 
    mutate(perc_mps = perc_mps)
})

w_percs |> 
  as_tibble() |> 
  select(house,
         district,
         name,
         perc_mps) |>
  write_csv("data/perc_in_mps_by_dist.csv")

w_percs |> 
  as_tibble() |> 
  select(house,
         district,
         name,
         perc_mps) |> 
  filter(perc_mps < .50)
