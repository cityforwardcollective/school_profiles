library(tidyverse)
library(wisconsink12)
library(sf)
library(tigris)
library(scales)


leges_sf <- read_rds("data/electeds_with_sf_2024.rda")
mke <- places(state = "WI") |> 
  filter(NAME == "Milwaukee") |> 
  st_transform(crs = st_crs(leges_sf))

leges_sf |> 
  as_tibble() |> 
  select(-geometry) |> 
  mutate(reviewed = "",
         needs_update = "",
         notes = "") |> 
  write_csv("data/electeds_for_audit.csv")

mps <- c("Traditional Public", "Instrumentality Charter")

mke_schools <- make_mke_schools() |> 
  filter(school_year == "2024-25") |> 
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

mke_schools <- make_mke_rc(private_type = "choice") |> 
  filter(school_year == "2024-25") |> 
  select(school_year,
         dpi_true_id,
         accurate_agency_type,
         student_count = school_enrollment) |> 
  mutate(is_mps = ifelse(accurate_agency_type %in% mps, TRUE, FALSE)) |> 
  left_join(geocodes |> 
              select(-school_year)) |> 
  filter(!is.na(lat)) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = st_crs(leges_sf))

w_percs <- map_df(1:nrow(leges_sf), function(i) {
  this <- leges_sf[i,]
  in_mke <- st_intersection(this, mke)
  if (nrow(in_mke) == 0) {
    perc_in_mke <-  0
  } else {
    perc_in_mke <- (as.numeric(st_area(in_mke) / st_area(this)))
  }
  
  # mke |> 
  #   ggplot() +
  #   geom_sf() + 
  #   geom_sf(data = this, fill = "red")
  
  j <- st_join(mke_schools, this, left = FALSE)
  
  if (nrow(j) > 0) {
    perc_mps <- j |> 
      as_tibble() |> 
      group_by(is_mps) |> 
      summarise(total = sum(student_count, na.rm = TRUE)) |> 
      mutate(perc = total / sum(total)) |> 
      filter(is_mps)
    
    if (nrow(perc_mps) > 0) {
      count_mps <- perc_mps[[1, "total"]]
      perc_mps <- perc_mps[[1, "perc"]]
    } else {
      perc_mps <- 0
      count_mps <- 0
    }
  } else {
    perc_mps <- perc_in_mke <- count_mps <- NA
  }
  
  this |> 
    mutate(perc_mps = perc_mps,
           count_mps = count_mps,
           perc_in_mke = perc_in_mke)
})

# w_percs |> 
#   as_tibble() |> 
#   select(house,
#          district,
#          name,
#          perc_mps) |>
#   write_csv("data/perc_in_mps_by_dist.csv")

w_percs |> 
  as_tibble() |> 
  transmute(
    house,
         district,
         name,
         perc_mps,
    count_mps,
    est_lost_funding = ((count_mps / perc_mps) - count_mps) * (16774 - 12500),
    perc_outside_mps = 1 - perc_mps,
    est_lost_funding = label_dollar()(est_lost_funding),
         perc_in_mke = round(perc_in_mke, digits = 2)) |> 
  filter(perc_mps > 0 &
           house %in% c("Senate", "Assembly")) |> 
  select(house,
         district, 
         name,
         perc_outside_mps,
         est_lost_funding,
         perc_district_in_mke = perc_in_mke) |> 
  write_csv("data/MKE Lege Delegation MPS Breakdown.csv")
