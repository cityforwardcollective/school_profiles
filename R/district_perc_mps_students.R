library(tidyverse)
library(wisconsink12)
library(sf)
library(tigris)
library(scales)
library(openxlsx)



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

# mke_schools <- make_mke_schools() |> 
#   filter(school_year == "2024-25") |> 
#   left_join(enrollment |> 
#               filter(group_by == "All Students")) |> 
#   select(school_year,
#          dpi_true_id,
#          accurate_agency_type,
#          student_count) |> 
#   mutate(is_mps = ifelse(accurate_agency_type %in% mps, TRUE, FALSE)) |> 
#   left_join(geocodes |> 
#               select(-school_year)) |> 
#   filter(!is.na(lat)) |> 
#   st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
#   st_transform(crs = st_crs(leges_sf))

mke_schools <- make_wi_rc(exclude_milwaukee = FALSE, private_type = "choice") |> 
  left_join(schools |> 
              select(school_year, dpi_true_id, milwaukee_indicator)) |> 
  filter(school_year == "2024-25") |> 
  select(school_year,
         dpi_true_id,
         accurate_agency_type,
         milwaukee_indicator,
         student_count = school_enrollment) |> 
  mutate(is_mps = ifelse(accurate_agency_type %in% mps &
                           milwaukee_indicator == 1, TRUE, FALSE)) |> 
  left_join(geocodes |> 
              select(-school_year)) |> 
  filter(!is.na(lat)) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = st_crs(leges_sf)) |> 
  # some out-state schools have NA for milwaukee indicator, 
  # this is stop gap to handle until i fix
  mutate(milwaukee_indicator = replace_na(milwaukee_indicator, 0))

w_percs <- map_df(1:nrow(leges_sf), function(i) {
  this <- leges_sf[i,]
  in_mke <- st_intersection(this, mke)
  
  j <- st_join(mke_schools, this, left = FALSE)
  real_mke_schools <- j |> 
    filter(milwaukee_indicator == 1)
  
  if (nrow(in_mke) == 0) {
    perc_in_mke <- perc_students_in_mke <- 0
  } else {
    perc_in_mke <- (as.numeric(st_area(in_mke) / st_area(this)))
    perc_students_in_mke <-   j |> 
      as_tibble() |> 
      group_by(milwaukee_indicator) |> 
      summarise(total = sum(student_count, na.rm = TRUE)) |> 
      mutate(perc = total / sum(total)) |> 
      filter(milwaukee_indicator == 1) |> 
      pull(perc)
  }
  
  mke |>
    ggplot() +
    geom_sf() +
    geom_sf(data = this, fill = "red")
  
  
  # if there are any MKE schools
  if (nrow(real_mke_schools) > 0) {
    # percent of MKE schools that are MPS
    perc_mps <- j |> 
      filter(milwaukee_indicator == 1) |> 
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
    perc_mps <- perc_in_mke <- perc_students_in_mke <- count_mps <- NA
  }
  
  this |> 
    mutate(perc_mps = perc_mps,
           count_mps = count_mps,
           perc_in_mke = perc_in_mke,
           perc_students_in_mke = perc_students_in_mke)
})

# w_percs |> 
#   as_tibble() |> 
#   select(house,
#          district,
#          name,
#          perc_mps) |>
#   write_csv("data/perc_in_mps_by_dist.csv")

output_data <- w_percs |> 
  as_tibble() |> 
  transmute(
    house,
         district,
         name,
    party_aff,
         perc_mps,
    count_mps,
    est_lost_funding = ((count_mps / perc_mps) - count_mps) * (16774 - 12500),
    perc_outside_mps = 1 - perc_mps,
    est_lost_funding = label_dollar()(est_lost_funding),
         perc_in_mke = round(perc_in_mke, digits = 2),
    perc_students_in_mke) |> 
  filter(perc_mps > 0 &
           house %in% c("Senate", "Assembly")) |> 
  transmute(house,
         district, 
         name,
         party_aff,
         est_lost_funding,
         perc_outside_mps = round(perc_outside_mps, 2),
         perc_district_area_in_mke = perc_in_mke,
         perc_students_in_mke = round(perc_students_in_mke, 2))


wb <- createWorkbook()
addWorksheet(wb, "Districts")
writeData(wb, "Districts", output_data)

addFilter(wb, "Districts", row = 1, cols = 1:ncol(output_data))


# Bold header style
header_style <- createStyle(textDecoration = "bold")
addStyle(
  wb,
  "Districts",
  style = header_style,
  rows = 1,
  cols = 1:ncol(output_data)
)

# Center align columns 2-7 (all rows including header)
center_style <- createStyle(halign = "center")
addStyle(
  wb,
  "Districts",
  style = center_style,
  rows = 1:(nrow(output_data) + 1),
  cols = 2:ncol(output_data),
  gridExpand = TRUE,
  stack = TRUE
)

# Format columns 6:8 as percentages (data rows only)
pct_style <- createStyle(numFmt = "0%")
addStyle(
  wb,
  "Districts",
  style = pct_style,
  rows = 2:(nrow(output_data) + 1),
  cols = 6:8,
  gridExpand = TRUE,
  stack = TRUE
)


setColWidths(
  wb,
  "Districts",
  cols = 1:ncol(output_data),
  widths = "auto"
)

saveWorkbook(
  wb,
  "data/MKE Lege Delegation MPS Breakdown.xlsx",
  overwrite = TRUE
)
