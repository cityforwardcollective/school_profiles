library(tidyverse)
library(wisconsink12)
library(ggbeeswarm)

d <- read_rds("data/cfc_scored.rda")

wi_prof <- read_rds("data/wi_all_school_prof.rda")


all_data_mke <- wi_prof |> 
  left_join(schools |> 
              select(school_year, 
                     dpi_true_id, 
                     broad_agency_type,
                     accurate_agency_type,
                     milwaukee_indicator)) |> 
  left_join(d) |> 
  filter(milwaukee_indicator == 1 &
           accurate_agency_type != "partnership")

# how many schools in each sector are hq?
counts <- all_data_mke |> 
  ungroup() |> 
  select(dpi_true_id,
         broad_agency_type,
         cfc_rating) |> 
  left_join(make_mke_rc() |> 
              filter(school_year == "2023-24") |> 
              select(dpi_true_id, school_enrollment)
              ) |> 
  unique() |> 
  mutate(hq = ifelse(cfc_rating %in% c(
    "Exceeds Expectations",
    "Significantly Exceeds Expectations"
  ), TRUE, FALSE)) |> 
  group_by(hq) |> 
  summarise(n = n(),
            enr = sum(school_enrollment, na.rm = TRUE)) |> 
  mutate(perc = enr / sum(enr),
         total_n = sum(n),
         total_enr = sum(enr))

this <- all_data_mke |> 
  group_by(cfc_rating, test_subject, pa) |>
  summarise(total = sum(total)) |> 
  mutate(perc = total / sum(total))

this |> 
  mutate(hq = ifelse(cfc_rating %in% c(
    "Exceeds Expectations",
    "Significantly Exceeds Expectations"
  ), TRUE, FALSE)) |> 
  ungroup() |> 
  group_by(hq, test_subject, pa) |> 
  summarise(total = sum(total)) |> 
  mutate(perc = total / sum(total, na.rm = TRUE)) |> 
  filter(pa) |> 
  select(-total) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

# WI

all_data_mke <- wi_prof |> 
  left_join(schools |> 
              select(school_year, 
                     dpi_true_id, 
                     broad_agency_type,
                     accurate_agency_type,
                     milwaukee_indicator)) |> 
  left_join(d) |> 
  filter(accurate_agency_type != "partnership" &
           milwaukee_indicator != 1)

# how many schools in each sector are hq?
counts <- all_data_mke |> 
  ungroup() |> 
  select(dpi_true_id,
         broad_agency_type,
         cfc_rating) |> 
  left_join(make_wi_rc(exclude_milwaukee = FALSE) |> 
              filter(school_year == "2023-24") |> 
              select(dpi_true_id, school_enrollment)
  ) |> 
  unique() |> 
  mutate(hq = ifelse(cfc_rating %in% c(
    "Exceeds Expectations",
    "Significantly Exceeds Expectations"
  ), TRUE, FALSE)) |> 
  group_by(hq) |> 
  summarise(n = n(),
            enr = sum(school_enrollment, na.rm = TRUE)) |> 
  mutate(perc = enr / sum(enr),
         total_n = sum(n),
         total_enr = sum(enr)) |> 
  filter(hq) |> 
  select(-c(hq))
