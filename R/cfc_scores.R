library(tidyverse)
library(wisconsink12)

# make_mke_rc() |> 
#   filter(is.na(sch_tgo) & 
#            !is.na(sch_ach) &
#            !is.na(sch_growth) &
#            school_year == "2022-23") |> 
#   select(school_year,
#          dpi_true_id,
#          sch_ach,
#          sch_growth,
#          sch_tgo,
#          sch_ot,
#          ends_with("weight")) |> 
#   mutate(new_agw = (ach_weight + growth_weight) / 2) |> 
#   View()

cfc_scored <- map2_df(c("sch_ach", "sch_growth", "sch_tgo", "sch_ot"),
        c("ach_weight", "growth_weight", "tgo_weight", "ot_weight"),
        function(x, y) {
          make_wi_rc(exclude_milwaukee = FALSE) |> 
            ungroup() |> 
            filter(school_year == "2023-24") |> 
            select(school_year,
                   dpi_true_id,
                   value = x,
                   weight = y) |> 
            mutate(score = x)
                   # new_weight = ifelse(aw, half_weight / 2, weight))
        }) |> 
  group_by(school_year, 
           dpi_true_id,
           aw = score %in% c("sch_ach", "sch_growth")) |> 
  mutate(half_weight = sum(weight) / 2,
         new_weight = ifelse(aw, half_weight, weight)) |> 
  arrange(dpi_true_id) |> 
  ungroup() |> 
  group_by(school_year, dpi_true_id) |> 
  summarise(cfc_score = weighted.mean(value, new_weight, na.rm = TRUE) |> 
              round(1)) |> 
  ungroup() |> 
  mutate(cfc_rating = case_when(
    cfc_score < 48.0 ~ "Fails to Meet Expectations",
    cfc_score < 58.0 ~ "Meets Few Expectations",
    cfc_score < 70.0 ~ "Meets Expectations",
    cfc_score < 83.0 ~ "Exceeds Expectations",
    cfc_score < 100.0 ~ "Significantly Exceeds Expectations",
    TRUE ~ NA
  ))

saveRDS(cfc_scored, "data/cfc_scored.rda")


