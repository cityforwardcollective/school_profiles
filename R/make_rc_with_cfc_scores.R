library(wisconsink12)

cfc_scored <- read_rds("data/cfc_scored.rda")

make_mke_rc(private_type = 'all') |>
  filter(school_year == "2023-24") |> 
  left_join(cfc_scored) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         cfc_score,
         cfc_rating,
         overall_score,
         overall_rating,
         sch_ach,
         sch_growth,
         sch_tgo,
         sch_ot,
         starts_with("per_")) |> 
  write_csv("../000_data_temp/mke_rc_w_cfc_scores_2023_24.csv")
