library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)

cfc_scored <- read_rds("data/cfc_scored.rda")

make_mke_schools() |>
  select(school_year, dpi_true_id, accurate_agency_type, school_name) |> 
  filter(school_year > "2020-21") |> 
  left_join(cfc_scored) |> 
  mutate(lq = ifelse(cfc_score < 48, TRUE, FALSE)) |> 
  group_by(dpi_true_id) |> 
  filter(sum(lq) == 3) |> 
  ungroup() |> 
  filter(school_year == "2023-24") |> 
  select(school_name, accurate_agency_type) |> 
  unique() |> 
  write_csv("data/pups.csv")


make_mke_rc() |> 
  filter(school_year > "2020-21") |> 
  mutate(lq = ifelse(overall_score < 48, TRUE, FALSE)) |> 
  group_by(dpi_true_id) |> 
  filter(sum(lq) == 3) |> 
  ungroup() |> 
  filter(school_year == "2023-24") |> 
  select(school_name, accurate_agency_type) |> 
  unique() |> View()

these_cols <- c("#4ba6db", cfc_darkblue)

make_mke_rc() |> 
  filter(school_year == "2023-24") |> 
  left_join(cfc_scored) |> 
  select(dpi_true_id,
         cfc_rating,
         overall_rating) |> 
  filter(!is.na(cfc_rating)) |> 
  pivot_longer(cols = 2:3) |> 
  mutate(value = factor(value,
                        levels = c(
                          "Fails to Meet Expectations",
                          "Meets Few Expectations",
                          "Meets Expectations",
                          "Exceeds Expectations",
                          "Significantly Exceeds Expectations"
                        )),
         name = ifelse(name == "cfc_rating", "CFC Metric", "WI Report Card")) |> 
  group_by(name, value) |> 
  count() |> 
  ggplot(aes(value, n, fill = name, label = n)) +
  geom_col(position = position_dodge(width = .7), width = .6) +
  geom_text(position = position_dodge(width = .7),
            family = "Verdana", fontface = "bold",
            aes(color = ifelse(n < 5, these_cols[1], "white"),
                vjust = ifelse(n >= 5, 1.5, -.5))) +
  scale_fill_manual(values = these_cols) +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = label_wrap_gen(width = 15)) +
  theme(legend.position = "top") +
  labs(fill = "", x = "", y = "")

ggsave("plots/rc_vs_cfc_scores_for_austin.png", bg = "transparent",
       width = 8, h = 5)
