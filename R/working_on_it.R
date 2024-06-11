cc |> 
  mutate(perc = enr / sum(enr),
         lab = glue("**{label_comma()(enr)}**<br>
                    <span style='font-size:8pt;'>({label_percent()(perc)})</span>")) |> 
  # filter(organisation_type != "State institution") |> 
  ggplot(aes(broad_agency_type, enr)) +
  geom_col(fill = cfc_darkblue, width = .5) +
  geom_textbox(aes(label = lab), halign = 0.5,
            vjust = -.25, family = "Verdana-Bold",
            fill = NA, box.size = 0) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  coord_cartesian(clip = "off") +
  labs(title = "Enrollment by School Sector",
       y = "Enrollment (2022-23 School Year)",
       x = "",
       caption = cap) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(b = 30)),
        plot.caption = element_text(hjust = 0, size = 8, color = "grey50"),
        plot.caption.position = "plot")

