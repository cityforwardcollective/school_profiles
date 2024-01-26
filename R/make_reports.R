library(tidyverse)
library(quarto)
library(glue)

leges_sf <- read_rds("data/electeds_with_sf_2023.rda")

rep <- "Jonathan Brostoff"
rind <- which(leges_sf$name == rep)
district <- leges_sf[[rind,"district"]] 
hon <- leges_sf[[rind,"title"]] 
house <- leges_sf[[rind,"house"]] 


of <- glue("{rep} - District {district}.pdf")
quarto::quarto_render("template_report/template_report.qmd", 
                      execute_params = list("representative" = rep,
                                            "district" = district,
                                            "honorific" = hon,
                                            "house" = house), 
                      output_file = of)

d <- glue("compiled_reports/{house}")

if (!dir.exists(d)) {
  dir.create(d)
}

file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
file.remove(of)
