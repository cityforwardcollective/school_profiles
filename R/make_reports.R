library(tidyverse)
library(quarto)
library(glue)
library(parallel)
library(doParallel)
library(furrr)
library(progressr)
library(tictoc)

leges_sf <- read_rds("data/electeds_with_sf_2023.rda")


make_reports <- function(i) {
  this <- leges_sf[i,]
  rep <- this[["name"]]
  district <- this[["district"]] 
  hon <- this[["title"]] 
  house <- this[["house"]] 
  
  of <- glue("{rep} - District {district}.pdf")
  quarto_render("template_report/template_report.qmd", 
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
}

# make_reports(9)


plan(multicore)

this_fun <- function(i) {
  p <- progressor(steps = length(i))
  
  future_walk(i, .options = furrr_options(scheduling = 2), function(i) {
    p()
    this <- leges_sf[i,]
    rep <- this[["name"]]
    district <- this[["district"]] 
    hon <- this[["title"]] 
    house <- this[["house"]] 
    
    of <- glue("{rep} - District {district}.pdf")
    quarto_render("template_report/template_report.qmd", 
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
  })
}




tic()
with_progress({
  this_fun(1:nrow(leges_sf))
})
toc()

tic()
with_progress({
  other_fun()
})
toc()

