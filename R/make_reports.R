library(tidyverse)
library(quarto)
library(glue)
library(parallel)
library(doParallel)
library(furrr)
library(progressr)
library(tictoc)
library(cityforwardcollective)

leges_sf <- read_rds("data/electeds_with_sf_2024.rda")


make_reports <- function(i) {
  this <- leges_sf[i,]
  rep <- this[["name"]]
  district <- this[["district"]] 
  hon <- this[["title"]] 
  house <- this[["house"]] 
  
  of <- glue("{rep} - District {district}.pdf")
  d <- glue("compiled_reports/{house}")
  
  if (!dir.exists(d)) {
    dir.create(d)
  }
  
  if (!file.exists(glue("{d}/{of}"))) {
    quarto_render("template_report/template_report.qmd", 
                  execute_params = list("representative" = rep,
                                        "district" = district,
                                        "honorific" = hon,
                                        "house" = house), 
                  output_file = of)
    
    
    file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
    file.remove(of)
  }
  
}

# make_reports(9)


# plan(multicore)

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
    d <- glue("compiled_reports/{house}")
    
    if (!dir.exists(d)) {
      dir.create(d)
    }
    
    if (!file.exists(glue("{d}/{of}"))) {
      quarto_render("template_report/template_report.qmd", 
                    execute_params = list("representative" = rep,
                                          "district" = district,
                                          "honorific" = hon,
                                          "house" = house), 
                    output_file = of)
      
      
      
      file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
      file.remove(of)
    }
      
  })
}

list.files("compiled_reports/Assembly") |> 
  length()



tic()
with_progress({
  this_fun(nrow(leges_sf):1)
})
toc()

# tic()
# with_progress({
#   other_fun()
# })
# toc()

