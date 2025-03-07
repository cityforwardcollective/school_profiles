# Note: leges_sf is made in the lege_profiles project

library(tidyverse)
library(quarto)
library(glue)
library(parallel)
library(doParallel)
library(furrr)
library(progressr)
library(tictoc)
library(cityforwardcollective)
library(sf)

leges_sf <- read_rds("data/electeds_with_sf_2024.rda")

mke <- st_read("../shapefiles/Milwaukee/City Limits/citylimit.shp") |> 
  st_transform(crs = st_crs(leges_sf))

these_leges <- map_df(1:nrow(leges_sf), function(i) {
  this <- leges_sf[i,]
  
  jo <- st_join(this, mke, left = FALSE)
  
  if (nrow(jo) > 0) {
    over <- 1
  } else {
    over <- 0
  }
  
  this |> 
    mutate(overlaps = over)
})

# make_reports <- function(i) {
#   this <- leges_sf[i,]
#   rep <- this[["name"]]
#   district <- this[["district"]] 
#   hon <- this[["title"]] 
#   house <- this[["house"]] 
#   
#   of <- glue("{rep} - District {district}.pdf")
#   d <- glue("compiled_reports/{house}")
#   
#   if (!dir.exists(d)) {
#     dir.create(d)
#   }
#   
#   if (!file.exists(glue("{d}/{of}"))) {
#     quarto_render("template_report/template_report.qmd", 
#                   execute_params = list("representative" = rep,
#                                         "district" = district,
#                                         "honorific" = hon,
#                                         "house" = house), 
#                   output_file = of)
#     
#     
#     file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
#     file.remove(of)
#   }
#   
# }

# make_reports(9)


# plan(multicore)


# this_fun <- function(i, overwrite = FALSE) {
#   p <- progressor(steps = length(i))
#   
#   future_walk(i, .options = furrr_options(), function(i) {
#     p()
#     this <- leges_sf[i,]
#     rep <- this[["name"]]
#     district <- this[["district"]] 
#     hon <- this[["title"]] 
#     house <- this[["house"]] 
#     
#     # if (house %in% c("Senate", "Assembly")) {
#     #   of <- glue("{house} District {district}.pdf")
#     # } else {
#     #   of <- glue("{District {district}.pdf")
#     # }
#     
#     of <- glue("{house} District {district}.pdf")
#      
#     d <- glue("compiled_reports/{house}")
#     
#     if (!dir.exists(d)) {
#       dir.create(d)
#     }
#     
#     if (!overwrite) {
#       if (!file.exists(glue("{d}/{of}"))) {
#         quarto_render("template_report/template_report.qmd", 
#                       execute_params = list("representative" = rep,
#                                             "district" = district,
#                                             "honorific" = hon,
#                                             "house" = house), 
#                       output_file = of)
#         
#         
#         
#         file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
#         file.remove(of)
#       }
#     } else {
#       quarto_render("template_report/template_report.qmd", 
#                     execute_params = list("representative" = rep,
#                                           "district" = district,
#                                           "honorific" = hon,
#                                           "house" = house), 
#                     output_file = of)
#       
#       
#       
#       file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
#       file.remove(of)
#     }
#     
#       
#   })
# }

# leges_sf <- leges_sf |> 
#   filter(house %in% c("Senate", "Assembly"))


# with_progress({
#   this_fun(1:nrow(leges_sf), overwrite = TRUE)
# })




do_it <- function(i, overwrite = TRUE) {
  cat(crayon::cyan(glue::glue("Starting row {i}")), "\n")
  this <- leges_sf[i,]
  rep <- this[["name"]]
  district <- this[["district"]] 
  hon <- this[["title"]] 
  house <- this[["house"]] 
  cat(crayon::blue(glue::glue("{house} D{district}")))
  
  
  # if (house %in% c("Senate", "Assembly")) {
  #   of <- glue("{house} District {district}.pdf")
  # } else {
  #   of <- glue("{District {district}.pdf")
  # }
  
  if (hon == "Mayor" | hon == "County Executive") {
    of <- glue("{house}.pdf")
    d_lab <- hon
  } else {
    of <- glue("{house} District {district}.pdf")
    d_lab <- glue("{house} District {district}")
  }
  
  d <- glue("compiled_reports/{house}")
  
  if (!dir.exists(d)) {
    dir.create(d)
  }
  
  if (!overwrite) { # if overwrite == FALSE
    if (!file.exists(glue("{d}/{of}"))) { # if the file does not exist
      quarto_render("template_report/template_report.qmd", 
                    execute_params = list("representative" = rep,
                                          "d_lab" = d_lab,
                                          "district" = district,
                                          "honorific" = hon,
                                          "house" = house), 
                    output_file = of)
      
      
      
      file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
      file.remove(of)
    }
  } else { # if file does exist
    quarto_render("template_report/template_report.qmd", 
                  execute_params = list("representative" = rep,
                                        "d_lab" = d_lab,
                                        "district" = district,
                                        "honorific" = hon,
                                        "house" = house), 
                  output_file = of)
    
    
    
    file.copy(from = of, to = glue("{d}/{of}"), overwrite = TRUE)
    file.remove(of)
  }
  
  
}

walk(1:2, do_it)


