library(tidyverse)
library(sf)
library(cityforwardcollective)
library(mapboxapi)
library(leaflet)
library(glue)
library(htmlwidgets)
library(webshot2)

leges_sf <- read_rds("data/electeds_with_sf_2024.rda")

mke <- st_read("../shapefiles/Milwaukee/City Limits/citylimit.shp") |> 
  st_transform(crs = st_crs(leges_sf)) 


sb <- leges_sf |> 
  filter(house == "Assembly" )

this <- st_join(sb, mke, left = FALSE)

top <- c(
  22,
  23,
  24,
  12,
  11,
  10,
  13,
  17,
  16,
  19,
  18
)


this_top <- this |> 
  filter(district %in% top)

this_bottom <- this |> 
  filter(!district %in% top)
reps_labs_top <- glue('<li>{this_top$district} — {this_top$name}</li>') |> 
  paste(collapse = "\n")

reps_labs_bottom <- glue('<li>{this_bottom$district} — {this_bottom$name}</li>') |> 
  paste(collapse = "\n")


bb <- st_bbox(st_union(this))

sb_lab <- st_centroid(this)


map <- leaflet(this, options = leafletOptions(zoomSnap = .1, zoomControl = FALSE)) |> 
  # addMapboxTiles(style_id = "light-v11",
  #                username = "mapbox", 
  #                scaling_factor = "0.5x") |> 
  addMapboxTiles(style_id = "streets-v12",
                 username = "mapbox", 
                 scaling_factor = "2x") |> 
  addPolygons(color = cfc_darkblue, 
              fillOpacity = .25, opacity = 1,
              weight = 2) |> 
  addPolygons(data = mke,
              fill = NA,
              opacity = 1,
              color = "white",
              weight = 2) |>
  addLabelOnlyMarkers(data = sb_lab,
                      label = ~district,
                      labelOptions = labelOptions(
                        noHide = TRUE,
                        direction = "center",
                        style = list(
                          "font-weight" = "bold", 
                          "font-family" = "Verdana",
                          "color" = "white",
                          "background-color" = alpha(cfc_darkblue, .75),
                          "border" = glue("1px solid {cfc_darkblue}"),                
                          "border-radius" = "4px"
                        ),
                        textsize = "10px"
                      )) |> 
  addControl(
    html = glue("<div style='font-family: Verdana;font-size: .7em;'>",
                "<h2 style='color: {cfc_darkblue};", 
                "text-align: center;'>Assembly Districts</h2>",
                "<ul style='list-style-type: none;'>{reps_labs_top}</ul>",
                "</div>"),
    position = "topleft" # Position of the title
  ) |> 
  addControl(
    html = glue("<div style='font-family: Verdana;font-size: .7em;'>",
                "<ul style='list-style-type: none;'>{reps_labs_bottom}</ul>",
                "</div>"),
    position = "bottomleft",  # Position of the title
  ) |> 
  fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])

map


saveWidget(map, "data/leaflet_map.html", selfcontained = TRUE)

# Convert the HTML file to a PNG
webshot("data/leaflet_map.html", vwidth = 2550/5, vheight = 4200/5,
        file = "plots/assembly_map.png", zoom = 2)
