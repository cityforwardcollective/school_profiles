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
  filter(house == "MPS School Board" )

reps_labs <- glue('<li>{sb$district} — {sb$name}</li>') |> 
  paste(collapse = "\n")

sb_noal <- sb |> 
  filter(district != "At-Large")

bb <- st_bbox(st_union(sb))

sb_lab <- st_centroid(sb_noal)


map <- leaflet(sb_noal, options = leafletOptions(zoomSnap = .1, zoomControl = FALSE)) |> 
  # addMapboxTiles(style_id = "light-v11",
  #                username = "mapbox", 
  #                scaling_factor = "0.5x") |> 
  addMapboxTiles(style_id = "streets-v12",
                 username = "mapbox", 
                 scaling_factor = "2x") |> 
  # addPolygons(weight = 2) |> 
  addPolygons(color = cfc_darkblue, 
              fillOpacity = .25, opacity = 1,
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
                          "background-color" = cfc_darkblue, # Semi-transparent white background
                          "border" = glue("1px solid {cfc_darkblue}"),                 # Border color and size
                          "border-radius" = "4px"
                          ),
                        textsize = "14px"
                      )) |> 
  addControl(
    html = glue("<div style='font-family: Verdana;'>",
                "<h2 style='color: {cfc_darkblue};", 
                "text-align: center;'>School Board Districts</h2>",
                "<ul style='list-style-type: none;'>{reps_labs}</ul>",
                "</div>"),
    position = "topright" # Position of the title
  ) |> 
  fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])

map


saveWidget(map, "data/leaflet_map.html", selfcontained = TRUE)

# Convert the HTML file to a PNG
webshot("data/leaflet_map.html", vwidth = 2550/5, vheight = 4200/5,
        file = "plots/leaflet_map.png", zoom = 2)
