library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(htmltools)

registrations <- read_csv(
  "data-raw/registration_81910771239_2026_05_06.csv",
  skip = 5,
  show_col_types = FALSE
)

country_counts <- registrations %>%
  filter(!is.na(`Country/Region`)) %>%
  mutate(`Country/Region` = str_to_upper(`Country/Region`)) %>%
  count(`Country/Region`, name = "registrants")

world_raw <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  filter(name != "Antarctica") %>%
  mutate(
    join_iso_a2 = case_when(
      name == "France" ~ "FR",
      TRUE ~ iso_a2
    )
  )

world_counts <- world_raw %>%
  left_join(
    country_counts,
    by = c("join_iso_a2" = "Country/Region")
  ) %>%
  mutate(
    registrants = replace_na(registrants, 0L),
    registrants_fill = if_else(
      registrants == 0,
      NA_real_,
      as.numeric(registrants)
    )
  )

# Check whether all registrants are represented on the map
input_registrants <- sum(country_counts$registrants, na.rm = TRUE)

plotted_registrants <- world_counts %>%
  st_drop_geometry() %>%
  summarise(total = sum(registrants, na.rm = TRUE)) %>%
  pull(total)

unmatched_countries <- country_counts %>%
  anti_join(
    world_raw %>%
      st_drop_geometry() %>%
      select(join_iso_a2, name),
    by = c("Country/Region" = "join_iso_a2")
  )

message("Input registrants: ", input_registrants)
message("Plotted registrants: ", plotted_registrants)
message("Unmatched registrants: ", input_registrants - plotted_registrants)

if (nrow(unmatched_countries) > 0) {
  message("Unmatched country codes:")
  print(unmatched_countries)
  
  warning(
    "Some registrants are not plotted. Check unmatched_countries.",
    call. = FALSE
  )
}

total_registrants <- plotted_registrants
generated_on <- format(Sys.Date(), "%d %b %Y")

pal <- colorBin(
  palette = "YlOrRd",
  domain = world_counts$registrants_fill,
  bins = c(1, 2, 5, 10, 20, 50, Inf),
  na.color = "#f0f0f0"
)

map_polygons <- function(map) {
  map %>%
    addPolygons(
      fillColor = ~pal(registrants_fill),
      fillOpacity = 0.85,
      color = "gray70",
      weight = 0.5,
      label = ~paste0(
        "<strong>", name, "</strong><br/>",
        "Registrants: ", registrants
      ) %>%
        lapply(HTML),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#333333",
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~registrants_fill,
      title = "Registrants per country",
      position = "bottomright"
    )
}

interactive_map_africa <- leaflet(world_counts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 20, lat = 2, zoom = 3) %>%
  map_polygons()

interactive_map_global <- leaflet(world_counts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 10, lat = 10, zoom = 2) %>%
  map_polygons()