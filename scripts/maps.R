library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(htmltools)

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

valid_country_codes <- world_raw %>%
  st_drop_geometry() %>%
  distinct(join_iso_a2, name)

check_country_mapping <- function(
    country_counts,
    count_column,
    dataset_label) {

  count_column <- rlang::ensym(count_column)

  input_total <- country_counts %>%
    summarise(
      total = sum(
        !!count_column,
        na.rm = TRUE
      )
    ) %>%
    pull(total)

  unmatched <- country_counts %>%
    anti_join(
      valid_country_codes,
      by = c(
        "country_code" = "join_iso_a2"
      )
    )

  unmatched_total <- unmatched %>%
    summarise(
      total = sum(
        !!count_column,
        na.rm = TRUE
      )
    ) %>%
    pull(total)

  plotted_total <- input_total - unmatched_total

  message(dataset_label, ":")
  message("  Input total: ", input_total)
  message("  Plotted total: ", plotted_total)
  message("  Unmatched total: ", unmatched_total)

  if (nrow(unmatched) > 0) {
    message("  Unmatched country codes:")
    print(unmatched)

    warning(
      dataset_label,
      " contains country codes that are not represented on the map.",
      call. = FALSE
    )
  }

  invisible(
    list(
      input_total = input_total,
      plotted_total = plotted_total,
      unmatched = unmatched
    )
  )
}

make_country_map <- function(
    country_counts,
    count_column,
    count_label,
    view = c("africa", "global")) {

  view <- match.arg(view)
  count_column <- rlang::ensym(count_column)

  map_data <- world_raw %>%
    left_join(
      country_counts,
      by = c(
        "join_iso_a2" = "country_code"
      )
    ) %>%
    mutate(
      count = replace_na(
        !!count_column,
        0L
      ),
      count_fill = if_else(
        count == 0,
        NA_real_,
        as.numeric(count)
      )
    )

  has_mapped_data <- any(!is.na(map_data$count_fill))

  if (has_mapped_data) {
    palette <- colorBin(
      palette = "YlOrRd",
      domain = map_data$count_fill,
      bins = c(1, 2, 5, 10, 20, 50, Inf),
      na.color = "#f0f0f0"
    )

    map_data <- map_data %>%
      mutate(fill_colour = palette(count_fill))
  } else {
    palette <- NULL
    map_data <- map_data %>%
      mutate(fill_colour = "#f0f0f0")
  }

  map <- leaflet(map_data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~fill_colour,
      fillOpacity = 0.85,
      color = "gray70",
      weight = 0.5,
      label = ~paste0(
        "<strong>",
        name,
        "</strong><br/>",
        count_label,
        ": ",
        count
      ) %>%
        lapply(HTML),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#333333",
        bringToFront = TRUE
      )
    )

  if (has_mapped_data) {
    map <- map %>%
      addLegend(
        pal = palette,
        values = ~count_fill,
        title = paste(
          count_label,
          "per country"
        ),
        position = "bottomright",
        labFormat = function(type, cuts, p) {
          if (type == "bin") {
            labels <- paste0(
              cuts[-length(cuts)],
              " – ",
              cuts[-1]
            )
            labels[length(labels)] <- paste0(
              cuts[length(cuts) - 1],
              "+"
            )
            labels
          } else {
            leaflet::labelFormat()(type, cuts, p)
          }
        }
      )
  }

  if (view == "africa") {
    map %>%
      setView(lng = 20, lat = 2, zoom = 3)
  } else {
    map %>%
      setView(lng = 10, lat = 10, zoom = 2)
  }
}
