library(tidyverse)
library(lubridate)

clean_email <- function(x) {
  x %>%
    str_trim() %>%
    str_to_lower() %>%
    na_if("")
}

clean_name <- function(x) {
  x %>%
    str_squish() %>%
    na_if("")
}

extract_file_date <- function(file) {
  date_text <- str_extract(
    basename(file),
    "\\d{4}_\\d{2}_\\d{2}"
  )

  if (is.na(date_text)) {
    stop(
      "Could not find a YYYY_MM_DD date in filename: ",
      basename(file),
      call. = FALSE
    )
  }

  as.Date(date_text, format = "%Y_%m_%d")
}

extract_meeting_id <- function(file) {
  match <- str_match(
    basename(file),
    "^(?:registration|participants)_([0-9]+)_"
  )

  meeting_id <- match[, 2]

  if (is.na(meeting_id)) {
    stop(
      "Could not find a Zoom meeting ID in filename: ",
      basename(file),
      call. = FALSE
    )
  }

  meeting_id
}

parse_zoom_datetime <- function(x, timezone) {
  parsed <- suppressWarnings(
    mdy_hms(x, tz = timezone, quiet = TRUE)
  )

  missing <- is.na(parsed) & !is.na(x)

  if (any(missing)) {
    parsed[missing] <- suppressWarnings(
      mdy_hm(x[missing], tz = timezone, quiet = TRUE)
    )
  }

  parsed
}

combine_date_and_time <- function(date, datetime_template, timezone) {
  as.POSIXct(
    paste(
      format(date, "%Y-%m-%d"),
      format(datetime_template, "%H:%M:%S")
    ),
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
}

first_non_missing <- function(x) {
  values <- x[!is.na(x) & x != ""]

  if (length(values) == 0) {
    NA_character_
  } else {
    values[[1]]
  }
}

earliest_datetime <- function(x, timezone) {
  if (all(is.na(x))) {
    as.POSIXct(NA, tz = timezone)
  } else {
    min(x, na.rm = TRUE)
  }
}
