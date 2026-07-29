library(tidyverse)
library(lubridate)

read_registration_metadata <- function(file, timezone) {
  metadata <- read_csv(
    file,
    skip = 2,
    n_max = 1,
    show_col_types = FALSE
  )

  required_columns <- c(
    "Topic",
    "Scheduled Time",
    "Duration (minutes)"
  )
  missing_columns <- setdiff(required_columns, names(metadata))

  if (length(missing_columns) > 0) {
    stop(
      "Registration report ",
      basename(file),
      " is missing metadata columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  metadata %>%
    transmute(
      seminar_title = Topic %>%
        str_remove(
          regex(
            "^Bioconductor Africa Seminar Series:\\s*",
            ignore_case = TRUE
          )
        ),
      scheduled_time_template = parse_zoom_datetime(
        `Scheduled Time`,
        timezone
      ),
      scheduled_duration_minutes = suppressWarnings(
        as.numeric(`Duration (minutes)`)
      )
    )
}

read_registration_report <- function(file, timezone) {
  # Rows 1 to 5 contain Zoom report metadata.
  # Row 6 contains the attendee column headings.
  registration_data <- read_csv(
    file,
    skip = 5,
    show_col_types = FALSE
  )

  required_columns <- c("Email", "Registration Time")
  missing_columns <- setdiff(required_columns, names(registration_data))

  if (length(missing_columns) > 0) {
    stop(
      "Registration report ",
      basename(file),
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  country_collected <- "Country/Region" %in% names(registration_data)

  if (!country_collected) {
    message(
      basename(file),
      ": Country/Region was not collected."
    )

    registration_data <- registration_data %>%
      mutate(`Country/Region` = NA_character_)
  }

  if ("Approval Status" %in% names(registration_data)) {
    approval_status <- registration_data$`Approval Status` %>%
      replace_na("") %>%
      str_trim()

    approved_row <- approval_status == "" |
      str_detect(
        approval_status,
        regex("approved", ignore_case = TRUE)
      )

    excluded_count <- sum(!approved_row)

    if (excluded_count > 0) {
      message(
        basename(file),
        ": excluded ",
        excluded_count,
        " registration row(s) not marked as approved."
      )
    }

    registration_data <- registration_data[approved_row, , drop = FALSE]
  }

  registration_data %>%
    transmute(
      email = clean_email(Email),
      registration_time = parse_zoom_datetime(
        `Registration Time`,
        timezone
      ),
      reported_country_code = `Country/Region` %>%
        str_trim() %>%
        str_to_upper() %>%
        na_if(""),
      country_collected = country_collected
    ) %>%
    filter(!is.na(email)) %>%
    group_by(email) %>%
    summarise(
      registration_time = earliest_datetime(
        registration_time,
        timezone
      ),
      reported_country_code = first_non_missing(
        reported_country_code
      ),
      country_collected = first(country_collected),
      .groups = "drop"
    )
}

read_participant_report <- function(file, notetaker_pattern) {
  participant_data <- read_csv(
    file,
    show_col_types = FALSE
  )

  required_columns <- c(
    "Name (original name)",
    "Email",
    "Duration (minutes)"
  )

  missing_columns <- setdiff(
    required_columns,
    names(participant_data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Participant report ",
      basename(file),
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  cleaned_data <- participant_data %>%
    transmute(
      name = clean_name(`Name (original name)`),
      email = clean_email(Email),
      duration_minutes = suppressWarnings(
        as.numeric(`Duration (minutes)`)
      )
    ) %>%
    filter(
      !is.na(name) | !is.na(email),
      !is.na(duration_minutes),
      duration_minutes > 0
    ) %>%
    mutate(
      possible_notetaker =
        str_detect(
          replace_na(name, ""),
          notetaker_pattern
        ) |
        str_detect(
          replace_na(email, ""),
          notetaker_pattern
        )
    )

  excluded_notetakers <- cleaned_data %>%
    filter(possible_notetaker) %>%
    select(name, email, duration_minutes)

  included_participants <- cleaned_data %>%
    filter(!possible_notetaker) %>%
    select(name, email, duration_minutes)

  message(
    basename(file),
    ": excluded ",
    nrow(excluded_notetakers),
    " possible AI notetaker row(s)."
  )

  # The excluded rows remain available in the returned object for manual
  # inspection, but are not printed during Quarto rendering.

  list(
    participants = included_participants,
    excluded_notetakers = excluded_notetakers
  )
}

prepare_attendance <- function(
    participant_data,
    registration_data,
    shared_link_name_pattern,
    include_host_in_attendance = FALSE) {

  participant_data <- participant_data %>%
    mutate(
      shared_link_row = str_detect(
        replace_na(name, ""),
        regex(
          shared_link_name_pattern,
          ignore_case = TRUE
        )
      )
    )

  normal_attendees <- participant_data %>%
    filter(!shared_link_row) %>%
    mutate(
      attendee_key = case_when(
        !is.na(email) ~ paste0("email:", email),
        !is.na(name) ~ paste0(
          "name:",
          str_to_lower(name)
        ),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(attendee_key)) %>%
    group_by(attendee_key) %>%
    summarise(
      name = first_non_missing(name),
      email = first_non_missing(email),
      duration_minutes = sum(
        duration_minutes,
        na.rm = TRUE
      ),
      attendance_source = "Zoom attendee",
      .groups = "drop"
    )

  shared_link_rows <- participant_data %>%
    filter(shared_link_row) %>%
    arrange(desc(duration_minutes))

  if (nrow(shared_link_rows) > 0) {
    host_row <- shared_link_rows %>%
      slice(1) %>%
      transmute(
        attendee_key = "host:laurah",
        name,
        email,
        duration_minutes,
        attendance_source = "Host"
      )

    additional_shared_link_attendees <- shared_link_rows %>%
      slice(-1) %>%
      mutate(shared_number = row_number()) %>%
      transmute(
        attendee_key = paste0(
          "shared-link:",
          shared_number
        ),
        name = paste0(
          "Shared-link attendee ",
          shared_number
        ),
        email = NA_character_,
        duration_minutes,
        attendance_source = "Shared joining link"
      )
  } else {
    host_row <- tibble(
      attendee_key = character(),
      name = character(),
      email = character(),
      duration_minutes = numeric(),
      attendance_source = character()
    )

    additional_shared_link_attendees <- host_row
  }

  attendance <- bind_rows(
    normal_attendees,
    host_row,
    additional_shared_link_attendees
  ) %>%
    left_join(
      registration_data,
      by = "email"
    ) %>%
    mutate(
      country_code = if_else(
        attendance_source == "Shared joining link",
        NA_character_,
        country_code
      ),
      country_source = if_else(
        attendance_source == "Shared joining link",
        "Unknown",
        replace_na(country_source, "Unknown")
      ),
      country_backfilled = if_else(
        attendance_source == "Shared joining link",
        FALSE,
        replace_na(country_backfilled, FALSE)
      )
    )

  if (!include_host_in_attendance) {
    attendance <- attendance %>%
      filter(attendance_source != "Host")
  }

  attendance
}
