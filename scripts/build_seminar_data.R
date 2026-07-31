library(tidyverse)
library(lubridate)

source("scripts/helpers.R")
source("scripts/read_zoom_reports.R")
source("scripts/maps.R")

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

data_directory <- "data-raw"
zoom_timezone <- "Europe/Dublin"

shared_link_name_pattern <- "^Laurah\\b"
include_host_in_attendance <- TRUE

# Keep organiser email addresses in an ignored local file rather than in this
# public script. See config-organisers.example.R.
organiser_config_file <- "config-organisers.R"

if (file.exists(organiser_config_file)) {
  source(organiser_config_file)
}

if (!exists("organiser_emails")) {
  organiser_emails <- character()
  message(
    "No organiser email configuration found. ",
    "Audience metrics currently include organisers."
  )
}

organiser_emails <- clean_email(organiser_emails)
organiser_emails <- organiser_emails[!is.na(organiser_emails)]

notetaker_pattern <- regex(
  paste(
    c(
      "otter\\.ai",
      "\\botter\\b",
      "fireflies\\.ai",
      "\\bfireflies\\b",
      "fathom",
      "read\\.ai",
      "\\bread ai\\b",
      "\\bread assistant\\b",
      "\\bnotetaker\\b",
      "\\bnote taker\\b",
      "\\bmeeting notes\\b",
      "\\bmeeting assistant\\b",
      "\\bai notes\\b",
      "\\bai notetaker\\b",
      "\\bai note taker\\b",
      "tl;dv",
      "\\btldv\\b",
      "\\bgrain bot\\b",
      "\\bgrain notetaker\\b",
      "\\bmeetgeek\\b",
      "\\bavoma\\b",
      "\\bsembly\\b",
      "\\bsupernormal\\b"
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# -------------------------------------------------------------------------
# Find reports
#
# Registration reports are matched to participant reports by Zoom meeting ID,
# not by date. This allows one cumulative registration report to be reused for
# every occurrence of a recurring meeting.
# -------------------------------------------------------------------------

registration_files <- tibble(
  registration_file = list.files(
    data_directory,
    pattern = "^registration_.*\\.csv$",
    full.names = TRUE
  )
) %>%
  mutate(
    meeting_id = map_chr(
      registration_file,
      extract_meeting_id
    ),
    registration_file_date = map_chr(
      registration_file,
      ~ as.character(extract_file_date(.x))
    ) %>%
      as.Date()
  )

participant_files <- tibble(
  participant_file = list.files(
    data_directory,
    pattern = "^participants_.*\\.csv$",
    full.names = TRUE
  )
) %>%
  mutate(
    meeting_id = map_chr(
      participant_file,
      extract_meeting_id
    ),
    file_date = map_chr(
      participant_file,
      ~ as.character(extract_file_date(.x))
    ) %>%
      as.Date()
  )

if (nrow(registration_files) == 0) {
  stop(
    "No registration reports were found in data-raw/.",
    call. = FALSE
  )
}

if (nrow(participant_files) == 0) {
  stop(
    "No participant reports were found in data-raw/.",
    call. = FALSE
  )
}

duplicate_registration_ids <- registration_files %>%
  count(meeting_id) %>%
  filter(n > 1)

if (nrow(duplicate_registration_ids) > 0) {
  message(
    "Meeting IDs with multiple registration reports: ",
    paste(duplicate_registration_ids$meeting_id, collapse = ", ")
  )

  stop(
    paste(
      "More than one registration report was found for at least one",
      "Zoom meeting ID. Keep only the most recent complete export."
    ),
    call. = FALSE
  )
}

duplicate_participant_occurrences <- participant_files %>%
  count(meeting_id, file_date) %>%
  filter(n > 1)

if (nrow(duplicate_participant_occurrences) > 0) {
  message(
    "Meeting occurrences with multiple participant reports were found."
  )

  stop(
    paste(
      "More than one participant report was found for at least one",
      "meeting ID and date. Remove duplicate downloads."
    ),
    call. = FALSE
  )
}

# -------------------------------------------------------------------------
# Read each registration report once
# -------------------------------------------------------------------------

registration_reports <- registration_files %>%
  mutate(
    metadata = map(
      registration_file,
      read_registration_metadata,
      timezone = zoom_timezone
    ),
    registrations_raw = map(
      registration_file,
      read_registration_report,
      timezone = zoom_timezone
    )
  ) %>%
  unnest(metadata)

# Each participant report is one seminar occurrence. A recurring meeting can
# therefore produce several rows linked to the same registration report.
seminar_data <- participant_files %>%
  left_join(
    registration_reports,
    by = "meeting_id"
  )

missing_registration_reports <- seminar_data %>%
  filter(is.na(registration_file))

if (nrow(missing_registration_reports) > 0) {
  message(
    "Participant reports without a matching registration report were found."
  )

  stop(
    "Some participant reports could not be matched by Zoom meeting ID.",
    call. = FALSE
  )
}

unused_registration_reports <- registration_reports %>%
  anti_join(participant_files, by = "meeting_id")

if (nrow(unused_registration_reports) > 0) {
  message(
    "Registration reports without participant reports: ",
    paste(basename(unused_registration_reports$registration_file), collapse = ", ")
  )
}

seminar_data <- seminar_data %>%
  mutate(
    session_start = map2_dbl(
      file_date,
      scheduled_time_template,
      ~ as.numeric(
        combine_date_and_time(
          .x,
          .y,
          zoom_timezone
        )
      )
    ) %>%
      as.POSIXct(
        origin = "1970-01-01",
        tz = zoom_timezone
      ),
    session_end = session_start +
      minutes(scheduled_duration_minutes),
    participant_report = map(
      participant_file,
      read_participant_report,
      notetaker_pattern = notetaker_pattern
    ),
    participants = map(
      participant_report,
      "participants"
    ),
    excluded_notetakers = map(
      participant_report,
      "excluded_notetakers"
    ),
    excluded_notetaker_rows = map_int(
      excluded_notetakers,
      nrow
    )
  ) %>%
  arrange(session_start) %>%
  group_by(meeting_id) %>%
  mutate(
    occurrence_number = row_number(),
    occurrence_total = n(),
    seminar_display_title = if_else(
      occurrence_total > 1,
      paste0(
        seminar_title,
        " (Session ",
        occurrence_number,
        " of ",
        occurrence_total,
        ")"
      ),
      seminar_title
    )
  ) %>%
  ungroup() %>%
  mutate(seminar_id = row_number())

# -------------------------------------------------------------------------
# Build a series-wide email-to-country lookup from every registration report
# -------------------------------------------------------------------------

all_reported_countries <- registration_reports %>%
  select(
    meeting_id,
    registrations_raw
  ) %>%
  unnest(registrations_raw) %>%
  filter(!is.na(reported_country_code))

country_conflicts <- all_reported_countries %>%
  distinct(email, reported_country_code) %>%
  count(email, name = "number_of_countries") %>%
  filter(number_of_countries > 1)

if (nrow(country_conflicts) > 0) {
  warning(
    paste(
      "Some email addresses have more than one reported country.",
      "The country from the most recent registration will be used."
    ),
    call. = FALSE
  )

  message(
    "Conflicting country values were found for ",
    nrow(country_conflicts),
    " email address(es)."
  )
}

country_lookup <- all_reported_countries %>%
  arrange(email, desc(registration_time)) %>%
  group_by(email) %>%
  summarise(
    lookup_country_code = first_non_missing(
      reported_country_code
    ),
    .groups = "drop"
  )

enrich_registrations <- function(
    registration_data,
    country_lookup) {

  registration_data %>%
    left_join(country_lookup, by = "email") %>%
    mutate(
      country_code = coalesce(
        reported_country_code,
        lookup_country_code
      ),
      country_source = case_when(
        !is.na(reported_country_code) ~
          "Reported for this registration",
        is.na(reported_country_code) &
          !is.na(lookup_country_code) ~
          "Matched from another seminar registration",
        TRUE ~ "Unknown"
      ),
      country_backfilled =
        is.na(reported_country_code) &
        !is.na(lookup_country_code)
    ) %>%
    select(
      email,
      registration_time,
      country_code,
      country_source,
      country_backfilled,
      country_collected
    )
}

registration_reports <- registration_reports %>%
  mutate(
    registrations_enriched = map(
      registrations_raw,
      enrich_registrations,
      country_lookup = country_lookup
    )
  )

# Replace the raw registration list attached to each occurrence with its
# enriched version, then retain only people registered by that session's end.
seminar_data <- seminar_data %>%
  select(-registrations_raw) %>%
  left_join(
    registration_reports %>%
      select(meeting_id, registrations_enriched),
    by = "meeting_id"
  ) %>%
  mutate(
    registrations = map2(
      registrations_enriched,
      session_end,
      ~ .x %>%
        filter(
          is.na(registration_time) |
            registration_time <= .y
        )
    ),
    final_registration_total = map_int(
      registrations_enriched,
      nrow
    ),
    total_registrants = map_int(
      registrations,
      nrow
    ),
    registrants_with_country = map_int(
      registrations,
      ~ sum(!is.na(.x$country_code))
    ),
    registrants_backfilled = map_int(
      registrations,
      ~ sum(.x$country_backfilled)
    ),
    country_collected = map_lgl(
      registrations_enriched,
      ~ any(.x$country_collected)
    ),
    attendance = map2(
      participants,
      registrations,
      prepare_attendance,
      shared_link_name_pattern = shared_link_name_pattern,
      include_host_in_attendance = include_host_in_attendance
    )
  ) %>%
  group_by(meeting_id) %>%
  arrange(session_start, .by_group = TRUE) %>%
  mutate(
    new_registrants_since_previous = total_registrants -
      lag(total_registrants, default = 0L)
  ) %>%
  ungroup() %>%
  arrange(session_start)

# -------------------------------------------------------------------------
# Series registration maps
#
# Each person is counted once even where one registration report supports
# several occurrences of a recurring meeting.
# -------------------------------------------------------------------------

all_registrations <- registration_reports %>%
  select(meeting_id, registrations_enriched) %>%
  unnest(registrations_enriched)

series_registrants <- all_registrations %>%
  arrange(email, desc(registration_time)) %>%
  group_by(email) %>%
  summarise(
    country_code = first_non_missing(country_code),
    .groups = "drop"
  )

total_unique_registrants <- nrow(series_registrants)

registration_country_counts <- series_registrants %>%
  filter(!is.na(country_code)) %>%
  count(country_code, name = "registrants")

registration_check <- check_country_mapping(
  registration_country_counts,
  registrants,
  "Series registrations"
)

mapped_unique_registrants <- registration_check$plotted_total

interactive_map_registrants_africa <- make_country_map(
  registration_country_counts,
  registrants,
  "Registrants",
  view = "africa"
)

interactive_map_registrants_global <- make_country_map(
  registration_country_counts,
  registrants,
  "Registrants",
  view = "global"
)

# -------------------------------------------------------------------------
# Attendance summaries and maps
# -------------------------------------------------------------------------

seminar_data <- seminar_data %>%
  mutate(
    total_attendees = map_int(
      attendance,
      nrow
    ),
    shared_link_attendees = map_int(
      attendance,
      ~ sum(
        .x$attendance_source == "Shared joining link"
      )
    ),
    attendees_with_country = map_int(
      attendance,
      ~ sum(!is.na(.x$country_code))
    ),
    attendees_with_backfilled_country = map_int(
      attendance,
      ~ sum(
        .x$country_backfilled,
        na.rm = TRUE
      )
    ),
    attendees_mapped = map_int(
      attendance,
      ~ sum(
        .x$country_code %in%
          valid_country_codes$join_iso_a2,
        na.rm = TRUE
      )
    ),
    attendance_country_counts = map(
      attendance,
      ~ .x %>%
        filter(!is.na(country_code)) %>%
        count(
          country_code,
          name = "attendees"
        )
    ),
    map_africa = map(
      attendance_country_counts,
      ~ make_country_map(
        .x,
        attendees,
        "Attendees",
        view = "africa"
      )
    ),
    map_global = map(
      attendance_country_counts,
      ~ make_country_map(
        .x,
        attendees,
        "Attendees",
        view = "global"
      )
    )
  )

# -------------------------------------------------------------------------
# Overall audience attendance and repeat engagement
#
# Audience metrics exclude configured organisers, hosts and AI notetakers.
# Shared-link attendees remain in individual session totals, but cannot be
# included in unique or repeat-attendance metrics because they lack stable
# identifiers.
# -------------------------------------------------------------------------

attendance_long <- seminar_data %>%
  select(
    seminar_id,
    meeting_id,
    seminar_title,
    seminar_display_title,
    occurrence_number,
    occurrence_total,
    session_start,
    attendance
  ) %>%
  unnest(attendance) %>%
  mutate(
    organiser = !is.na(email) & email %in% organiser_emails,
    identifiable = !is.na(email)
  )

audience_attendance <- attendance_long %>%
  filter(
    attendance_source != "Host",
    !organiser
  )

identifiable_audience_attendance <- audience_attendance %>%
  filter(identifiable) %>%
  distinct(email, seminar_id, .keep_all = TRUE)

unique_audience_attendees <- identifiable_audience_attendance %>%
  arrange(email, desc(session_start)) %>%
  distinct(email, .keep_all = TRUE)

total_unique_audience_attendees <- nrow(unique_audience_attendees)

overall_attendance_country_counts <- unique_audience_attendees %>%
  filter(!is.na(country_code)) %>%
  count(country_code, name = "attendees")

overall_attendance_check <- check_country_mapping(
  overall_attendance_country_counts,
  attendees,
  "Series attendance"
)

mapped_unique_audience_attendees <- overall_attendance_check$plotted_total

interactive_map_attendance_africa <- make_country_map(
  overall_attendance_country_counts,
  attendees,
  "Attendees",
  view = "africa"
)

interactive_map_attendance_global <- make_country_map(
  overall_attendance_country_counts,
  attendees,
  "Attendees",
  view = "global"
)

repeat_attendance_by_person <- identifiable_audience_attendance %>%
  group_by(email) %>%
  summarise(
    sessions_attended = n_distinct(seminar_id),
    topics_attended = n_distinct(meeting_id),
    .groups = "drop"
  )

repeat_session_attendees <- repeat_attendance_by_person %>%
  filter(sessions_attended >= 2)

cross_topic_attendees <- repeat_attendance_by_person %>%
  filter(topics_attended >= 2)

total_repeat_session_attendees <- nrow(repeat_session_attendees)
total_cross_topic_attendees <- nrow(cross_topic_attendees)

repeat_session_percentage <- if (
  total_unique_audience_attendees > 0
) {
  100 * total_repeat_session_attendees /
    total_unique_audience_attendees
} else {
  NA_real_
}

cross_topic_percentage <- if (
  total_unique_audience_attendees > 0
) {
  100 * total_cross_topic_attendees /
    total_unique_audience_attendees
} else {
  NA_real_
}

recurring_topic_engagement <- seminar_data %>%
  filter(occurrence_total > 1) %>%
  distinct(meeting_id, seminar_title, occurrence_total) %>%
  mutate(
    attendance_distribution = map(
      meeting_id,
      function(current_meeting_id) {
        identifiable_audience_attendance %>%
          filter(meeting_id == current_meeting_id) %>%
          group_by(email) %>%
          summarise(
            sessions_attended = n_distinct(seminar_id),
            .groups = "drop"
          ) %>%
          count(
            sessions_attended,
            name = "attendees"
          ) %>%
          complete(
            sessions_attended = seq_len(
              first(
                seminar_data$occurrence_total[
                  seminar_data$meeting_id == current_meeting_id
                ]
              )
            ),
            fill = list(attendees = 0L)
          )
      }
    ),
    attendees_two_or_more = map_int(
      attendance_distribution,
      ~ sum(.x$attendees[.x$sessions_attended >= 2])
    ),
    attendees_all_sessions = map2_int(
      attendance_distribution,
      occurrence_total,
      ~ sum(.x$attendees[.x$sessions_attended == .y])
    )
  )


# -------------------------------------------------------------------------
# Programme summary
# -------------------------------------------------------------------------

total_sessions <- nrow(seminar_data)
total_topics <- n_distinct(seminar_data$meeting_id)

# -------------------------------------------------------------------------
# Console checks
# -------------------------------------------------------------------------

walk(
  seq_len(nrow(seminar_data)),
  function(i) {
    seminar <- seminar_data[i, ]

    message("")
    message(seminar$seminar_display_title, ":")
    message(
      "  Registered by this session: ",
      seminar$total_registrants
    )
    message(
      "  New registrations since previous occurrence: ",
      seminar$new_registrants_since_previous
    )
    message(
      "  Final cumulative registration total: ",
      seminar$final_registration_total
    )
    message(
      "  Registrant countries backfilled: ",
      seminar$registrants_backfilled
    )
    message(
      "  AI notetaker rows excluded: ",
      seminar$excluded_notetaker_rows
    )
    message(
      "  Attendees counted: ",
      seminar$total_attendees
    )
    message(
      "  Additional shared-link attendees: ",
      seminar$shared_link_attendees
    )
    message(
      "  Attendee countries backfilled: ",
      seminar$attendees_with_backfilled_country
    )
    message(
      "  Attendees represented on map: ",
      seminar$attendees_mapped
    )
    message(
      "  Attendees not represented on map: ",
      seminar$total_attendees - seminar$attendees_mapped
    )

    if (seminar$total_attendees > seminar$attendees_mapped) {
      warning(
        seminar$seminar_display_title,
        " has attendees who cannot be represented on the country map.",
        call. = FALSE
      )
    }

    check_country_mapping(
      seminar$attendance_country_counts[[1]],
      attendees,
      seminar$seminar_display_title
    )
  }
)

generated_on <- format(Sys.Date(), "%d %b %Y")
