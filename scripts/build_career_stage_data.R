library(tidyverse)

# This script expects scripts/build_seminar_data.R to have been sourced first.
# It reuses the cleaned registration and attendance objects created there.

if (!exists("registration_reports")) {
  stop(
    "Source scripts/build_seminar_data.R before scripts/build_career_stage_data.R.",
    call. = FALSE
  )
}

if (!exists("identifiable_audience_attendance")) {
  stop(
    "Identifiable attendance data were not found. ",
    "Source scripts/build_seminar_data.R first.",
    call. = FALSE
  )
}

# -------------------------------------------------------------------------
# Standardise ECR responses
# -------------------------------------------------------------------------

standardise_ecr <- function(x) {
  case_when(
    str_detect(
      x,
      regex("^yes\\b", ignore_case = TRUE)
    ) ~ "Yes",

    str_detect(
      x,
      regex("^no\\b", ignore_case = TRUE)
    ) ~ "No",

    str_detect(
      x,
      regex("^not sure\\b", ignore_case = TRUE)
    ) ~ "Not sure",

    TRUE ~ NA_character_
  )
}

# -------------------------------------------------------------------------
# Registration responses
#
# read_zoom_reports.R has already:
# - filtered rows using Zoom approval status
# - cleaned email addresses
# - parsed registration times
# - detected whether career-stage and ECR questions were collected
# -------------------------------------------------------------------------

career_registration_data <- registration_reports %>%
  select(
    meeting_id,
    registration_file_date,
    registrations_raw
  ) %>%
  unnest(registrations_raw) %>%
  mutate(
    ecr_status = standardise_ecr(ecr_response)
  )

question_availability_by_topic <- career_registration_data %>%
  group_by(meeting_id) %>%
  summarise(
    career_stage_collected = any(career_stage_collected),
    ecr_collected = any(ecr_collected),
    .groups = "drop"
  )

career_stage_reports <- registration_reports %>%
  select(
    meeting_id,
    registration_file_date,
    registrations_raw
  ) %>%
  mutate(
    career_stage_collected = map_lgl(
      registrations_raw,
      ~ any(.x$career_stage_collected)
    ),
    ecr_collected = map_lgl(
      registrations_raw,
      ~ any(.x$ecr_collected)
    )
  ) %>%
  filter(career_stage_collected | ecr_collected) %>%
  select(
    meeting_id,
    registration_file_date,
    career_stage_collected,
    ecr_collected
  ) %>%
  arrange(registration_file_date)

career_data_start_date <- if (
  nrow(career_stage_reports) > 0
) {
  min(career_stage_reports$registration_file_date)
} else {
  as.Date(NA)
}

# -------------------------------------------------------------------------
# Unique registrant summaries
#
# Where a person registered more than once, use their most recent available
# response. Each person is counted once.
# -------------------------------------------------------------------------

latest_ecr_response <- career_registration_data %>%
  filter(
    ecr_collected,
    !is.na(ecr_status)
  ) %>%
  arrange(
    email,
    desc(registration_time),
    desc(registration_file_date)
  ) %>%
  distinct(email, .keep_all = TRUE)

latest_career_stage_response <- career_registration_data %>%
  filter(
    career_stage_collected,
    !is.na(career_stage)
  ) %>%
  arrange(
    email,
    desc(registration_time),
    desc(registration_file_date)
  ) %>%
  distinct(email, .keep_all = TRUE)

ecr_respondents <- nrow(latest_ecr_response)

ecr_yes <- sum(
  latest_ecr_response$ecr_status == "Yes"
)

ecr_no <- sum(
  latest_ecr_response$ecr_status == "No"
)

ecr_not_sure <- sum(
  latest_ecr_response$ecr_status == "Not sure"
)

ecr_percentage <- if (ecr_respondents > 0) {
  100 * ecr_yes / ecr_respondents
} else {
  NA_real_
}

ecr_summary <- latest_ecr_response %>%
  count(
    ecr_status,
    name = "people"
  ) %>%
  mutate(
    percentage = 100 * people / sum(people)
  ) %>%
  arrange(
    factor(
      ecr_status,
      levels = c("Yes", "No", "Not sure")
    )
  )

career_stage_respondents <- nrow(
  latest_career_stage_response
)

career_stage_summary <- latest_career_stage_response %>%
  count(
    career_stage,
    name = "people",
    sort = TRUE
  ) %>%
  mutate(
    percentage = 100 * people / sum(people)
  )

# -------------------------------------------------------------------------
# Attendee summaries
#
# Restrict this analysis to identifiable audience attendees from topics whose
# registration form collected the relevant question. Then match those people
# by cleaned email address to their most recent available response.
#
# This gives a profile of actual attendees during the period in which these
# questions were collected, without counting the same person more than once.
# -------------------------------------------------------------------------

attendance_with_question_flags <- identifiable_audience_attendance %>%
  left_join(
    question_availability_by_topic,
    by = "meeting_id"
  )

ecr_attendee_emails <- attendance_with_question_flags %>%
  filter(
    replace_na(ecr_collected, FALSE)
  ) %>%
  distinct(email)

career_stage_attendee_emails <- attendance_with_question_flags %>%
  filter(
    replace_na(career_stage_collected, FALSE)
  ) %>%
  distinct(email)

attendee_ecr <- ecr_attendee_emails %>%
  inner_join(
    latest_ecr_response %>%
      select(
        email,
        ecr_status
      ),
    by = "email"
  )

attendee_career_stage <- career_stage_attendee_emails %>%
  inner_join(
    latest_career_stage_response %>%
      select(
        email,
        career_stage
      ),
    by = "email"
  )

ecr_attendee_respondents <- nrow(attendee_ecr)

ecr_attendee_yes <- sum(
  attendee_ecr$ecr_status == "Yes"
)

ecr_attendee_no <- sum(
  attendee_ecr$ecr_status == "No"
)

ecr_attendee_not_sure <- sum(
  attendee_ecr$ecr_status == "Not sure"
)

ecr_attendee_percentage <- if (
  ecr_attendee_respondents > 0
) {
  100 * ecr_attendee_yes /
    ecr_attendee_respondents
} else {
  NA_real_
}

ecr_attendee_summary <- attendee_ecr %>%
  count(
    ecr_status,
    name = "people"
  ) %>%
  mutate(
    percentage = 100 * people / sum(people)
  ) %>%
  arrange(
    factor(
      ecr_status,
      levels = c("Yes", "No", "Not sure")
    )
  )

career_stage_attendee_respondents <- nrow(
  attendee_career_stage
)

career_stage_attendee_summary <- attendee_career_stage %>%
  count(
    career_stage,
    name = "people",
    sort = TRUE
  ) %>%
  mutate(
    percentage = 100 * people / sum(people)
  )

# -------------------------------------------------------------------------
# Console checks
# -------------------------------------------------------------------------

message(
  "Career-stage/ECR information available from ",
  format(career_data_start_date, "%d %b %Y"),
  "."
)

message(
  "Unique registrants with ECR response: ",
  ecr_respondents
)

message(
  "Unique ECR registrants answering Yes: ",
  ecr_yes
)

message(
  "Unique attendees with ECR response: ",
  ecr_attendee_respondents
)

message(
  "Unique ECR attendees answering Yes: ",
  ecr_attendee_yes
)

message(
  "Unique registrants with career-stage response: ",
  career_stage_respondents
)

message(
  "Unique attendees with career-stage response: ",
  career_stage_attendee_respondents
)
