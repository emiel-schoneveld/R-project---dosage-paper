## Transform practice data
# By Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)

# Load data ----
## Data from practice logs
data_logs_raw <- read_xlsx(
  here('input/logs_lessons_anonymous.xlsx')
)

## Wordreading data
data_logs_lesson_dose <- readRDS(
  here('output/data_logs_lesson_dose.rds')
)

## Wordreading data
data_wordreading <- readRDS(
  here('output/data_wordreading.rds')
)

# Set global variables ----
## Start and end of year
start_of_schoolyear = as.Date("2023-08-20")
end_of_schoolyear = as.Date("2024-07-20")

## Session length maximum and minimum
min_session_length = 2
max_session_length = 60

# Transform data_logs ----
## Rename columns ----
data_logs_lesson <- data_logs_raw |> 
  rename(
    student_ID = Leerlingnummer,
    lesson_form = LessonType
  )

## Add DMT date info ----
data_logs_lesson <- data_logs_lesson |> 
  left_join(
    data_wordreading
  )

## Recode missing values ----
data_logs_lesson <- data_logs_lesson |> 
  mutate(
    EndDate = na_if(EndDate, "NULL")
  )

## Filter out all nonvalid lesson types ----
data_logs_lesson <- data_logs_lesson |> 
  filter(
    lesson_form %in% c("ResearchFlits", "Flits", "ResearchRowRead", "RowRead")
  )

## Convert strings into date and time class ----
data_logs_lesson <- data_logs_lesson |> 
  mutate(
    lesson_date = as.Date.character(CreationDate),
    lesson_time_start = substring(StartDate, 0, 19) |> as.POSIXlt.character(tz = "CET"),
    lesson_time_end = substring(EndDate, 0, 19) |> as.POSIXlt.character(tz = "CET")
  )

## Filter out lessons with invalid dates ----
data_logs_lesson <- data_logs_lesson |> 
  filter(
    # Delete all lessons that do not have start or enddate
    !is.na(EndDate),
    !is.na(StartDate),
    !is.na(lesson_date),
    
    # Delete all lessons outside the semester
    lesson_date >= start_of_schoolyear,
    lesson_date <= end_of_schoolyear,
    
    # Include all lessons after the premeasurement or before the postmeasurement
    lesson_date > wordreading_date_pre,
    lesson_date < wordreading_date_post
  )

## Compute lesson duration ----
data_logs_lesson <- data_logs_lesson |> 
  mutate(
    lesson_length = difftime(lesson_time_end, lesson_time_start,  units = "mins") |> as.numeric()
  )

## Add total words read and total words presented ----
data_logs_lesson <- data_logs_lesson |> 
  left_join(
    data_logs_lesson_dose
  ) |> 
  mutate(
    # For rowreading set tries to unique
    lesson_dose_tries = if_else(
      lesson_form == "ResearchRowRead",
      lesson_dose_unique,
      lesson_dose_tries
    )
  )

## Summarise logs per session ----
data_logs_session <- data_logs_lesson |> 
  group_by(
    student_ID,
    lesson_date
  ) |> 
  summarise(
    session_starttime = min(lesson_time_start, na.rm = T),
    session_endtime = max(lesson_time_end, na.rm = T),
    session_length_readingtime = sum(lesson_length, na.rm = T),
    session_dose_tries = sum(lesson_dose_tries, na.rm = T),
    session_dose_unique = sum(lesson_dose_unique, na.rm = T),
    session_dose_accurate = sum(lesson_dose_accurate, na.rm = T),
    session_dose_audioplays = sum(lesson_dose_audioplays, na.rm = T),
  ) |> 
  ungroup() |> 
  mutate( # Add week number
    session_week = strftime(session_starttime, format = "%V") |> as.character()
  ) |> 
  rename(
    'session_date' = lesson_date
  )

## Compute session length ----
data_logs_session <- data_logs_session |> 
  mutate(
    session_length = difftime(session_endtime, session_starttime, units = "mins") |> as.numeric()
  )

## Filter by session length ----
data_logs_session <- data_logs_session |> 
  filter(
    session_length >= min_session_length,
    session_length <= max_session_length,
  )

## Compute student level variables ----
### Session length, words read, accuracy and total time ----
data_logs_student_nofreq_nodur <- data_logs_session |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    # Time dimensions
    practice_length = mean(session_length, na.rm = T),
    
    # Words read
    practice_cii_words_tries = sum(session_dose_tries, na.rm = T),
    practice_cii_words_unique = sum(session_dose_unique, na.rm = T),
    practice_cii_words_accurate = sum(session_dose_accurate, na.rm = T),
    practice_cii_words_audioplays = sum(session_dose_audioplays, na.rm = T),
    
    # Time total
    practice_cii_time = sum(session_length)/60,
    practice_cii_readingtime = sum(session_length_readingtime)/60,
  ) |> 
  mutate(
    # Accuracy
    practice_accuracy = practice_cii_words_accurate / practice_cii_words_unique,
    
    # Incorrect attempts
    practice_cii_words_attempts = practice_cii_words_tries - practice_cii_words_unique
  ) |> 
  ungroup()

### Frequency and duration (number of weeks) ----
data_logs_student_wide <- data_logs_session |> 
  group_by(
    student_ID,
    session_week
  ) |> 
  summarise(
    practice_frequency = n()
  ) |> 
  ungroup() |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    practice_frequency = mean(practice_frequency, na.rm = T),
    practice_duration = n()
  ) |> 
  right_join(
    y = data_logs_student_nofreq_nodur
  )

## Combine data into one wide dataset ----
data_practice <- data_logs_student_wide

## Save data ----
saveRDS(
  data_practice,
  here("output/data_practice.rds")
)

