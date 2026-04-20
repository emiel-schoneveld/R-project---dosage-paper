## Transform practice data
# By Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(lavaan)
library(here)

# Load data ----
## Data from practice logs
data_logs_old <- read_xlsx(
  here('input/logs_lessons_anonymous.xlsx')
)

# Set global variables ----
## Start and end of year
start_of_semester = as.Date("2023-08-20")
end_of_semester = as.Date("2024-07-20")

## Session length maximum and minimum
min_session_length = 2
max_session_length = 60

# Transform data_logs ----
## Rename columns ----
data_logs_lesson <- data_logs_old |> 
  rename(
    student_ID = Leerlingnummer,
    lesson_dose = TotalLessonItems,
    lesson_form = LessonType
  )

## Recode missing values ----
data_logs_lesson <- data_logs_lesson |> 
  mutate(
    EndDate = na_if(EndDate, "NULL")
  )

## Filter lesson data ----
data_logs_lesson <- data_logs_lesson |> 
  filter(
    lesson_form %in% c("ResearchFlits", "Flits", "ResearchRowRead", "RowRead"),
    !is.na(EndDate),
    StartDate >= start_of_semester,
    EndDate <= end_of_semester
  )

## Convert strings into date and time class ----
data_logs_lesson <- data_logs_lesson |> 
  mutate(
    lesson_date = as.Date.character(CreationDate),
    lesson_time_start = substring(StartDate, 0, 19) |> as.POSIXlt.character(tz = "CET"),
    lesson_time_end = substring(EndDate, 0, 19) |> as.POSIXlt.character(tz = "CET")
  )

## Compute lesson duration ----
data_logs_lesson <- data_logs_lesson |> 
  mutate(
    lesson_length = difftime(lesson_time_end, lesson_time_start,  units = "mins") |> as.numeric()
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
    session_dose = sum(lesson_dose, na.rm = T),
    session_length_readingtime = sum(lesson_length, na.rm = T)
  ) |> 
  ungroup()

## Compute session length
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
data_logs_student <- data_logs_session |> 
  group_by(
    student_ID
  ) |> 
  summarise(
    practice_length = mean(session_length, na.rm = T),
    practice_dose = mean(session_dose, na.rm = T),
    practice_cii_time = sum(session_length, na.rm = T)/60,
    practice_cii_readingtime = sum(session_length_readingtime, na.rm = T)/60,
    practice_cii_words = sum(session_dose, na.rm = T)/1000,
  ) |> 
  ungroup()

### Compute and add frequency and number of weeks ----
#### Add week number
data_logs_session <- data_logs_session |> 
  mutate(
    session_week = strftime(session_starttime, format = "%V") |> as.character()
  )

### Compute and add frequency and number of weeks
data_logs_student <- data_logs_session |> 
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
    y = data_logs_student
  )

## Save data ----
saveRDS(
  data_logs_student,
  here("output/data_logs_student.rds")
)


