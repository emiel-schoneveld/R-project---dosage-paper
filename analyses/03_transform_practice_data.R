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
data_logs_lesson_dosetotal <- readRDS(
  here('output/data_logs_lesson_dosetotal.rds')
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
    lesson_dose_accurate = TotalLessonItems,
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
    # Exclude all lesson on the midmeasurement
    lesson_date > wordreading_date_pre,
    lesson_date != wordreading_date_mid,
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
    data_logs_lesson_dosetotal
  ) |> 
  mutate(
    # For rowreading set total to total words presented
    lesson_dose_total = if_else(
      lesson_form == "ResearchRowRead",
      lesson_dose_accurate,
      lesson_dose_total
    ),
    
    # For rowreading set accuracy to words read accurately first time
    lesson_dose_accurate = if_else(
      lesson_form == "ResearchRowRead",
      CorrectWord,
      lesson_dose_accurate
    ),
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
    session_dose_accurate = sum(lesson_dose_accurate, na.rm = T),
    session_dose_total = sum(lesson_dose_total, na.rm = T),
    session_length_readingtime = sum(lesson_length, na.rm = T)
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

## Add semester variable ----
### Add measurement dates
data_logs_session <- data_logs_session |> 
  left_join(
    data_wordreading
  )

### Add variable that denotes in which semester the session occured
data_logs_session <- data_logs_session |> 
  mutate(
    session_semester = case_when(
      # Exclude all sessions on the test dates
      session_date == wordreading_date_pre ~ NA,
      session_date == wordreading_date_mid ~ NA,
      session_date == wordreading_date_post ~ NA,
      
      # Add semester
      session_date < wordreading_date_pre ~ NA,
      session_date < wordreading_date_mid ~ 'semester_1',
      session_date < wordreading_date_post ~ 'semester_2',
      session_date > wordreading_date_post ~ NA,
    )
  )

### Filter out all sessions not in a valid semester ----
data_logs_session <- data_logs_session |> 
  filter(
    !is.na(session_semester)
  )

## Compute student level means ----
### Splitted per semester ----
#### Student means ----
data_logs_student_semester_nofreq_nodur <- data_logs_session |> 
  group_by(
    student_ID,
    session_semester
  ) |> 
  summarise(
    practice_length = mean(session_length),
    practice_dose_accurate = mean(session_dose_accurate),
    practice_dose_total = mean(session_dose_total),
    practice_cii_time = sum(session_length)/60,
    practice_cii_readingtime = sum(session_length_readingtime)/60,
    practice_cii_words_accurate = sum(session_dose_accurate),
    practice_cii_words_total = sum(session_dose_total),
    practice_accuracy = practice_cii_words_accurate / practice_cii_words_total
  ) |> 
  ungroup()

#### Compute and add frequency and number of weeks
data_logs_student_semester_long <- data_logs_session |> 
  group_by(
    student_ID,
    session_semester,
    session_week
  ) |> 
  summarise(
    practice_frequency = n()
  ) |> 
  ungroup() |> 
  group_by(
    student_ID,
    session_semester
  ) |> 
  summarise(
    practice_frequency = mean(practice_frequency, na.rm = T),
    practice_duration = n()
  ) |> 
  right_join(
    y = data_logs_student_semester_nofreq_nodur
  )

#### Pivot wider
data_logs_student_semester_wide <- data_logs_student_semester_long |> 
  relocate(
    student_ID, session_semester
  ) |> 
  pivot_wider(
    names_from = session_semester,
    values_from = contains('practice'),
  )

### Not splitted per semester ----
#### Student means ----
data_logs_student_nofreq_nodur <- data_logs_session |> 
  group_by(
    student_ID,
    # session_semester
  ) |> 
  summarise(
    practice_length = mean(session_length),
    practice_dose_accurate = mean(session_dose_accurate),
    practice_dose_total = mean(session_dose_total),
    practice_cii_time = sum(session_length)/60,
    practice_cii_readingtime = sum(session_length_readingtime)/60,
    practice_cii_words_accurate = sum(session_dose_accurate),
    practice_cii_words_total = sum(session_dose_total),
    practice_accuracy = practice_cii_words_accurate / practice_cii_words_total
  ) |> 
  ungroup()

#### Compute and add frequency and number of weeks
data_logs_student <- data_logs_session |> 
  group_by(
    student_ID,
    # session_semester,
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
data_practice <- data_logs_student_semester_wide |> 
  full_join(
    data_logs_student
  )

## Save data ----
saveRDS(
  data_practice,
  here("output/data_practice.rds")
)
