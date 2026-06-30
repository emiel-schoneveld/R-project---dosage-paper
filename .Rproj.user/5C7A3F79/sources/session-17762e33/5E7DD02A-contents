## Transform words data to be smaller file for git saving
# By Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)

# Load data ----
## Data from words
data_logs_words <- read.csv(
  "C:/Users/RSCHONE/researchdrive/RICDE-FMG-3578-Flits-Tutorlezen (Projectfolder)/data_analyse/Oefendata logs Flits! Tutorlezen/originals/df_words.csv"
) |> as_tibble() |> 
  rename(
    'CourseProgressId' = courseprogessid
  ) 

## Data from practice logs
data_logs_raw <- read_xlsx(
  here('input/logs_lessons_anonymous.xlsx')
) |> 
  dplyr::select(
    CourseProgressId, Duration, LessonType
  )

# Join duration to data_logs_words ----
data_logs_words <- data_logs_words |> 
  left_join(
    data_logs_raw
  )

# Filter lessons based on duration
data_logs_words <- data_logs_words |> 
  filter(
    Duration > 0,
    Duration < 15 * 60
  )

# Filter words with zero tries
data_logs_words <- data_logs_words |> 
  filter(
    tries > 0
  )

# Filter Exam lessons
data_logs_words <- data_logs_words |> 
  filter(
    LessonType != 'Exam'
  )

# Add exposures and accurate attempts per word ----
data_logs_words <- data_logs_words |> 
  mutate(
    # Exposures
    exposures = case_when(
      # DMT: one exposure per word
      (LessonType == "DMT") ~ 1,
      # RowRead: one exposure per word
      str_detect(LessonType, 'RowRead') ~ 1,
      # Flits: multiple exposures per word
      str_detect(LessonType, 'Flits') ~ tries
    ),
    
    # Accuracay at first attempt
    accurate_firsttry = case_when(
      # DMT: accurate if one try, inaccurate if multiple tries
      (LessonType == 'DMT') & (tries == 1) ~ 1,
      (LessonType == 'DMT') & (tries > 1) ~ 0,
      # RowRead: accurate if one try, inaccurate if multiple tries
      str_detect(LessonType, 'RowRead') & (tries == 1) ~ 1,
      str_detect(LessonType, 'RowRead') & (tries > 1) ~ 0,
      # Flits: accurate if one try and no audioplays, inaccurate if multiple tries or any audioplays
      str_detect(LessonType, 'Flits') & ((tries == 1) & (audioplays == 0)) ~ 1,
      str_detect(LessonType, 'Flits') & ((tries > 1) | (audioplays > 0)) ~ 0,
    ),
    
    # Accurate at any attempt
    accurate_anytry = case_when(
      # DMT: accurate if progressed to next word before third attempt, unclear if third attempt is accurate or not because software does not register more than 3 tries
      (LessonType == 'DMT') & (tries < 3) ~ 1,
      (LessonType == 'DMT') & (tries == 3) ~ NA,
      # RowRead: accurate if one try, inaccurate if multiple tries
      str_detect(LessonType, 'RowRead') & (tries == 1) ~ 1,
      str_detect(LessonType, 'RowRead') & (tries > 1) ~ 0,
      # Flits: accurate if no audioplays, inaccurate if any audioplays
      str_detect(LessonType, 'Flits') & (audioplays == 0) ~ 1,
      str_detect(LessonType, 'Flits') & (audioplays > 0) ~ 0,
    ),
  )

## Check tries, exposure and accuracy combinations for lessontypes
data_logs_words |> 
  filter(
    str_detect(LessonType, 'Flits'),
    # tries != 1,
    audioplays != 0,
    # exposures != 1,
    # accurate_firsttry == 1,
    accurate_anytry != 0,
    # accurate_firsttry != accurate_anytry,
    # is.na(accurate_firsttry)
    # is.na(accurate_anytry)
  ) |> 
  dplyr::select(!contains('ours'))

# Summarise data ----
data_logs_lesson_dose <- data_logs_words |> 
  group_by(
    CourseProgressId
  ) |> 
  summarise(
    lesson_dose_exposures = sum(exposures, na.rm = T),
    lesson_dose_unique = n(),
    lesson_dose_audioplays = sum(audioplays, na.rm = T),
    lesson_dose_accurate_firsttry = sum(accurate_firsttry, na.rm = T),
    lesson_dose_accurate_anytry = sum(accurate_anytry, na.rm = T),
  )

# Save data ----
saveRDS(
  data_logs_lesson_dose,
  here("output/data_logs_lesson_dose.rds")
)
