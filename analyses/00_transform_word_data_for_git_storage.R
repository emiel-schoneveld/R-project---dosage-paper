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
    CourseProgressId, Duration
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

# Summarise data ----
data_logs_lesson_dose <- data_logs_words |> 
  mutate(
    accurate_firsttry = if_else(
      tries == 1 & audioplays == 0,
      1,
      0
    ),
    accurate_anytry = if_else(
      audioplays == 0,
      1,
      0
    )
  ) |> 
  group_by(
    CourseProgressId
  ) |> 
  summarise(
    lesson_dose_exposures = sum(tries, na.rm = T),
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

