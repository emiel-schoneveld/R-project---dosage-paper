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
data_logs_words <- read.csv(
  "C:/Users/RSCHONE/researchdrive/RICDE-FMG-3578-Flits-Tutorlezen (Projectfolder)/data_analyse/Oefendata logs Flits! Tutorlezen/originals/df_words.csv"
)

# Summarise data ----
data_logs_lesson_dose <- data_logs_words |> 
  rename(
    'CourseProgressId' = courseprogessid
  ) |> 
  mutate(
    accurate = if_else(
      tries == 1 & audioplays == 0,
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
    lesson_dose_accurate = sum(accurate, na.rm = T)
  )

# Save data ----
saveRDS(
  data_logs_lesson_dose,
  here("output/data_logs_lesson_dose.rds")
)

