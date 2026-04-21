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
data_logs_lesson_dosetotal <- data_logs_words |> 
  rename(
    'CourseProgressId' = courseprogessid
  ) |> 
  group_by(
    CourseProgressId
  ) |> 
  summarise(
    lesson_dose_total = sum(tries, na.rm = T)
  )

# Save data ----
saveRDS(
  data_logs_lesson_dosetotal,
  here("output/data_logs_lesson_dosetotal.rds")
)
