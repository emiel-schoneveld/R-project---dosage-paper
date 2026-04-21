### Combine data
# By Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(here)

# Load data ----
## Test data
data_wordreading <- readRDS(
  here('output/data_wordreading.rds')
)

## Characteristics data
data_characteristics <- readRDS(
  here('output/data_characteristics.rds')
)

## Practice data
data_logs_student <- readRDS(
  here('output/data_logs_student.rds')
)
data_logs_student_semester <- readRDS(
  here('output/data_logs_student_semester.rds')
)

# Combine data_logs_student, data_wordreading and data_characteristics ----
## Combine
data_yearly <- data_logs_student |> 
  left_join(
    data_characteristics
  ) |> 
  left_join(
    data_wordreading
  )
data_semester <- data_logs_student_semester |> 
  left_join(
    data_characteristics
  ) |> 
  left_join(
    data_wordreading
  )

## Columnbind total and semester data into one tibble
data <- data_yearly |>  
  full_join(
    data_semester
  )

## Write
saveRDS(
  data,
  here("output/data.rds")
)
