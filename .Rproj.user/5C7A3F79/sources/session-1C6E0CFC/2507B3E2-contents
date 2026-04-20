### Combine data
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
## Test data
data_wordreading <- readRDS(
  here('output/data_wordreading.rds')
)

## Characteristics data
data_characteristics <- readRDS(
  here('output/data_characteristics.rds')
)

## Characteristics data
data_logs_student <- readRDS(
  here('output/data_logs_student.rds')
)

# Combine data_logs_student, data_wordreading and data_characteristics ----
## Combine
data <- data_logs_student |> 
  left_join(
    data_characteristics
  ) |> 
  left_join(
    data_wordreading
  )

## Write
saveRDS(
  data,
  here("output/data_transformed.rds")
)


