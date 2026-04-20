# Descriptive analyses 
# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(lavaan)
library(here)

# Load data ----
data <- readRDS(
  here('output/data_transformed.rds')
)

# Descriptive analysis ----
## Summary ----
summary(data)

## Correlation table
data |> 
  dplyr::select(
    where(is.numeric)
  ) |> 
  cor(
    use = "pairwise.complete.obs"
  ) #|> View()

data |> 
  dplyr::select(
    contains('practice'),
    contains('wordreading')
  ) |> 
  cor(
    use = "pairwise.complete.obs"
  ) #|> View()

data |> 
  group_by(grade) |> 
  summarise(
    mean(wordreading_score_pre, na.rm = T)
  )
