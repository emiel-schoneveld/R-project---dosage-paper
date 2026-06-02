# Data cleaning
# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)
library(ggplot2)

# Load data ----
data <- readRDS(
  here::here('output/data.rds')
)

# Remove outliers ----
## Identify outliers and missing data ----
### DMT ----
data <- data |> 
  mutate(
    wordreading_missing_pre = is.na(wordreading_score_pre),
    wordreading_missing_post = is.na(wordreading_score_post),
    wordreading_invalid_pre = wordreading_score_pre > 120,
    wordreading_invalid_post = wordreading_score_post > 120,
    wordreading_outlier_pre = abs(
      wordreading_score_pre - mean(wordreading_score_pre, na.rm = T)
    ) >= 3.3*sd(wordreading_score_pre, na.rm = T),
    wordreading_outlier_post = abs(
      wordreading_score_post - mean(wordreading_score_post, na.rm = T)
    ) >= 3.3*sd(wordreading_score_post, na.rm = T),
  )

#### Summarise outliers
data |> 
  summarise(
    perc_missing_pre = 100*sum(wordreading_missing_pre, na.rm = T)/n(),
    perc_missing_post = 100*sum(wordreading_missing_post, na.rm = T)/n(),
    
    perc_invalid_pre = 100*sum(wordreading_invalid_pre, na.rm = T)/n(),
    perc_invalid_post = 100*sum(wordreading_invalid_post, na.rm = T)/n(),
    
    perc_outlier_pre = 100*sum(wordreading_outlier_pre, na.rm = T)/n(),
    perc_outlier_post = 100*sum(wordreading_outlier_post, na.rm = T)/n(),
  ) #|> View()

#### Remove invalid and outliers
data <- data |> 
  mutate(
    wordreading_score_pre = if_else(
      wordreading_invalid_pre | wordreading_outlier_pre,
      NA,
      wordreading_score_pre
    ),
    wordreading_score_post = if_else(
      wordreading_invalid_post | wordreading_outlier_post,
      NA,
      wordreading_score_post
    ),
  )

#### Inspect valid scores
data |> 
  summarise(
    perc_valid_pre = 100 * sum(!is.na(wordreading_score_pre), na.rm = T) / n(),
    perc_valid_post = 100 * sum(!is.na(wordreading_score_post), na.rm = T) / n(),
  )

# Filter 2 control participants based on condition ----
data <- data |> 
  filter(
    condition != 'control'
  )

# filter participants with less than 10 sessions ----
data <- data |> 
  filter(
    practice_cii_sessions >= 20
  )