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

# Filter data ----
## Filter out 2 control participants based on condition
data <- data |> 
  filter(
    condition != 'control'
  )

# Remove outliers ----
## Identify outliers and missing data ----
### DMT ----
data <- data |> 
  mutate(
    DMT_missing_pre = is.na(fluency_DMT_pre),
    DMT_missing_post = is.na(fluency_DMT_post),
   
    discrete_missing_pre = is.na(fluency_discrete_pre),
    discrete_missing_post = is.na(fluency_discrete_post),
    
    serial_missing_pre = is.na(fluency_serial_pre),
    serial_missing_post = is.na(fluency_serial_post),
    
    LED_missing_pre = is.na(fluency_LED_pre),
    LED_missing_post = is.na(fluency_LED_post),
    
    pseudo_missing_pre = is.na(fluency_pseudo_pre),
    pseudo_missing_post = is.na(fluency_pseudo_post),
  )

#### Summarise outliers
data |> 
  summarise(
    perc_missing_DMT_pre = 100*sum(DMT_missing_pre, na.rm = T)/n(),
    perc_missing_DMT_post = 100*sum(DMT_missing_post, na.rm = T)/n(),
    
    perc_missing_discrete_pre = 100*sum(discrete_missing_pre, na.rm = T)/n(),
    perc_missing_discrete_post = 100*sum(discrete_missing_post, na.rm = T)/n(),
    
    perc_missing_serial_pre = 100*sum(serial_missing_pre, na.rm = T)/n(),
    perc_missing_serial_post = 100*sum(serial_missing_post, na.rm = T)/n(),
    
    perc_missing_LED_pre = 100*sum(LED_missing_pre, na.rm = T)/n(),
    perc_missing_LED_post = 100*sum(LED_missing_post, na.rm = T)/n(),
    
    perc_missing_pseudo_pre = 100*sum(pseudo_missing_pre, na.rm = T)/n(),
    perc_missing_pseudo_post = 100*sum(pseudo_missing_post, na.rm = T)/n(),
  ) |> 
  pivot_longer(
    everything(),
    names_prefix = 'perc_',
    names_sep = '_',
    names_to = c('type', 'measure', 'pre/post'),
    values_to = 'percentage'
  )

#### Remove invalid and outliers
data_cleaned <- data

#### Inspect valid scores
data_cleaned |> 
  summarise(
    perc_valid_pre_DMT = 100 * sum(!is.na(fluency_DMT_pre), na.rm = T) / n(),
    perc_valid_post_DMT = 100 * sum(!is.na(fluency_DMT_post), na.rm = T) / n(),
    perc_valid_pre_discrete = 100 * sum(!is.na(fluency_discrete_pre), na.rm = T) / n(),
    perc_valid_post_discrete = 100 * sum(!is.na(fluency_discrete_post), na.rm = T) / n(),
    perc_valid_pre_serial = 100 * sum(!is.na(fluency_serial_pre), na.rm = T) / n(),
    perc_valid_post_serial = 100 * sum(!is.na(fluency_serial_post), na.rm = T) / n(),
    perc_valid_pre_LED = 100 * sum(!is.na(fluency_LED_pre), na.rm = T) / n(),
    perc_valid_post_LED = 100 * sum(!is.na(fluency_LED_post), na.rm = T) / n(),
    perc_valid_pre_pseudo = 100 * sum(!is.na(fluency_pseudo_pre), na.rm = T) / n(),
    perc_valid_post_pseudo = 100 * sum(!is.na(fluency_pseudo_post), na.rm = T) / n(),
  )

# Filtering data based on accuracy
data_cleaned |> 
  filter(
    accuracy_anytry < .80
  ) |> nrow()
data_cleaned <- data_cleaned |> 
  filter(
    accuracy_anytry >= .79
  )

# Write cleaned data ----
saveRDS(
  data_cleaned,
  here("output/data_cleaned.rds")
)
