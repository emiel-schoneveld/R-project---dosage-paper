### Transform data
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
## Student test data
data_wordreading_raw <- read_xlsx(
  here('input/data_toetsgegevens.xlsx')
)

# Transform data_wordreading ----
## Rename columns 
data_wordreading_wide <- data_wordreading_raw |> 
  rename(
    student_ID = Leerlingnummer,
    wordreading_version_pre = Woordleestoets_versie_pre,
    wordreading_version_mid = Woordleestoets_versie_mid,
    wordreading_version_post = Woordleestoets_versie_post,
    wordreading_score_pre = Woordleestoets_vaardigheidsscore_pre,
    wordreading_score_mid = Woordleestoets_vaardigheidsscore_mid,
    wordreading_score_post = Woordleestoets_vaardigheidsscore_post
  )

## Convert columns to numeric
data_wordreading_wide <- data_wordreading_wide |> 
  mutate(
    wordreading_score_pre = wordreading_score_pre |> as.numeric(),
    wordreading_score_mid = wordreading_score_mid |> as.numeric(),
    wordreading_score_post = wordreading_score_post |> as.numeric()
  )

## Select columns ----
data_wordreading_wide <- data_wordreading_wide |> 
  dplyr::select(
    # select student ID
    student_ID, 
    
    # Select all wordreading variables of pre, mid and post measurement
    contains('wordreading') & (contains('pre') | contains('mid') | contains('post'))
  )

## Transform into long format ----
data_wordreading_long <- data_wordreading_wide |>
  pivot_longer(
    contains('score'),
    names_prefix = "wordreading_score_",
    names_to = 'measurement_score',
    values_to = 'score'
  ) |>
  pivot_longer(
    contains('version'),
    names_prefix = "wordreading_version_",
    names_to = 'measurement_version',
    values_to = 'version'
  ) |>
  filter(
    measurement_score == measurement_version
  ) |>
  rename(
    'measurement' = measurement_score
  ) |>
  dplyr::select(
    !measurement_version
  )
    
## Filter on test version ----
data_wordreading_long <- data_wordreading_long |>
  filter(
    version == "DMT"
  ) |> 
  dplyr::select(
    !version
  )

data_wordreading_wide <- data_wordreading_wide |>
  filter(
    wordreading_version_pre == "DMT",      
    wordreading_version_mid == "DMT",
    wordreading_version_post == "DMT"
  )

## Pivort wider ----
data_wordreading <- data_wordreading_long |> 
  pivot_wider(
    names_from = measurement,
    values_from = score,
    names_prefix = 'wordreading_'
  )

## Save data ----
saveRDS(
  data_wordreading,
  here("output/data_wordreading.rds")
)

