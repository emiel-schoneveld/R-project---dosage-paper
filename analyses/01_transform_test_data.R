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
data_wordreading_all <- read_xlsx(
  here('input/data_toetsgegevens.xlsx')
)

# Transform data_wordreading ----
## Rename columns and convert to numeric ----
data_wordreading_all <- data_wordreading_all |> 
  rename(
    student_ID = Leerlingnummer,
    wordreading_version_pre = Woordleestoets_versie_pre,
    wordreading_version_post = Woordleestoets_versie_post,
    wordreading_score_pre = Woordleestoets_vaardigheidsscore_pre,
    wordreading_score_post = Woordleestoets_vaardigheidsscore_post
  ) |> 
  mutate(
    wordreading_score_pre = wordreading_score_pre |> as.numeric(),
    wordreading_score_post = wordreading_score_post |> as.numeric()
  )

## Select columns ----
data_wordreading_all <- data_wordreading_all |> 
  dplyr::select(
    student_ID,
    contains('wordreading') & (contains('pre') | contains('post'))
  )

## Remove all values that are not a DMT and filter outliers ----
### For premeasurement
data_wordreading_pre <- data_wordreading_all |> 
  dplyr::select(
    student_ID,
    contains('pre')
  ) |> 
  filter(
    wordreading_version_pre == 'DMT',
    wordreading_score_pre <= 120,
  ) |> 
  dplyr::select(
    !contains('version')
  )

## Join filtered post measurement with pre measurement ----
data_wordreading <- data_wordreading_all |> 
  dplyr::select(
    student_ID,
    contains('post')
  ) |> 
  filter(
    wordreading_version_post == 'DMT',
    wordreading_score_post <= 120,
  ) |> 
  dplyr::select(
    !contains('version')
  ) |> 
  full_join(
    data_wordreading_pre
  )

## Save data ----
saveRDS(
  data_wordreading,
  here("output/data_wordreading.rds")
)

