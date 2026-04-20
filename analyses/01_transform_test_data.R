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
    wordreading_score_post = Woordleestoets_vaardigheidsscore_post,
    wordreading_date_pre = Woordleestoets_datum_pre,
    wordreading_date_mid = Woordleestoets_datum_mid,
    wordreading_date_post = Woordleestoets_datum_post,
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
  pivot_longer(
    contains('date'),
    names_prefix = 'wordreading_date_',
    names_to = 'measurement_date',
    values_to = 'date_raw'
  ) |>
  filter(
    measurement_score == measurement_version,
    measurement_score == measurement_date
  ) |>
  rename(
    'measurement' = measurement_score
  ) |>
  dplyr::select(
    !contains('measurement_')
  )
    
## Filter on test version ----
data_wordreading_long <- data_wordreading_long |>
  filter(
    version == "DMT"
  ) |> 
  dplyr::select(
    !version
  )

## Convert date to date class ----
data_wordreading_long <- data_wordreading_long |> 
  mutate(
    # Split raw strings
    date_string = if_else(
      str_detect(date_raw, '-'),
      date_raw,
      NA
    ),
    date_numeric = if_else(
      !str_detect(date_raw, '-'),
      date_raw,
      NA
    ) |> as.numeric(),
    
    # Recode impossible dates
    date_string = recode(
      # recode impossible dates
      date_string,
      '29-02-2023' = '28-02-2023',
      .default = date_string
    ),
    
    # Convert to date class
    date_string_dateclass = dmy(date_string),
    date_numeric_dateclass = as.Date(date_numeric, origin = "1899-12-30"),
    
    # Combine both date columns into one
    date = if_else(
      !is.na(date_numeric_dateclass),
      date_numeric_dateclass,
      date_string_dateclass
    )
  ) |> 
  dplyr::select(
    !contains('date_')
  )

## Pivot wider ----
data_wordreading <- data_wordreading_long |> 
  pivot_wider(
    names_from = measurement,
    values_from = score:date,
    names_glue = "wordreading_{.value}_{measurement}"
  )

## Save data ----
saveRDS(
  data_wordreading,
  here("output/data_wordreading.rds")
)
