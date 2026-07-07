### Transform data
# By Emiel Schoneveld

# General syntax ----
## Clear environment
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
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
    wordreading_version_post = Woordleestoets_versie_post,
    wordreading_score_pre = Woordleestoets_vaardigheidsscore_pre,
    wordreading_score_post = Woordleestoets_vaardigheidsscore_post,
    wordreading_date_pre = Woordleestoets_datum_pre,
    wordreading_date_post = Woordleestoets_datum_post,
  )

## Convert columns to numeric
data_wordreading_wide <- data_wordreading_wide |> 
  mutate(
    wordreading_score_pre = wordreading_score_pre |> as.numeric(),
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

## Add missing dates ----
### Check for which respondents a date is missing
data_wordreading |> 
  filter(
    is.na(wordreading_date_pre) |
      is.na(wordreading_date_post)
  )

### Add school_ID
data_wordreading <- data_wordreading |> 
  mutate(
    school_ID = str_extract(student_ID, "^[^-]+")
  )

### Set dates for imputing if no school mean is present
day_before_start_of_schoolyear = as.Date("2023-08-19")
day_after_end_of_schoolyear = as.Date("2024-07-21")

### Impute missing dates
data_wordreading_imputed_dates <- data_wordreading |> 
  group_by(school_ID) |>
  mutate(
    # Set missing dates to school median if missing
    wordreading_date_pre = if_else(
      is.na(wordreading_date_pre),
      median(wordreading_date_pre, na.rm = TRUE),
      wordreading_date_pre
    ),
    wordreading_date_post = if_else(
      is.na(wordreading_date_post),
      median(wordreading_date_post, na.rm = TRUE),
      wordreading_date_post
    )
  )|>
  ungroup() |>
  mutate(
    # Set pre measurement date to day before start of schoolyear if no premeasurement date
    wordreading_date_pre = if_else(
      is.na(wordreading_date_pre),
      day_before_start_of_schoolyear,
      wordreading_date_pre
    ),
    # Set post measurement date to day after end of schoolyear if no postmeasurement date
    wordreading_date_post = if_else(
      is.na(wordreading_date_post),
      day_after_end_of_schoolyear,
      wordreading_date_post
    )
  )

### Check for which respondents a date is missing
data_wordreading_imputed_dates |> 
  filter(
    is.na(wordreading_date_pre) |
      is.na(wordreading_date_post)
  )

## Save data ----
saveRDS(
  data_wordreading_imputed_dates,
  here("output/data_wordreading.rds")
)
