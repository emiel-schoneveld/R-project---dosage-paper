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
## Student characteristics data
data_characteristics_old <- read_xlsx(
  here('input/achtergrondgegevens.xlsx')
)

# Transform data_characteristics ----
## Rename and transform columns
data_characteristics <- data_characteristics_old |> 
  rename(
    student_ID = Leerlingnummer,
    grade = Leerjaar,
    condition = Conditie,
    class_name = Groep,
    gender = Geslacht,
    dateofbirth = Geboortedatum,
    language_home = Thuistaal,
    language_preference = Voorkeurstaal,
    school_ID = school
  ) |>
  mutate(
    grade = as.numeric(grade) - 2,
    grade = str_c('grade_', grade),
    condition = recode(
      condition,
      'flitsen' = "LED",
      'flitsen&rijtjes' = 'combination',
      'rijtjes' = 'listreading',
      'controle' = 'control'
    ),
    gender = recode(
      gender,
      "jongen" = "boy",
      'meisje' = "girl",
      'X' = 'X'
    ),
    class_ID = paste0(
      school_ID, '-', class_name
    )
  )

## Save data ----
saveRDS(
  data_characteristics,
  here("output/data_characteristics.rds")
)


