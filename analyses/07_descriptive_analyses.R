# Descriptive analyses
# General syntax ----
## Clear environment
# rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)
library(ggplot2)

# Load data ----
data <- readRDS(
  here::here('output/data_cleaned.rds')
)

# Descriptive analysis ----
## Summary ----
summary(data)

## participant table ----
data |> 
  group_by(
    # grade
  ) |> 
  summarise(
    N = n(),
    N_perc = 100 * N / nrow(data),
    age_M = mean(age),
    age_SD = sd(age),
    language_Dutch_perc = 100*sum(str_detect(language_home, 'Nederlands'))/n(),
    percentage_girl = 100*sum(gender == "girl")/n(),
    percentage_X = 100*sum(gender == "X")/n(),
    reading_fluency_M = mean(wordreading_score_pre, na.rm = T),
    reading_fluency_SD = sd(wordreading_score_pre, na.rm = T),
    reading_fluency_min = min(wordreading_score_pre, na.rm = T),
    reading_fluency_max = max(wordreading_score_post, na.rm = T)
  )

# Descriptives for Peter and Madelon ----
cors <- data |> 
  dplyr::select(
    contains('cii_'),
    contains('practice_accuracy'),
    contains('score') & !contains('mid')
  ) |> 
  cor(
    use = "pairwise.complete"
  ) |> round(4)
cors[upper.tri(cors)] <- NA
cors

data |> 
  dplyr::select(
    grade,
    contains('cii_'),
    contains('practice_accuracy'),
    contains('score') & !contains('mid')
  ) |> 
  filter(
    # grade == "grade_2"
  ) |> 
  summary()

data |> 
  dplyr::select(
    contains('cii_'),
    contains('practice_accuracy'),
    contains('score') & !contains('mid')
  ) |> 
  pivot_longer(
    contains('e'),
    names_to = "variable",
    values_to = "value"
  ) |> 
  mutate(
    variable = str_remove(variable, 'practice_') |> as_factor()
  ) |> 
  ggplot(
    aes(
      x = value,
    )
  ) +
  geom_histogram() +
  facet_wrap(
    ~variable,
    scales = 'free'
    )

data |> 
  ggplot(
    aes(
      x = practice_accuracy_firsttry
    )
  ) + geom_histogram() +
  facet_grid(~grade)

data |> 
  ggplot(
    aes(
      x = practice_cii_words_unique,
      y = practice_accuracy_firsttry,
    )
  ) + 
  geom_point() +
  facet_wrap(~grade)

data |> 
  ggplot(
    aes(
      x = practice_cii_words_accurate_firsttry,
      y = practice_cii_words_attempts,
    )
  ) + 
  geom_abline(intercept = 0, slope = 2, color = 'grey') +
  geom_point() +
  facet_wrap(~grade)



data |> 
  filter(
    grade == 'grade_4'
  ) |> 
  mutate(
    threshold = practice_accuracy_firsttry < .80
  ) |> pull(threshold)

