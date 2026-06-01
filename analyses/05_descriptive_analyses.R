# Descriptive analyses 
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

# Write cleaned data ----
saveRDS(
  data,
  here("output/data_cleaned.rds")
)




cors <- data |> 
  dplyr::select(
    contains('cii_'),
    practice_accuracy,
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
    practice_accuracy,
    contains('score') & !contains('mid')
  ) |> 
  filter(
    # grade == "grade_2"
  ) |> 
  summary()

data |> 
  dplyr::select(
    contains('cii_'),
    practice_accuracy,
    contains('score') & !contains('mid')
  ) |> 
  pivot_longer(
    contains('e'),
    names_to = "variable",
    values_to = "value"
  ) |> 
  ggplot(
    aes(
      x = value
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
      x = practice_cii_words_unique,
      y = practice_accuracy,
      color = practice_cii_sessions
    )
  ) + 
  geom_abline(intercept = 0, slope = 1, color = 'grey') +
  geom_point()

lm(
  wordreading_score_post ~ 
    wordreading_score_pre 
  # + practice_cii_words_unique + practice_accuracy
  + practice_cii_words_tries*practice_accuracy
  ,
  data = data |> 
    filter(
      # grade == "grade_4"
    ) |> 
    mutate(
      practice_cii_words_tries = practice_cii_words_tries - mean(practice_cii_words_tries, na.rm = T),
      practice_accuracy = practice_accuracy - mean(practice_accuracy, na.rm = T),
      
      # practice_cii_words_unique = practice_cii_words_unique / sd(practice_cii_words_unique, na.rm = T),
      # practice_accuracy = practice_accuracy / sd(practice_accuracy, na.rm = T)
    )
) |> summary()

