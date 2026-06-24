# Interaction analysis
# By Emiel Schoneveld

# General syntax ----
## Clear environment
# rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(here)
library(ggplot2)
library(lavaan)
library(lavaanPlot)
library(broom)
library(effectsize)
library(patchwork)

## Load functions and themes
source(
  here::here('analyses/00_functions_themes_analysis.R')
)

# Accuracy inspection ----
## Specify general model ----
mod_interaction <- 
  as.formula(
    '
    fluency_post ~ fluency_pre + time + words_exposures*accuracy_anytry
    '
  )

mod_interaction_fullmediation <- 
  as.formula(
    '
    fluency_post ~ fluency_pre + words_exposures*accuracy_anytry
    '
  )

mod_interaction_partial <- 
  as.formula(
    '
    fluency_post ~ fluency_pre + time + words_exposures*accuracy_anytry
    '
  )

## Fit models ----
### DMT ----
estimates_interaction_DMT <- data |> 
  mutate(
    fluency_pre = fluency_DMT_pre,
    fluency_post = fluency_DMT_post,
    words_exposures = words_exposures / 1000,
    words_exposures = words_exposures - mean(words_exposures),
    accuracy_anytry = accuracy_anytry - mean(accuracy_anytry, na.rm = T)
  ) |>
  group_by(grade) |>
  nest() |>
  mutate(
    fit          = map(data, ~ lm(mod_interaction_partial, data = .x)),
    results      = map(fit, ~ broom::tidy(.x, conf.int = TRUE, conf.level = 0.95)),
    standardized = map(fit, ~ effectsize::standardize_parameters(.x, ci = 0.95) |>
                         select(std_estimate = Std_Coefficient,
                                std_ci_low   = CI_low,
                                std_ci_high  = CI_high))
  ) |>
  unnest(c(results, standardized)) |>
  mutate(outcome = 'DMT', .before = 1)

### discrete ----
estimates_interaction_discrete <- data |> 
  mutate(
    fluency_pre = fluency_discrete_pre,
    fluency_post = fluency_discrete_post,
    words_exposures = words_exposures / 1000,
    words_exposures = words_exposures - mean(words_exposures),
    accuracy_anytry = accuracy_anytry - mean(accuracy_anytry, na.rm = T)
  ) |>
  group_by(grade) |>
  nest() |>
  mutate(
    fit          = map(data, ~ lm(mod_interaction_fullmediation, data = .x)),
    results      = map(fit, ~ broom::tidy(.x, conf.int = TRUE, conf.level = 0.95)),
    standardized = map(fit, ~ effectsize::standardize_parameters(.x, ci = 0.95) |>
                         select(std_estimate = Std_Coefficient,
                                std_ci_low   = CI_low,
                                std_ci_high  = CI_high))
  ) |>
  unnest(c(results, standardized)) |>
  mutate(outcome = 'discrete', .before = 1)

### serial ----
estimates_interaction_serial <- data |> 
  mutate(
    fluency_pre = fluency_serial_pre,
    fluency_post = fluency_serial_post,
    words_exposures = words_exposures / 1000,
    words_exposures = words_exposures - mean(words_exposures),
    accuracy_anytry = accuracy_anytry - mean(accuracy_anytry, na.rm = T)
  ) |>
  group_by(grade) |>
  nest() |>
  mutate(
    fit          = map(data, ~ lm(mod_interaction_fullmediation, data = .x)),
    results      = map(fit, ~ broom::tidy(.x, conf.int = TRUE, conf.level = 0.95)),
    standardized = map(fit, ~ effectsize::standardize_parameters(.x, ci = 0.95) |>
                         select(std_estimate = Std_Coefficient,
                                std_ci_low   = CI_low,
                                std_ci_high  = CI_high))
  ) |>
  unnest(c(results, standardized)) |>
  mutate(outcome = 'serial', .before = 1)

## Inspect results ----
### Bind results (add missing values explicitely)
estimates_interaction <- bind_rows(
  estimates_interaction_DMT,
  estimates_interaction_discrete,
  estimates_interaction_serial
) |> 
  dplyr::select(
    !c(data, fit)
  ) |> 
  mutate(
    significant = case_when(
      p.value < .05 ~ 'Significant',
      p.value >= .05 ~ 'Not significant'
    )
  ) |> 
  arrange(
    outcome, grade, term
  )

estimates_interaction_expanded <- estimates_interaction |> 
  full_join(
    tibble(
      outcome = c('discrete', 'serial', NA),
      term = c('time', NA, NA),
      grade = c('grade_2', 'grade_3', 'grade_4')
    ) |> 
      expand(
        outcome, term, grade
      ) |> 
      drop_na()
  )

### View results
estimates_interaction |> 
  filter(
    str_detect(term, ':')
  ) #|> View()

### Plot results
p_interaction_time <- estimates_interaction_expanded |> 
  filter(
    term == 'time'
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ time') +
  common_theme_labs +
  labs(x = NULL) +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_interaction_exposures <- estimates_interaction_expanded |> 
  filter(
    term == 'words_exposures'
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ words') +
  common_theme_labs +
  labs(x = NULL) +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_interaction_accuracy <- estimates_interaction_expanded |> 
  filter(
    term == 'accuracy_anytry'
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ accuracy') +
  common_theme_labs +
  labs(x = NULL) +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))

p_interaction_accuracyexposures <- estimates_interaction_expanded |> 
  filter(
    str_detect(term, ':')
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = significant,
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ accuracy:words') +
  common_theme_labs +
  labs(x = NULL) +
  scale_fill_manual(values = c('Significant' = sig_color, 'Not significant' = insig_color))


# Plot all results ----
p_interaction <- p_interaction_exposures +
  p_interaction_time +
  p_interaction_accuracy +
  p_interaction_accuracyexposures +
  plot_layout(
    axis = "collect",
    guides = "collect"
  )


