# Descriptive analyses 
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

## Visualizing parameters
### Define common theme
common_theme_labs <- theme_minimal() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
common_theme_nolabs <- theme_minimal() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Load data ----
data <- readRDS(
  here::here('output/data_cleaned.rds')
)

# Mediation analysis ----
## Specify model ----
### Full mediation model
mod_fullmediation <- '
# regression
## Predicting reading
fluency_post ~ fluency_pre + c(post_words_2, post_words_3, post_words_4)*words_exposures

## Predicting total words
words_exposures ~ c(words_time_2, words_time_3, words_time_4)*time

## Covariances
fluency_pre ~~ time + words_exposures

# variances
fluency_pre ~~ fluency_pre
fluency_post ~~ fluency_post
words_exposures ~~ words_exposures
time ~~ time
'

### Partial mediation addition
mod_addition_partialmediation <- '
fluency_post ~ c(post_time_2, post_time_3, post_time_4)*time
'

### Total effects addition
mod_addition_totaltimeeffect <- '
post_time_total_2 := (post_words_2*words_time_2) + post_time_2
post_time_total_3 := (post_words_3*words_time_3) + post_time_3
post_time_total_4 := (post_words_4*words_time_4) + post_time_4
'

## Fit models ----
### DMT ----
fit_partialmediation_DMT <- sem(
  c(
    mod_fullmediation,
    mod_addition_partialmediation,
    mod_addition_totaltimeeffect
    ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_DMT_pre,
      fluency_post = fluency_DMT_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

estimates_mediation_DMT <- parameterEstimates(
  fit_partialmediation_DMT,
  standardized = T
  ) |> 
  dplyr::select(
    lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
  ) |> 
  filter(
    op %in% c('~', ':=')
  ) |> 
  arrange(
    lhs, rhs, group
  ) |> 
  mutate(
    outcome = 'DMT',
    .before = 1
  )

### discrete ----
fit_partialmediation_discrete <- sem(
  c(
    mod_fullmediation,
    mod_addition_partialmediation,
    mod_addition_totaltimeeffect
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_discrete_pre,
      fluency_post = fluency_discrete_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

estimates_mediation_discrete <- parameterEstimates(
  fit_partialmediation_discrete,
  standardized = T
) |> 
  dplyr::select(
    lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
  ) |> 
  filter(
    op %in% c('~', ':=')
  ) |> 
  arrange(
    lhs, rhs, group
  ) |> 
  mutate(
    outcome = 'discrete',
    .before = 1
  )
 
### serial ----
fit_partialmediation_serial <- sem(
  c(
    mod_fullmediation,
    mod_addition_partialmediation,
    mod_addition_totaltimeeffect
  ),
  data = data |> 
    mutate(
      words_exposures = words_exposures / 1000,
      fluency_pre = fluency_serial_pre,
      fluency_post = fluency_serial_post,
    )
  , 
  missing = "FIML",
  group = "grade",
  group.label = c('grade_2', 'grade_3', 'grade_4'),
  cluster = "class_ID",
)

estimates_mediation_serial <- parameterEstimates(
  fit_partialmediation_serial,
  standardized = T
) |> 
  dplyr::select(
    lhs, op, rhs, group, label, est, se, pvalue, ci.lower, ci.upper, std.all
  ) |> 
  filter(
    op %in% c('~', ':=')
  ) |> 
  arrange(
    lhs, rhs, group
  ) |> 
  mutate(
    outcome = 'serial',
    .before = 1
  )

## Inspect results ----
### Bind results
estimates_mediation <- bind_rows(
    estimates_mediation_DMT,
    estimates_mediation_discrete,
    estimates_mediation_serial
  ) |> as_tibble() |> 
  mutate(
    sig = pvalue < .05,
    group = case_when(
      group == 1 ~ 'grade_2',
      group == 2 ~ 'grade_3',
      group == 3 ~ 'grade_4',
    )
  ) |> 
  rename(
    grade = group
  )

### View results
estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'post'),
    str_detect(rhs, 'time'),
    # str_detect(op, ':=')
  ) #|> View()

### Plot results
p_mediation_postwords <- estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'post'),
    str_detect(rhs, 'words'),
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = as_factor(sig),
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ words') +
  common_theme_labs


p_mediation_posttime <- estimates_mediation |> 
  filter(
    str_detect(op, '~'),
    str_detect(lhs, 'post'),
    str_detect(rhs, 'time'),
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = as_factor(sig),
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ time') +
  common_theme_nolabs


p_mediation_posttimetotal <- estimates_mediation |> 
  filter(
    str_detect(op, ':='),
    str_detect(label, 'post_time_total'),
  ) |> 
  mutate(
    grade = str_c(
      'grade_',
      str_sub(label, -1, -1)
    )
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = as_factor(sig),
      label = std.all |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ time (total effect)') +
  common_theme_nolabs

p_mediation <- p_mediation_postwords +
  p_mediation_posttime +
  p_mediation_posttimetotal +
  plot_layout(
    axis = 'collect',
    guides = 'collect'
    )

# Accuracy inspection ----
## Specify general model ----
mod_interaction <- 
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
    fit          = map(data, ~ lm(mod_interaction, data = .x)),
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
    fit          = map(data, ~ lm(mod_interaction, data = .x)),
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
    fit          = map(data, ~ lm(mod_interaction, data = .x)),
    results      = map(fit, ~ broom::tidy(.x, conf.int = TRUE, conf.level = 0.95)),
    standardized = map(fit, ~ effectsize::standardize_parameters(.x, ci = 0.95) |>
                         select(std_estimate = Std_Coefficient,
                                std_ci_low   = CI_low,
                                std_ci_high  = CI_high))
  ) |>
  unnest(c(results, standardized)) |>
  mutate(outcome = 'serial', .before = 1)

## Inspect results ----
### Bind results
estimates_interaction <- bind_rows(
  estimates_interaction_DMT,
  estimates_interaction_discrete,
  estimates_interaction_serial
) |> 
  dplyr::select(
    !c(data, fit)
  ) |> 
  mutate(
    sig = p.value < .05
  ) |> 
  arrange(
    outcome, grade, term
  )

### View results
estimates_interaction |> 
  filter(
    str_detect(term, ':')
  ) #|> View()

### Plot results
p_interaction_exposures <- estimates_interaction |> 
  filter(
    term == 'words_exposures'
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = as_factor(sig),
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ words') +
  common_theme_labs

p_interaction_accuracy <- estimates_interaction |> 
  filter(
    term == 'accuracy_anytry'
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = as_factor(sig),
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ accuracy') +
  common_theme_nolabs

p_interaction_accuracyexposures <- estimates_interaction |> 
  filter(
    str_detect(term, ':')
  ) |> 
  ggplot(
    aes(
      x = grade,
      y = outcome,
      fill = as_factor(sig),
      label = std_estimate |> round(2)
    )
  ) +
  geom_tile(color = 'black') +
  geom_text() +
  ggtitle('Post ~ accuracy:words') +
  common_theme_nolabs

p_interaction <- p_interaction_exposures +
  p_interaction_accuracy +
  p_interaction_accuracyexposures +
  plot_layout(
    axis = "collect",
    guides = "collect"
  )
  

# Plot all results ----
p_mediation_formatted <- p_mediation +
  plot_annotation(
  title = 'Mediation analysis',
  subtitle = 'Using the SEM model'
)
p_interaction_formatted <- p_interaction +
  plot_annotation(
    title = 'Interaction analysis',
    subtitle = 'Using the OLS model'
  )
